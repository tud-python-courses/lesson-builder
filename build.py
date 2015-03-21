"""build script for our lesson repositories"""
import json
import os
import re
import subprocess
import itertools
import logging


DEBUG = True


CONFIG_NAME = 'build_conf.json'
BUILD_TIMEOUT = 2 * 60  # seconds

LOGGER = logging.getLogger(__name__)
logging.basicConfig(
    format='[%(levelname)10s]:%(message)s',
    filename='builder.log'
)

ERROR_LOG_FILE = 'builder-error.log'
INFO_LOG_FILE = 'builder.log'

TEX_OUTPUT = {
    'htlatex': 'html',
    'pdflatex': 'pdf',
    'xelatex': 'pdf'
}


def Popen(*args, **kwargs):
    """
    subprocess.Popen constructor that sets in- and output depending on whether
    this is executed in DEBUG mode

    :param args: constructor args
    :param kwargs: constructor kwargs
    :return: subprocess.Popen instance
    """
    if DEBUG:
        return subprocess.Popen(*args, **kwargs)
    else:
        return subprocess.Popen(
            *args,
            stderr=open(ERROR_LOG_FILE, mode='w+'),
            stdout=open(INFO_LOG_FILE, mode='w+'),
            **kwargs
        )


def partition(condition, iterable, output_class=tuple):
    """
    split an iterable into two according to a function evaluating to either
     true or false on each element

    :param condition: boolean function
    :param iterable: iterable to split
    :param output_class: type of the returned iterables
    :return: two iterables
    """
    true = []
    false = []

    for i in iterable:
        if condition(i):
            true.append(i)
        else:
            false.append(i)

    return output_class(true), output_class(false)


class Build:
    """
    Abstracts builds
    """
    def __init__(self, name, command, target_dir, source_dir, files, base_dir=''):
        self.name = name
        if command not in TEX_OUTPUT:
            raise ValueError('Unrecognized TeX command {}'.format(command))
        self.command = command
        self.target_dir = os.path.join(base_dir, target_dir)
        self.source_dir = os.path.join(base_dir, source_dir)
        self.files = files

    def abuild_file(self, file):
        """
        Start a build of a single file and return the running process wrapper

        :param file: file to build
        :return:
        """
        source = os.path.join(self.source_dir, file)

        if not os.path.exists(self.target_dir):
            os.makedirs(self.target_dir)

        return file, Popen(
            (
                self.command,
                '-halt-on-error',
                '-interaction', 'nonstopmode',
                '-output-directory', self.target_dir,
                source
            )
        )

    def abuild(self):
        """
        Start this build and return a tuple of the running processes

        :return:
        """
        return tuple(self.abuild_file(file) for file in self.files)


class GitRepository:
    """Abstracts a git repository"""
    GIT_URL_REGEX = re.compile(
        '^\w+://(?P<host>\w+\.\w+)/(?P<name>[\w_-]+/[\w_-]+).git$'
    )

    host_urls = {
        'github.com': 'https://github.com'
    }

    """Model for git repository"""
    def __init__(self, name, host='github.com'):
        self.name = name
        self.host = host

    @classmethod
    def from_url(cls, url):
        """
        Alternative constructor from the clone url
        :param url:
        :return:
        """
        match = re.match(cls.GIT_URL_REGEX, url)

        return cls(
            name=match.group('name'),
            host=match.group('host')
        )


    @staticmethod
    def dir_from_url(url):
        """
        Construct the standard directory a git repository would have

        :param url: the url of the git repository
        :return: directory name
        """
        return url.rsplit('.', 1)[0].rsplit('/', 1)[1]

    def apull(self, directory='.'):
        """
        Perform git pull for this repository asynchronous

        :param directory: directory to pull into
        :return: Executing process
        """
        return Popen(('git', '-C', directory, 'pull'))

    def aclone(self, into_dir=None):
        """
        Perform git clone for this repository asynchronous

        :param into_dir: directory in which to clone
        :return: Executing process
        """
        if not self.name:
            raise ValueError('Cannot clone without url')
        url = self.host_urls[self.host] + '/' + self.name + '.git'

        action = ['git', 'clone', url]
        if into_dir:
            action.append(into_dir)
        return Popen(action)

    def refresh(self):
        """
        Pull or clone as appropriate (not async)

        :return: None
        """
        proc = self.apull()
        code = proc.wait()
        if not code == 0:
            proc2 = self.aclone()
            code2 = proc2.wait()

            if not code2 == 0:
                LOGGER.error(
                    'Pull and clone failed with {}'.format(code2)
                )
                return False
        return True


class Include:
    """
    Abstract include
    """
    def __init__(self, repository=None, directory=None, target_dir=None):
        self.repository = repository
        self.directory = directory
        self.target_dir = target_dir

    @classmethod
    def from_config(cls, git_url, directory=None, target_dir=None):
        """
        Alternative constructor from the config dict:

        :param git_url: url
        :param directory:
        :param target_dir:
        :return:
        """
        return cls(
            GitRepository.from_url(git_url),
            directory,
            target_dir
        )

    @classmethod
    def realtive_to(
            cls,
            base_dir,
            git_url=None,
            directory=None,
            target_dir=None
    ):
        """
        Alternative constructor from the config arguments
        relative to a directory

        :param base_dir:
        :param git_url:
        :param directory:
        :param target_dir:
        :return:
        """
        return cls.from_config(
            git_url,
            os.path.join(base_dir, directory),
            os.path.join(base_dir, target_dir)
        )


def refresh_includes(includes):
    """
    Pull/clone the include repositories and return those that succeeded

    :param includes:
    :return:
    """
    # calculating pull results
    pull_results = map(
        lambda a: (a[0], a[1].wait()),
        # starting pulls
        ((r, r.repository.apull()) for r in includes)
    )
    pull_success, pull_failed = partition(lambda a: a[1] == 0, pull_results)
    clone_results = map(
        lambda a: (a[0], a[1].wait()),
        tuple(map(lambda a: (a[0], a[0].repository.aclone()), pull_failed))
    )

    clone_success, clone_failed = partition(
        lambda a: a[1] == 0,
        clone_results
    )

    for include, code in clone_failed:
        LOGGER.error(
            'Pull and clone failed for {} with code {}'.format(
                include.repository.name, code
            )
        )

    first_only = lambda a: a[0]

    return tuple(
        itertools.chain(
            map(first_only, pull_success), map(first_only, clone_success)
        )
    )

    # logger.info(
    #     'Building includes: {}'.format(
    #         ', '.join(directories)
    #     )
    # )
    #
    # for directory in directories:
    #     build_directory(directory)


def read_conf(wd):
    """
    Open the config file and return the json stuff within

    :param wd:
    :return:
    """
    with open(os.path.join(wd, CONFIG_NAME)) as file:
        return json.load(file)


def abuild_directory(wd):
    """
    Asynchronously build whats defined in the build_conf in that directory

    :param wd: working directory
    :return: executing builds
    """
    try:
        conf = read_conf(wd)
    except ValueError as e:
        LOGGER.error(
            'Could not parse config {} due to error {}'.format(
                os.path.join(wd, CONFIG_NAME), e)
        )
        return ()
    except FileNotFoundError as e:
        LOGGER.error(e)
        return ()

    includes_folder = os.path.join(wd, conf.get('includes_directory', ''))

    includes = tuple(
        Include.realtive_to(includes_folder, **i) for i in conf.get('include', ())
    )

    building_includes = tuple(itertools.chain.from_iterable(
        abuild_directory(include.directory)
        for include in refresh_includes(includes)
    ))

    builds = tuple(
        Build(name, base_dir=wd, **b_conf)
        for name, b_conf in conf.get('builds', {}).items()
    )

    building_own = tuple((b, b.abuild()) for b in builds)

    return building_includes + building_own


def output_from_command(file, command):
    """
    Construct the default file name for the output of an executed command

    :param file: name of input file
    :param command: executed command
    :return: string name
    """
    return file.rsplit('.', 1)[0] + '.' + TEX_OUTPUT[command]


def build_and_report(wd):
    """
    Build a directory and return some printable information on how it went

    :param wd: working directory
    :return:
    """
    building = abuild_directory(wd)

    def finish_builds(builds):
        """
        Wait for the executing builds and collect the return codes

        :param builds:
        :return:
        """
        waited = False
        for file, process in builds:
            try:
                if waited:
                    yield file, process.wait(0)
                else:
                    yield file, process.wait(BUILD_TIMEOUT)
            except subprocess.TimeoutExpired:
                waited = True
                yield file, process.kill()

    def print_finished(builds):
        """
        Transform the finished builds into some useful printable information
        about their status

        :param builds:
        :return:
        """
        return '\n'.join(
            'Build {} with {} files:\n      {}'.format(
                build.name, len(building_files), '\n      '.join(
                    '{}   ->   {}    with code {}'.format(
                        file, output_from_command(file, build.command), status
                    )
                    for file, status in (
                        finish_builds(building_files)
                    )
                )
            )
            for build, building_files in builds
        )

    return print_finished(building)


def main():
    """Main function"""
    import sys
    script, wd, *l = sys.argv
    print(build_and_report(wd))

if __name__ == '__main__':
    main()
