"""build script for our lesson repositories"""
import functools
import json
import os
import subprocess
import itertools
import logging
import io


DEBUG = True


CONFIG_NAME = 'build_conf.json'

logger = logging.getLogger(__name__)
logging.basicConfig(format='[%(levelname)10s]:%(message)s')

LATEX_OUTPUT = {
    'htlatex': 'html',
    'pdflatex': 'pdf',
    'xelatex': 'pdf'
}


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
    def __init__(self, name, command, target_dir, source_dir, files, base_dir=''):
        self.name = name
        self.command = command
        self.target_dir = os.path.join(base_dir, target_dir)
        self.source_dir = os.path.join(base_dir, source_dir)
        self.files = files

    def abuild_file(self, file):
        source = os.path.join(self.source_dir, file)

        if not DEBUG:
            cmd = functools.partial(subprocess.Popen, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            cmd = subprocess.Popen
        return file, cmd(
            (
                self.command,
                '-halt-on-error',
                '-interaction', 'nonstopmode',
                '-output-directory', self.target_dir,
                source
            )
        )

    def abuild(self):
        return tuple(self.abuild_file(file) for file in self.files)


class GitRepository:
    """Model for git repository"""
    def __init__(self, url, directory):
        self.url = url
        if not directory:
            if not url:
                raise ValueError('Need either url or directory')
            directory = self.dir_from_url(url)
        self.directory = directory

    @staticmethod
    def dir_from_url(url):
        """
        Construct the standard directory a git repository would have

        :param url: the url of the git repository
        :return: directory name
        """
        return url.rsplit('.', 1)[0].rsplit('/', 1)[1]

    def apull(self):
        """
        Perform git pull for this repository asynchronous

        :return: Executing process
        """
        return subprocess.Popen(('git', '-C', self.directory, 'pull'))

    def aclone(self):
        """
        Perform git clone for this repository asynchronous

        :return: Executing process
        """
        if not self.url:
            raise ValueError('Cannot clone without url')
        action = ['git', 'clone', self.url]
        if self.directory:
            action.append(self.directory)
        return subprocess.Popen(action)

    def refresh(self):
        proc = self.apull()
        code = proc.wait()
        if not code == 0:
            proc2 = self.aclone()
            code2 = proc2.wait()

            if not code2 == 0:
                logger.error(
                    'Pull and clone failed with {}'.format(code2)
                )
                return False
        return True


class Include:
    def __init__(self, git_url=None, directory=None, target_dir=None):
        self.git_url = git_url
        self.directory = directory
        self.target_dir = target_dir

    def repo(self):
        return GitRepository(self.git_url, self.directory)

    @classmethod
    def realtive_to(cls, base_dir, git_url=None, directory=None, target_dir=None):
        return cls(
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
    repos = tuple(i.repo() for i in includes)

    # calculating pull results
    pull_results = map(
        lambda a: (a[0], a[1].wait()),
        # starting pulls
        ((r, r.apull()) for r in repos)
    )
    pull_success, pull_failed = partition(lambda a: a[1] == 0, pull_results)
    clone_results = map(
        lambda a: (a[0], a[1].wait()),
        tuple(map(lambda a: (a[0], a[0].aclone()), pull_failed))
    )

    clone_success, clone_failed = partition(
        lambda a: a[1] == 0,
        clone_results
    )

    for repo, code in clone_failed:
        logger.error(
            'Pull and clone failed for {} with code {}'.format(
                repo.url, code
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
    with open(os.path.join(wd, CONFIG_NAME)) as file:
        return json.load(file)


def abuild_directory(wd):
    try:
        conf = read_conf(wd)
    except ValueError as e:
        logger.error(
            'Could not parse config {} due to error {}'.format(
                os.path.join(wd, CONFIG_NAME), e)
        )
        return ()
    except FileNotFoundError as e:
        logger.error(e)
        return ()

    includes_folder = os.path.join(wd, conf.get('includes_directory', ''))

    includes = tuple(
        Include.realtive_to(includes_folder, **i) for i in conf.get('include', ())
    )

    building_includes = tuple(itertools.chain.from_iterable(
        abuild_directory(include.directory)
        for include in refresh_includes(includes)
    ))

    builds = tuple(Build(name, base_dir=wd, **b_conf) for name, b_conf in conf.get('builds', {}).items())

    building_own = tuple((b, b.abuild()) for b in builds)

    return building_includes + building_own


def output_from_command(file, command):
    return file.rsplit('.', 1)[0] + '.' + LATEX_OUTPUT[command]


def build_and_report(wd):
    building = abuild_directory(wd)

    def finish_builds(builds):
        for file, process in builds:
            yield file, process.wait()

    def print_finished(builds):
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

    print(
        print_finished(
            building
        )
    )


def main():
    import sys
    script, wd, *l = sys.argv
    build_and_report(wd)

if __name__ == '__main__':
    main()
