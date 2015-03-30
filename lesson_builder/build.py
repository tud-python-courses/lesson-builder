"""build script for our lesson repositories"""
import json
import os
import subprocess
import itertools
import logging
from . import github, config


LOGGER = logging.getLogger(__name__)
TEX_OUTPUT = {
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

        return file, subprocess.Popen(
            (
                self.command,
                '-halt-on-error',
                '-interaction=nonstopmode',
                '-output-directory', self.target_dir,
                source
            )
        )

    def abuild(self):
        """
        Start this build and return a tuple of the running processes

        :return:
        """
        if not os.path.exists(self.target_dir):
            os.makedirs(self.target_dir)
        return tuple(self.abuild_file(file) for file in self.files)


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
            github.GitRepository.from_url(git_url),
            directory,
            target_dir
        )

    @classmethod
    def relative_to(
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
        tuple((i, i.repository.apull(i.directory)) for i in includes)
    )
    pull_success, pull_failed = partition(lambda a: a[1] == 0, pull_results)
    clone_results = map(
        lambda a: (a[0], a[1].wait()),
        tuple(
            (include, include.repository.aclone(include.directory))
            for include, status in pull_failed)
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


def read_conf(wd):
    """
    Open the config file and return the json stuff within

    :param wd:
    :return:
    """
    with open(os.path.join(wd, config.CONFIG_NAME)) as file:
        return json.load(file)


def abuild_directory(wd):
    """
    Asynchronously build whats defined in the build_conf in that directory
    returns an empty tuple if something fails

    :param wd: working directory
    :return: executing builds
    """
    try:
        conf = read_conf(wd)
    except ValueError as e:
        LOGGER.error(
            'Could not parse config {} due to error {}'.format(
                os.path.join(wd, config.CONFIG_NAME), e)
        )
        return ()
    except FileNotFoundError as e:
        LOGGER.error(e)
        return ()

    includes_folder = os.path.join(wd, conf.get('includes_directory', ''))

    includes = tuple(
        Include.relative_to(includes_folder, **i) for i in conf.get('include', ())
    )

    building_includes = tuple(itertools.chain.from_iterable(
        abuild_directory(include.directory)
        for include in refresh_includes(includes)
    ))

    builds = tuple(
        Build(name, base_dir=wd, **b_conf)
        for name, b_conf in conf.get('builds', {}).items()
    )

    # this catches any FileNotFoundError's and logs them so we still build the good configs
    building_own = tuple(catch_abuilds(builds))

    return building_includes + building_own


def catch_abuilds(builds):
    for build in builds:
        try:
            yield build, build.abuild()
        except FileNotFoundError as e:
            LOGGER.critical(
                'Build {} could not execute due to {}'.format(build.name, e)
            )


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
                    code = process.wait(0)
                else:
                    code = process.wait(config.BUILD_TIMEOUT)
            except subprocess.TimeoutExpired:
                waited = True
                code = process.kill()

            if code == 0:
                yield process, code
            else:
                yield process, '{} and \nstdout: {}\n stderr: {}'.format(
                    code,
                    process.stdout.read(),
                    process.stderr.read()
                )


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
