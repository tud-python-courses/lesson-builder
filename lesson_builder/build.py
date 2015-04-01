"""build script for our lesson repositories"""
import json
import os
import itertools
import logging
import subprocess
import collections
from . import github, config, misc


LOGGER = logging.getLogger(__name__)
TEX_OUTPUT = {
    'htlatex': 'html',
    'pdflatex': 'pdf',
    'xelatex': 'pdf',
    'hevea': 'html'
}
PATHS = {
    'app': config.BASE_DIRECTORY
}
ADDITIONAL_COMMAND_OPTIONS = {
    'htlatex': ('-halt-on-error', '-interaction=nonstopmode',),
    'pdflatex': ('-halt-on-error', '-interaction=nonstopmode',),
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
    def __init__(self, name, command, target_dir, source_dir, files):
        self.name = name
        if command not in TEX_OUTPUT:
            raise ValueError('Unrecognized TeX command {}'.format(command))
        self.command = command
        self.target_dir = target_dir
        self.source_dir = source_dir
        self.files = files

    @classmethod
    def relative(cls, target_dir, source_dir, *args, base_dir='', **kwargs):
        return cls(
            *args,
            target_dir=os.path.join(base_dir, target_dir),
            source_dir=os.path.join(base_dir, source_dir),
            **kwargs
        )

    def output_to_location(self, file):
        return {
            'hevea': ('-o', '{}/{}'.format(self.target_dir, output_from_command(file, self.command)))
        }.get(self.command, ('-output-directory', self.target_dir))

    def abuild_file(self, file, cwd='.', env=os.environ):
        """
        Start a build of a single file and return the running process wrapper

        :param file: file to build
        :return:
        """
        source = os.path.join(self.source_dir, file)

        if not os.path.exists(self.target_dir):
            os.makedirs(self.target_dir)

        return file, misc.Popen(
            (self.command,)
            + self.output_to_location(file)
            + ADDITIONAL_COMMAND_OPTIONS.get(self.command, ()) + (source,),
            env=env,
            cwd=cwd
        )

    def abuild(self, cwd='.', env=os.environ):
        """
        Start this build and return a tuple of the running processes

        :return:
        """
        if not os.path.exists(self.target_dir):
            os.makedirs(self.target_dir)
        return tuple(self.abuild_file(file, cwd=cwd, env=env) for file in self.files)


class Include:
    """
    Abstract include
    """
    def __init__(self, repository=None, directory=None):
        self.repository = repository
        self.directory = directory

    @classmethod
    def from_config(cls, git_url, directory=None):
        """
        Alternative constructor from the config dict:

        :param git_url: url
        :param directory:
        :return:
        """
        return cls(
            github.GitRepository.from_url(git_url),
            directory
        )

    @classmethod
    def relative_to(
            cls,
            base_dir,
            git_url=None,
            directory=None
    ):
        """
        Alternative constructor from the config arguments
        relative to a directory

        :param base_dir:
        :param git_url:
        :param directory:
        :return:
        """
        return cls.from_config(
            git_url,
            os.path.join(base_dir, directory)
        )


def refresh_includes(includes):
    """
    Pull/clone the include repositories and return those that succeeded

    :param includes:
    :return:
    """

    wait = lambda a: (a[0], a[1].wait())

    code_is_0 = lambda a: a[1] == 0

    # calculating pull results
    pull_results = map(
        wait,
        # starting pulls
        tuple((i, i.repository.apull(i.directory)) for i in includes)
    )
    pull_success, pull_failed = partition(code_is_0, pull_results)
    clone_results = map(
        wait,
        tuple(
            (include, include.repository.aclone(include.directory))
            for include, status in pull_failed
        )
    )

    clone_success, clone_failed = partition(
        code_is_0,
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


def refresh_tex_includes(repo: github.GitRepository, directory):
    repo.refresh(directory)


def build_includes(conf: dict, wd: str, env: dict) -> ((Build, subprocess.Popen), ...):
    includes_folder = os.path.join(wd, conf.get('includes_directory', ''))

    includes = tuple(
        Include.relative_to(includes_folder, **i) for i in conf.get('include', ())
    )

    building = tuple(itertools.chain.from_iterable(
        abuild_directory(include.directory, env=env)
        for include in refresh_includes(includes)
    ))

    finished = finish_builds(building)
    return finished


def handle_env_conf(conf: dict, env: dict) -> dict:
    if 'tex_include' in conf:
        template_dir = conf['tex_include']['directory'].format(**PATHS)
        repo = github.GitRepository.from_url(conf['tex_includes']['clone_url'])
        refresh_tex_includes(repo, template_dir)
        return collections.ChainMap(
            {
                'TEXINPUTS': '.:{}//:'.format(template_dir)
            },
            env
        )
    else:
        return env


def abuild_directory(wd: str, env: dict=os.environ) -> ((Build, subprocess.Popen), ...):
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

    env = handle_env_conf(conf, env)

    finished_includes = build_includes(conf, wd, env)

    builds = (
        Build.relative(name, base_dir=wd, **b_conf)
        for name, b_conf in conf.get('builds', {}).items()
    )

    # this catches any FileNotFoundError's and logs them so we still build the good configs
    building_own = tuple(catch_abuilds(builds, env=env))

    return finished_includes + building_own


def catch_abuilds(builds, env=os.environ):
    for build in builds:
        try:
            yield build, build.abuild(cwd=build.source_dir, env=env)
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


def finish_builds(builds):
    """
    Wait for the executing builds or kill them

    :param builds:
    :return:
    """

    for file, process in builds:
        if process.poll() is None:
            try:
                process.wait(config.BUILD_TIMEOUT)
            except subprocess.TimeoutExpired:
                process.kill()
    return builds


def build_and_report(wd):
    """
    Build a directory and return some printable information on how it went

    :param wd: working directory
    :return:
    """
    building = abuild_directory(wd)

    def format_status(process):
        code = process.returncode
        if code == 0:
            return code
        else:
            return '{}\n{}'.format(
                code, misc.error_capture_format(
                    ('stdout', process.stdout.read().decode() if process.stdout else ''),
                    ('stderr', process.stderr.read().decode() if process.stderr else '')
                )
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
                        file,
                        output_from_command(file, build.command),
                        format_status(process)
                    )
                    for file, process in (
                        finish_builds(building_files)
                    )
                )
            )
            for build, building_files in builds
        )

    return print_finished(building)
