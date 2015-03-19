import build

__author__ = 'Justus Adam'
__version__ = '0.1'


def build_repo(cwd, git_url):
    conf = build.read_conf(cwd)
    matching = (
        build.Include(**a) for a in
        filter(lambda a: a.get('git_url', '') == git_url, conf['include'])
    )

    for i in matching:
        build.build_and_report(i.directory)


def build_all(cwd):
    build.build_and_report(cwd)