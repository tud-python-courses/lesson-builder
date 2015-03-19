import build

__author__ = 'Justus Adam'
__version__ = '0.1'


def build_repo(cwd, git_urls):
    conf = build.read_conf(cwd)
    matching = (
        build.Include(**a) for a in
        filter(lambda a: a.get('git_url', '') in git_urls, conf['include'])
    )

    for i in matching:
        build.build_and_report(i.directory)


def handle_payload(json_object):
    pass


def build_all(cwd):
    build.build_and_report(cwd)