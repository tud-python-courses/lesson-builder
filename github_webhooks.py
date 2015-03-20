#!/usr/local/bin/python3

import functools
import json
import cgi
import cgitb
import logging
import os

cgitb.enable()


APP_DIRECTORY = '/Users/justusadam/projects/Ruby/lesson-builder'
WATCH_CONF_NAME = 'watch_conf.json'
skip_strings = {'[skip build]', '[build skip]'}

try:
    import build
except ImportError:
    import sys
    sys.path.append(APP_DIRECTORY)
    import build

__author__ = 'Justus Adam'
__version__ = '0.1'


relative = functools.partial(os.path.join, APP_DIRECTORY)


def apply(function, iterable):
    for i in iterable:
        function(i)


def handle_payload(payload):
    yield "Content-Type: \"text/html\""
    yield ""

    for skip_string in skip_strings:
        if skip_string in payload['head_commit']['message']:
            yield "Commit message demands skip"
            raise StopIteration

    conf_path = relative(WATCH_CONF_NAME)

    with open(conf_path) as f:
        known = json.load(f)

    repo = payload['repository']
    repo_name = repo['name']

    mapped = {
        a['name']: a for a in known
    }

    if repo_name not in mapped:
        yield "Repository not on watchlist"
    else:
        if 'id' not in mapped[repo_name]:
            mapped[repo_name]['id'] = repo['id']
            with open(conf_path, mode='w') as f:
                json.dump(list(mapped.values()), f, indent=4)

        repo_path = relative(mapped[repo_name]['directory'])
        repo_obj = build.GitRepository(repo_name)

        if not os.path.exists(repo_path):
            os.makedirs(repo_path)
            code = try_clone(repo_obj, repo_path)
        else:
            code = try_pull(repo_obj, repo_path)

        if code != 0:
            yield "Clone failed with code {}".format(code)
        else:
            logging.getLogger(__name__).debug(build.build_and_report(repo_path))
            yield "Build finished"


def try_clone(repo, path):
    return repo.aclone(path).wait()


def try_pull(repo, path):
    code = repo.apull(path).wait()
    if code != 0:
        code = try_clone(repo, path)
    return code


def build_all(cwd):
    build.build_and_report(cwd)


def do(payload):
    data = json.loads(payload)

    apply(print, handle_payload(data))


def main():
    payload = cgi.FieldStorage().read_lines_to_eof()
    do(payload)

if __name__ == '__main__':
    main()
    # cgi.test()