#!/usr/local/bin/python3

"""
CGI Script handling github webhooks

this script mainly takes care of in- and output
and hands off most of the actual work to the lesson_builder package
"""

import json
import cgi
import logging
import os

import cgitb
cgitb.enable()


APP_DIRECTORY = '/Users/justusadam/projects/Ruby/lesson-builder'


def relative(*args, to=APP_DIRECTORY):
    """
    path relative to the APP_DIRECTORY or any other

    convenience wrapper around os.path.join

    :param args:
    :param to:
    :return:
    """
    return os.path.join(to, *args)


REPOS_DIRECTORY = relative('repos')
WATCH_CONF_NAME = 'watch_conf.json'
SKIP_STRINGS = {'[skip build]', '[build skip]'}


try:
    # I don't know why I try to import here, but might as well
    import lesson_builder
except ImportError:
    import sys
    sys.path.append(APP_DIRECTORY)
    import lesson_builder


github = lesson_builder.github
build = lesson_builder.build

lesson_builder.config.DEBUG = False

__author__ = 'Justus Adam'
__version__ = '0.1'


def apply(function, iterable):
    """
    Apply function to all elements of iterable

    :param function:
    :param iterable:
    :return:
    """
    for i in iterable:
        function(i)


def handle_push(event, raw_data):
    """
    Handle the payload received and yield a somewhat useful response

    :param event: github.Event instace
    :param raw_data: raw bytes of the message
    :return:
    """
    payload = event.payload

    yield "Content-Type: \"text/html\""
    yield ""

    for skip_string in SKIP_STRINGS:
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
        if not github.verify(
                mapped[repo_name],
                raw_data,
                os.environ['HTTP_HEADERS'],
                os.environ['HTTP_USER_AGENT']
        ):
            yield "Unknown requester"
            raise StopIteration
        if 'id' not in mapped[repo_name]:
            mapped[repo_name]['id'] = repo['id']
            with open(conf_path, mode='w') as f:
                json.dump(list(mapped.values()), f, indent=4)

        repo_path = relative(mapped[repo_name]['directory'], to=REPOS_DIRECTORY)
        repo_obj = github.GitRepository(repo_name)

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
    """
    Clone a repository and wait for it to finish

    :param repo: repository
    :param path:
    :return: returncode
    """
    return repo.aclone(path).wait()


def try_pull(repo, path):
    """
    Pull the repository and clone it if it fails

    :param repo:
    :param path:
    :return: returncode
    """
    code = repo.apull(path).wait()
    if code != 0:
        code = try_clone(repo, path)
    return code


def handle_ping(event):
    pass


def do(payload):
    """
    Do what needs to be done

    parse and handle the payload, print the results

    :param payload:
    :return: None
    """
    event = github.Event.from_request(json.loads(payload))

    if event.type == github.PUSH:
        apply(print, handle_push(event, payload))
    elif event.type == github.PING:
        apply(print, handle_ping(event))


def hello():
    print('<h1>This is the webhook receiver</h1>')
    print('I dont think you\'ll want to reach me this way.')


def main():
    """Main function"""
    payload = cgi.FieldStorage().read_lines_to_eof()
    if not payload:
        hello()
    else:
        do(payload)

if __name__ == '__main__':
    main()
    # cgi.test()