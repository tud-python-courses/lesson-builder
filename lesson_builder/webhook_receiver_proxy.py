"""
CGI Script handling github webhooks

this script mainly takes care of in- and output
and hands off most of the actual work to the build module
"""

import sys
import json
import logging
import os
import cgi
import html

from . import github, build, config


APP_DIRECTORY = config.BASE_DIRECTORY

_default_data_directory = '.data'

__author__ = 'Justus Adam'
__version__ = '0.1'


def relative(*args, to=APP_DIRECTORY):
    """
    path relative to the APP_DIRECTORY or any other

    convenience wrapper around os.path.join

    :param args:
    :param to:
    :return:
    """
    return os.path.abspath(os.path.join(to, *args))


REPOS_DIRECTORY = relative('repos')
WATCH_CONF_NAME = 'watch_conf.json'
SKIP_STRINGS = {'[skip build]', '[build skip]'}


config.DEBUG = False


def apply(function, iterable):
    """
    Apply function to all elements of iterable

    :param function:
    :param iterable:
    :return:
    """
    for i in iterable:
        function(i)


def get_watch_conf():
    conf_path = relative(WATCH_CONF_NAME)
    with open(conf_path) as f:
        return json.load(f)


def write_watch_conf(data):
    conf_path = relative(WATCH_CONF_NAME)
    with open(conf_path, mode='w') as f:
        json.dump(data, f, indent=4)


def handle_push(event, raw_data):
    """
    Handle the payload received and yield a somewhat useful response

    :param event: github.Event instace
    :param raw_data: raw bytes of the message
    :return:
    """
    payload = event.payload

    for skip_string in SKIP_STRINGS:
        if skip_string in payload['head_commit']['message']:
            yield "Commit message demands skip"
            raise StopIteration

    known = get_watch_conf().get('watched', ())

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
                get_header(SIGNATURE),
                os.environ['HTTP_USER_AGENT']
        ):
            yield "Unknown requester"
            raise StopIteration
        if 'id' not in mapped[repo_name]:
            mapped[repo_name]['id'] = repo['id']
            write_watch_conf(list(mapped.values()))

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
    logging.getLogger(__name__).info(
        'Received ping event with payload\n{}'.format(event.payload)
    )

    directory = get_watch_conf().get('data_directory', _default_data_directory)
    hook_id = event.payload['hook_id']
    file_path = relative(directory, 'hook_{}.conf.json'.format(hook_id))
    if os.path.exists(file_path):
        mode = 'w'
    else:
        mode = 'w+'
    with open(file_path, mode=mode) as file:
        json.dump(event.payload['hook'], fp=file, indent=4)
    yield 'Ping Received'


def do(payload):
    """
    Do what needs to be done

    parse and handle the payload, print the results

    :param payload:
    :return: None
    """
    payload = json.loads(payload)
    event = github.Event(
        type=get_header(EVENT_TYPE),
        repo=github.GitRepository.from_json(payload['repository']),
        payload=payload
    )

    if event.type == github.PUSH:
        return handle_push(event, payload)
    elif event.type == github.PING:
        return handle_ping(event)
    else:
        logging.getLogger(__name__).error(
            'Unknown event {} with payload {}'.format(event, event.payload)
        )


ok_html_headers = 'Content-Type: text/html; charset=utf-8'


ok_handled_header = 'Content-Type: text/plain; charset=utf-8'


ok_format_string = """
<html>
<head>
{head}
</head>
<body>
{body}
</body>
</html>
"""


hello = """
<h1>This is the webhook receiver</h1>
I don't think you'll want to reach me this way.
"""


def ok(head='', body=''):
    print(ok_html_headers)
    print('')
    print(ok_format_string.format(
        head=head,
        body=body
    ))


CONTENT_TYPE = 'ct'
EVENT_TYPE = 'event'
SIGNATURE = 'signature'


aliases = {
    EVENT_TYPE: ('X-GitHub-Event', 'X_GITHUB_EVENT', 'HTTP_X_GITHUB_EVENT'),
    CONTENT_TYPE: ('Content-Type', 'content-type', 'CONTENT_TYPE'),
    SIGNATURE: ('X-Hub-Signature', 'HTTP_X_HUB_SIGNATURE')
}


def get_header(name):
    header_aliases = aliases[name]
    for alias in header_aliases:
        if alias in os.environ:
            return os.environ[alias]
    else:
        raise KeyError(
            'For key {} with environ {}'.format(
                name,
                str(os.environ)
            )
        )


def handle_request():
    """Main function"""

    # _, ce = cgi.parse_header(get_header(CONTENT_TYPE))
    payload = sys.stdin.read()
    if not payload:
        ok(body=hello)
    else:
        print(ok_handled_header)
        print()
        print('\n'.join(do(payload)))