import functools
import logging
import re
import subprocess

__author__ = 'Justus Adam'
__version__ = '0.1'


DEBUG = True
LOGGER = logging.getLogger(__name__)


# define the name of github events
PUSH = 'PushEvent'
PING = 'ping'


_event_types = {
    PUSH,
    PING
}


Popen = functools.partial(
    subprocess.Popen,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE
) if not DEBUG else subprocess.Popen


class Event:
    """A Github event"""
    def __init__(self, type, payload, repo):
        if type not in _event_types:
            raise ValueError('Unrecognized event type {}'.format(type))
        self.type = type
        self.payload = payload
        self.repo = repo

    @property
    def repository(self):
        return self.repo

    @classmethod
    def from_request(cls, json):
        return cls(
            type=json['type'],
            payload=json['payload'],
            repo=GitRepository.from_json(json['repo'])
        )


class GitRepository:
    """Abstracts a git repository"""
    GIT_URL_REGEX = re.compile(
        '^\w+://(?P<host>\w+\.\w+)/(?P<name>[\w_-]+/[\w_-]+).git$'
    )

    host_urls = {
        'github.com': 'https://github.com'
    }

    """Model for git repository"""
    def __init__(self, name, host='github.com', id=None, url=None, api_url=None):
        self.name = name
        self.id = id
        self.host = host
        self._url = url
        self._api_url = api_url

    @property
    def url(self):
        if self._url is None:
            return 'https://{host}/{name}.git'.format(host=self.host, name=self.name)
        else:
            return self._url

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
            host=match.group('host'),
            url=url
        )

    @classmethod
    def from_json(cls, json):
        return cls(
            name=json['name'],
            id=json['id'],
            api_url=json['url']
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