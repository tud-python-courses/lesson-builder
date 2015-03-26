import functools
import hmac
import json
import logging

import re
import subprocess
import urllib.request
import hashlib

__author__ = 'Justus Adam'
__version__ = '0.1'


DEBUG = True
LOGGER = logging.getLogger(__name__)


# define the name of github events
PUSH = 'push'
PING = 'ping'


SAFE_GITHUB_API_BASE_URL = 'https://api.github.com'


DIGEST_ENCRYPTION_ALGORITHM = hashlib.sha1


Popen = functools.partial(
    subprocess.Popen,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE
) if not DEBUG else subprocess.Popen


class Event:
    """A Github event"""

    names = {
        PING, PUSH
    }

    def __init__(self, type, payload, repo):
        if type not in self.names:
            raise ValueError('Unrecognized event type {}'.format(type))
        self.type = type
        self.payload = payload
        self.repo = repo

    def identifier(self):
        return self.type

    @property
    def repository(self):
        return self.repo


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


def webhook_config(url, content_type='json', secret=None, insecure_ssl='0'):
    c = {
        'url': url,
        'content_type': content_type,
        'insecure_ssl': insecure_ssl
    }
    if secret is not None:
        c['secret'] = secret
    return c


class Webhook:
    def __init__(
            self,
            repo:GitRepository,
            name,
            config,
            events=(),
            active=False
    ):
        self.name = name
        self.config = config
        self.events = events
        self.active = active
        self.repo = repo

    @classmethod
    def create(cls, name, config, events, active):
        obj = cls(name, config, events, active)
        obj.activate_remote()
        return obj

    def to_json(self):
        return json.dumps({
            'name': self.name,
            'config': self.config,
            'events': self.events_to_json(),
            'active': True
        })

    def events_to_json(self):
        return tuple(event.identifier() for event in self.events)

    def activate_remote(self):
        url = '{base}/repos/{name}/hooks'.format(
            base=SAFE_GITHUB_API_BASE_URL,
            name=self.repo.name
        )
        data = self.to_json()
        request = urllib.request.Request(
            url,
            data=data,
            method='post'
        )
        urllib.request.urlopen(request)


def verify(conf, data, signature, user_agent):
    """
    Verify whether the request contains the characteristic features
    of an authentic Github hook and if set verifies the secret in the request
    is authentic

    :param conf: watch config
    :param data: raw input data
    :return: boolean
    """
    try:
        if user_agent.startswith('GitHub-Hookshot/'):
            if 'secret' in conf:
                secret = conf['secret']
                computed = hmac.new(
                    secret,
                    data,
                    DIGEST_ENCRYPTION_ALGORITHM
                ).hexdigest()
                if not computed == signature:
                    LOGGER.error(
                        'Signature {} did not match computed hash '
                        '{} with secret {}'.format(
                            signature, computed, secret
                        )
                    )
                    return False
                else:
                    return True
            else:
                return True
        else:
            LOGGER.error(
                'User agent {} is not allowed'.format(
                    user_agent
                )
            )
            return False
    except KeyError as e:
        LOGGER.error(
            'Missing key {} in environ'.format(e)
        )
        LOGGER.debug(
            'Headers: {}\nUserAgent: {}'.format(signature, user_agent)
        )
        return False