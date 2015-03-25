import json
import os
import unittest
from lesson_builder import github, webhook_receiver_proxy

__author__ = 'Justus Adam'
__version__ = '0.1'


formats = (
    ('python-lessons', '898088'),
    ('ruby-lessons', '99989'),
    ('c-lessons', '283')
)


test_folder = os.path.dirname(__file__)


class TestHook(unittest.TestCase):
    def setUp(self):
        os.environ['HTTP_USER_AGENT'] = 'GitHub-Hookshot/test'
        os.environ['HTTP_HEADERS'] = ''

    def test_build(self):
        with open(os.path.join(test_folder, 'test_positive.json')) as json_file:
            json_string = json_file.read()
        for name, _id in formats:
            string = json_string % (dict(repo_name=name, id=_id))
            try:
                event = github.Event.from_request(json.loads(string))
            except ValueError as e:
                raise ValueError(
                    '{}\nin\n{}'.format(e,string)
                )
            self.assertIn(
                'Build finished',
                tuple(webhook_receiver_proxy.handle_push(event, string))
            )

    def test_skip(self):
        with open(os.path.join(test_folder, 'test_negative.json')) as json_file:
            json_string = json_file.read()
        for name, _id in formats:
            string = json_string % (dict(repo_name=name, id=_id))
            event = github.Event.from_request(json.loads(string))
            self.assertIn(
                'Commit message demands skip',
                tuple(webhook_receiver_proxy.handle_push(event, string))
            )