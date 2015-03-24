__author__ = 'justusadam'
__version__ = '0.1'

import unittest
from lesson_builder import github
import re


class TestGitRepo(unittest.TestCase):
    def test_url_regex(self):
        regex = github.GitRepository.GIT_URL_REGEX
        m = re.match(
            regex, 'https://github.com/test/this.git'
        )
        self.assertEqual(m.group('host'), 'github.com')
        self.assertEqual(m.group('name'), 'test/this')


if __name__ == '__main__':
    unittest.main()
