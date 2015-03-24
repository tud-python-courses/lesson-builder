__author__ = 'Justus Adam'
__version__ = '0.1'


if __name__ == '__main__':
    import unittest
    import sys
    import os

    m = os.path.dirname(__file__)

    sys.path = [m, os.path.split(m)[0]] + sys.path

    import test

    unittest.main(test)