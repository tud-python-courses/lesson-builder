import functools
import subprocess

__author__ = 'Justus Adam'
__version__ = '0.1'


Popen = functools.partial(
    subprocess.Popen,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE
)


_capture_format_header = """-------------------------------------------------------------\n"""
_capture_format_body = """   captured {}:
-------------------------------------------------------------
{}
-------------------------------------------------------------
"""


def error_capture_format(*ms):
    return _capture_format_header + ''.join(_capture_format_body.format(*a) for a in ms)



class Monad:
    __slots__ = ()

    def bind(self, other):
        raise NotImplementedError

    def m_return(self, func):
        raise NotImplementedError

    __ge__ = bind


class Maybe(Monad):
    __slots__ = 'x',

    def bind(self, other):
        if not callable(other):
            raise ValueError('other must be callable')
        if self.x is None:
            return self
        else:
            return Maybe(other(self.x))

    def none(self):
        return self.x is None

    def some(self):
        return self.x is not None

    def m_return(self, func):
        pass

    def __init__(self, x=None):
        self.x = x

    def get(self):
        return self.x

    def set(self, val):
        self.x = val