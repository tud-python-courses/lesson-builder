__author__ = 'Justus Adam'
__version__ = '0.1'


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