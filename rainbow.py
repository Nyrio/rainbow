from itertools import chain, product
from hashlib import sha256
from math import exp

from sortedcontainers import SortedDict


class Spritz:
    def __init__(self, seed):
        s = list(range(256))
        j = 0
        for i in range(256):
            j = (j + s[i] + seed[i % 128]) % 256
            s[i], s[j] = s[j], s[i]
        self.s = s
        self.i = 0
        self.j = 0
        self.k = 0
        self.z = 0
        self.w = 197  # coprime to 256

    def __iter__(self):
        return self

    def __next__(self):
        self.i = (self.i + self.w) % 256
        self.j = (self.k + self.s[(self.j + self.s[self.i]) % 256]) % 256
        self.k = (self.k + self.i + self.s[self.j]) % 256
        self.s[self.i], self.s[self.j] = self.s[self.j], self.s[self.i]
        self.z = self.s[(self.j +
                         self.s[(self.i +
                                 self.s[(self.z + self.k) % 256]) % 256]
                         ) % 256]
        return self.z


class RC4:
    def __init__(self, seed):
        s = list(range(256))
        j = 0
        for i in range(256):
            j = (j + s[i] + seed[i % 128]) % 256
            s[i], s[j] = s[j], s[i]
        self.s = s
        self.i = 0
        self.j = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.i = (self.i + 1) % 256
        self.j = (self.j + self.s[self.i]) % 256
        self.s[self.i], self.s[self.j] = self.s[self.j], self.s[self.i]
        return self.s[(self.s[self.i] + self.s[self.j]) % 256]


class RainbowTable:
    def __init__(self, hash_fun, reduce_funs, table=None, N=-1):
        self.h_fun = hash_fun
        self.r_funs = reduce_funs
        self.table = table
        self.N = N

    def gen_chain(self, seed, i=0, j=None):
        j = j or len(self.r_funs)
        u = seed
        for r_fun in self.r_funs[i:j]:
            u = r_fun(self.h_fun(u))
        return u

    @property
    def sucess_probability(self):
        p = 1
        m = len(self.table)
        for i in range(1, len(self.r_funs)):
            p *= 1 - m/self.N
            m = self.N * (1 - exp(-m/self.N))
        return 1 - p

    def create(self, seeds):
        if self.table is not None:
            raise ValueError('Table already created')
        t = SortedDict()
        for s in seeds:
            last = self.gen_chain(s)
            if last in t:
                t[last].append(s)
            else:
                t[last] = [s]
        self.table = t

    def crack(self, h):
        l = len(self.r_funs)
        for i in reversed(range(l)):
            # rebuild last chain element
            last = self.gen_chain(self.r_funs[i](h), i=i+1)
            seeds = self.table.get(last)
            # found pw in the table! rebuild previous chain element
            if seeds is not None:
                for seed in seeds:
                    pw = self.gen_chain(seed, j=i)
                    # detect false alarms
                    if self.h_fun(pw) == h:
                        return pw


def sha(x):
    return sha256(x.encode('utf-8')).hexdigest()


def reduce_factory(space, n):
    space = list(space)
    l = len(space)
    def reduce_gen(i):
        s = hex(i)[2:]
        return lambda x: space[int(sha(s+x), base=16) % l]
    return [reduce_gen(i) for i in range(n)]


def gen_space(char_set, max_len):
    return chain.from_iterable((''.join(x) for x in product(char_set,
                                                            repeat=i+1))
                               for i in range(max_len))


def gen_seeds(seed, r_fun, n):
    rc4 = RC4(seed)
    for _ in range(n):
        yield r_fun(''.join(chr(next(rc4)) for i in range(256)))
