from hashlib import sha512
import rainbow



space = list(rainbow.gen_space('abcdefghijklmnopqrstuvwxyz', 4))
r_funs = rainbow.reduce_factory(space, 20)
seed = bytes(sha512(b'trolol').hexdigest(), encoding='utf-8')

table = rainbow.RainbowTable(rainbow.sha, r_funs, N=len(space))
table.create(rainbow.gen_seeds(seed, r_funs[0], 600000))
