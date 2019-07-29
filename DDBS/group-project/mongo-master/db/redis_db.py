#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-11 00:27
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
import pickle

from redis import Redis as PyRedis

from utils.func import available_value


class Redis(PyRedis):
    # session = None

    def __init__(self, host, port, db=None, redis_password=None, decode_responses=True, enable=True):
        # print(host, str(port))
        self.enable = enable
        super().__init__(host=host, port=port, db=db, password=redis_password, decode_responses=decode_responses)

    def get(self, name, default=None):
        if not self.enable:
            return None
        res = super().get(name)
        # if decode: res = res.decode()
        return res if res else default

    def set(self, name, value, ex=None, px=None, nx=False, xx=False):
        if not self.enable:
            return None
        return super().set(name, available_value(value), ex=ex, px=px, nx=nx, xx=xx)

    def set_dict(self, name, value):
        if not self.enable:
            return None
        return self.set_pickle(name, value)
        # return self.set(name, json.dumps(value))

    def get_dict(self, name, default=None):
        if default is None:
            default = {}

        if not self.enable:
            return {}
        return self.get_pickle(name, default)
        # res = self.get(name)
        # if res:
        #     return json.loads(res)
        # return default

    def set_pickle(self, name, value):
        if not self.enable:
            return None
        return self.set(name, pickle.dumps(value, 0).decode())

    def get_pickle(self, name, default=None):
        if not self.enable:
            return None
        res = self.get(name)
        return pickle.loads(res.encode()) if res else default

    # def smembers(self, name, default=[]):
    #     res = super().smembers(name)
    #     return [val.decode() for val in list(res)] if res else default

    def delete_by_pattern(self, pattern):
        if not self.enable:
            return -1
        keys = self.keys(pattern=pattern)
        if keys == [] or keys == ():
            return 1
        return self.delete(*keys)


if __name__ == '__main__':
    pass
