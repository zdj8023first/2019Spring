#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-06-02 23:37
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from config import DBMS
from db.redis_db import Redis
from utils.func import singleton, check_alias


@singleton
class RedisService(object):

    def __init__(self):
        self.redis = dict()
        for dbms in DBMS().all:
            self.redis[dbms] = self._gen_redis(dbms)

    def get_redis(self, dbms) -> Redis:
        check_alias(dbms)
        return self.redis[dbms]

    def _gen_redis(self, dbms):
        # print(DBMS().redis[dbms]['host'])
        # host = DBMS().redis[dbms]['host'],
        # port = DBMS().redis[dbms]['port'],
        # db = 0,
        # password = DBMS().redis[dbms]['redis_password'],
        # decode_responses = True
        args = DBMS().redis[dbms]
        return Redis(**args)

    def reset_redis(self):
        for dbms, red in self.redis.items():
            red.delete_by_pattern('*')
