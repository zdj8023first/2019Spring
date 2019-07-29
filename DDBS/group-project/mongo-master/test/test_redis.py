#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-06-02 22:08
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from service.redis_service import RedisService


def test_set_get():
    RedisService().get_redis("Beijing").set('test', 'value')
    value = RedisService().get_redis("Beijing").get('test')
    assert value == 'value'
    RedisService().get_redis("Hong Kong").set('test', 'hong kong')
    value = RedisService().get_redis("Hong Kong").get('test')
    assert value == 'hong kong'

# def pickler():
#     from main import init
#     init()
#     from service.user_service import UserService
#     from db.redis_db import Redis
#     import dill
#     uid = 1
#     user = UserService().get_user_by_uid(uid=uid)
