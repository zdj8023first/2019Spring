#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 17:37
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import logging


class Config(object):
    # logging 配置
    debug_level = logging.DEBUG
    log_in_file = False
    log_file_name = './mongo.log'

    redis_enable = False

    # WEB 配置
    WEB_PORT = 5000
    IS_DEBUG = False

    #  MongoDB 相关配置


class DBMS:
    DBMS1 = 'Beijing'
    DBMS2 = 'Hong Kong'
    DBMS3 = 'DBMS3'

    all = [DBMS1, DBMS2]

    db_name = 'mongo'

    redis = {
        DBMS1: {
            'host': '127.0.0.1',
            'port': 6379,
            'redis_password': '',
            'enable': Config.redis_enable
        },
        DBMS2: {
            'host': '127.0.0.1',
            'port': 6380,
            'redis_password': '',
            'enable': Config.redis_enable
        },
        DBMS3: {
            'host': '127.0.0.1',
            'port': 6381,
            'redis_password': '',
            'enable': Config.redis_enable
        }
    }

    configs = {
        DBMS1: {
            'host': '127.0.0.1',
            'port': 27017
        },
        DBMS2: {
            'host': '127.0.0.1',
            'port': 27018
        },
        DBMS3: {
            'host': '127.0.0.1',
            'port': 27019
        },
    }

    region = {
        # 第一个地区为偶数， 第二个地区为奇数
        'values': ['Beijing', 'Hong Kong'],
        'Beijing': [DBMS1],
        'Hong Kong': [DBMS2]
    }

    category = {
        # 第一个为偶数， 第二个为奇数
        'values': ['science', 'technology'],
        'science': [DBMS1, DBMS2],
        'technology': [DBMS2]
    }

    def get_all_dbms_by_category(self):
        """
            返回每个category对应的一个数据库地址
        :return: list， 每个category一个数据库
        """
        dbs = list()
        for category in self.category['values']:
            dbs.append(self.get_best_dbms_by_category(category))

        return dbs

    def get_all_dbms_by_region(self):
        """
            返回每个region对应的一个数据库地址
        :return: list， 每个region一个数据库
        """
        dbs = list()
        for region in self.region['values']:
            dbs.append(self.get_best_dbms_by_region(region))

        return dbs

    def get_dbms_by_region(self, region):
        """
            通过region的值返回其内容存储的所有数据库地址
            # TODO 通过读取配置文件来进行返回
        :param region:
        :return:
        """
        return self.region[region]

    def get_best_dbms_by_region(self, region):
        """
            该方法用于返回当前region最好的服务器地址，目前默认为第一个
            TODO 返回当前region所对应的可连接的DBMS
        :param region:
        :return:
        """
        return self.get_dbms_by_region(region)[0]

    def get_dbms_by_category(self, category):
        return self.category[category]

    def get_best_dbms_by_category(self, category):
        """
            category，目前默认为第一个
            TODO 返回当前category所对应的可连接的DBMS
        :param category:
        :return:
        """
        return self.get_dbms_by_category(category)[0]
