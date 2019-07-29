#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-17 01:04
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import logging

from config import DBMS
from model.ids import Ids
from utils.func import check_alias, singleton

logger = logging.getLogger('IdsService')


@singleton
class IdsService(object):

    def init(self, db_alias, uid=0, aid=0, rid=0, bid=0):
        if self.get_model(db_alias).count(ids=0) == 0:
            ids = self.get_model(db_alias)()
            ids.ids = 0
            ids.uid = uid
            ids.aid = aid
            ids.rid = rid
            ids.bid = bid
            ids.pid = 0
            ids.save()
            return ids

    @staticmethod
    def get_model(dbms: str):
        class Model(Ids):
            meta = {
                'db_alias': dbms,
                'collection': 'ids'
            }
            pass

        return Model

    def next_id(self, name):
        db_alias = DBMS.DBMS1
        model = self.get_model(db_alias)
        check_alias(db_alias)
        kwargs = {
            'inc__' + name: 1
        }
        ids = model.objects(ids=0).only(name).modify(upsert=True, **kwargs)
        # logger.debug('set {}: {}'.format(name, getattr(ids, name)))
        return getattr(ids, name)

    def sync_id(self, name, value):
        logger.info("IDS 字段： {} 数据不同步, 同步数据中...".format(name))
        for dbms in DBMS.all:
            self.__sync_id(name, value, db_alias=dbms)

    def __sync_id(self, name, value, db_alias=None):
        check_alias(db_alias=db_alias)
        kwargs = {
            name: value
        }
        self.get_model(db_alias).get_one(ids=0).update(**kwargs)
