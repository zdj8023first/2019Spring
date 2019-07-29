#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-15 23:06
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com


# from model.be_read import BeRead


from model.be_read import BeRead
from service.article_service import ArticleService
from service.ids_service import IdsService
from utils.func import *

logger = logging.getLogger('ReadService')


@singleton
class BeReadService(object):
    field_names = ["bid", "aid", "readNum", "readUidList", "commentNum", "commentUidList", "agreeNum", "agreeUidList",
                   "shareNum", "shareUidList", "timestamp", "last_update_time"]

    def __init__(self):
        self.models = dict()
        self.classes = dict()
        for dbms in DBMS.all:
            self.models[dbms] = list()
            self.classes[dbms] = self.__gen_model(dbms)

    def __gen_model(self, dbms):
        class Model(BeRead):
            meta = {
                'db_alias': dbms,
                'collection': 'be_read'
            }
            pass

        return Model

    def get_model(self, dbms):
        return self.classes[dbms]

    def update_many(self, models=None, db_alias=None):

        if db_alias is None:
            for dbms in DBMS.all:
                self.update_many(models, db_alias=dbms)
        else:
            if models is None:
                models = self.models[db_alias]
                if models is not None:
                    self.get_model(db_alias).update_many(models)
                    # del self.models[db_alias]
                    self.models[db_alias] = list()

    @staticmethod
    def get_bid():
        return IdsService().next_id('bid')

    def get_one_by_aid(self, aid: str, db_alias=None, **kwargs) -> BeRead:
        if db_alias is None:
            article = ArticleService().get_one_by_aid(aid, only=['category'])
            return self.get_one_by_aid(aid, db_alias=get_best_dbms_by_category(article.category))
        else:
            check_alias(db_alias)
            return self.get_model(db_alias).get_one(aid=aid, **kwargs)

    def add_one(self, aid, uid, readOrNot, commentOrNot, agreeOrNot, shareOrNot, timestamp=None,
                is_multi=False):

        article = ArticleService().get_one_by_aid(aid, only=['category'])
        if article is None:
            return None

        # be_read = self.get_by_aid(aid)
        be_read = None
        # bid = self.get_bid() if be_read is None else be_read.bid
        for dbms in get_dbms_by_category(article.category):
            # logger.info("save be read to : {} uid: {}".format(dbms, uid))
            be_read = self.new_record(aid, uid, readOrNot, commentOrNot, agreeOrNot, shareOrNot, timestamp,
                                      db_alias=dbms, is_multi=is_multi)

        return be_read

    def new_record(self, aid, uid, readOrNot=1, commentOrNot=0, agreeOrNot=0, shareOrNot=0, timestamp=None,
                   db_alias=None, is_multi=False):
        check_alias(db_alias)

        be_read = self.get_one_by_aid(aid, db_alias=db_alias)
        if be_read is None:
            logger.info("没有 {} 关联的beread记录".format(aid))
            # return None
            be_read = self.get_model(db_alias)()
            be_read.aid = aid
            be_read.bid = self.get_bid()
            be_read.readNum = 0
            be_read.readUidList = []
            be_read.commentNum = 0
            be_read.commentUidList = []
            be_read.agreeNum = 0
            be_read.agreeUidList = []
            be_read.shareNum = 0
            be_read.shareUidList = []
            be_read.timestamp = timestamp or get_timestamp()

        if readOrNot:
            be_read.readNum += 1
            if uid not in be_read.readUidList:
                be_read.readUidList.append(uid)
        if commentOrNot:
            be_read.commentNum += 1
            if uid not in be_read.commentUidList:
                be_read.commentUidList.append(uid)
        if agreeOrNot:
            be_read.agreeNum += 1
            if uid not in be_read.agreeUidList:
                be_read.agreeUidList.append(uid)
        if shareOrNot:
            be_read.shareNum += 1
            if uid not in be_read.shareUidList:
                be_read.shareUidList.append(uid)

        be_read.last_update_time = datetime.datetime.utcnow()
        if is_multi:
            self.models[db_alias].append(be_read)
        else:
            be_read.save()
        return be_read

    def get_tops_reads(self, top_n=10):
        popular = list()
        aids = list()
        for dbms in DBMS.all:
            logger.info("get_tops_be_reads on {}".format(dbms))
            pop = self.__get_popular('readNum', only=['readNum'], top_n=top_n, db_alias=dbms)

            for p in pop:
                if p.aid not in aids:
                    popular.append(p)
                    aids.append(p.aid)

        return sort_dict_in_list(popular, 'readNum')[:top_n]

    def get_tops_comments(self, top_n=10):
        popular = list()
        aids = list()
        for dbms in DBMS.all:
            logger.info("get_tops_comment on {}".format(dbms))
            pop = self.__get_popular('commentNum', only=['commentNum'], top_n=top_n, db_alias=dbms)

            for p in pop:
                if p.aid not in aids:
                    popular.append(p)
                    aids.append(p.aid)

        return sort_dict_in_list(popular, 'commentNum')[:top_n]

    def get_tops_agrees(self, top_n=10):
        popular = list()
        aids = list()
        for dbms in DBMS.all:
            logger.info("get_tops_agrees on {}".format(dbms))
            pop = self.__get_popular('agreeNum', only=['agreeNum'], top_n=top_n, db_alias=dbms)

            for p in pop:
                if p.aid not in aids:
                    popular.append(p)
                    aids.append(p.aid)

        return sort_dict_in_list(popular, 'agreeNum')[:top_n]
        pass

    def get_tops_shares(self, top_n=10):
        popular = list()
        aids = list()
        for dbms in DBMS.all:
            logger.info("get_tops_shares on {}".format(dbms))
            pop = self.__get_popular('shareNum', only=['shareNum'], top_n=top_n, db_alias=dbms)

            for p in pop:
                if p.aid not in aids:
                    popular.append(p)
                    aids.append(p.aid)

        return sort_dict_in_list(popular, 'shareNum')[:top_n]
        pass

    def __get_popular(self, field, only, top_n=10, db_alias=None):
        check_alias(db_alias)
        return self.get_model(db_alias).objects.only('aid', *only).order_by('-' + field).limit(top_n)
        pass
