#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-06 16:20
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import datetime
import logging

from bson import ObjectId

from config import DBMS
from model.popular import Popular
from service.article_service import ArticleService
from service.read_service import ReadService
from utils.func import date_to_timestamp, get_timestamp, check_alias, singleton

logger = logging.getLogger('PopularService')


@singleton
class PopularService(object):
    field_names = ['temporalGranularity', 'articleAidDict', 'timestamp', 'update_time']

    def __init__(self):
        self.models = dict()
        self.classes = dict()
        for dbms in DBMS.all:
            self.models[dbms] = list()
            self.classes[dbms] = self.__gen_model(dbms)

    def __gen_model(self, dbms):
        class Model(Popular):
            meta = {
                'db_alias': dbms,
                'collection': 'popular'
            }
            pass

        return Model

    def get_model(self, dbms):
        return self.classes[dbms]

    def del_by_filed(self, field, value, **kwargs):
        re = None
        for dbms in DBMS.all:
            re = self.__del_by_filed(field, value, db_alias=dbms, **kwargs)
            pass
        return re

    def __del_by_filed(self, field, value, db_alias=None, **kwargs):
        check_alias(db_alias)
        kwargs[field] = value
        re = self.get_model(db_alias).delete_one(**kwargs)
        return re

    def del_by_id(self, pid, **kwargs):
        if not isinstance(pid, ObjectId):
            pid = ObjectId(pid)
        re = self.del_by_filed('id', pid, **kwargs)

        return re

    def count(self, db_alias=None, **kwargs):
        """
            计算db_alias所对应的的数据库下满足条件的用户数量
        :param db_alias:
        :param kwargs:  查询参数字典， 为空则统计所有用户数量
        :return:
        """
        check_alias(db_alias)
        return self.get_model(db_alias).count(**kwargs)

    def get_ranks(self, temporalGranularity, db_alias, page_num=1, page_size=20, only: list = None,
                  exclude: list = None, sort_by=None, **kwargs):
        check_alias(db_alias)
        return self.get_model(db_alias).get_all(page_num, page_size, only=only, exclude=exclude, sort_by=sort_by,
                                                temporalGranularity=temporalGranularity, **kwargs)
        pass

    # def get_daily_ranks(self, page_num, page_size, only: list = None, exclude: list = None, sort_by=None,
    #                     db_alias=None):
    #     self.get_ranks(page_num, page_size, only, exclude, sort_by, db_alias, temporalGranularity='daily')

    def update_many(self, models=None, db_alias=None):

        if db_alias is None:
            for dbms in DBMS.all:
                self.update_many(models, db_alias=dbms)
        else:
            if models is None:
                models = self.models[db_alias]
                if models is not None:
                    self.get_model(db_alias).update_many(models)
                    self.models[db_alias].clear()

    def __update_rank(self, rank, articles, db_alias):
        rank.articleAidDict = {}
        for aid, count in articles:
            # print("{}, {}".format(aid, ArticleService().has_article(aid, db_alias)))
            if ArticleService().has_article(aid, db_alias):
                rank.articleAidDict[str(aid)] = count

        rank.update_time = get_timestamp()
        # print(rank.articleAidDict)
        rank.save()

    def __update_daily_rank(self, articles, _date, db_alias):
        rank = self._get_daily_rank(_date, db_alias)
        if rank is None:
            rank = self.get_model(db_alias)()
            rank.timestamp = date_to_timestamp(_date)
            rank.temporalGranularity = 'daily'
        self.__update_rank(rank, articles, db_alias)

    def __update_weekly_rank(self, articles, _date, db_alias):
        rank = self._get_weekly_rank(_date, db_alias)
        if rank is None:
            rank = self.get_model(db_alias)()
            rank.timestamp = date_to_timestamp(_date)
            rank.temporalGranularity = 'weekly'
        self.__update_rank(rank, articles, db_alias)

    def __update_monthly_rank(self, articles, _date, db_alias):
        rank = self._get_monthly_rank(_date, db_alias)
        if rank is None:
            rank = self.get_model(db_alias)()
            rank.timestamp = date_to_timestamp(_date)
            rank.temporalGranularity = 'monthly'
        self.__update_rank(rank, articles, db_alias)

    def update_popular(self, _date=None, db_alias=None, daily_pop=None, weekly_pop=None, monthly_pop=None):
        if isinstance(_date, datetime.datetime):
            _date = _date.date()
        _date = _date or datetime.date.today()
        if daily_pop is None:
            daily_pop = ReadService().get_daily_popular(_date)
            weekly_pop = ReadService().get_weekly_popular(_date)
            monthly_pop = ReadService().get_monthly_popular(_date)
        if db_alias is None:
            for dbms in DBMS.all:
                self.update_popular(_date=_date, db_alias=dbms,
                                    daily_pop=daily_pop,
                                    weekly_pop=weekly_pop,
                                    monthly_pop=monthly_pop)
            pass
        else:

            check_alias(db_alias)

            self.__update_daily_rank(daily_pop, _date, db_alias)
            self.__update_weekly_rank(weekly_pop, _date, db_alias)
            self.__update_monthly_rank(monthly_pop, _date, db_alias)

            logger.info("update popular on {}, date:{}".format(db_alias, _date))
            pass

    def _get_daily_rank(self, _date, db_alias=None):
        return self.get_model(db_alias).get_one(timestamp=date_to_timestamp(_date), temporalGranularity='daily')

    def _get_weekly_rank(self, _date, db_alias):
        return self.get_model(db_alias).get_one(timestamp=date_to_timestamp(_date), temporalGranularity='weekly')

    def _get_monthly_rank(self, _date, db_alias):
        return self.get_model(db_alias).get_one(timestamp=date_to_timestamp(_date), temporalGranularity='monthly')

    def get_daily_articles(self, _date, db_alias=None):
        """
            获取每日热门文章
        :param _date:   当前日期， timestamp 或 date都行
        :param db_alias:
        :return:
        """
        if db_alias is None:
            articles = []
            aids = []
            for dbms in DBMS().get_all_dbms_by_category():
                pops = self.get_daily_articles(_date, db_alias=dbms)
                for article in pops:
                    if article.aid not in aids:
                        articles.append(article)
                        aids.append(article.aid)
            return articles
            pass
        else:
            rank = self._get_daily_rank(_date, db_alias)
            return self.__get_articles_by_rank(rank, db_alias)

    def get_weekly_articles(self, _date, db_alias=None):
        if db_alias is None:
            articles = []
            aids = []
            for dbms in DBMS.all:
                pops = self.get_weekly_articles(_date, db_alias=dbms)
                for article in pops:
                    if article.aid not in aids:
                        articles.append(article)
                        aids.append(article.aid)
            return articles
            pass
        else:
            rank = self._get_weekly_rank(_date, db_alias)
            return self.__get_articles_by_rank(rank, db_alias)

    def get_monthly_articles(self, _date, db_alias=None):
        if db_alias is None:
            articles = []
            aids = []
            for dbms in DBMS.all:
                pops = self.get_monthly_articles(_date, db_alias=dbms)
                for article in pops:
                    if article.aid not in aids:
                        articles.append(article)
                        aids.append(article.aid)
            return articles
        else:
            rank = self._get_monthly_rank(_date, db_alias)

            return self.__get_articles_by_rank(rank, db_alias)
        # populars = list()
        # aids = list(int(aid) for aid in rank.articleAidDict.keys())
        # articles = ArticleService().get_articles_by_aids(aids, only=['title'], db_alias=db_alias)
        # for aid, count in rank.articleAidDict.items():
        #     articles[int(aid)].count = count
        #     populars.append(articles[int(aid)])
        # populars = list()
        #
        # for aid, count in rank.articleAidDict.items():
        #     article = ArticleService().get_one_by_aid(int(aid), db_alias=db_alias)
        #     article.count = count
        #     populars.append(article)
        # return populars

    def __get_articles_by_rank(self, rank, db_alias):
        populars = list()
        if rank is None or rank.articleAidDict == []:
            return populars
        aids = list(int(aid) for aid in rank.articleAidDict.keys())
        articles = ArticleService().get_articles_by_aids(aids, only=['title', 'category'], db_alias=db_alias)
        for aid, count in rank.articleAidDict.items():
            articles[int(aid)].count = count
            populars.append(articles[int(aid)])
        return populars

    def get_articles(self, _date, level, db_alias=None):
        """
            获取每日热门文章
        :param level:  查询的级别 daily, weekly, monthly
        :param _date:   当前日期， timestamp 或 date都行
        :param db_alias:
        :return:
        """
        if db_alias is None:
            articles = []
            aids = []
            for dbms in DBMS().get_all_dbms_by_category():
                pops = self.get_articles(_date, level, db_alias=dbms)
                for article in pops:
                    if article.aid not in aids:
                        articles.append(article)
                        aids.append(article.aid)
            return articles
            pass
        else:
            rank = self._get_rank(_date, level, db_alias)
            return self.__get_articles_by_rank(rank, db_alias)

    def _get_rank(self, _date, level, db_alias=None):
        return self.get_model(db_alias).get_one(timestamp=date_to_timestamp(_date), temporalGranularity=level)
