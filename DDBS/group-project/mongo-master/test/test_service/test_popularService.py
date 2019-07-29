#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-19 14:31
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from config import DBMS
from service.popular_service import PopularService
from test.test_base import TestBase
from utils.func import timestamp_to_datetime, str_to_datetime, pretty_models


class TestPopularService(TestBase):
    _date = '2017-09-25'

    def test_update_popular(self):
        _date = timestamp_to_datetime(1506342197000).date()
        PopularService().update_popular(_date)

    def test_get_daily_popular(self):
        # _date = '2017-09-25'
        # _date = '2017-10-06'
        popular = PopularService()._get_daily_rank(str_to_datetime(self._date), db_alias=DBMS.DBMS2)
        pretty_models([popular], PopularService.field_names)
        # print(populars)

    def test_get_weekly_popular(self):
        # _date = '2017-09-25'
        # _date = '2017-10-06'
        popular = PopularService()._get_weekly_rank(str_to_datetime(self._date), db_alias=DBMS.DBMS2)
        pretty_models([popular], PopularService.field_names)
        # print(populars)

    def test_get_monthly_popular(self):
        # _date = '2017-09-25'
        # _date = '2017-10-06'
        popular = PopularService()._get_monthly_rank(str_to_datetime(self._date), db_alias=DBMS.DBMS2)
        pretty_models([popular], PopularService.field_names)
        # print(populars)

    def test_get_daily_articles(self):
        _date = str_to_datetime(self._date)
        populars = PopularService().get_daily_articles(_date, DBMS.DBMS2)

        pretty_models(populars, ['aid', 'title', 'count'])

    def test_get_weekly_articles(self):
        _date = str_to_datetime(self._date)
        populars = PopularService().get_weekly_articles(_date, DBMS.DBMS2)

        pretty_models(populars, ['aid', 'title', 'count'])

    def test_get_monthly_articles(self):
        _date = str_to_datetime(self._date)
        populars = PopularService().get_monthly_articles(_date, DBMS.DBMS2)

        pretty_models(populars, ['aid', 'title', 'count'])
