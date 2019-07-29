#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-17 23:48
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from service.article_service import ArticleService
from service.be_read_service import BeReadService
from test.test_base import TestBase


class TestBeReadService(TestBase):

    @classmethod
    def setup_class(cls) -> None:
        super().setup_class()
        cls.beReadService = BeReadService()

    # def test_get_be_id(self):
    #     bid = self.beReadService.get_bid()
    #     print(bid)

    def test_get_by_aid(self):
        article = ArticleService().get_articles_by_title('title5')[0]
        beread = self.beReadService.get_one_by_aid(article.aid)
        print(beread)

    # def test_get_by_bid(self):
    #     be = self.beReadService.get_by_bid(1)
    #     print(be)
    #
    #     be = self.beReadService.get_by_bid(2)
    #     print(be)

    def test_add_be_read_record(self):
        # 该方法在测试添加read记录时会自动测试
        pass

    def test_get_total_popular(self):
        from utils.func import pretty_models
        populars = self.beReadService.get_tops_reads()
        pretty_models(populars, ['bid', 'aid', 'readNum'])
        populars = self.beReadService.get_tops_comments()
        pretty_models(populars, ['bid', 'aid', 'commentNum'])
        populars = self.beReadService.get_tops_agrees()
        pretty_models(populars, ['bid', 'aid', 'agreeNum'])
        populars = self.beReadService.get_tops_shares()
        pretty_models(populars, ['bid', 'aid', 'shareNum'])
