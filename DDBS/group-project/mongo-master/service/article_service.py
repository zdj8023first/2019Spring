#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 20:42
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from model.article import Article
from service.ids_service import IdsService
from utils.func import *


@singleton
class ArticleService(object):
    # fields = list(Article._db_field_map.keys())
    field_names = list(Article._db_field_map.keys())
    update_forbid = ['aid', 'timestamp', 'update_time', 'category']

    def __init__(self):
        self.logger = logging.getLogger('ArticleService')
        self.models = dict()
        self.classes = dict()
        for dbms in DBMS.all:
            self.models[dbms] = list()
            self.classes[dbms] = self.__gen_model(dbms)

    def __gen_model(self, dbms):
        class Model(Article):
            meta = {
                'db_alias': dbms,
                'collection': 'article'
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
    def get_id():

        return IdsService().next_id('aid')

    def has_article(self, aid, db_alias):
        return self.get_model(db_alias).count_documents(aid=aid)

    def count(self, db_alias=None, exclude=None, **kwargs):
        check_alias(db_alias)
        return self.get_model(db_alias).count(**kwargs)

    def get_articles_by_title(self, title, page_num=1, page_size=20, db_alias=None, **kwargs) -> list:
        """
            根据文章标题进行搜索
            # TODO 测试该方法是否有用
        :param page_size:
        :param page_num:
        :param db_alias:
        :param title:
        :return: list, 里边存的article
        """
        articles = list()  # 保存所有数据库上的不同文章

        titles = list()  # 保存在查询过程中已经出现过的文章

        if db_alias is None:
            for dbms in DBMS.all:
                # tmp_articles = self.__search_by_title(title, db_alias=dbms)
                tmp_articles = self.get_articles(title__contains=title, page_num=page_num, page_size=page_size,
                                                 db_alias=dbms, **kwargs)
                for article in tmp_articles:
                    # 判断是否已经在其他DBMS中出现过了
                    if article.title not in titles:
                        articles.append(article)
                        titles.append(article.title)
        else:
            # articles = self.__search_by_title(title, db_alias=db_alias)
            articles = self.get_articles(title__contains=title, page_num=page_num, page_size=page_size,
                                         db_alias=db_alias, **kwargs)
        return articles

    # def __search_by_title(self, title, db_alias=None) -> list:
    #     """
    #         因为DBMS2上存有所有的文章，所以默认直接在DBMS2上搜索就行
    #         # TODO 测试该方法是否有用
    #     :param title:
    #     :param db_alias:
    #     :return: list, 里边存的article
    #     """
    #     check_alias(db_alias)
    #     return self.get_model(db_alias).objects(title__contains=title)

    def get_articles_by_category(self, category, page_num=1, page_size=20, db_alias=None, **kwargs) -> list:
        """
            根据文章标题进行搜索
            # TODO 测试该方法是否有用
        :param db_alias:
        :param category:
        :return: list, 里边存的article
        """
        articles = list()  # 保存所有数据库上的不同文章

        titles = list()  # 保存在查询过程中已经出现过的文章

        if db_alias is None:
            for dbms in get_dbms_by_category(category):
                # tmp_articles = self.__search_by_category(category, db_alias=dbms)
                tmp_articles = self.get_articles(category=category, page_num=page_num, page_size=page_size,
                                                 db_alias=dbms, **kwargs)
                for article in tmp_articles:
                    # 判断是否已经在其他DBMS中出现过了
                    if article.title not in titles:
                        articles.append(article)
                        titles.append(article.title)
        else:
            # articles = self.__search_by_category(category, db_alias=db_alias)
            articles = self.get_articles(category=category, page_num=page_num, page_size=page_size, db_alias=db_alias,
                                         **kwargs)
        return articles

    # def __search_by_category(self, category, db_alias=None) -> list:
    #     """
    #         # TODO 测试该方法是否有用
    #     :param category:
    #     :param db_alias:
    #     :return: list, 里边存的article
    #     """
    #     check_alias(db_alias)
    #     return self.get_model(db_alias).objects(category=category)

    def add_an_article(self, title, authors, category, abstract, articleTags, language, text, image=None,
                       video=None, timestamp=None, is_multi=False):
        from service.be_read_service import BeReadService
        aid = self.get_id()
        bid = BeReadService().get_bid()
        for dbms in get_dbms_by_category(category):
            self.__add_an_article(aid, title, authors, category, abstract, articleTags, language, text, image,
                                  video, timestamp, db_alias=dbms, is_multi=is_multi)
            self.init_be_read_for_article(bid, aid, db_alias=dbms, is_multi=is_multi)
        return aid

    def __add_an_article(self, aid, title, authors, category, abstract, articleTags, language, text, image=None,
                         video=None, timestamp=None, db_alias=None, is_multi=False):

        check_alias(db_alias)
        article = self.get_model(db_alias)()
        article.aid = aid
        article.title = title
        article.authors = authors
        article.category = category
        article.abstract = abstract
        article.articleTags = articleTags
        article.language = language
        article.text = text
        article.image = image
        article.video = video
        article.update_time = datetime.datetime.utcnow()
        article.timestamp = timestamp or get_timestamp()

        if is_multi:
            self.models[db_alias].append(article)
            return True
        else:
            return self.save_article(article)

    def init_be_read_for_article(self, bid, aid, db_alias=None, timestamp=None, is_multi=False):
        from service.be_read_service import BeReadService

        check_alias(db_alias=db_alias)

        be_read = BeReadService().get_model(db_alias)()
        be_read.aid = aid
        be_read.bid = bid
        be_read.timestamp = timestamp or get_timestamp()
        be_read.last_update_time = datetime.datetime.utcnow()
        if is_multi:
            BeReadService().models[db_alias].append(be_read)
        else:
            be_read.save()
        return True

    def save_article(self, article):
        if article.save() is not None:
            # self.logger.info('文章"{}"保存成功'.format(article.title))
            return True
        return False

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

    def del_by_aid(self, aid, **kwargs):
        re = self.del_by_filed('aid', aid, **kwargs)

        return re

    def del_article(self, article):
        if article is not None:
            for dbms in get_dbms_by_category(article.category):
                self.__del_by_filed('aid', article.aid, db_alias=dbms)
        return True

    def get_articles(self, page_num=1, page_size=20, db_alias=None, **kwargs):
        # TODO 去掉默认db设置，在所有数据库中，根据数量进行分页以及返回相关数据
        # category = kwargs.get('category', None)
        # if category is not None:
        #     db_alias = get_best_dbms_by_category(category)
        if db_alias is None:
            articles = []
            aids = []
            for dbms in DBMS.all:
                tmp_articles = self.get_articles(page_num, page_size, db_alias=dbms, **kwargs)
                for article in tmp_articles:
                    if article.aid not in aids:
                        articles.append(article)
                        aids.append(article.aid)
            return list(articles)
        else:
            check_alias(db_alias)
            return list(self.get_model(db_alias).get_all(page_num, page_size, **kwargs))

    def get_articles_by_aids(self, aids: list, db_alias: str = None, **kwargs) -> dict:
        """
            根据aids列表一次性返回对应的article信息

        :param aids:    待查询的aids
        :param db_alias:    待查询的数据库
        :param fields:  需要显示或者不需要显示的字段， None则查询全部字段
                            {'field name': 1}  显示该字段
                            {'field name': 0}  不显示该字段
        :return:
        """
        # TODO 去掉默认db设置，在所有数据库中，根据数量进行分页以及返回相关数据
        check_alias(db_alias)
        # if fields is not None and isinstance(fields, dict):
        #     return self.get_model(db_alias).objects.fields(**fields).in_bulk(aids)
        # else:
        #     return self.get_model(db_alias).objects.in_bulk(aids)

        return self.get_model(db_alias).get_many_by_ids(aids, **kwargs)

    # def get_an_article_by_id(self, _id):
    #     # TODO 修改实现方法
    #     article = None
    #     for dbms in DBMS.all:
    #         article = self.__get_an_article_by_aid(_id, db_alias=dbms)
    #         if article is not None:
    #             break
    #     return article
    #
    # def __get_an_article_by_aid(self, aid, db_alias=None):
    #     check_alias(db_alias)
    #     if not isinstance(aid, ObjectId):
    #         aid = ObjectId(aid)
    #     return self.get_model(db_alias).get(id=aid)

    def get_one_by_aid(self, aid, db_alias=None, **kwargs) -> Article:
        category = kwargs.get('category', None)
        if category is not None:
            db_alias = get_best_dbms_by_category(category)
        if db_alias is None:
            # TODO 修改实现方法
            article = None
            for dbms in DBMS().get_all_dbms_by_category():
                article = self.get_one_by_aid(aid, db_alias=dbms, **kwargs)
                if article is not None:
                    break
            return article
        else:
            check_alias(db_alias)
            return self.get_model(db_alias).get_one(aid=aid, **kwargs)

    def update_by_aid(self, aid, db_alias=None, **kwargs):
        """
           根据aid进行内容的更新

        :param aid:
        :param db_alias:
        :param kwargs:
        :return:
        """
        self.get_model(db_alias).objects(aid=aid).update(**kwargs)

    # def __get_an_article_by_aid(self, aid, db_alias=None):
    #     check_alias(db_alias)
    #     return self.get_model(db_alias).get(aid=aid)

    def update_one(self, article, condition: dict):
        """
            根据aid更新文章

        :param article:
        :param condition:  Json格式更新内容
        :return:
        """
        art = None
        for dbms in get_dbms_by_category(article.category):
            art = self.__update(article.aid, condition, db_alias=dbms)
            if art is None:
                return None
        return art

    def __update(self, aid, condition: dict, db_alias=None):
        check_alias(db_alias)
        article = self.get_one_by_aid(aid, db_alias=db_alias)
        if article is None:
            return

        forbid = ("aid", 'update_time', 'category')
        for key, value in condition.items():
            if key not in forbid and hasattr(article, key):
                setattr(article, key, value)
        article.update_time = datetime.datetime.utcnow()
        article.save()
        article.reload()
        return article

    # =============== 已测试 ===============

    @staticmethod
    def pretty_articles(articles: list):
        pretty_models(articles, ArticleService.field_names)

    def import_from_dict(self, data):
        for dbms in get_dbms_by_category(data['category']):
            article = self.get_model(dbms)()
            article.aid = int(data['aid'])
            article.title = data['title']
            article.authors = data['authors']
            article.category = data['category']
            article.abstract = data['abstract']
            article.articleTags = data['articleTags']
            article.language = data['language']
            article.text = data['text']
            article.image = data['image']
            article.video = data['video']
            article.update_time = datetime.datetime.utcnow()
            article.timestamp = int(data['timestamp'])
            self.models[dbms].append(article)

            from service.be_read_service import BeReadService

            be_read = BeReadService().get_model(dbms)()
            be_read.aid = int(data['aid'])
            be_read.bid = int(data['aid'])
            be_read.timestamp = int(data['timestamp'])
            be_read.last_update_time = datetime.datetime.utcnow()
            BeReadService().models[dbms].append(be_read)

        pass


if __name__ == '__main__':
    from main import init

    init()
    _articles = ArticleService.get_articles()
