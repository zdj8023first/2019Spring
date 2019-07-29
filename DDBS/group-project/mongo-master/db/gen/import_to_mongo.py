#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-03 16:57
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import json
import logging

from main import init
from service.article_service import ArticleService
from service.read_service import ReadService, Read
from service.user_service import UserService
from .genTable import USERS_NUM, ARTICLES_NUM, READS_NUM

data_path = './db/gen/'

article_file_name = 'article.dat'
user_file_name = 'user.dat'
read_file_name = 'read_tmp.dat'

init()
logger = logging.getLogger(__name__)


def print_bar(now, total):
    print('\rprocess:\t {now} / {total}  {rate}%'.format(now=now + 1, total=total,
                                                         rate=round((now + 1) / total * 100, 2)), end='')


def users():
    i = 0
    with open(data_path + user_file_name, 'r') as f:
        for line in f:
            data = json.loads(line)
            # user = User(data['name'], data['pwd'], data['gender'], data['email'], data['phone'], data['dept'],
            #             data['grade'], data['language'], data['region'], data['role'], data['preferTags'],
            #             int(data['obtainedCredits']))
            UserService.register(data['name'], data['pwd'], data['gender'], data['email'], data['phone'], data['dept'],
                                 data['grade'], data['language'], data['region'], data['role'], data['preferTags'],
                                 int(data['obtainedCredits']))
            i += 1
            print_bar(i, USERS_NUM)
            pass


def articles():
    i = 0
    with open(data_path + article_file_name, 'r') as f:
        for line in f:
            data = json.loads(line)
            ArticleService.add_an_article(title=data['title'], authors=data['authors'], category=data['category'],
                                          abstract=data['abstract'], articleTags=data['articleTags'],
                                          language=data['language'], text=data['text'], image=data['image'],
                                          video=data['video'])
            i += 1
            print_bar(i, ARTICLES_NUM)


def read():
    i = 0
    with open(data_path + read_file_name, 'r') as f:
        for line in f:
            data = json.loads(line)
            article = ArticleService.get_an_article(title='title' + data['aid'])
            name = 'user' + data['uid'] if data['uid'] != 0 else 'admin'
            user = UserService.get_user_by_name(name=name)
            new_read = Read()
            new_read.aid = article
            new_read.uid = user
            new_read.readOrNot = int(data['readOrNot'])
            new_read.readTimeLength = int(data['readTimeLength'])
            new_read.readSequence = int(data['readSequence'])
            new_read.commentOrNot = int(data['commentOrNot'])
            new_read.commentDetail = data['commentDetail']
            new_read.agreeOrNot = int(data['agreeOrNot'])
            new_read.shareOrNot = int(data['shareOrNot'])
            ReadService.add_one(new_read)

            i += 1
            print_bar(i, READS_NUM)
