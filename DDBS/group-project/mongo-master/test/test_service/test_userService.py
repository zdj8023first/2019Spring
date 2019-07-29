#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-14 13:12
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from config import DBMS
from service.user_service import UserService
from test.test_base import TestBase
from utils.func import pretty_models


class TestUserService(TestBase):

    def test_count(self):
        print()
        cnt = UserService().count(db_alias=DBMS.DBMS1)
        print('DBMS1:', cnt)
        cnt = UserService().count(db_alias=DBMS.DBMS2)
        print('DBMS2:', cnt)
        cnt = UserService().count_all()
        print('all: ', cnt)

    def test_user_list(self):
        only = ['uid', 'name']
        # users = UserService().users_list(only=only, db_alias=DBMS.DBMS1)
        users = UserService().get_users(only=only, db_alias=DBMS.DBMS1)
        # UserService().pretty_users(users)
        pretty_models(users, only)
        print('total: {}'.format(len(users)))

        exclude = ['pwd']
        # users = UserService().users_list(exclude=exclude, db_alias=DBMS.DBMS2)
        users = UserService().get_users(exclude=exclude, db_alias=DBMS.DBMS2)
        # UserService().pretty_users(users)
        pretty_models(users, list(x for x in UserService.field_names if x not in exclude))
        print('total: {}'.format(len(users)))

    def test_users(self):
        users = UserService().get_users(page_num=4)
        # UserService().pretty_users(users)
        pretty_models(users, UserService.field_names)
        print('total: {}'.format(len(users)))
        assert len(users) == 20

    def test_register(self):
        re1 = UserService().register('gahon', 'password', 'male', 'asdf', 'asdfasdf', 'asdf', 'asdf', 'asdf',
                                     'Beijing', 'asdf', 'asdfas', '123')

        assert re1
        print(re1)

    def test_get_user_by_name(self):
        user = UserService().get_user_by_name('user4', exclude=['pwd'])
        assert user is not None
        print(user)

        # admin1 = UserService().get_user_by_name('admin', db_alias=DBMS.DBMS1, only=['name'])
        # print(admin1)
        #
        # admin2 = UserService().get_user_by_name('admin', db_alias=DBMS.DBMS2, exclude=['name'])
        # print(admin2)
        # assert (admin1 is None) if (admin2 is not None) else (admin1 is not None)

    def test_get_by_uid(self):
        user = UserService().get_user_by_name('user4')
        user1 = UserService().get_user_by_uid(user.uid)
        assert user1 is not None and user1.name == user.name
        print(user)
        # user = UserService().get_user_by_uid(23)
        # # assert user is not None
        # print(user)

    def test_login(self):
        UserService().register('test1', 'password', 'male', 'asdf', 'asdfasdf', 'asdf', 'asdf', 'asdf',
                               'Beijing', 'asdf', 'asdfas', '123')
        user = UserService().login('test1', 'password')
        assert user is not None
        print(user)

        user = UserService().login('test1', 'qweqwe')
        assert user is None

    def test_update(self):
        user = UserService().get_user_by_name('user4')
        user = UserService().update_by_uid(user.uid, gender='female', email='ewrqwr')
        assert user is not None
        print(user)

        user = UserService().update_by_name('user56', gender='female', email='awerasfsa')
        assert user is not None
        print(user)

    def test_del(self):
        # UserService().del_user_by_uid(49)
        # user = UserService().get_user_by_uid(49)
        # assert user is None

        UserService().del_user_by_name('gahon')
        user = UserService().get_user_by_name('gahon')
        assert user is None
