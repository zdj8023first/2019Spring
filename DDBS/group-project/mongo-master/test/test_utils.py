#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-18 00:10
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from test.test_base import TestBase
from utils.func import *


class TestUtils(TestBase):

    def test_get_id_by_region(self):
        new_id = get_id_by_region(34, DBMS.region['values'][0])
        assert new_id == 34

        new_id = get_id_by_region(34, DBMS.region['values'][1])
        assert new_id == 35

        new_id = get_id_by_region(33, DBMS.region['values'][0])
        assert new_id == 34

        pass

    def test_get_id_by_category(self):
        new_id = get_id_by_category(34, DBMS.category['values'][0])
        assert new_id == 34

        new_id = get_id_by_category(34, DBMS.category['values'][1])
        assert new_id == 35

        new_id = get_id_by_category(33, DBMS.category['values'][0])
        assert new_id == 34
