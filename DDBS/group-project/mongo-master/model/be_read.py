#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-03 14:22
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from mongoengine import *

from db.mongodb import BaseDB
from utils.func import timestamp_to_str


class BeRead(BaseDB):
    # 可以尝试使用LazyReferenceField，看能否优化查询性能

    meta = {
        'abstract': True,
        'indexes': [
            # 'bid',
            'aid',
            # 'readNum',
            # 'commentNum',
            # 'agreeNum',
            # 'shareNum'
        ]
    }

    bid = IntField(primary_key=True)
    aid = IntField(required=True, unique=True)
    readNum = IntField(default=0)
    readUidList = ListField(IntField(required=False), default=list())
    commentNum = IntField(default=0)
    commentUidList = ListField(IntField(required=False), default=list())
    agreeNum = IntField(default=0)
    agreeUidList = ListField(IntField(required=False), default=list())
    shareNum = IntField(default=0)
    shareUidList = ListField(IntField(required=False), default=list())
    timestamp = IntField(required=True)
    last_update_time = DateTimeField(required=True)

    def to_dict(self, **kwargs):
        """
            将该类数据转换为dict，以供快捷转换为str或者list

        :param include: 需要返回显示的字段名，为空的话则显示全部字段
        :param exclude: 不需要返回的字段，include为空才生效
        :return: dict
        """
        # 时间处理

        my_dict = super(BeRead, self).to_dict(**kwargs)

        if 'last_update_time' in my_dict.keys():
            my_dict['last_update_time'] = self.last_update_time.strftime('%Y-%m-%d %H:%M:%S')

        if 'timestamp' in my_dict.keys():
            my_dict['timestamp'] = timestamp_to_str(self.timestamp)

        return my_dict

    @classmethod
    def add_read_record(cls, read):
        pass

    def never_read(self, user):
        return user not in self.readUidList

    def never_comment(self, user):
        if user not in self.commentUidList:
            return True
        else:
            return False

    def never_agree(self, user):
        if user not in self.agreeUidList:
            return True
        else:
            return False

    def never_share(self, user):
        if user not in self.shareUidList:
            return True
        else:
            return False

    @classmethod
    def get_top_read(cls, num=10):
        return cls.objects.order_by('-readNum').limit(num)

    @classmethod
    def get_top_comment(cls, num=10):
        return cls.objects.order_by('-commentNum').limit(num)

    @classmethod
    def get_top_agree(cls, num=10):
        return cls.objects.order_by('-agreeNum').limit(num)

    @classmethod
    def get_top_share(cls, num=10):
        return cls.objects.order_by('-shareNum').limit(num)
