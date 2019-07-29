#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-03 16:05
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from mongoengine import *

from db.mongodb import BaseDB

temporalChoices = ('daily', 'weekly', 'monthly')


class Popular(BaseDB):
    meta = {
        'abstract': True,
        'indexes': [
            # 'temporalGranularity',
            # 'timestamp',
            ('temporalGranularity', 'timestamp')
        ]
    }

    temporalGranularity = StringField(max_length=7, choices=temporalChoices)
    articleAidDict = DictField(default={})
    timestamp = IntField(required=True)
    update_time = IntField(required=True)

    # timestamp = DateTimeField(default=datetime.now)
    @property
    def create_time(self):
        # 创建时间
        return self.id.generation_time

    def to_dict(self, **kwargs):
        """
            将该类数据转换为dict，以供快捷转换为str或者list

        :param include: 需要返回显示的字段名，为空的话则显示全部字段
        :param exclude: 不需要返回的字段，include为空才生效
        :return: dict
        """
        # 时间处理

        my_dict = super(Popular, self).to_dict(**kwargs)

        from utils.func import timestamp_to_str
        if 'update_time' in my_dict.keys():
            my_dict['update_time'] = timestamp_to_str(self.update_time)

        if 'timestamp' in my_dict.keys():
            my_dict['timestamp'] = timestamp_to_str(self.timestamp)[:10]

        my_dict['id'] = str(my_dict['id'])
        return my_dict
