from mongoengine import *

from db.mongodb import BaseDB


class Read(BaseDB):
    meta = {
        'abstract': True,
        'indexes': [
            # 'rid',
            'uid',
            'aid',
            ('aid', 'uid'),
            # ('aid', 'readOrNot'),
            # ('aid', 'agreeOrNot'),
            # ('aid', 'commentOrNot'),
            # ('aid', 'shareOrNot'),
        ]
    }

    rid = IntField(primary_key=True)
    uid = IntField(required=True)
    aid = IntField(required=True)
    readOrNot = IntField(default=1)
    readTimeLength = IntField(default=0)
    readSequence = IntField(default=1)
    agreeOrNot = IntField(default=0)
    commentOrNot = IntField(default=0)
    shareOrNot = IntField(default=0)
    commentDetail = StringField(default='')

    timestamp = IntField(required=True)

    def to_dict(self, **kwargs):
        """
            将该类数据转换为dict，以供快捷转换为str或者list

        :param include: 需要返回显示的字段名，为空的话则显示全部字段
        :param exclude: 不需要返回的字段，include为空才生效
        :return: dict
        """
        # 时间处理

        my_dict = super(Read, self).to_dict(**kwargs)

        if 'timestamp' in my_dict.keys():
            from utils.func import timestamp_to_str
            my_dict['timestamp'] = timestamp_to_str(self.timestamp)

        return my_dict
