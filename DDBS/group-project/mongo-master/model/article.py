from mongoengine import *

from db.mongodb import BaseDB
from utils.func import timestamp_to_str


class Article(BaseDB):
    aid = IntField(primary_key=True)  # 通过aid的奇偶来判断存到哪一个数据库上去
    title = StringField(required=True)

    category = StringField(required=True)
    abstract = StringField(required=True)
    articleTags = StringField(required=True)
    authors = StringField(required=True)
    language = StringField(required=True)
    text = StringField(required=True)
    image = StringField()
    video = StringField()
    update_time = DateTimeField(required=True)
    timestamp = IntField(required=True)

    meta = {
        'abstract': True,
        'indexes': [
            # 'aid',
            'title',
            'category',
            # 'articleTags',
            'authors'
        ]
    }

    def to_dict(self, **kwargs):
        """
            将该类数据转换为dict，以供快捷转换为str或者list

        :param only: 需要返回显示的字段名，为空的话则显示全部字段
        :param exclude: 不需要返回的字段，include为空才生效
        :return: dict
        """
        # 时间处理

        my_dict = super(Article, self).to_dict(**kwargs)

        if 'update_time' in my_dict.keys():
            my_dict['update_time'] = self.update_time.strftime('%Y-%m-%d %H:%M:%S')

        if 'timestamp' in my_dict.keys():
            my_dict['timestamp'] = timestamp_to_str(self.timestamp)

        return my_dict
