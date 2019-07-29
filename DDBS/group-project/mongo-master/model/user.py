from mongoengine import *

from db.mongodb import BaseDB


class User(BaseDB):
    meta = {
        'abstract': True,
        # 'allow_inheritance': True,
        'index_background': True,
        'indexes': [
            # 'uid',
            'name',
        ]
    }
    uid = IntField(primary_key=True)  # Beijing 偶数， hk 奇数
    name = StringField(required=True, unique=True)
    pwd = StringField(required=True)
    gender = StringField(required=True)
    email = StringField(default='')
    phone = StringField(default='')
    dept = StringField(default='')
    grade = StringField(default='')
    language = StringField(default='')
    region = StringField(required=True)
    role = StringField(default='')
    preferTags = StringField(default='')
    obtainedCredits = StringField(default='')
    timestamp = IntField(required=True)

    def to_dict(self, **kwargs):
        """
            将该类数据转换为dict，以供快捷转换为str或者list

        :param include: 需要返回显示的字段名，为空的话则显示全部字段
        :param exclude: 不需要返回的字段，include为空才生效
        :return: dict
        """
        # 时间处理

        my_dict = super(User, self).to_dict(**kwargs)

        if 'timestamp' in my_dict.keys():
            from utils.func import timestamp_to_str
            my_dict['timestamp'] = timestamp_to_str(self.timestamp)

        return my_dict

    def update(self, pwd, gender, email, phone, dept, grade, language, region, role, preferTags,
               obtainedCredits: int):
        self.pwd = pwd
        self.gender = gender
        self.email = email
        self.phone = phone
        self.dept = dept
        self.grade = grade
        self.language = language
        self.region = region
        self.role = role
        self.preferTags = preferTags
        self.obtainedCredits = obtainedCredits
        self.save()
