#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 20:13
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from model.user import User
from service.ids_service import IdsService
from utils.func import *

logger = logging.getLogger('userService')


@singleton
class UserService(object):
    field_names = list(User._db_field_map.keys())
    forbid_field = ['uid', 'region', 'timestamp']

    def __init__(self):
        self.user = User
        self.models = dict()
        self.classes = dict()
        for dbms in DBMS.all:
            self.models[dbms] = list()
            self.classes[dbms] = self.__gen_model(dbms)

    def __gen_model(self, dbms):
        class Model(User):
            meta = {
                'db_alias': dbms,
                'collection': 'user'
            }
            pass

        return Model

    def get_model(self, dbms):
        return self.classes[dbms]

    @staticmethod
    def hasattr(key):
        """
            判断User中是否有key这个属性
        :param key:  待判断的属性
        :return: True or False
        """
        return hasattr(User, key)

    @staticmethod
    def get_uid():
        """
            通过region来获取当前region所对应的数据库的下一个id应该是啥
        # :param region: 地区，该参数需要和consts 类中Region所对应的值相等
        :return:
        """
        return IdsService().next_id('uid')

    def register(self, name, pwd, gender, email, phone, dept, grade, language, region, role, preferTags,
                 obtainedCredits, timestamp=None, is_multi=False):
        """
            用户注册，返回注册结果和信息
        :param is_multi: 是不是批量注册， 批量注册的话不会自动保存，主要用于导入数据
        :param timestamp:
        :param name:  用户名
        :param pwd:     密码
        :param gender:  性别
        :param email:
        :param phone:
        :param dept:
        :param grade:
        :param language:
        :param region: 地区，该值应与Region类中的值一样
        :param role:
        :param preferTags:
        :param obtainedCredits:
        :return: list[True or False, msg]
        """

        if self.is_name_used(name):
            logger.info('用户名已存在')
            return False, '用户名已存在'

        re = False, '注册失败'

        uid = self.get_uid()
        for dbms in get_dbms_by_region(region):
            re = self.__register(uid, name, pwd, gender, email, phone, dept, grade, language, region, role,
                                 preferTags, obtainedCredits, timestamp, db_alias=dbms, is_multi=is_multi)
            if not re[0]:
                break
        return re

    def __register(self, uid, name, pwd, gender, email, phone, dept, grade, language, region, role, preferTags,
                   obtainedCredits: int, timestamp=None, db_alias=None, is_multi=False):

        check_alias(db_alias)

        user = self.get_model(db_alias)()
        user.uid = uid
        user.name = name
        user.pwd = pwd
        user.gender = gender
        user.email = email
        user.phone = phone
        user.dept = dept
        user.grade = grade
        user.language = language
        user.region = region
        user.role = role
        user.preferTags = preferTags
        user.obtainedCredits = obtainedCredits
        user.timestamp = timestamp or get_timestamp()

        if is_multi:
            self.models[db_alias].append(user)
            return True, ''
        else:
            re = user.save()
            if re is not None:
                # logger.info('用户：{} 注册成功'.format(name))
                return True, '用户：{} 注册成功'.format(name)
            return False, '保存至数据库失败'

    # def users_list(self, page_num=1, page_size=20, only: list = None, exclude: list = None, db_alias=None,
    #                **kwargs) -> list:
    #     """
    #         获取db_alias 所对应的数据库里边的用户列表，采用分页
    #         可以通过**kwargs进行高级查询，
    #     :param exclude: 在查询结果中排出某些字段
    #     :param only:    只查询指定字段的值
    #     :param page_num:    查询页码
    #     :param page_size:   每页的数据量大小
    #     :param db_alias:    数据库的别名
    #     :param kwargs:      其他的一些查询参数
    #     :return:
    #     """
    #     check_alias(db_alias)
    #     return self.get_model(db_alias).get_all(page_num, page_size, only=only, exclude=exclude, **kwargs)

    def get_users(self, page_num=1, page_size=20, only: list = None, exclude: list = None, db_alias=None, **kwargs):
        # TODO 去掉默认db设置，在所有数据库中，根据数量进行分页以及返回相关数据
        region = kwargs.get('region', None)
        if region is not None:
            db_alias = get_best_dbms_by_region(region)
        if db_alias is None:
            users = []
            for region in DBMS.region['values']:
                count = self.count(db_alias=get_best_dbms_by_region(region))
                if count >= (page_num - 1) * page_size:
                    us = self.get_users(page_num, page_size, only=only, exclude=exclude,
                                        db_alias=get_best_dbms_by_region(region), **kwargs)
                    users.extend(us)
                if len(users) == page_size:
                    break
                else:
                    page_num = (page_num * page_size - count) / (page_size - len(users))
                    page_size = page_size - len(users)
            return list(users)
        else:
            check_alias(db_alias)
            return list(
                self.get_model(db_alias).get_all(page_num, page_size, only=only, exclude=exclude, **kwargs))

    def count(self, db_alias=None, **kwargs):
        """
            计算db_alias所对应的的数据库下满足条件的用户数量
        :param db_alias:
        :param kwargs:  查询参数字典， 为空则统计所有用户数量
        :return:
        """
        check_alias(db_alias)
        return self.get_model(db_alias).count(**kwargs)

    def count_all(self, **kwargs):
        """
            统计所有数据库中的用户总量
        :param kwargs:  制定特殊查询参数
        :return:
        """
        if 'region' in kwargs.keys():
            return self.count(db_alias=get_best_dbms_by_region(kwargs.get('region')))

        count = 0
        for dbms in DBMS.all:
            count += self.count(db_alias=dbms, **kwargs)
        return count

    def is_name_used(self, name: str, db_alias: str = None):
        used = False
        if db_alias is None:
            for dbms in DBMS.all:
                used = self.is_name_used(name, db_alias=dbms)
                if used:
                    break
            return used
        else:
            check_alias(db_alias)
            return self.get_model(db_alias).count(name=name) != 0

    def get_user_by_name(self, name: str, db_alias: str = None, **kwargs) -> User:
        """
            通过用户名进行查询
        :param name:    查询的用户名
        :param db_alias:    查找的数据库名称， 如果为空的话则查询所有数据库
        :return:
        """
        user = None
        if db_alias is None:
            for dbms in DBMS.all:
                user = self.get_user_by_name(name, db_alias=dbms, **kwargs)
                if user is not None:
                    break
            return user
        else:
            check_alias(db_alias)
            return self.get_model(db_alias).get_one(name=name, **kwargs)

    # def get_user_by_uid(self, _id, db_alias=None) -> User:
    #     """
    #         根据id进行用户查询
    #     :param db_alias:
    #     :param _id:
    #     :return:
    #     """
    #     if db_alias is None:
    #         user = None
    #         for dbms in DBMS.all:
    #             user = self.get_user_by_uid(_id=_id, db_alias=dbms)
    #             if user is not None:
    #                 break
    #         return user
    #     else:
    #         check_alias(db_alias)
    #         if not isinstance(_id, ObjectId):
    #             _id = ObjectId(_id)
    #         return self.get_model(db_alias).get(id=_id)

    def get_user_by_uid(self, uid, db_alias=None, **kwargs) -> User:
        """
            根据id进行用户查询
        :param db_alias:
        :param uid:
        :return:
        """
        if db_alias is None:
            user = None
            for dbms in DBMS.all:
                user = self.get_user_by_uid(uid=uid, db_alias=dbms, **kwargs)
                if user is not None:
                    break
            return user
        else:
            check_alias(db_alias)
            user = self.get_model(db_alias).get_one(uid=uid, **kwargs)
            if user is not None:
                user.avatar = "https://wpimg.wallstcn.com/f778738c-e4f8-4870-b634-56703b4acafe.gif"
            return user

    def login(self, username, password) -> User:
        """
            用户登录
        :param username:
        :param password:
        :return: User实体，登录失败返回None
        """
        user = None
        for dbms in DBMS.all:
            user = self.__login(username, password, db_alias=dbms)
            if user is not None:
                break
        return user

    def __login(self, username, password, db_alias=None):
        check_alias(db_alias)
        user = self.get_model(db_alias).get_one(name=username, pwd=password)
        if user is not None:
            logger.info("用户 {} 登录成功".format(username))
            return user
        else:
            logger.info("用户名或者密码错误")
        return None

    def logout(self, name):
        logger.info('用户 {} 退出登录'.format(name))
        return True

    def update_by_uid_with_dbms(self, uid, db_alias, **kwargs):
        check_alias(db_alias)

        return self.get_model(db_alias).objects(uid=uid).update_one(**kwargs)

    def update_by_uid(self, uid, db_alias=None, **kwargs):
        """
            根据name进行更新
        :param db_alias:
        :param uid:
        :param kwargs:  更新的数据
        :return:
        """
        if db_alias is None:
            user = self.get_user_by_uid(uid, only=['region'])
            if user is None:
                if db_alias is None:
                    logger.info("uid:{}不存在".format(uid))
                else:
                    logger.info("数据不同步， DBMS: {}中找不到{}的信息".format(db_alias, uid))
                return None

            # TODO 如何判断更新失败？（例如更新时网络异常）
            for dbms in get_dbms_by_region(user.region):
                user = self.update_by_uid(uid, db_alias=dbms, **kwargs)
                # if user is None:
                #     break
            return user
        else:
            check_alias(db_alias)
            user = self.get_user_by_uid(uid, db_alias=db_alias)
            return self._update_one(user, **kwargs)

    def update_by_name(self, name, db_alias=None, **kwargs):
        """
            根据name进行更新
        :param db_alias:
        :param name:
        :param kwargs:  更新的数据
        :return:
        """
        user = self.get_user_by_name(name, db_alias=db_alias)
        if user is None:
            if db_alias is None:
                logger.info("name:{}不存在".format(name))
            else:
                logger.info("数据不同步， DBMS: {}中找不到{}的信息".format(db_alias, name))
            return None
        if db_alias is None:

            # TODO 如何判断更新失败？（例如更新时网络异常）
            for dbms in get_dbms_by_region(user.region):
                user = self.update_by_name(name, db_alias=dbms, **kwargs)
                if user is None:
                    break
            return user
        else:
            check_alias(db_alias)
            return self._update_one(user, **kwargs)

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

    def _update_one(self, user: User, **kwargs):
        """
            根据user实例进行更新用户数据
        :param user:    待更新的用户数据
        # :param db_alias:
        :param kwargs:      更新的字段和对应的值
        :return:
        """
        forbid = ('name', 'id', 'uid', 'region')
        if user is None:
            logger.info("用户不存在")
            return None

        for key in kwargs:
            if key not in forbid and hasattr(user, key):
                setattr(user, key, kwargs[key])
            else:
                logger.info("修改不存在的字段或者禁止字段： {}".format(key))

        # TODO 如何判断更新失败？（例如更新时网络异常）
        return user.save()

    def del_user_by_name(self, name, db_alias=None):

        user = self.get_user_by_name(name=name, db_alias=db_alias, only=['region'])

        if user is not None:
            if db_alias is None:
                for dbms in get_dbms_by_region(user.region):
                    self.del_user_by_name(name, db_alias=dbms)
                    # if not success:
                    #     return False
                return True
            else:
                return user.delete()
        return False

    # def del_user_by_id(self, uid, db_alias=None):
    #     user = self.get_user_by_uid(uid=uid, db_alias=db_alias)
    #     if db_alias is None:
    #         if user is not None:
    #             for dbms in get_dbms_by_region(user.region):
    #                 self.del_user_by_id(uid, db_alias=dbms)
    #                 # if not success:
    #                 #     return False
    #         return True
    #     else:
    #         check_alias(db_alias)
    #         return user.delete()

    def del_user_by_uid(self, uid, db_alias=None):
        user = self.get_user_by_uid(uid=uid, db_alias=db_alias, only=['region'])
        if db_alias is None:
            if user is not None:
                for dbms in get_dbms_by_region(user.region):
                    self.del_user_by_uid(uid, db_alias=dbms)
                    # if not success:
                    #     return False
            return True
        else:
            check_alias(db_alias)
            return user.delete()

    @staticmethod
    def pretty_users(users):
        pretty_models(users, UserService.field_names)

    # ============================= 待调整     =======================

    # @staticmethod
    # @switch_mongo_db(cls=User)
    # def update_user_by_admin(user, db_alias=None):
    #     check_alias(db_alias)
    #     user.save()
    #     # TODO 如何判断更新失败？（例如更新时网络异常）
    #     return True

    # @staticmethod
    # @switch_mongo_db(cls=User)
    # def del_user_by_id(_id, db_alias=None):
    #     check_alias(db_alias)
    #     user = User.get(id=_id)
    #     if user is not None:
    #         user.delete()
    #         return True
    #     return False

    def import_user_from_dict(self, data):
        for dbms in get_dbms_by_region(data['region']):
            user = self.get_model(dbms)()
            user.uid = int(data['uid'])
            user.name = data['name']
            user.pwd = data['pwd']
            user.gender = data['gender']
            user.email = data['email']
            user.phone = data['phone']
            user.dept = data['dept']
            user.grade = data['grade']
            user.language = data['language']
            user.region = data['region']
            user.role = data['role']
            user.preferTags = data['preferTags']
            user.obtainedCredits = data['obtainedCredits']
            user.timestamp = int(data['timestamp'])
            self.models[dbms].append(user)
        pass


if __name__ == '__main__':
    from main import init

    init()

    _users = UserService().get_users()
    UserService().pretty_users(_users)
