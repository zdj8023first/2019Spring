#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-29 13:32
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
import json

from config import DBMS
from service.user_service import UserService
from utils.func import show_next


class UserUI(object):

    @staticmethod
    def login(username, password):
        return UserService().login(username, password)

    @staticmethod
    def user_query_all(page_num=1, page_size=20, **kwargs):
        print("1. Beijing")
        print("2. Hong Kong")
        print("3. Beijing and Hong Kong")
        print("输入其他内容返回")
        mode = input("请选择操作： ")
        if mode == '1':
            total = UserService().count(db_alias=DBMS.DBMS1, **kwargs)
            UserUI.query_all(total, page_num, page_size, db_alias=DBMS.DBMS1, **kwargs)
        elif mode == '2':
            total = UserService().count(db_alias=DBMS.DBMS2, **kwargs)
            UserUI.query_all(total, page_num, page_size, db_alias=DBMS.DBMS2, **kwargs)
        elif mode == '3':
            # TODO 添加两个地区查询的方式
            pass
        return

    @staticmethod
    def query_all(total, page_num=1, page_size=20, db_alias=None, **kwargs):

        users = UserService().get_users(page_num, page_size, db_alias=db_alias, **kwargs)

        if len(users) == 0:
            print("未查找到相关用户")
            return
        UserService().pretty_users(users)

        show_next(page_num=page_num, page_size=page_size, db_alias=db_alias, next_func=UserUI.query_all, total=total,
                  **kwargs)

    @staticmethod
    def user_query_by_name():
        name = input("\n请输入查询的用户名: ")
        user = UserService().get_user_by_name(name)
        if user is None:
            print("用户名不存在")
        else:
            UserService().pretty_users([user])
            input("按回车键返回")
        return None

    @staticmethod
    def user_query_by_condition(**kwargs):
        condition = input("\n请输入json格式的查询条件: ")

        try:
            con = json.loads(condition)
            for key, value in con.items():
                if UserService().hasattr(key):
                    kwargs[key] = value
                else:
                    print("输入的JSON字段错误，请检查。")
                    return None
            return UserUI.user_query_all(**kwargs)
        except json.JSONDecodeError:
            print("输入格式错误")
            return None

    @staticmethod
    def update_user_info():
        forbid = ['id', 'name', 'region']
        username = input("请输入要更新的用户名：")
        user = UserService().get_user_by_name(username)
        if user is None:
            print("用户不存在")
            return None
        UserService().pretty_users([user])
        update_date = input("请输入修改的内容(json格式)： ")
        try:
            con = json.loads(update_date)
            return UserService().update_by_uid(user.uid, **con)
        except json.JSONDecodeError:
            print("输入不是json格式")
            return None

    @staticmethod
    def del_user():
        username = input("请输入要删除的用户名： ")
        user = UserService().get_user_by_name(username)
        if user is None:
            print("该用户不存在")
            return None
        UserService().pretty_users([user])
        choice = input("是否删除该用户？（Y or N）： ")
        if choice == 'Y' or choice == 'y' or choice == 'yes':
            UserService().del_user_by_uid(user.uid)
            print("删除成功")

        return None

    @staticmethod
    def add_user():
        # TODO 使用选项模式来进行固定项的选择

        name = input("请输入name: ")
        pwd = input("请输入pwd: ")
        gender = input("请输入gender: ")
        email = input("请输入email: ")
        phone = input("请输入phone: ")
        dept = input("请输入dept: ")
        grade = input("请输入grade: ")
        language = input("请输入language: ")
        region = input("请输入region: ")
        role = input("请输入role: ")
        preferTags = input("请输入preferTags: ")
        obtainedCredits = (input("请输入obtainedCredits: "))

        if region != "Beijing" and region != "Hong Kong":
            print("输入的region错误，请重新输入！")
            return
        UserService().register(name, pwd, gender, email, phone, dept, grade, language, region, role, preferTags,
                               obtainedCredits)
        print("添加用户成功")
        return
        pass

    @staticmethod
    def user_manage():
        print("\n" + "=" * 20)
        print("1. 查询所有用户\n2. 根据用户名查询\n3. 根据条件查询\n4. 更改用户信息\n5. 添加用户\n6. 删除用户\n7. 返回上一级")
        print("=" * 20)
        mode = input("请选择操作：  ")
        if mode == '1':
            UserUI.user_query_all()
        elif mode == '2':
            UserUI.user_query_by_name()
        elif mode == '3':
            UserUI.user_query_by_condition()
        elif mode == '4':
            UserUI.update_user_info()
        elif mode == '5':
            UserUI.add_user()
        elif mode == '6':
            UserUI.del_user()
        elif mode == '7':
            return None
        else:
            print("输入错误")
        UserUI.user_manage()


if __name__ == '__main__':
    from main import init

    init()
    UserUI.user_manage()
