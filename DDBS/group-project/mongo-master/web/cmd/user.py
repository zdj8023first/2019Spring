#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 23:32
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
import json

from service.user_service import UserService
from web.cmd.ArticleUI import article_manage


def login_by_user(username, password):
    if username is None or password is None:
        return None
    user = UserService().login(username, password)
    if user is not None:
        print("登录成功。。。\n\n\n")
        return user_manage(user)

    print("用户名或密码错误，请重新输入")
    return None


def user_manage(user):
    print("=" * 20)
    print("登录用户： {0}\t 权限： user\n".format(user.name))
    print("1. 个人信息查询及修改")
    print("2. 文章管理")
    print("0. 退出登录")
    print("=" * 20)
    mode = input("请选择操作:  ")

    if mode == '1':
        show_or_update_user(user)
    elif mode == '2':
        article_manage(user=user, role='user')
        pass
    elif mode == '3':
        pass
    elif mode == '4':
        pass
    elif mode == '5':
        pass
    elif mode == '6':
        pass
    elif mode == '0':
        return
        pass
    else:
        print("输入错误")
    user_manage(user)


def show_or_update_user(user):
    UserService().pretty_users([user])

    while True:
        print("1. 修改个人信息")
        print("2. 返回上一级")
        mode = input("请选择操作： ")
        if mode == '1':
            update_info(user)
            return
        elif mode == '2':
            return
        else:
            print("输入错误")

        pass


def update_info(user, **kwargs):
    update_date = input("请输入修改的内容(json格式)： ")
    try:
        con = json.loads(update_date)
        for key, value in con.items():
            kwargs[key] = value
        if UserService().update_by_uid(user.uid, **kwargs):
            user.reload()
            print("更新成功")
        else:
            print("更新失败，请稍后重试")
    except json.JSONDecodeError:
        print("输入不是json格式")
    return None
