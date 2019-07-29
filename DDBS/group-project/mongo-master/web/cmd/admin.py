#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 23:31
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from service.user_service import UserService
from web.cmd.ArticleUI import article_manage
from web.cmd.UserUI import UserUI


def login_by_admin(username, password):
    if username == 'admin' and password is not None:
        admin = UserService().login(username, password)
        if admin is not None:
            return admin_manage(admin)
    print("用户名或密码错误，请重新输入")
    return None


def admin_manage(admin):
    print("\n" + "=" * 20)
    print("当前登录用户： admin\t 权限： admin\n")
    print("1. 用户管理\n2. 文章管理\n3. 退出登录")
    print("=" * 20)
    mode = input("请选择操作:  ")
    if mode == '1':
        UserUI.user_manage()
    elif mode == '2':
        article_manage(role='admin', user=admin)
    elif mode == '3':
        return None
    else:
        print("输入错误。。。")
    admin_manage(admin)


if __name__ == '__main__':
    from main import init

    init()
    # UserUI.user_manage()
    user = UserUI.login('admin', 'admin')
    admin_manage(user)
