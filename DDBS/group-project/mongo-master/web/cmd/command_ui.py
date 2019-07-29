#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 23:06
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from web.cmd.admin import login_by_admin
from web.cmd.user import login_by_user


# TODO 检查所有的输入操作是否都有正常输入，或者说输入是否合法
def menu():
    print("=" * 20)
    print("欢迎使用信息查询系统")
    print("请选择登录方式")
    print("1. 管理员登录\n2. 用户登录\n3. 用户注册\n4. 退出程序")
    print("=" * 20)
    mode = input("选择：")

    if mode == '1':
        username = input("请输入用户名：")
        password = input("请输入密 码： ")
        login_by_admin(username, password)
    elif mode == '2':
        username = input("请输入用户名：")
        password = input("请输入密 码： ")
        login_by_user(username, password)
    elif mode == '3':
        from web.cmd.UserUI import UserUI
        UserUI.add_user()
    elif mode == '4':
        return None
    else:
        print("输入错误请重新输入！\n")
    return menu()


if __name__ == '__main__':
    from main import init

    init()
    menu()
