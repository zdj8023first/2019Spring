#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-27 01:46
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import logging

from config import Config
from db.mongodb import init_connect
from web.cmd.command_ui import menu


def init():
    init_connect()
    logging.basicConfig(level=Config.debug_level,
                        filename=Config.log_file_name if Config.log_in_file else None,
                        filemode='w',
                        datefmt='%Y/%m/%d %H:%M:%S',
                        format='%(asctime)s %(levelname)s %(name)s line_%(lineno)d: %(message)s')


def start():
    init()
    logging.info('start')
    menu()


if __name__ == "__main__":
    start()
