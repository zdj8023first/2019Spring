#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-06-08 15:17
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import logging
from datetime import timedelta

from flask import Flask, request
from flask_cors import CORS
from flask_jwt_extended import JWTManager

from config import Config
from service.user_service import UserService
from utils.func import singleton, get_best_dbms_by_region
from web.result import Result


@singleton
class Web:
    session = None
    jwt = None
    log = None

    def __init__(self):
        self.session = Flask(__name__)
        self.log = logging.getLogger('werkzeug')
        # self.log.setLevel(logging.ERROR)

        self.register_blueprint()
        self.session.config['JWT_SECRET_KEY'] = 'this is my key'  # 目前都是本地，暂不用放配置文件
        self.session.config['JWT_ACCESS_TOKEN_EXPIRES'] = timedelta(seconds=60 * 60 * 24 * 7)  # Token 超时时间 7 天
        self.jwt = JWTManager(self.session)
        CORS(self.session, supports_credentials=True)

    def register_blueprint(self):
        from web.api.user import users
        from web.api.dashboard import dashboard
        from web.api.article import articles
        from web.api.be_read import records
        from web.api.popular import populars
        from web.api.read import reads

        self.session.register_blueprint(dashboard, url_prefix='/api/dashboard')
        self.session.register_blueprint(users, url_prefix='/api/users')
        self.session.register_blueprint(articles, url_prefix='/api/articles')
        self.session.register_blueprint(records, url_prefix='/api/records')
        self.session.register_blueprint(populars, url_prefix='/api/populars')
        self.session.register_blueprint(reads, url_prefix='/api/reads')

    @classmethod
    def run(cls):
        self = cls()
        self.start()

    def start(self):
        self.loading_setting()
        self.run_session()
        # if not Config().WEB_ENABLE or Config().is_slave(): return
        # if Config().IS_DEBUG:
        #     self.run_session()
        # else:
        # create_thread_and_run(self, 'run_session', wait=False)

    def run_session(self):
        # debug = False
        debug = Config().IS_DEBUG
        self.session.run(debug=debug, port=Config().WEB_PORT, host='0.0.0.0')

    def loading_setting(self):
        @self.session.before_request
        def cors():
            if request.method == 'OPTIONS':
                return

        @self.jwt.user_loader_callback_loader
        def user_loader_callback(identity):
            user = UserService().get_user_by_uid(identity['uid'], db_alias=get_best_dbms_by_region(identity['region']))
            return user

        @self.jwt.user_loader_error_loader
        def custom_user_loader_error(identity):
            ret = {
                "msg": "User {} not found".format(identity)
            }
            return Result.gen_failed(404, msg=ret)


if __name__ == '__main__':
    from main import init

    init()

    Web().run()
