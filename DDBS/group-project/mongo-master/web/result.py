#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-05-28 18:52
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from flask import jsonify


class Result(object):
    def __init__(self, code, message=None, data=None):
        self.code = code
        self.message = message
        self.data = data
        pass

    def to_json(self):
        return jsonify(code=self.code, message=self.message, data=self.data)
        pass

    @classmethod
    def gen_success(cls, data=None, msg=None):
        return cls(200, msg, data).to_json()

    @classmethod
    def gen_failed(cls, code, msg=None, data=None):
        return cls(code, msg, data).to_json()
