#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-06-08 15:57
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from flask import request, Blueprint

from web.result import Result

dashboard = Blueprint('dashboard', __name__)


@dashboard.route('', methods=['GET'])
def get_dashboard_info():
    from config import DBMS
    from service.user_service import UserService
    from service.article_service import ArticleService
    from service.read_service import ReadService
    from service.popular_service import PopularService
    dbms = request.args.get('dbms')
    nums = {
        'users': UserService().count(db_alias=dbms),
        'articles': ArticleService().count(db_alias=dbms),
        'reads': ReadService().count(db_alias=dbms),
        'populars': PopularService().count(temporalGranularity='daily', db_alias=dbms)
    }

    charts = {
        'users': [],
        'articles': [],
        'reads': []
    }

    for dbms in DBMS().get_all_dbms_by_region():
        charts['users'].append({'name': dbms, 'value': UserService().count(db_alias=dbms)})
        charts['articles'].append({'name': dbms, 'value': ArticleService().count(db_alias=dbms)})
        charts['reads'].append({'name': dbms, 'value': ReadService().count(db_alias=dbms)})

    data = {
        'nums': nums,
        'charts': charts
    }

    # if dbms == 'Beijing':
    #     data = {
    #         'users': 12354,
    #         'articles': 533366,
    #         'reads': 23424,
    #         'populars': 90372
    #     }
    # else:
    #     data = {
    #         'users': 63234,
    #         'articles': 1284,
    #         'reads': 724933,
    #         'populars': 8422
    #     }
    return Result.gen_success(data=data)
    pass
