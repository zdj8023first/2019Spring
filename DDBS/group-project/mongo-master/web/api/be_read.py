#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-06-04 22:25
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from flask import request, Blueprint

from config import DBMS
from service.be_read_service import BeReadService
from service.read_service import ReadService
from service.user_service import UserService
from utils.func import get_best_dbms_by_category, timestamp_to_str, sort_dict_in_list
from web.result import Result

records = Blueprint('records', __name__)


@records.route('<int:aid>', methods=['GET'])
def get_be_reads(aid):
    category = request.args.get('category')
    if category is not None:

        if category not in DBMS().category['values']:
            return Result.gen_failed('404', '类别不存在')

    record = {}

    be_read = BeReadService().get_one_by_aid(aid,
                                             exclude=['readUidList', 'agreeUidList', 'shareUidList'],
                                             db_alias=get_best_dbms_by_category("technology"))
    # print(be_read)
    if be_read is not None:
        record = be_read.to_dict(
            exclude=['commentUidList', 'readUidList', 'agreeUidList', 'shareUidList'])
        comments = []
        for uid in be_read.commentUidList:
            comment = []
            user = None
            for dbms in DBMS().get_all_dbms_by_region():
                user = UserService().get_user_by_uid(uid=uid, db_alias=dbms, only=['name'])

                if user is not None:
                    comment = ReadService().get_by_uid_and_aid(uid=int(uid),
                                                               aid=int(aid),
                                                               only=['rid', 'commentDetail', 'timestamp'],
                                                               commentOrNot=1,
                                                               db_alias=dbms)
                    break

            if (comment != []) and user is not None:
                for c in comment:
                    if c.commentDetail != '' and c.commentDetail is not None:
                        comments.append(
                            {'rid': c.rid,
                             'name': user.name,
                             'avatar': getattr(user, 'avatar',
                                               'https://wpimg.wallstcn.com/f778738c-e4f8-4870-b634-56703b4acafe.gif'),
                             'commentDetail': c.commentDetail,
                             'timestamp': timestamp_to_str(c.timestamp)
                             })
        record['comments'] = sort_dict_in_list(comments, sort_by='timestamp', reverse=True)
    return Result.gen_success(data=record)

# class ArticleRecord(MethodView):
#     def get(self, aid):
#
#         category = request.args.get('category')
#         if category is not None:
#
#             if category not in DBMS().category['values']:
#                 return Result.gen_failed('404', '类别不存在')
#
#         record = {}
#
#         be_read = BeReadService().get_one_by_aid(aid,
#                                                  exclude=['readUidList', 'agreeUidList', 'shareUidList'],
#                                                  db_alias=get_best_dbms_by_category("technology"))
#         # print(be_read)
#         if be_read is not None:
#             record = be_read.to_dict(
#                 exclude=['commentUidList', 'readUidList', 'agreeUidList', 'shareUidList'])
#             comments = []
#             for uid in be_read.commentUidList:
#                 comment = []
#                 user = None
#                 for dbms in DBMS().get_all_dbms_by_region():
#                     user = UserService().get_user_by_uid(uid=uid, db_alias=dbms, only=['name'])
#
#                     if user is not None:
#                         comment = ReadService().get_by_uid_and_aid(uid=int(uid),
#                                                                    aid=int(aid),
#                                                                    only=['rid', 'commentDetail', 'timestamp'],
#                                                                    commentOrNot=1,
#                                                                    db_alias=dbms)
#                         break
#
#                 if (comment != []) and user is not None:
#                     for c in comment:
#                         if c.commentDetail != '' and c.commentDetail is not None:
#                             comments.append(
#                                 {'rid': c.rid,
#                                  'name': user.name,
#                                  'avatar': getattr(user, 'avatar',
#                                                    'https://wpimg.wallstcn.com/f778738c-e4f8-4870-b634-56703b4acafe.gif'),
#                                  'commentDetail': c.commentDetail,
#                                  'timestamp': timestamp_to_str(c.timestamp)
#                                  })
#             record['comments'] = sort_dict_in_list(comments, sort_by='timestamp', reverse=True)
#         return Result.gen_success(data=record)
