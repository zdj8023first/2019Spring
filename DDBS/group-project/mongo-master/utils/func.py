#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-29 14:31
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

import datetime
import functools
import json
import logging
import threading
import time

import pytz
from mongoengine.base import BaseDocument

from config import DBMS

logger = logging.getLogger('utils')


def singleton(cls):
    """
    将一个类作为单例
    来自 https://wiki.python.org/moin/PythonDecoratorLibrary#Singleton
    """

    cls.__new_original__ = cls.__new__

    @functools.wraps(cls.__new__)
    def singleton_new(cls, *args, **kw):
        it = cls.__dict__.get('__it__')
        if it is not None:
            return it

        cls.__it__ = it = cls.__new_original__(cls, *args, **kw)
        it.__init_original__(*args, **kw)
        return it

    cls.__new__ = singleton_new
    cls.__init_original__ = cls.__init__
    cls.__init__ = object.__init__

    return cls


def available_value(value):
    """
    判断是否为redis能存储的数据类型： str or bytes
    :param value:
    :return:
    """
    if isinstance(value, str) or isinstance(value, bytes):
        return value
    return str(value)


def utc_2_local(t):
    return t.astimezone()


# 使json能够转化datetime对象
class DateEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return utc_2_local(obj).strftime('%Y-%m-%d %H:%M:%S')
        elif isinstance(obj, datetime.date):
            return obj.strftime("%Y-%m-%d")
        else:
            return json.JSONEncoder.default(self, obj)


# 将 MongoDB 的 document转化为json形式
def convert_mongo_2_json(o):
    def convert(dic_data):
        # 对于引用的Id和该条数据的Id，这里都是ObjectId类型的
        from bson import ObjectId
        # 字典遍历
        for key, value in dic_data.items():
            # 如果是列表，则递归将值清洗
            if isinstance(value, list):
                for index, l in enumerate(value):
                    if isinstance(l, ObjectId):
                        value[index] = str(l)
                    elif isinstance(l, dict):
                        convert(l)
            else:
                if isinstance(value, ObjectId):
                    dic_data[key] = str(dic_data.get_one(key))
        return dic_data

    ret = {}
    # 判断其是否为Document
    if isinstance(o, BaseDocument):
        """
        转化为son形式，son的说明，摘自官方
        SON data.
        A subclass of dict that maintains ordering of keys and provides a
        few extra niceties for dealing with SON. SON provides an API
        similar to collections.OrderedDict from Python 2.7+.
        """
        data = o.to_mongo()
        # 转化为字典
        data = data.to_dict()
        ret = convert(data)
    # 将数据转化为json格式， 因json不能直接处理datetime类型的数据，故需要区分处理
    ret = json.dumps(ret, cls=DateEncoder)
    return ret


def show_next(page_num, page_size, total, next_func, db_alias=None, **kwargs):
    """
        展示上一页、下一页选项， cmd 界面接口需要
    :param page_num: 当前的
    :param page_size:
    :param total:
    :param next_func:
    :param db_alias:
    :param kwargs:
    :return:
    """
    total_pages = int((total - 1) / page_size) + 1
    flag = False

    print("\n\t\t\t\t\t当前第{page_num}页， 总共{total_pages}页，共{total}条数据"
          .format(page_num=page_num,
                  total_pages=total_pages,
                  total=total))
    if total_pages > 1:
        flag = True
    while flag:
        print("\t\t1. 上一页\t2. 下一页\t3. 指定页数\t4.返回上一级")
        mode = input("请选择操作： ")
        if mode == '1':
            if page_num <= 1:
                print("当前就在第一页哟")
            else:
                return next_func(total=total, page_num=page_num - 1, db_alias=db_alias, **kwargs)
        elif mode == '2':
            if page_num >= total_pages:
                print("当前在最后一页哟")
            else:
                return next_func(total=total, page_num=page_num + 1, db_alias=db_alias, **kwargs)
        elif mode == '3':
            num = int(input('请输入跳转页数: '))
            if 0 < num <= total_pages:
                return next_func(total=total, page_num=num, db_alias=db_alias, **kwargs)
            else:
                print("输入页码错误")
        elif mode == '4':
            return None
    input("\n\t按回车键返回")
    pass


def merge_dict_and_sort(dict_1, dict_2) -> list:
    """
        合并两个简单字典，然后进行排序输出list
    :param dict_1:
    :param dict_2:
    :return:
    """
    dict_1 = merge_dict(dict_1, dict_2)

    return sorted(dict_1.items(), key=lambda item: item[1], reverse=True)


def merge_dict(dict_1, dict_2) -> dict:
    """
        合并两个字典，相同name的value进行简单相加
    :param dict_1:
    :param dict_2:
    :return:
    """
    for key, value in dict_2.items():
        if dict_1.get(key) is None:
            dict_1.setdefault(key, value)
        else:
            dict_1[key] += value

    return dict_1


def sort_dict(data: dict) -> list:
    """
        通过value对字典进行排序， 返回list
    :param data:
    :return: list
    """
    return sorted(data.items(), key=lambda item: item[1], reverse=True)


def sort_dict_in_list(data: list, sort_by, reverse=True):
    from operator import itemgetter
    return sorted(data, key=itemgetter(sort_by), reverse=reverse)


# 工具类简单，如果是字节，转成str
def bytes_to_str(s, encoding='utf-8'):
    """Returns a str if a bytes object is given."""
    if isinstance(s, bytes):
        return s.decode(encoding)
    return s


class DbmsAliasError(Exception):
    pass


def check_alias(db_alias):
    """
        检查所传入的db_alias 是否是一个合法的DBMS地址
    :param db_alias:
    :return:
    """
    if db_alias not in DBMS.all:
        raise DbmsAliasError(f"alias {db_alias} is wrong, please check")


def is_odd(uid):
    """
        判断是不是奇数
    :param uid:
    :return:
    """
    return int(uid) % 2 == 1


def get_dbms_by_region(region):
    """
        通过region的值返回其内容存储的所有数据库地址
        # TODO 通过读取配置文件来进行返回
    :param region:
    :return:
    """
    return DBMS.region[region]


def get_best_dbms_by_region(region):
    """
        该方法用于返回当前region最好的服务器地址，目前默认为第一个
        TODO 返回当前region所对应的可连接的DBMS
    :param region:
    :return:
    """
    return get_dbms_by_region(region)[0]


def get_dbms_by_category(category):
    return DBMS.category[category]


def get_best_dbms_by_category(category):
    """
        category，目前默认为第一个
        TODO 返回当前category所对应的可连接的DBMS
    :param category:
    :return:
    """
    return get_dbms_by_category(category)[0]


def get_id_by_region(_id, region):
    """
        通过region字段来计算正确的id
            DBMS1 -> 偶数
            DBMS2 -> 奇数

    :param _id: mongo查询所返回的id
    :param region:  该当前数据的region地址
    :return:
    """
    if DBMS.region[region][0] == DBMS.DBMS1:
        return _id if not is_odd(_id) else _id + 1
    else:
        return _id if is_odd(_id) else _id + 1


# def get_dbms_by_uid(uid):
#     """
#         通过uid返回该uid对应的用户数据存储地址
#         TODO 从配置文件获取地址配置信息
#             # DBMS1 -> 偶数
#             # DBMS2 -> 奇数
#     :param uid:
#     :return:
#     """
#     if is_odd(uid):
#         return get_dbms_by_region(DBMS.region['values'][1])
#     else:
#         return get_dbms_by_region(DBMS.region['values'][0])


# def get_best_dbms_by_uid(uid):
#     """
#         该方法用于返回当前uid最好的服务器地址，目前默认为第一个
#         TODO 返回当前uid所对应的可连接的DBMS
#     :param uid:
#     :return:
#     """
#     return get_dbms_by_uid(int(uid))[0]


def get_id_by_category(_id, category):
    """
        DBMS1 -> 偶数
        DBMS2 -> 奇数
    :param _id:
    :param category:
    :return:
    """
    if DBMS.category[category][0] == DBMS.DBMS1:
        return _id if not is_odd(_id) else _id + 1
    else:
        return _id if is_odd(_id) == 1 else _id + 1


# def get_dbms_by_aid(aid):
#     # TODO 统一奇偶
#     if is_odd(aid):
#         return get_dbms_by_category(DBMS.category['values'][1])
#     else:
#         return get_dbms_by_category(DBMS.category['values'][0])


def get_best_dbms():
    """
        该方法用于返回当前aid最好的服务器地址，目前默认为第一个
        TODO 返回当前aid所对应的可连接的DBMS
    :param aid:
    :return:
    """
    return DBMS.all[0]


def timestamp_to_time(timestamp: int):
    if len(str(timestamp)) == 13:
        timestamp = int(timestamp / 1000)
    time_s = time.localtime(timestamp)
    return time.strftime('%Y-%m-%d %H:%M:%S', time_s)


def str_to_time(string):
    return datetime.datetime.strptime(string, '%Y-%m-%d %H:%M:%S')


def timestamp_to_datetime(timestamp):
    tz = pytz.timezone("Asia/Shanghai")
    timestamp = int(timestamp)
    if len(str(timestamp)) == 13:
        return datetime.datetime.fromtimestamp(timestamp / 1000, tz=tz)
    else:
        return datetime.datetime.fromtimestamp(timestamp, tz=tz)


def timestamp_to_str(timestamp):
    tz = pytz.timezone("Asia/Shanghai")
    timestamp = int(timestamp)
    if len(str(timestamp)) == 13:
        return datetime.datetime.fromtimestamp(timestamp / 1000, tz=tz).strftime('%Y-%m-%d %H:%M:%S')
    else:
        return datetime.datetime.fromtimestamp(timestamp, tz=tz).strftime('%Y-%m-%d %H:%M:%S')


def datetime_to_str(_datetime):
    return _datetime.strftime('%Y-%m-%d %H:%M:%S')


def date_to_datetime(_date):
    if isinstance(_date, datetime.date):
        return datetime.datetime.strptime(str("{}-{}-{}".format(_date.year, _date.month, _date.day + 1)), '%Y-%m-%d')


def datetime_to_timestamp(_date: datetime.datetime):
    if isinstance(_date, str) and len(_date) == 13:
        return int(_date)
    if isinstance(_date, int):
        return _date
    # if isinstance(_date, datetime.date):
    #     _date = date_to_datetime(_date)
    return int(_date.timestamp() * 1000)


def date_to_timestamp(_date: datetime.date):
    if isinstance(_date, str) and len(_date) == 13:
        return int(_date)
    if isinstance(_date, int):
        return _date
    _datetime = datetime.datetime.strptime(str("{}-{}-{}".format(_date.year, _date.month, _date.day)), '%Y-%m-%d')
    return int(_datetime.timestamp() * 1000)


def str_to_datetime(_date: str):
    return datetime.datetime.strptime(_date, '%Y-%m-%d')


def get_timestamp():
    return int(datetime.datetime.now().timestamp() * 1000)


def pretty_models(models, field_names: list):
    from prettytable import PrettyTable
    from datetime import datetime

    x = PrettyTable()

    if not isinstance(models, list):
        models = list(models)

    if len(models) == 0:
        return
        # field_names = (
    #     'aid', 'title', 'category', 'abstract', 'articleTags', 'authors', 'language', 'timestamp', 'update_time')
    x.field_names = field_names
    if isinstance(models[0], dict):
        for model in models:
            # 需要对时间进行时区转换
            x.add_row(list(model[key] for key in field_names))
        print(x)
    else:
        for model in models:
            # 需要对时间进行时区转换
            x.add_row(list(model.__getattribute__(key).astimezone()
                           if isinstance(model.__getattribute__(key), datetime)
                           else model.__getattribute__(key)
                           for key in field_names
                           ))
        print(x)
    pass


def create_thread_and_run(jobs, callback_name, wait=True, daemon=True, args=(), kwargs={}):
    threads = []
    if not isinstance(jobs, list): jobs = [jobs]
    for job in jobs:
        thread = threading.Thread(target=getattr(job, callback_name), args=args, kwargs=kwargs)
        thread.setDaemon(daemon)
        thread.start()
        threads.append(thread)
    if wait:
        for thread in threads: thread.join()


# 计算时间函数
def print_run_time(func):
    def wrapper(*args, **kw):
        local_time = time.time()
        func(*args, **kw)
        logger.info('\ncurrent Function [%s] run time is %.2fs' % (func.__name__, time.time() - local_time))

    return wrapper


MAX_AUTO_RECONNECT_ATTEMPTS = 5


def auto_reconnect(mongo_op_func):
    from pymongo.errors import AutoReconnect
    """Gracefully handle a reconnection event."""

    @functools.wraps(mongo_op_func)
    def wrapper(*args, **kwargs):
        for attempt in range(MAX_AUTO_RECONNECT_ATTEMPTS):
            try:
                return mongo_op_func(*args, **kwargs)
            except AutoReconnect as e:
                wait_t = 0.5 * pow(2, attempt)  # exponential back off
                logging.warning("PyMongo auto-reconnecting... %s. Waiting %.1f seconds.", str(e), wait_t)
                time.sleep(wait_t)

    return wrapper
