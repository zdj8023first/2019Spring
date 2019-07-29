#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-28 20:43
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com

from model.read import Read
from service.be_read_service import BeReadService
from service.ids_service import IdsService
from service.user_service import UserService
from utils.func import *

logger = logging.getLogger('ReadService')


@singleton
class ReadService(object):
    field_names = ['rid', 'aid', 'uid', 'readOrNot', 'readTimeLength', 'readSequence', 'commentOrNot', 'agreeOrNot',
                   'shareOrNot', 'commentDetail', 'timestamp']

    def __init__(self):
        self.models = dict()
        self.classes = dict()
        for dbms in DBMS.all:
            self.models[dbms] = list()
            self.classes[dbms] = self.__gen_model(dbms)

    def __gen_model(self, dbms):
        class Model(Read):
            meta = {
                'db_alias': dbms,
                'collection': 'read'
            }
            pass

        return Model

    def get_model(self, dbms):
        return self.classes[dbms]

    def update_many(self, models=None, db_alias=None):

        if db_alias is None:
            for dbms in DBMS.all:
                self.update_many(models, db_alias=dbms)
        else:
            if models is None:
                models = self.models[db_alias]
                if models is not None:
                    self.get_model(db_alias).update_many(models)
                    # del self.models[db_alias]
                    self.models[db_alias] = list()

    @staticmethod
    def get_id():
        """
            通过去ids表中查询得到当前read表中下一个rid的理论值（因为还得根据region进行判断
        :return: rid
        """
        return IdsService().next_id('rid')

    def count(self, db_alias=None, **kwargs):
        """
            查询对应数据库中的read记录总数
        :param db_alias:    待查询的数据库名称
        :param kwargs:      高级查询条件，详细用法见mongo engine用法
        :return:
        """
        check_alias(db_alias)
        return self.get_model(db_alias).count(**kwargs)

    def count_all(self, **kwargs):
        """
            统计所有数据库中的用户总量
        :param kwargs:  制定特殊查询参数
        :return:
        """
        count = 0
        for dbms in DBMS().get_all_dbms_by_region():
            count += self.count(db_alias=dbms, **kwargs)
        return count

    def get_by_uid_and_aid(self, uid, aid, db_alias=None, **kwargs) -> list:

        if db_alias is None:
            for dbms in DBMS.all:
                reads = self.get_by_uid_and_aid(uid, aid, db_alias=dbms, **kwargs)
                if reads is not None or reads != []:
                    return reads
        else:
            return self.get_model(db_alias).get_all(uid=uid, aid=aid, **kwargs)
        pass

    def get_by_rid(self, rid, db_alias=None, **kwargs):

        if db_alias is None:
            for dbms in DBMS.all:
                read = self.get_by_rid(rid, db_alias=dbms, **kwargs)
                if read is not None:
                    return read
        else:
            return self.get_model(db_alias).get_one(rid=rid, **kwargs)
        pass

    @auto_reconnect
    def add_one(self, aid, uid, readOrNot=1, readTimeLength=0, readSequence=0, commentOrNot=0, commentDetail='',
                agreeOrNot=0, shareOrNot=0, timestamp=None, is_multi=False, db_alias=None, **kwargs):
        """
            保存一个新的read记录
        :param db_alias:
        :param is_multi:
        :param aid:
        :param uid:
        :param readOrNot:
        :param readTimeLength:
        :param readSequence:
        :param commentOrNot:
        :param commentDetail:
        :param agreeOrNot:
        :param shareOrNot:
        :param timestamp:
        :return:
        """
        # logger.info('save read:{}'.format(new_read))

        # 根据user的region去得到正确的rid， 因为read表分布和user的region分布式一样的
        dbmses = []
        if db_alias is not None:
            dbmses.append(db_alias)
        else:
            user = UserService().get_user_by_uid(uid, only=['region'])
            if user is None:
                return None
            dbmses.append(get_dbms_by_region(user.region))
        # read = ReadService().get_by_uid_and_aid(uid=uid, aid=aid, only=['rid'])
        # if read is not None:
        #     rid = read.rid
        # else:
        rid = self.get_id()

        new_read = None
        # 根据用户的region去查询当前region对应的所有数据库
        for dbms in dbmses:
            # if read is None:
            new_read = self.new_record(rid, aid, uid, readOrNot, readTimeLength, readSequence, commentOrNot,
                                       commentDetail, agreeOrNot, shareOrNot, timestamp, db_alias=dbms,
                                       is_multi=is_multi)
            # else:
            #     new_read = self.update(rid, aid, uid, readOrNot, readTimeLength, readSequence, commentOrNot,
            #                            commentDetail, agreeOrNot, shareOrNot, db_alias=dbms)
            if new_read is None:
                break

        # 待修改
        BeReadService().add_one(aid, uid, readOrNot, commentOrNot, agreeOrNot, shareOrNot, timestamp,
                                is_multi=is_multi)

        return new_read

    def new_record(self, aid, uid, rid=None, readOrNot=1, readTimeLength=0, readSequence=1, commentOrNot=0,
                   commentDetail='', agreeOrNot=0, shareOrNot=0, timestamp=None, db_alias=None, is_multi=False,
                   **kwargs):
        # 在db_alias 所选中的数据库中 建立实例保存read数据

        check_alias(db_alias)
        if rid is None:
            rid = self.get_id()

        new_read = self.get_model(db_alias)()
        new_read.rid = rid
        new_read.aid = aid
        new_read.uid = uid
        new_read.readOrNot = int(readOrNot)
        new_read.readTimeLength = int(readTimeLength)
        new_read.readSequence = int(readSequence)
        new_read.commentOrNot = int(commentOrNot)
        new_read.commentDetail = commentDetail
        new_read.agreeOrNot = int(agreeOrNot)
        new_read.shareOrNot = int(shareOrNot)
        new_read.timestamp = timestamp or get_timestamp()
        # logger.info("save read to dbms:{}, record: aid: {}, uid: {}".format(db_alias, aid, uid))
        if is_multi:
            self.models[db_alias].append(new_read)
        else:
            new_read.save()
        return new_read

    def update(self, rid, aid, uid, readOrNot, readTimeLength, readSequence, commentOrNot, commentDetail,
               agreeOrNot, shareOrNot, db_alias=None):
        # 在db_alias 所选中的数据库中 建立实例保存read数据
        check_alias(db_alias)
        new_read = self.get_by_rid(rid=rid, db_alias=db_alias)
        new_read.rid = rid
        if new_read.aid != aid or new_read.uid != uid:
            logger.info("update read date error,"" rid: {}, old-> uid:{}, aid:{},"
                        " new ->: uid:{}, aid:{}".format(rid, new_read.uid, new_read.rid, uid, rid))
            return None

        new_read.readOrNot = int(readOrNot) or new_read.readOrNot
        new_read.readTimeLength += int(readTimeLength)
        new_read.readSequence += int(readSequence)
        new_read.commentOrNot = int(commentOrNot) or new_read.commentOrNot
        new_read.commentDetail += "\n" + commentDetail
        new_read.agreeOrNot = int(agreeOrNot) or new_read.agreeOrNot
        new_read.shareOrNot = int(shareOrNot) or new_read.shareOrNot
        new_read.timestamp = get_timestamp()
        new_read.save()
        return new_read

    def update_by_uid_aid(self, uid, aid, db_alias=None, **kwargs):
        check_alias(db_alias)
        num = self.get_model(db_alias).objects(uid=uid, aid=aid).update_one(**kwargs)
        if num == 0:
            rid = IdsService().next_id('rid')
            new_read = self.get_model(db_alias)()
            new_read.rid = rid
            for key, value in kwargs.items():
                setattr(new_read, key, value)
            new_read.save()
        pass

    def get_reads(self, page_num=1, page_size=20, db_alias=None, **kwargs):
        """
            获取read表的list
            # TODO 查询多个数据库时涉及到的分页问题需解决
        :param page_num:
        :param page_size:
        :param db_alias:    带查询的数据库名称, 为空的话则两个数据库都查询
        :param kwargs:      查询条件
        :return:
        """
        if db_alias is None:
            # reads = list()
            # for dbms in DBMS.all:
            #     tmp = self.get_reads(page_num, page_size, db_alias=dbms, **kwargs)
            #     for r in tmp:
            #         reads.append(r)
            # return reads

            reads = []
            for region in DBMS.region['values']:
                count = self.count(db_alias=get_best_dbms_by_region(region))
                if count >= (page_num - 1) * page_size:
                    us = self.get_reads(page_num, page_size, db_alias=get_best_dbms_by_region(region), **kwargs)
                    reads.extend(us)
                if len(reads) == page_size:
                    break
                else:
                    page_num = (page_num * page_size - count) / (page_size - len(reads))
                    page_size = page_size - len(reads)
            return list(reads)
        else:
            check_alias(db_alias)
            return self.get_model(db_alias).get_all(page_num, page_size, **kwargs)

    def del_read_by_rid(self, rid, db_alias=None):
        """
            通过rid进行删除， 如果不指定数据库的话将在所有数据库中查询
        :param rid:
        :param db_alias:
        :return:   1： 删除成功
                    0： 当前数据库没有rid信息
        """
        del_num = 0
        if db_alias is None:
            for dbms in DBMS.all:
                del_num += self.del_read_by_rid(rid, db_alias=dbms)
        else:
            check_alias(db_alias)
            # if not isinstance(rid, ObjectId):
            #     rid = ObjectId(rid)
            # del_num = self.get_model(db_alias).objects(rid=rid).delete()
            del_num = self.get_model(db_alias).delete_one(rid=rid)
        return del_num

    def del_reads_by_uid(self, uid: int, db_alias=None):
        """
            通过uid进行删除该uid对应的所有历史记录， 如果不指定数据库的话将在所有数据库中查询
        :param uid:
        :param db_alias:
        :return:  0： 当前数据库没有该uid对应的记录
                 >0: 删除的记录条数
        """
        del_num = 0
        if db_alias is None:
            user = UserService().get_user_by_uid(uid)
            if user is None:
                logger.info("删除不存在的用户'{}'的记录".format(uid))
                return 0
            for dbms in get_dbms_by_region(user.region):
                del_num += self.del_reads_by_uid(uid, db_alias=dbms)
        else:
            check_alias(db_alias)
            del_num = self.get_model(db_alias).delete_one(uid=uid)
        return del_num

    def get_by_rids(self, rids: list, db_alias: str = None, **kwargs) -> dict:
        """
            根据aids列表一次性返回对应的article信息

        :param uids:    待查询的aids
        :param db_alias:    待查询的数据库
        :param fields:  需要显示或者不需要显示的字段， None则查询全部字段
                            {'field name': 1}  显示该字段
                            {'field name': 0}  不显示该字段
        :return:
        """
        # TODO 去掉默认db设置，在所有数据库中，根据数量进行分页以及返回相关数据
        check_alias(db_alias)

        return self.get_model(db_alias).get_many_by_ids(rids, **kwargs)

    def get_history(self, uid: int, page_num=1, page_size=20, **kwargs):
        """
                查询uid的所有历史记录，默认20分页
        :param uid:
        :param page_num:
        :param page_size:
        :return:
        """
        # 选择一个连接最好的节点进行历史记录查询
        user = UserService().get_user_by_uid(uid, only=['region'])
        if user is None:
            return []
        return self.__get_history(uid, page_num, page_size, db_alias=get_best_dbms_by_region(user.region), **kwargs)

    def __get_history(self, uid, page_num=1, page_size=20, db_alias=None, **kwargs):
        check_alias(db_alias)
        return self.get_model(db_alias).get_all(page_num, page_size, uid=uid, **kwargs)

    def compute_popular(self, end_date: datetime.date, before_days, top_n=10):
        """
            计算 end_date 这天结束 到 before_days 之间各篇文章阅读的频率
        :param end_date:    date格式，一般传入当天时间
        :param before_days:
        :param top_n:
        :return:
        """

        freq_all = dict()

        def get_date_timestamp(_date):
            # 根据date计算第二天0点的timestamp
            _date = _date + datetime.timedelta(days=1)
            _re = datetime.datetime.strptime(str("{}-{}-{}".format(_date.year, _date.month, _date.day)), '%Y-%m-%d')
            return int(_re.timestamp() * 1000)

        start = get_date_timestamp(end_date - datetime.timedelta(days=before_days))
        end = get_date_timestamp(end_date)
        for dbms in DBMS.all:
            #  TODO 比较 aggregate 和 item_frequencies 的性能差距
            # freq = self.__get_popular_by_freq(start, end,  db_alias=dbms)
            freq = self.__get_popular_by_aggregate(start, end, db_alias=dbms)
            # print(freq)

            freq_all = merge_dict(freq_all, freq)

        return sort_dict(freq_all)[:top_n]

    def __get_popular_by_freq(self, start, end, db_alias=None):
        check_alias(db_alias)

        return self.get_model(db_alias).objects(timestamp__gte=start, timestamp__lte=end).item_frequencies('aid')
        pass

    def __get_popular_by_aggregate(self, start, end, db_alias=None):
        check_alias(db_alias)
        #  TODO 比较 aggregate 和 item_frequencies 的性能差距
        freq = self.get_model(db_alias).objects(timestamp__gte=start, timestamp__lte=end).aggregate(
            {'$group': {'_id': '$aid', 'count': {'$sum': 1}}})

        re = dict()
        for item in freq:
            re[item['_id']] = item['count']

        return re

    def get_daily_popular(self, end_date: datetime.date, before_days=1, top_n=10):
        if isinstance(end_date, datetime.datetime):
            end_date = end_date.date()
        return self.compute_popular(end_date, before_days, top_n)

    def get_weekly_popular(self, end_date: datetime.date, before_days=7, top_n=10):
        if isinstance(end_date, datetime.datetime):
            end_date = end_date.date()
        return self.compute_popular(end_date, before_days, top_n)

    def get_monthly_popular(self, end_date: datetime.date, before_days=30, top_n=10):
        if isinstance(end_date, datetime.datetime):
            end_date = end_date.date()
        return self.compute_popular(end_date, before_days, top_n)

    @staticmethod
    def pretty_reads(reads):
        pretty_models(reads, ReadService.field_names)

    def import_from_dict(self, data):
        user = UserService().get_user_by_uid(data['uid'], only=['region'])

        for db_alias in get_dbms_by_region(user.region):
            new_read = self.get_model(db_alias)()
            new_read.rid = int(data['rid'])
            new_read.aid = int(data['aid'])
            new_read.uid = int(data['uid'])
            new_read.readOrNot = int(data['readOrNot'])
            new_read.readTimeLength = int(data['readTimeLength'])
            new_read.readSequence = int(data['readSequence'])
            new_read.commentOrNot = int(data['commentOrNot'])
            new_read.commentDetail = data['commentDetail']
            new_read.agreeOrNot = int(data['agreeOrNot'])
            new_read.shareOrNot = int(data['shareOrNot'])
            new_read.timestamp = data['timestamp']

            self.models[db_alias].append(new_read)

            BeReadService().add_one(int(data['aid']), int(data['uid']), int(data['readOrNot']),
                                    int(data['commentOrNot']),
                                    int(data['agreeOrNot']), int(data['shareOrNot']), data['timestamp'],
                                    is_multi=True)
        pass
