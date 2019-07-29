import functools
import logging

from mongoengine import Document, connect, register_connection, DoesNotExist, ValidationError
from mongoengine.context_managers import switch_db
from pymongo import UpdateOne

logger = logging.getLogger('db')


class BaseDB(Document):
    meta = {
        'abstract': True,
    }

    def __str__(self):
        #     # 重写str方法， 将ObjectId 和datetime格式正确的输出
        # return convert_mongo_2_json(self)
        return str(self.to_dict())

    def to_dict(self, only: list = None, exclude: list = None, other: list = None):
        """
            将该类数据转换为dict，以供快捷转换为str或者list

        :param other:
        :param only: 需要返回显示的字段名，为空的话则显示全部字段
        :param exclude: 不需要返回的字段，only为空才生效
        :return: dict
        """

        if exclude is None:
            exclude = list()
        if only is None:
            only = list()

        if not isinstance(only, list) or not isinstance(exclude, list):
            raise BaseException('传入类型不一致')
        _fields = list(self._db_field_map.keys())

        if len(only) != 0:
            my_dict = self.__to_dict(only)

        elif len(exclude) != 0:
            for _field in exclude:
                if _field in _fields[::-1]:
                    _fields.remove(_field)
            my_dict = self.__to_dict(_fields)
        else:
            my_dict = self.__to_dict(_fields)

        if other is not None:
            for o in other:
                my_dict[o] = getattr(self, o)
        return my_dict

    def __to_dict(self, _fields):
        my_dict = dict()
        for filed in _fields:
            my_dict[filed] = getattr(self, filed)
        return my_dict

    @classmethod
    def get_all(cls, page_num=1, page_size=20, only: list = None, exclude: list = None, sort_by=None, **kwargs) -> list:
        """
            分页数据查询

        :param sort_by: 根据某个字段排序
        :param exclude: 查询结果中不包含指定字段, list：字段名
        :param only:    查询结果中只包含指定字段, list：字段名
        :param page_num: 查询的页码，默认第一页
        :param page_size: 每一页显示的数量
        :param kwargs: 查询的条件
        :return: 符合条件的分页数后据
        """
        offset = (page_num - 1) * page_size
        if sort_by is not None:
            if only is not None:
                return cls.objects(**kwargs).order_by(sort_by).only(*only).skip(offset).limit(page_size)
            if exclude is not None:
                return cls.objects(**kwargs).order_by(sort_by).exclude(*exclude).skip(offset).limit(page_size)
            return cls.objects(**kwargs).order_by(sort_by).skip(offset).limit(page_size)
        else:
            if only is not None:
                return cls.objects(**kwargs).only(*only).skip(offset).limit(page_size)
            if exclude is not None:
                return cls.objects(**kwargs).exclude(*exclude).skip(offset).limit(page_size)
            return cls.objects(**kwargs).skip(offset).limit(page_size)

    @classmethod
    def count(cls, **kwargs):
        """

        :return: 当前数据的总量
        """
        # return cls.objects(**kwargs).count()
        return cls._get_collection().count_documents(cls.objects(**kwargs)._query)

    @classmethod
    def count_documents(cls, **kwargs):
        """

        :return: 当前数据的总量
        """
        # return cls.objects(**kwargs).count()
        return cls._get_collection().count_documents(cls.objects(**kwargs)._query)

    @classmethod
    def find(cls, only: list = None, exclude: list = None, **kwargs):
        """
            查询所有数据（未分页）
        :param kwargs: 查询条件
        :return:
        """
        if only is not None:
            return cls.objects(**kwargs).only(*only)
        if exclude is not None:
            return cls.objects(**kwargs).exclude(*exclude)

        return cls.objects(**kwargs)

    @classmethod
    def find_first(cls, only: list = None, exclude: list = None, **kwargs):
        """
        返回符合条件的所有值的第一个

        :param kwargs: 查询条件
        :return:
        """
        if only is not None:
            return cls.objects(**kwargs).only(*only).limit(1).first()
        if exclude is not None:
            return cls.objects(**kwargs).exclude(*exclude).limit(1).first()

        return cls.objects(**kwargs).limit(1).first()

    @classmethod
    def get_one(cls, only: list = None, exclude: list = None, **kwargs):
        """
        获取一个unique的项

        :param kwargs: 查询条件
        :return:
        """
        try:
            if only is not None:
                return cls.objects.only(*only).get(**kwargs)
            if exclude is not None:
                return cls.objects.exclude(*exclude).get(**kwargs)

            return cls.objects(**kwargs).get()
        except DoesNotExist:
            return None

    @classmethod
    def get_many_by_ids(cls, ids: list, only: list = None, exclude: list = None, **kwargs):
        """
        获取一个unique的项

        :param kwargs: 查询条件
        :return:
        """

        if only is not None:
            return cls.objects.only(*only).in_bulk(ids)
        if exclude is not None:
            return cls.objects.exclude(*exclude).in_bulk(ids)

        return cls.objects(**kwargs).in_bulk(ids)

    @classmethod
    def insert_one(cls, data):
        """
            插入一条数据
            由于加入了引用，所以该方法使用会出错
            TODO 去掉该方法以及该方法的相关引用

        :param data: 插入的数据类容，需要符合cls的类型
        :return:
        """
        return cls.objects.insert(data)

    @classmethod
    def insert_many(cls, data: list):
        """
            插入数据需要为list，否则报错

        :param data:
        :return:
        """
        if isinstance(data, list):
            cls.objects.insert(data, load_bulk=False)
            return True
        else:
            raise BaseException('data type error, should be a list')

    @classmethod
    def update_many(cls, entities):
        bulk_operations = []

        for entity in entities:
            try:
                entity.validate()
                bulk_operations.append(
                    UpdateOne({'_id': entity.id}, {'$set': entity.to_mongo().to_dict()}, upsert=True))

            except ValidationError:
                pass

        collection = None
        if bulk_operations:
            collection = cls._get_collection() \
                .bulk_write(bulk_operations, ordered=False)
        return collection

    def delete(self, signal_kwargs=None, **write_concern):

        return self._collection.delete_one(self._qs.filter(**self._object_key)._query, **write_concern).deleted_count

    @classmethod
    def delete_by(cls, **kwargs):
        """
            删除符合条件的数据
        :param kwargs:
        :return:
        """
        return cls._get_collection().delete_many(cls.objects(**kwargs)._query).deleted_count

    @classmethod
    def delete_one(cls, **kwargs):
        if len(kwargs) != 0:
            return cls._get_collection().delete_one(cls.objects(**kwargs)._query).deleted_count
        else:
            return 0
        pass


from config import DBMS


def init_connect():
    """
        初始化mongo连接， 不传参数则从Config文件中读取
        默认连接北京节点，然后分别注册两个节点的信息
    :return:
    """

    # from utils.consts import DBMS
    # tz_aware=True 设置时区修正，mongoDB的时区默认为UTC0，需要加上这个加入时区信息
    connect(DBMS.db_name, host=DBMS.configs[DBMS.DBMS1]['host'], port=DBMS.configs[DBMS.DBMS1]['port'], tz_aware=True)

    for dbms in DBMS.all:
        register_connection(alias=dbms, db=DBMS.db_name, host=DBMS.configs[dbms]['host'],
                            port=DBMS.configs[dbms]['port'],
                            tz_aware=True)

    # connect(Config.mongo_db_name)
    # register_connection(alias=DBMS.DBMS1, db=Config.mongo_db_name, host=Config.bj_mongo_host,
    #                     port=Config.bj_mongo_port,
    #                     tz_aware=True)
    # register_connection(alias=DBMS.DBMS2, db=Config.mongo_db_name, host=Config.hk_mongo_host,
    #                     port=Config.hk_mongo_port, tz_aware=True)


from utils.func import DbmsAliasError


# from utils.consts import DBMS


def switch_mongo_db(cls, default_db=None, allow_None=False):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):

            try:
                db_alias = kwargs.get('db_alias')
                if db_alias is None:
                    db_alias = default_db
                    if allow_None:
                        return func(*args, **kwargs)
                if db_alias not in DBMS.all:
                    raise DbmsAliasError('db_alias error, {} , all:{}'.format(db_alias, DBMS.all))
                # print("switch db: cls={0}, db_alias={1}".format(cls.__name__, db_alias))
                logger.debug("switch db:func={0}, cls={1}, db_alias={2}".format(func.__name__, cls.__name__, db_alias))
                with switch_db(cls, db_alias):
                    return func(*args, **kwargs)
            except KeyError:
                logger.warning("没有指定数据库连接，使用默认数据库连接")
                return func(*args, **kwargs)

        return wrapper

    return decorator
