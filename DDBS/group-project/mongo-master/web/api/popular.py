from flask import request, Blueprint
from flask.views import MethodView

from service.popular_service import PopularService
from service.redis_service import RedisService
from utils.func import check_alias, DbmsAliasError
from web.result import Result

populars = Blueprint('populars', __name__)


@populars.route('', methods=['GET'])
def get_populars():
    page_num = int(request.args.get('page', 1))
    page_size = int(request.args.get('size', 20))
    dbms = request.args.get('dbms')
    temporalGranularity = request.args.get('level')
    sort_by = request.args.get('sort_by') or 'timestamp'

    try:
        check_alias(db_alias=dbms)
    except DbmsAliasError:
        return Result.gen_failed('404', 'dbms error')

    timestamp = request.args.get('timestamp')
    cons = {
        'timestamp': timestamp
    }
    kwargs = {}
    for key, value in cons.items():
        if value is not None and value != '':
            kwargs[key] = value

    _REDIS_KEY_ = f"POPULAR_LIST:{dbms}:{page_num}:{page_size}:{temporalGranularity}:{sort_by}:{kwargs}"
    data = RedisService().get_redis(dbms).get_dict(_REDIS_KEY_)
    if data is None or data == {}:
        ranks = PopularService().get_ranks(temporalGranularity=temporalGranularity, db_alias=dbms,
                                           page_num=page_num,
                                           page_size=page_size, sort_by=sort_by, **kwargs)

        ranks = list(rank.to_dict() for rank in ranks)
        total = PopularService().count(temporalGranularity=temporalGranularity, db_alias=dbms, **kwargs)
        data = {
            'total': total,
            'list': ranks
        }
        RedisService().get_redis(dbms).set_dict(_REDIS_KEY_, data)
    return Result.gen_success(data)
    pass


@populars.route('', methods=['POST'])
def update_populars():
    from utils.func import timestamp_to_datetime
    timestamp = request.json.get('timestamp')
    if timestamp is not None:
        _date = timestamp_to_datetime(timestamp).date()
        print("更新热门文章： 日期 -> {}".format(_date))
        PopularService().update_popular(_date=_date)
    else:
        PopularService().update_popular()
    return Result.gen_success()
    # return Result.gen_failed(234, 'shibai')
    pass


@populars.route('<int:pid>', methods=['DELETE'])
def delete_popular(pid):
    if pid is None:
        return Result.gen_failed('404', 'pid not found')

    dbms = request.args.get('dbms')
    if dbms is None:
        return Result.gen_failed('5000', '请带上dbms参数')
    # 删除缓存
    RedisService().get_redis(dbms).delete(f"POPULAR:{pid}")
    RedisService().get_redis(dbms).delete_by_pattern(f"POPULAR_LIST:{dbms}:*")

    PopularService().del_by_id(pid)

    return Result.gen_success('删除成功')


@populars.route('/today', methods=['GET'])
def get_today_populars():
    level = request.args.get('level', 'daily')
    timestamp = request.args.get("t")
    dbms = request.args.get('dbms')

    try:
        check_alias(db_alias=dbms)
    except DbmsAliasError:
        return Result.gen_failed('404', 'dbms error')

    articles = PopularService().get_articles(timestamp, level, db_alias=dbms)
    articles = list(article.to_dict(only=['aid', 'title', 'count', 'category']) for article in articles)
    return Result.gen_success(data=articles)


# class PopularList(MethodView):
#     def get(self):
#         page_num = int(request.args.get('page', 1))
#         page_size = int(request.args.get('size', 20))
#         dbms = request.args.get('dbms')
#         temporalGranularity = request.args.get('level')
#         sort_by = request.args.get('sort_by') or 'timestamp'
#
#         try:
#             check_alias(db_alias=dbms)
#         except DbmsAliasError:
#             return Result.gen_failed('404', 'dbms error')
#
#         timestamp = request.args.get('timestamp')
#         cons = {
#             'timestamp': timestamp
#         }
#         kwargs = {}
#         for key, value in cons.items():
#             if value is not None and value != '':
#                 kwargs[key] = value
#
#         _REDIS_KEY_ = f"POPULAR_LIST:{dbms}:{page_num}:{page_size}:{temporalGranularity}:{sort_by}:{kwargs}"
#         data = RedisService().get_redis(dbms).get_dict(_REDIS_KEY_)
#         if data is None or data == {}:
#             ranks = PopularService().get_ranks(temporalGranularity=temporalGranularity, db_alias=dbms,
#                                                page_num=page_num,
#                                                page_size=page_size, sort_by=sort_by, **kwargs)
#
#             ranks = list(rank.to_dict() for rank in ranks)
#             total = PopularService().count(temporalGranularity=temporalGranularity, db_alias=dbms, **kwargs)
#             data = {
#                 'total': total,
#                 'list': ranks
#             }
#             RedisService().get_redis(dbms).set_dict(_REDIS_KEY_, data)
#         return Result.gen_success(data)
#         pass
#
#     def post(self):
#         from utils.func import timestamp_to_datetime
#         timestamp = request.json.get('timestamp')
#         if timestamp is not None:
#             _date = timestamp_to_datetime(timestamp).date()
#             print("更新热门文章： 日期 -> {}".format(_date))
#             PopularService().update_popular(_date=_date)
#         else:
#             PopularService().update_popular()
#         return Result.gen_success()
#         # return Result.gen_failed(234, 'shibai')
#         pass


# class PopularCURD(MethodView):
#     # def get(self, pid):
#     #     # aid = request.args.get('aid', None)
#     #     if pid is None:
#     #         return Result.gen_failed('404', 'aid not found')
#     #     category = request.args.get('category', None)
#     #     if category is not None:
#     #         article = PopularService().get
#     #     else:
#     #         article = PopularService().get_daily_articles()
#     #     return Result.gen_success(article)
#
#     def delete(self, pid):
#         if pid is None:
#             return Result.gen_failed('404', 'pid not found')
#
#         dbms = request.args.get('dbms')
#         if dbms is None:
#             return Result.gen_failed('5000', '请带上dbms参数')
#         # 删除缓存
#         RedisService().get_redis(dbms).delete(f"POPULAR:{pid}")
#         RedisService().get_redis(dbms).delete_by_pattern(f"POPULAR_LIST:{dbms}:*")
#
#         PopularService().del_by_id(pid)
#
#         return Result.gen_success('删除成功')


class PopuparToday(MethodView):

    def get(self):
        level = request.args.get('level', 'daily')
        timestamp = request.args.get("t")
        dbms = request.args.get('dbms')

        try:
            check_alias(db_alias=dbms)
        except DbmsAliasError:
            return Result.gen_failed('404', 'dbms error')

        articles = PopularService().get_articles(timestamp, level, db_alias=dbms)
        articles = list(article.to_dict(only=['aid', 'title', 'count', 'category']) for article in articles)
        return Result.gen_success(data=articles)
