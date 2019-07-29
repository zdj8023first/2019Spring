import logging
from random import random

from config import DBMS
from utils.func import timestamp_to_datetime, print_run_time

logger = logging.getLogger('gen')
USERS_NUM = 10000
ARTICLES_NUM = 200000
READS_NUM = 1000000

# USERS_NUM = 1000
# ARTICLES_NUM = 2000
# READS_NUM = 10000

uid_region = {}
aid_lang = {}


# from multiprocessing import Manager
#
# uid_region = Manager().dict()
# aid_lang = Manager().dict()


# multi_uid_region = Manager().dict(uid_region)
# multi_aid_lang = Manager().dict(aid_lang)


# Beijing:60%   Hong Kong:40%
# en:20%    zh:80%
# 20 depts
# 3 roles
# 50 tags
# 0~99 credits

def gen_an_user(i):
    timeBegin = 1506328859000
    user = {}
    user["timestamp"] = str(timeBegin + i)
    user["uid"] = str(i)
    user["name"] = "user%d" % i if i != 0 else "admin"
    user["pwd"] = "password" if i != 0 else "admin"
    user["gender"] = "male" if random() > 0.33 else "female"
    user["email"] = "email%d" % i
    user["phone"] = "phone%d" % i
    user["dept"] = "dept%d" % int(random() * 20)
    user["grade"] = "grade%d" % int(random() * 4 + 1)
    user["language"] = "en" if random() > 0.8 else "zh"
    user["region"] = "Beijing" if random() > 0.4 else "Hong Kong"
    user["role"] = "role%d" % int(random() * 3)
    user["preferTags"] = "tags%d" % int(random() * 50)
    user["obtainedCredits"] = str(int(random() * 100))

    uid_region[user["uid"]] = user["region"]
    return user


# science:45%   technology:55%
# en:50%    zh:50%
# 50 tags
# 2000 authors
def gen_an_article(i):
    timeBegin = 1506000000000
    article = {}
    article["timestamp"] = str(timeBegin + i)
    article["aid"] = str(i)
    article["title"] = "title%d" % i
    article["category"] = "science" if random() > 0.55 else "technology"
    article["abstract"] = "abstract of article %d" % i
    article["articleTags"] = "tags%d" % int(random() * 50)
    article["authors"] = "author%d" % int(random() * 2000)
    article["language"] = "en" if random() > 0.5 else "zh"
    article["text"] = "text %d's location on hdfs" % i
    article["image"] = "image %d's location on hdfs" % i
    article["video"] = "video %d's location on hdfs" % i

    aid_lang[article["aid"]] = article["language"]
    return article


# user in Beijing read/agree/comment/share an english article with the probability 0.6/0.2/0.2/0.1
# user in Hong Kong read/agree/comment/share an Chinese article with the probability 0.8/0.2/0.2/0.1
p = {}
p["Beijing" + "en"] = [0.6, 0.2, 0.2, 0.1]
p["Beijing" + "zh"] = [1, 0.3, 0.3, 0.2]
p["Hong Kong" + "en"] = [1, 0.3, 0.3, 0.2]
p["Hong Kong" + "zh"] = [0.8, 0.2, 0.2, 0.1]


def gen_an_read(i):
    timeBegin = 1506332297000
    read = {}
    read["timestamp"] = str(timeBegin + i * 10000)
    read["uid"] = str(int(random() * USERS_NUM))
    read["aid"] = str(int(random() * ARTICLES_NUM))

    region = uid_region[read["uid"]]
    lang = aid_lang[read["aid"]]
    ps = p[region + lang]

    if (random() > ps[0]):
        # read["readOrNot"] = "0";
        return gen_an_read(i)
    else:
        read["readOrNot"] = "1"
        read["readTimeLength"] = str(int(random() * 100))
        read["readSequence"] = str(int(random() * 4))
        read["agreeOrNot"] = "1" if random() < ps[1] else "0"
        read["commentOrNot"] = "1" if random() < ps[2] else "0"
        read["shareOrNot"] = "1" if random() < ps[3] else "0"
        read["commentDetail"] = "comments to this article: (" + read["uid"] + "," + read["aid"] + ")"
    return read


def print_bar(now, total):
    print('\rprocess:\t {now} / {total}  {rate}%'.format(now=now + 1, total=total,
                                                         rate=round((now + 1) / total * 100, 2)), end='')
    if now == total:
        print('\n\n')


@print_run_time
def gen_users():
    for i in range(USERS_NUM):
        print_bar(i, USERS_NUM)
        data = gen_an_user(i)
        UserService().import_user_from_dict(data)
    UserService().update_many()
    # UserService().register(data['name'], data['pwd'], data['gender'], data['email'], data['phone'], data['dept'],
    #                        data['grade'], data['language'], data['region'], data['role'], data['preferTags'],
    #                        data['obtainedCredits'], int(data['timestamp']), is_multi=True)


@print_run_time
def gen_articles():
    print()
    for i in range(ARTICLES_NUM):
        print_bar(i, ARTICLES_NUM)
        data = gen_an_article(i)
        ArticleService().import_from_dict(data)
        if (i + 1) % 50000 == 0:
            logger.info(f"\n saving articles, {i}")
            ArticleService().update_many()
            logger.info(f"\n saving article's be read, {i}")
            BeReadService().update_many()
    ArticleService().update_many()
    BeReadService().update_many()
    # ArticleService().add_an_article(title=data['title'], authors=data['authors'], category=data['category'],
    #                                 abstract=data['abstract'], articleTags=data['articleTags'],
    #                                 language=data['language'], text=data['text'], image=data['image'],
    #                                 video=data['video'], timestamp=int(data['timestamp']), is_multi=True)


@print_run_time
def gen_reads():
    print()
    for i in range(READS_NUM):
        print_bar(i, READS_NUM)
        data = gen_an_read(i)

        data['rid'] = i

        ReadService().import_from_dict(data)
        if (i + 1) % 50000 == 0:
            logger.info("\n\n saving reads")
            ReadService().update_many()
            BeReadService().update_many()
    ReadService().update_many()
    BeReadService().update_many()
    # article = ArticleService().get_articles_by_title(title='title' + data['aid'], only=['aid'], page_size=1,
    #                                                  db_alias=DBMS.DBMS2)[0]
    #
    # name = 'user' + data['uid'] if data['uid'] != '0' else 'admin'
    # user = UserService().get_user_by_name(name=name, only=['uid'])
    #
    # ReadService().add_one(article.aid, user.uid, int(data['readOrNot']), int(data['readTimeLength']),
    #                       int(data['readSequence']), int(data['commentOrNot']),
    #                       data['commentDetail'], int(data['agreeOrNot']), int(data['shareOrNot']),
    #                       timestamp=int(data["timestamp"]), is_multi=True)


@print_run_time
def gen_populars():
    print()
    timeBegin = 1506332297000
    timeEnd = timeBegin + READS_NUM * 10000
    for timestamp in range(timeBegin, timeEnd, 86400000):
        logger.info('gen popular on date: {}'.format(timestamp_to_datetime(timestamp).date()))
        PopularService().update_popular(_date=timestamp_to_datetime(timestamp).date())


def gen_users_by_threads(start, end):
    from db.mongodb import init_connect
    init_connect()
    logger.info("start gen user range ({}, {})".format(start, end))
    for i in range(start, end):
        # print_bar(i - start, end - start)
        if i % 100 == 0:
            logger.info('.', end='')
        data = gen_an_user(i)
        UserService().register(data['name'], data['pwd'], data['gender'], data['email'], data['phone'], data['dept'],
                               data['grade'], data['language'], data['region'], data['role'], data['preferTags'],
                               data['obtainedCredits'], int(data['timestamp']))

    logger.info("finish gen user range ({}, {})".format(start, end))


def gen_articles_by_threads(start, end):
    from db.mongodb import init_connect
    init_connect()
    logger.info("start gen articles range ({}, {})".format(start, end))
    for i in range(start, end):
        if i % 100 == 0:
            print('.', end='')
        # print_bar(i - start, end - start)
        data = gen_an_article(i)
        ArticleService().add_an_article(title=data['title'], authors=data['authors'], category=data['category'],
                                        abstract=data['abstract'], articleTags=data['articleTags'],
                                        language=data['language'], text=data['text'], image=data['image'],
                                        video=data['video'], timestamp=int(data['timestamp']))
    logger.info("finish gen articles range ({}, {})".format(start, end))


def gen_reads_by_threads(start, end):
    from db.mongodb import init_connect
    init_connect()
    logger.info("start gen reads range ({}, {})".format(start, end))
    for i in range(start, end):
        if i % 100 == 0:
            print('.', end='')

        # print_bar(i - start, end - start)
        data = gen_an_read(i)

        article = ArticleService().get_articles_by_title(title='title' + data['aid'], only=['aid'], page_size=1,
                                                         db_alias=DBMS.DBMS2)[0]

        name = 'user' + data['uid'] if data['uid'] != '0' else 'admin'
        user = UserService().get_user_by_name(name=name, only=['uid'])

        ReadService().add_one(article.aid, user.uid, int(data['readOrNot']), int(data['readTimeLength']),
                              int(data['readSequence']), int(data['commentOrNot']),
                              data['commentDetail'], int(data['agreeOrNot']), int(data['shareOrNot']),
                              timestamp=int(data["timestamp"]))

    logger.info("finish gen reads range ({}, {})".format(start, end))


@print_run_time
def create_by_threads(target, NUM):
    from multiprocessing import Pool

    pool = Pool()
    logger.info('start')
    threads = []
    thread_num = 4
    part = int(NUM / thread_num) if NUM % thread_num == 0 else int(NUM / thread_num + 1)
    for i in range(thread_num):
        # gen_users_by_thread(i * part, (i + 1) * part)
        pool.apply_async(target, args=(i * part, min(NUM, (i + 1) * part)))
    #     thread = threading.Thread(target=target, args=(i * part, min(NUM, (i + 1) * part)))
    #     thread.setDaemon(True)
    #     thread.start()
    #     threads.append(thread)
    # for thread in threads:
    #     thread.join()
    pool.close()
    pool.join()
    logger.info('finish')


from mongoengine.context_managers import switch_db


def drop(service):
    for dbms in DBMS.all:
        model = service().get_model(dbms)
        with switch_db(model, dbms):
            model.drop_collection()


from service.user_service import UserService
from service.article_service import ArticleService
from service.read_service import ReadService
from service.be_read_service import BeReadService
from service.popular_service import PopularService
from service.ids_service import IdsService


def reset_db():
    drop(UserService)
    drop(ArticleService)
    drop(ReadService)
    drop(BeReadService)
    drop(PopularService)
    drop(IdsService)
    IdsService().init(DBMS.DBMS1)


def gen_data_by_threads():
    # 30.29s

    from main import init
    from mongoengine import connection, connect
    init()
    db = connect(DBMS.db_name, host=DBMS.configs[DBMS.DBMS1]['host'], port=DBMS.configs[DBMS.DBMS1]['port'],
                 tz_aware=True)
    reset_db()

    db.close()

    connection._connections = {}
    connection._connection_settings = {}
    connection._dbs = {}

    # 85.79s 116.59s
    logger.info('\n导入user数据...')
    create_by_threads(gen_users_by_threads, USERS_NUM)

    logger.info('\n导入article数据...')
    create_by_threads(gen_articles_by_threads, ARTICLES_NUM)

    logger.info('\n导入read数据...')

    save_data()
    create_by_threads(gen_reads_by_threads, READS_NUM)

    init()
    gen_populars()


def gen_ids():
    ids = IdsService().get_model(DBMS.DBMS1)()
    ids.ids = 0
    ids.aid = ARTICLES_NUM
    ids.uid = USERS_NUM
    ids.rid = READS_NUM
    ids.bid = ARTICLES_NUM
    ids.save()


def gen_data():
    # host = '127.0.0.1'
    # connect('mongo-new', host=host, port=27017)

    # 71.67s
    logger.info('\n生成user数据...')
    gen_users()

    logger.info('\n生成article数据...')
    gen_articles()

    # print('\n导入user 和 article数据...')
    # for dbms in DBMS.all:
    #     print('\n导入user数据... in ', dbms)
    #     UserService().get_model(dbms).update_many(UserService().models[dbms])
    #     print('\n导入article数据... in ', dbms)
    #     ArticleService().get_model(dbms).update_many(ArticleService().models[dbms])
    #     print('\n导入BeRead数据... in ', dbms)
    # 
    #     BeReadService().get_model(dbms).update_many(BeReadService().models[dbms])
    # 
    #     UserService().models[dbms].clear()
    #     ArticleService().models[dbms].clear()
    #     BeReadService().models[dbms].clear()
    #     ReadService().models[dbms].clear()
    #
    # save_data()
    logger.info('\n生成read数据...')
    gen_reads()

    gen_ids()

    # # print('\n导入read数据...')
    # # for dbms in DBMS.all:
    # #     ReadService().get_model(dbms).update_many(ReadService().models[dbms])
    # #     BeReadService().get_model(dbms).update_many(BeReadService().models[dbms])
    # #
    # #     ReadService().models[dbms].clear()
    # #     BeReadService().models[dbms].clear()
    #
    logger.info('\n导入populars数据...')
    gen_populars()


def save_data():
    global uid_region, aid_lang
    import json
    with open('uid_region.data', 'w') as uid:
        json.dump(uid_region, uid)
    with open('aid_region.data', 'w') as aid:
        json.dump(aid_lang, aid)


def read_data():
    global uid_region, aid_lang
    import pickle
    uid_region = pickle.load('uid_region.pickle')
    aid_lang = pickle.load('aid_lang.pickle')


@print_run_time
def main():
    from main import init
    init()
    reset_db()

    gen_data()
    # gen_data_by_threads()


if __name__ == '__main__':
    main()
    # from main import init
    #
    # init()
    # gen_populars()
