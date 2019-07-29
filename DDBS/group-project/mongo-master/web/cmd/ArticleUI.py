#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Time    : 2019-04-29 13:32
# @Author  : Gahon
# @Email   : Gahon1995@gmail.com
from service.article_service import ArticleService
from service.popular_service import PopularService
from service.read_service import ReadService
from utils.func import *


def article_manage(role='user', user=None):
    print("\n\n" + "=" * 20)
    print("1. 查询所有文章")
    print("2. 查看指定文章")
    print("3. 查看热门文章")
    print("4. 创建新文章")
    print('5. 查看我创建的文章')
    print("6. 查看阅读历史")
    print("7. 删除指定文章")
    print("8. 更新文章信息")
    if role == 'admin':
        print("9. 更新热门文章")
        print("10. 查看所有阅读历史")
    print("0. 返回上一级")
    print("=" * 20)

    mode = input("请选择操作: ")
    if mode == '1':
        articles_query_all()
        pass
    elif mode == '2':
        choices_article(user)
        pass
    elif mode == '3':
        show_popular()
        pass
    elif mode == '4':
        create_article(user)
        pass
    elif mode == '5':
        show_my_article(user)
        pass
    elif mode == '6':
        history(user)
        pass
    elif mode == '6':
        del_an_article(user, role)
        pass
    elif mode == '8':
        update_an_article(role, user)
        pass
    elif mode == '9':
        update_popular()
        pass
    elif mode == '10':
        history_admin()
        pass
    elif mode == '0':
        return None
        pass
    else:
        pass

    article_manage(role, user)


def articles_query_all(page_num=1, page_size=20, **kwargs):
    print("1. Beijing")
    print("2. Hong Kong (所有数据）")
    print("输入其他内容返回")
    mode = input("请选择操作： ")
    if mode == '1':
        total = ArticleService().count(db_alias=DBMS.DBMS1, **kwargs)
        query_all(total, page_num, page_size, db_alias=DBMS.DBMS1, **kwargs)
    elif mode == '2':
        total = ArticleService().count(db_alias=DBMS.DBMS2, **kwargs)
        query_all(total, page_num, page_size, db_alias=DBMS.DBMS2, **kwargs)
    return


def query_all(total, page_num=1, page_size=20, db_alias=None, **kwargs):
    articles = ArticleService().get_articles(page_num, page_size, db_alias=db_alias, **kwargs)

    if len(articles) == 0:
        print("未找到文章信息")
        return

    ArticleService().pretty_articles(articles)
    show_next(page_num, page_size, total, db_alias=db_alias, next_func=query_all, **kwargs)

    pass


def choices_article(user):
    # from prettytable import PrettyTable
    print("1. 根据aid查询")
    print("2. 根据title查询")
    print("3. 返回")
    mode = input("请选择：")

    if mode == '1':
        aid = input("请输入要阅读的aid: ")
        article = ArticleService().get_one_by_aid(aid=aid)
        if article is not None:
            print_an_article(article)
            mode = input("是否阅读该文章：（y or n)")
            if mode == 'y' or mode == 'Y' or mode == 'yes' or mode == 'YES':
                read_an_article(user, article)
            else:
                return
        else:
            print("未找到相关文章")
            return
        pass
    elif mode == '2':
        keyword = input("请输入要查看的文章部分标题： ")
        if keyword == '':
            return
        articles = ArticleService().get_articles_by_title(keyword, db_alias=DBMS.DBMS2)
        # TODO 文章数量过多时分页显示
        if len(articles) == 0:
            print("未找到相关文章")
            return
        x = PrettyTable()
        x.field_names = ("num", "title", "authors", "abstract")
        for index, article in enumerate(articles):
            x.add_row([index, article.title, article.authors, article.abstract])
        print(x)
        while True:
            try:
                index = int(input("请选择要阅读的序号（ -1 或者不输入任何内容退出）： "))
            except ValueError:
                print("输入错误")
                return
            if 0 <= index < len(articles):
                read_an_article(user, articles[index])
                break
            elif index == -1:
                break
            else:
                print("输入错误，请重新输入")
        pass
    else:
        return


def read_an_article(user, article):
    # from service.read_service import Read, ReadService
    import time

    time_read_start = time.time()

    # input("\n按回车返回")
    # TODO 阅读完后的操作，点赞、评论、分享等

    aid = article.aid
    uid = user.uid
    commentOrNot, commentDetail, agreeOrNot, shareOrNot = 0, '', 0, 0
    while True:
        print("1. 点赞 \t2. 评论\t3. 分享\t4. 返回")
        mode = input("请选择操作: ")
        if mode == '1':
            agreeOrNot = 1
            print("点赞成功")
        elif mode == '2':
            commentOrNot = 1
            detail = input("请输入评论信息： ")
            commentDetail = detail
            print("评论成功")
        elif mode == '3':
            shareOrNot = 1
            print("分享成功")
        elif mode == '4':
            time_read_end = time.time()
            readTimeLength = time_read_end - time_read_start
            ReadService().add_one(aid, uid, 1, readTimeLength, 1, commentOrNot, commentDetail, agreeOrNot, shareOrNot)
            return
        else:
            print("输入错入，请重新输入")

    pass


def print_an_article(article):
    print("\n" + "=" * 50)
    print("Title: {title:<13}  Category: {category:<10}"
          .format(title=article.title,
                  category=article.category))
    print("Authors: {authors:<13} Tags: {tags:<8} Language: {language:<10}"
          .format(authors=article.authors,
                  tags=article.articleTags,
                  language=article.language))
    print("time: {0}".format(article.update_time.strftime('%Y-%m-%d %H:%M:%S')))
    print("abstract: {0}".format(article.abstract))
    print("=" * 22 + "content" + "=" * 22)
    print(article.text)
    print("\n" + "=" * 50)
    pass


def show_popular(_date=None):
    _date = _date or datetime.date.today()
    # TODO 展示热门文章
    print('=' * 30)
    print("当前查询日期： {}".format(_date.strftime("%Y-%m-%d")))
    print('1. 日热门文章')
    print('2. 周热门文章')
    print('3. 月热门文章')
    print('4. 查询指定日期热门')
    print('5. 返回上一级')
    print('=' * 30)

    mode = input("请选择操作：")
    if mode == '1':
        show_daily_popular(_date)
        pass
    elif mode == '2':
        show_weekly_popular(_date)
        pass
    elif mode == '3':
        show_monthly_popular(_date)
        pass
    elif mode == '4':
        new_date = input("请输入查询的日期： （例如 2017-09-25）: ")
        try:
            new_date = str_to_datetime(new_date).date()
            return show_popular(new_date)
        except ValueError:
            print("输入的日期格式错误")
        pass
    elif mode == '5':
        return
    else:
        pass
    show_popular(_date)


def show_daily_popular(_date=None):
    _date = _date or datetime.date.today()
    articles = PopularService().get_daily_articles(_date)
    if len(articles) == 0:
        print("当前并无相关记录")
    pretty_models(articles, ['aid', 'title', 'count'])
    # show_rank(rank)
    input("\n按回车键返回")
    pass


def show_weekly_popular(_date=None):
    _date = _date or datetime.date.today()

    articles = PopularService().get_weekly_articles(_date)
    if len(articles) == 0:
        print("当前并无相关记录")
        return
    pretty_models(articles, ['aid', 'title', 'count'])
    input("\n按回车键返回")
    # show_rank(articles)
    pass


def show_monthly_popular(_date=None):
    _date = _date or datetime.date.today()

    articles = PopularService().get_monthly_articles(_date)
    if len(articles) == 0:
        print("当前并无相关记录")
        return
    pretty_models(articles, ['aid', 'title', 'count'])
    input("\n按回车键返回")
    # show_rank(rank)
    pass


def show_rank(rank):
    if rank is None:
        print("当前并无数据，请联系管理员生成")
        return
    x = PrettyTable()
    x.field_names = ('index', 'aid', 'count')
    print("\nlast update time: {}".format(timestamp_to_str(rank.update_time)))
    for index, aid in enumerate(rank.articleAidDict):
        x.add_row((index, aid, rank.articleAidDict[aid]))
    print(x)
    input("\n按回车键返回")


def create_article(user):
    # TODO 输入内容为空判断
    #       根据Article表的项内容创建文章，并且保存
    title = input("请输入title: ")
    while title == '':
        title = input("输入不能为空，请重新输入: ")
    category = input("请输入category: ")
    while category != 'science' and category != 'technology':
        print('category 输入错误，应该为science or technology')
        category = input("请输入category: ")
    abstract = input("请输入abstract: ")
    articleTags = input("请输入articleTags: ")
    language = input("请输入language: ")
    text = input("请输入text: ")
    image = input("请输入image: ")
    video = input("请输入video: ")
    ArticleService().add_an_article(title, user.name, category, abstract, articleTags, language, text, image, video)
    pass


from prettytable import PrettyTable

x = PrettyTable()


def history(user):
    total = ReadService().count(uid=user.uid, db_alias=get_best_dbms_by_region(user.region))
    if total == 0:
        print("当前没有任何阅读记录哟")
        return
    show_history(user, total)


def show_history(user, total, page_num=1, page_size=20, **kwargs):
    # TODO 数据量大时分页显示
    x.clear()
    x.field_names = ('index', 'aid', 'readTimeLength', 'readSequence',
                     'agreeOrNot', 'commentOrNot', 'shareOrNot', 'commentDetail', 'timestamp')
    # TODO 展示历史阅读数据
    #       根据Read 表的数据展示当前用户的阅读历史
    reads = ReadService().get_history(user.uid, page_num, page_size)
    for index, read in enumerate(reads):
        # title = ReadService().get_by_rid(rid=read.aid)
        x.add_row((index, read.aid, read.readTimeLength, read.readSequence,
                   read.agreeOrNot, read.commentOrNot, read.shareOrNot, read.commentDetail,
                   timestamp_to_str(read.timestamp)))

    print(x)
    show_next(page_num, page_size, total, show_history, **kwargs)
    # input("按回车键返回")
    pass


def history_admin():
    for index, dbms in enumerate(DBMS.all):
        print("{}. {}".format(index + 1, dbms))
    db = input("请选择查看的数据库: ")
    if db == '':
        return

    while not db.isdigit() or int(db) < 1 or int(db) > len(DBMS.all):
        db = input("输入错误，请重新选择: ")

    db_alias = DBMS.all[int(db) - 1]
    total = ReadService().count(db_alias=db_alias)
    show_history_admin(total, db_alias=db_alias)


def show_history_admin(total, page_num=1, page_size=20, db_alias=None, **kwargs):
    # TODO 数据量大时分页显示
    # x.clear()
    x.clear_rows()
    x.field_names = ('index', 'aid', 'uid', 'readTimeLength', 'readSequence',
                     'agreeOrNot', 'commentOrNot', 'shareOrNot', 'commentDetail', 'readTime')
    # TODO 展示历史阅读数据
    #       根据Read 表的数据展示当前用户的阅读历史
    #       目前只展示DBMS1里边的阅读数据
    reads = ReadService().get_reads(page_num, page_size, db_alias=db_alias)
    for index, read in enumerate(reads):
        x.add_row((index, read.aid, read.uid, read.readTimeLength, read.readSequence,
                   read.agreeOrNot, read.commentOrNot, read.shareOrNot, read.commentDetail, read.timestamp))

    print(x)
    show_next(page_num, page_size, total, show_history_admin, **kwargs)
    # input("按回车键返回")
    pass


def del_an_article(user, role):
    aid = input("请输入要删除的文章aid： ")
    if aid == '':
        return
    if role == 'user' and user is not None:
        article = ArticleService().get_an_article(id=aid, authors=user.name)
        if article is None:
            print("您无权限删除该文章或者该文章不存在")
            return
        ArticleService().del_article(article)
    elif role == 'admin':
        ArticleService().del_by_aid(_id=aid)
    else:
        print("用户不存在或者权限错误")
        return

    print("删除成功")
    return None
    pass


def update_an_article(role, user):
    # TODO 管理员或者用户更新文章信息
    aid = input("请输入要更新的文章的id：")
    if aid is None or aid == '':
        print("无输入，返回上一级")
        return
    try:
        article = ArticleService().get_one_by_aid(aid=aid)
    except Exception:
        print("id不存在")
        return
    if article is None:
        print("文章不存在。")
        return
    if (role == 'user' and article.authors == user.name) or role == 'admin':
        print_an_article(article)
        update_info = input("请输入要更改的信息（json格式：）")
        if update_info == '':
            return
        try:
            con = json.loads(update_info)
            if ArticleService().update_one(article, con) is not None:
                print("更新成功")
            else:
                print("更新失败")
        except json.JSONDecodeError:
            print("输入不是json格式")
            return None
    else:
        print("无权限对该文章进行操作")

    pass


def update_popular():
    print("开始更新")
    PopularService().update_popular()
    # print("更新一周热门")
    # PopularService().update_monthly_rank()
    # print("更新一月热门")
    # PopularService().update_weekly_rank()
    print("更新完成")
    pass


def show_my_article(user):
    articles = ArticleService().get_articles(authors=user.name)
    ArticleService().pretty_articles(articles)

    input("按回车返回")
    return


if __name__ == '__main__':
    from main import init

    init()
    # articles_query_all()
    # read_an_article(user)
    from service.user_service import UserService

    _user = UserService().get_user_by_name('admin')
    # print(_user)
    # choices_article(_user)
    article_manage('admin', _user)
