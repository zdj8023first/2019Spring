import json

from config import Config
from main import init
from service.article_service import ArticleService
from service.redis_service import RedisService
from service.user_service import UserService
from utils.func import pretty_models
from web.my_web import Web


# from web.app import app, api_rules


def print_json(json_data):
    print(json.dumps(json_data, indent=4, ensure_ascii=False))


class TestFlask:

    @classmethod
    def setup_class(cls) -> None:
        print("=" * 50 + "INIT" + "=" * 50)
        # print("连接数据库")
        Config.redis_enable = False
        Config.log_in_file = False
        # DBMS.db_name = 'test'
        init()

        # app.testing = True
        # cls.client = app.test_client()
        Web().session.testing = True
        # Web().run()
        cls.client = Web().session.test_client()
        cls.header = {'Content-Type': 'application/json'}
        # api_rules()
        print("=" * 50 + "INIT FINISH" + "=" * 50)

    def test_login(self):
        response = self.client.post('/api/users/login', json={'username': 'admin', 'password': 'admin'})
        print(response.data)

    def test_users(self):
        response = self.client.get('/api/users', query_string={'dbms': 'Beijing'})
        # print(response.data)
        pretty_models(response.json['data']['list'], UserService.field_names)
        assert len(response.json['data']['list']) == 20

    def test_get_user(self):
        res = self.client.get('/api/users/1')
        print(res.data)

    def test_dashboard(self):
        res = self.client.get('/api/dashboard?dbms=Beijing')
        print(res.data)

    def test_get_articles(self):
        res = self.client.get('/api/articles', query_string={'dbms': 'Beijing'})
        print(res.data)
        # pretty_models(res.json['data']['list'], ArticleService.field_names)

    def test_get_one_article(self):
        res = self.client.get('/api/articles/4')
        print(res.data)

    def test_populars(self):
        RedisService().get_redis('Hong Kong').delete_by_pattern('POPULAR*')
        response = self.client.get('/api/populars', query_string={'dbms': 'Hong Kong', 'level': 'weekly'})
        RedisService().get_redis('Hong Kong').delete_by_pattern('POPULAR*')
        print(response.data)

    def test_popular_today(self):
        query = {
            'dbms': 'Beijing',
            'level': 'daily',
            't': 1506268800000
        }
        response = self.client.get('/api/public/populars', query_string=query)
        print(response.data)

    def test_article_comments(self):
        aid = 0
        query = {
            'category ': 'science'
        }

        response = self.client.get(f'/api/records/{aid}', query_string=query)

        print_json(response.json)

    def test_article_record(self):
        uid = 0
        aid = 0
        region = UserService().get_user_by_uid(uid, only=['region']).region
        category = ArticleService().get_one_by_aid(aid, only=['category']).category
        data = {
            'uid': uid,
            'aid': aid,
            'region': region,
            'category': category,
            'readTimeLength': 80,
            'commentOrNot': 1,
            'commentDetail': '第二次评论',
            'agreeOrNot': 1
        }

        response = self.client.post(f'/api/reads', json=data)
        print_json(response.json)
