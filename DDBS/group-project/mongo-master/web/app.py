from flask import Flask, render_template, request, jsonify
from flask_cors import CORS
from flask_jwt_extended import JWTManager, jwt_required, create_access_token, current_user

from service.user_service import UserService
from utils.func import get_best_dbms_by_region
from web.result import Result

app = Flask(__name__)
CORS(app, supports_credentials=True)

app.config['JWT_SECRET_KEY'] = 'sfwertyad'
jwt = JWTManager(app)


def create_app():
    app.config['JSON_SORT_KEYS'] = False  # 使返回的字段不排序， 完成开发后可删除
    api_rules()
    app.run()


def api_rules():
    # from web.api.user import UserGetUpdateDelete, UsersList
    from web.api.article import ArticleList, ArticleCURD
    from web.api.read import ReadsList, ReadCURD
    from web.api.popular import PopularList, PopularCURD, PopuparToday

    # app.add_url_rule('/api/users/<uid>', view_func=UserGetUpdateDelete.as_view('user'))
    # app.add_url_rule('/api/users', view_func=UsersList.as_view('users'))

    app.add_url_rule('/api/articles/<aid>', view_func=ArticleCURD.as_view('article'))
    app.add_url_rule('/api/articles', view_func=ArticleList.as_view('articles'))

    app.add_url_rule('/api/reads/<rid>', view_func=ReadCURD.as_view('read'))
    app.add_url_rule('/api/reads', view_func=ReadsList.as_view('reads'))

    app.add_url_rule('/api/populars/<pid>', view_func=PopularCURD.as_view('popular'))
    app.add_url_rule('/api/populars', view_func=PopularList.as_view('populars'))
    app.add_url_rule('/api/public/populars', view_func=PopuparToday.as_view('popular_today'))

    from web.api.be_read import ArticleRecord
    app.add_url_rule('/api/records/<aid>', view_func=ArticleRecord.as_view('record'))


# def get_jwt_user():
#     info = get_jwt_identity()
#     user = UserService().get_user_by_uid(uid=info['uid'], db_alias=get_best_dbms_by_region(info['region']))
#     return user
#

@jwt.user_loader_callback_loader
def user_loader_callback(identity):
    user = UserService().get_user_by_uid(identity['uid'], db_alias=get_best_dbms_by_region(identity['region']))
    return user


@jwt.user_loader_error_loader
def custom_user_loader_error(identity):
    ret = {
        "msg": "User {} not found".format(identity)
    }
    return jsonify(ret), 404


@app.before_request
def cors():
    if request.method == 'OPTIONS':
        return


@app.route('/api/user/login', methods=['POST'])
def login():
    if not request.is_json:
        return jsonify({"msg": "Missing JSON in request"}), 400
    username = request.json.get('username')
    password = request.json.get('password')
    print(username, password)
    user = None
    if username and password:
        user = UserService().login(username, password)
    if user:
        access_token = create_access_token(identity=user.to_dict(only=['uid', 'name', 'region']),
                                           expires_delta=False)
        return Result.gen_success({"token": access_token})
    else:
        return Result.gen_failed(404, "用户名或密码错误")


@app.route('/api/user/info', methods=['GET'])
@jwt_required
def user_info():
    user = current_user
    info = user.to_dict(other=['avatar'])
    if info['name'] == 'admin':
        info['roles'] = ['admin']
    else:
        info['roles'] = ['user']
    # info['avatar'] = "https://wpimg.wallstcn.com/f778738c-e4f8-4870-b634-56703b4acafe.gif"
    return Result.gen_success(info)
    pass


@app.route('/api/user/logout', methods=['GET', "POST"])
@jwt_required
def logout():
    # user = get_jwt_user()
    return Result.gen_success("")
    pass


@app.route('/api/dashboard', methods=['GET'])
def dashboard():
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


@app.route('/admin')
def admin(is_login=False):
    if not is_login:
        return render_template('admin/login.html')
    pass


@app.route('/admin/login', methods=['post', 'get'])
def admin_login():
    username = request.form.get_one('username')
    password = request.form.get_one('password')
    success = False
    if username == 'admin' and password:
        success = UserService.login(username, password)
    if success:
        return 'welcome\n' + username + '\n' + password
    else:
        return render_template('admin/login.html', message="登录失败")
    pass


if __name__ == '__main__':
    from main import init

    init()
    create_app()
