from config import DBMS
from service.article_service import ArticleService
from service.read_service import ReadService
from service.user_service import UserService
from test.test_base import TestBase


class TestReadService(TestBase):

    @classmethod
    def setup_class(cls) -> None:
        super().setup_class()
        cls.readService = ReadService()

    # def setup_method(self) -> None:
    #     self.readService = ReadService()

    def test_count(self):
        for dbms in DBMS.all:
            cnt = self.readService.count(db_alias=dbms)
            print("count {}: {}".format(dbms, cnt))

    def test_save_read(self, aid=None, uid=None):
        aid = aid or ArticleService().get_articles_by_title('title4')[0].aid
        uid = uid or UserService().get_user_by_name('user4').uid
        read = self.readService.add_one(aid, uid, 1, 34, 2, 0, 'sdf', 1, 1)
        assert read is not None
        self.readService.pretty_reads([read])
        return read

    def test_reads_list(self):
        reads = self.readService.get_reads()
        assert reads is not None
        self.readService.pretty_reads(reads)
        assert len(reads) == 20

        reads = self.readService.get_reads(db_alias=DBMS.DBMS1)
        assert reads is not None
        self.readService.pretty_reads(reads)
        assert len(reads) == 20

    def test_del_read_by_rid(self):
        aid = ArticleService().get_articles_by_title('title5')[0].aid
        uid = UserService().get_user_by_name('user5').uid
        read = self.test_save_read(aid, uid)
        num = self.readService.del_read_by_rid(read.rid)
        print(num)
        # assert num == 1

    def test_del_read_by_self(self):
        read = self.test_save_read()
        num = read.delete()
        print(num)
        assert num == 1

    def test_del_reads_by_uid(self):
        user = UserService().get_user_by_name('user4')
        num = self.readService.del_reads_by_uid(user.uid)
        print(num)
        # assert num > 0

    def test_get_history(self):
        user = UserService().get_user_by_name('user4')
        reads = self.readService.get_history(str(user.uid))
        self.readService.pretty_reads(reads)

    def test__get_popular(self):
        from utils.func import merge_dict_and_sort
        start = 1506332297000
        end = 1506332347001
        # [{'_id': 237, 'count': 1}
        read1 = self.readService._ReadService__get_popular_by_freq(start, end, db_alias=DBMS.DBMS2)
        read2 = self.readService._ReadService__get_popular_by_freq(start, end, db_alias=DBMS.DBMS1)
        print(merge_dict_and_sort(read1, read2))

        read1 = self.readService._ReadService__get_popular_by_aggregate(start, end, db_alias=DBMS.DBMS2)
        read2 = self.readService._ReadService__get_popular_by_aggregate(start, end, db_alias=DBMS.DBMS1)
        print(merge_dict_and_sort(read1, read2))
        pass

    def test_get_popular(self):
        from utils.func import timestamp_to_datetime
        _date = timestamp_to_datetime(1506338537000)

        reads = self.readService.compute_popular(_date.date(), before_days=7)
        print(reads)

    def test_get_daily_popular(self):
        # TODO 测试数据量大的时候导入热门数据
        from utils.func import timestamp_to_datetime
        _date = timestamp_to_datetime(1506340567000)

        reads = self.readService.get_daily_popular(_date.date())
        print(reads)

    def test_get_weekly_popular(self):
        from utils.func import timestamp_to_datetime
        _date = timestamp_to_datetime(1506340567000)

        reads = self.readService.get_weekly_popular(_date.date())
        print(reads)

    def test_get_month_popular(self):
        from utils.func import timestamp_to_datetime
        _date = timestamp_to_datetime(1506342197000)

        reads = self.readService.get_monthly_popular(_date.date())
        print(reads)

    def test_get_by_uid_and_aid(self):
        aid = ArticleService().get_articles_by_title('title5')[0].aid
        uid = UserService().get_user_by_name('user5').uid
        read1 = self.test_save_read(aid, uid)
        read = self.readService.get_by_uid_and_aid(uid, aid)
        assert read.aid == read1.aid and read.uid == read1.uid

        read = self.readService.get_by_rid(read1.rid)
        assert read.aid == read1.aid and read.uid == read1.uid

    def test_read_twice(self):
        aid = ArticleService().get_articles_by_title('title2')[0].aid
        uid = UserService().get_user_by_name('user2').uid
        read1 = self.readService.add_one(aid, uid, 1, 34, 2, 0, 'sdf', 0, 1)
        ReadService().pretty_reads([read1])
        read2 = self.readService.add_one(aid, uid, 1, 34, 2, 1, 'asdaff', 1, 1)
        ReadService().pretty_reads([read2])
        read1.reload()
        assert read1.readSequence == read2.readSequence

    def test_update_by_aid_uid(self):
        aid = 1
        uid = 1
        readOrNot = 0
