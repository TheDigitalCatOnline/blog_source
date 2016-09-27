import date_time_logger as dtl
import datetime


def test_logger_initialization_no_arguments():
    dtl.DateTimeLogger()


def test_logger_initialization_with_datetime_argument():
    now = datetime.datetime.now()
    dt = dtl.DateTimeLogger(now)

    assert dt.initial_datetime == now

