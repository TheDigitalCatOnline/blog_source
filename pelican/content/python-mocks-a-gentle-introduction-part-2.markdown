Title: Python Mocks: a gentle introduction - Part 2
Date: 2016-04-12 17:00:00 +0100
Category: Programming
Tags: decorators, OOP, Python, Python2, Python3, TDD
Authors: Leonardo Giordani
Slug: python-mocks-a-gentle-introduction-part-2
Summary: 

In the first post I introduced you to Python mocks, objects that can imitate other objects and work as placeholders, replacing external systems during unit testing. I described the basic behaviour of mock objects, the `return_value` and `side_effect` attributes, and the `assert_called_with()` method.

In this post I will briefly review the remaining `assert_*` methods and some interesting attributes that allow to check the calls received by the mock object. Then I will introduce and exemplify patching, which is a very important topic in testing.

Last, I will give you a complete example of a package that provides access to a data storage through a REST API written in Flask.


## Other assertions and attributes

The [official documentation](https://docs.python.org/dev/library/unittest.mock.html) of the mock library lists many other assertion, namely `assert_called_once_with()`, `assert_any_call()`, `assert_has_calls()`, `assert_not_called()`. If you grasped how `assert_called_with()` works, you will have no troubles in understanding how those other behave. Be sure to check the documentation to get a full description of what mock object can assert about their history after being used by your code.

Together with those methods, mock objects also provide some usefult attributes, two of them were already reviewd in the first post. The remaining attributes are as expected mostly call-related, and are `called`, `call_count`, `call_args`, `call_args_list`, `method_calls`, `mock_calls`. While these also are very well descripted in the official documentation, I want to point out the two `method_calls` and `mock_calls` attributes, that store the detailed list of methods which are called on the mock, and the `call_args_list` attribute that lists the parameters of every call.

Do not forget that methods called on a mock object are mocks themselves, so you may first access the main mock object to get information about the called methods, and then access those methods to get the arguments they received.

## Patching

Mocks are very simple to introduce in your tests whenever your objects accept classes or instances from outside. In that case, as described, you just have to instantiate the `Mock` class and pass the resulting object to your system. However, when the external classes instantiated by your library are hardcoded this simple trick does not work. In this case you have no chance to pass a mock object instead of the real one.
  
This is exactly the case addressed by patching. Patching, in a testing framework, means to replace a globally reachable object with a mock, thus achieving the target of having the code run unmodified, while part of it has been hot swapped, that is, replaced at run time.

## A warm-up example

Let me give you one of the most classic examples of patching: dates and time. Imagine you are developing a library that logs events on files, and those files are named according to a given time. When you initialize the system you instantiate the logger passing the current date and time, and the logger creates a suitable file in some directory previously configured. The focal point here is that the file name the logger uses is time-dependant, since every time you run the library, even during tests, the output out the naming function is going to be different.
 
Let us write some tests and then implement the actual code. I will run through the example following the TDD methodology, writing a test that exposes a missing feature and then changing the code to make it pass. You may use the `mockplayground` code given in the first post as boilerplate to set up a workspace for this example.

First of all I want the `DateTimeLogger` class to allow being called without parameters. This is the code of the `tests/test_datetimelogger.py` file 
 
``` python
import date_time_logger as dtl


def test_logger_initialization_no_arguments():
    dtl.DateTimeLogger()
```

This is enough to get an `ImportError` from pytest. Usually I do not like tests that lack an explicit assert statement, but for the first initialization tests this is usually the case, so I will turn a blind eye to it.

The code that makes this test pass is very simple, perhaps even trivial. Create the `date_time_logger.py` file with the following code

``` python
class DateTimeLogger:
    pass
```

Now I will add a test to check if the object can be initialized with a given time. For the purpose of just checking the initialization of the class we need no special features, it is enough to get the current datetime through `datetime.datetime.now()` and use it. The file becomes

``` python
import date_time_logger as dtl
import datetime


def test_logger_initialization_no_arguments():
    dtl.DateTimeLogger()


def test_logger_initialization_with_datetime_argument():
    dtl.DateTimeLogger(datetime.datetime.now())
```

Which makes pytest exit with a `TypeError`, since `object` does not accept any parameter. To fix it we introduce a suitable `__init__()` method

``` python
class DateTimeLogger:
    def __init__(self, dt=None):
        pass
```

which is enough to make the test pass. This shows however that the test is not really that rich, so perhaps (but it is a matter of taste) it could be a good idea to enrich it and test that the object correctly stores the initial date and time. So the test now becomes

``` python
def test_logger_initialization_with_datetime_argument():
    now = datetime.datetime.now()
    dt = dtl.DateTimeLogger(now)

    assert dt.initial_datetime == now
```

and the code to make it pass becomes

``` python
class DateTimeLogger:
    def __init__(self, dt=None):
        self.initial_datetime = dt
```