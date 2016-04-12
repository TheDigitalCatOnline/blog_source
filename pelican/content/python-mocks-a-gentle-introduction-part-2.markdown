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