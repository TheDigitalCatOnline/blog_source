Title: Python Mocks: a gentle introduction - Part 1
Date: 2016-03-06 18:00:00 +0100
Category: Programming
Tags: decorators, OOP, Python, Python2, Python3, TDD
Authors: Leonardo Giordani
Slug: python-mocks-a-gentle-introduction-part-1
Summary: 

As already stressed in the two introductory posts on TDD (you can find them [here](categories/tdd/)) testing requires to write some code that uses the functions and objects you are going to develop. This means that you need to isolate a given (external) function, that is part of your public API, and demonstrate that it works with standard inputs and on edge cases.

For example, if you are going to develop an object that stores percentages (such as for example poll results), you should test the following conditions: the class can store a standard percentage such as 42%, the class shall give an error if you try to store a negative percentage, the class shall give an error if you store a percentage greater than 100%.

Tests shall be idempotent, which in mathematics and computer science identifies a process that can be run multiple times without changing the status of the system, and isolated. A test shall not change its behaviour depending on previous executions of itself, nor depend on the previous execution (or missing execution) of other tests.

Such restrictions, which guarantee that your tests are not passing due to a temporary configuration of the system or the order in which they are run, can raise big issues when dealing with external libraries and systems, or with intrinsically mutable concepts such as time. In the testing discipline, such issues are mostly faced using mocks, that is objects that pretend to be other objects.
 
In this series of posts I am going to review the Python `mock` library and exemplify its use. I will not cover everything you may do with mock, obviously, but hopefully I'll give you the information you need to start using this powerful library.

## Installation

First of all, mock is a Python library which development started around 2008. It was selected to be included in the standard library as of Python 3.3, which however does not prevent you to use other libraries if you prefer.
 
Python 3 users, thus, are not required to take any step, while for Python 2 projects you are still required to issue a `pip install mock` to install it into the system or the current virtualenv.

You may find the official documentation [here](https://docs.python.org/dev/library/unittest.mock.html). It is very detailed, and as always I strongly recommend taking your time to run through it.

## Basic concepts

A mock, in the testing lingo, is an object that simulates the behaviour of another (more complex) object. When you (unit)test an object of your library you need sometimes other systems your object want to connect to, but you do not really want to be forced to run them, for several reasons.

The first one is that connecting with external systems means having a complex testing environment, that is you are dropping the isolation requirement of you tests. If your object wants to connect with a website, for example, you are forced to have a running Internet connection, and if the remote website is down you cannot test your library.

The second reason is that the setup of an external system is usually slow in comparison with the speed of unit tests. We expect to run hundred of tests in seconds, and if we have to fetch information from a remote server for each of them the time easily increases by several orders of magnitude. Remember: having slow tests means that you cannot run them while you develop, which in turn means that you will not really use them for TDD.
     
The third reason is more subtle, and has to to with the mutable nature of an external system, thus I'll postpone the discussion of this issue for the moment.

Let us try and work with a mock in Python and see what it can do. First of all fire up a Python shell or a [Jupyter Notebook](jupyter.org) and import the library (remember to install it if you are running Python 2.

``` python
from unittest import mock
```

The main object that the library provides is `Mock` and you can instantiate it without any argument

``` python
m = mock.Mock()
```

This object has the peculiar property of creating methods and attributes on the fly when you require them. Let us first look inside the object to take a glance of what it provides

``` pycon
>>> dir(m)
['assert_any_call', 'assert_called_once_with', 'assert_called_with', 'assert_has_calls', 'attach_mock', 'call_args', 'call_args_list', 'call_count', 'called', 'configure_mock', 'method_calls', 'mock_add_spec', 'mock_calls', 'reset_mock', 'return_value', 'side_effect']
```

As you can see there are some methods which are already defined into the `Mock` object. Let us read a non-existing attribute

``` pycon
>>> m.some_attribute
<Mock name='mock.some_attribute' id='140222043808432'>
>>> dir(m)
['assert_any_call', 'assert_called_once_with', 'assert_called_with', 'assert_has_calls', 'attach_mock', 'call_args', 'call_args_list', 'call_count', 'called', 'configure_mock', 'method_calls', 'mock_add_spec', 'mock_calls', 'reset_mock', 'return_value', 'side_effect', 'some_attribute']
```

Well, as you can see this class is somehow different from what you are accustomed to. First of all its instances do not raise an `AttributeError` when asked for a non-existent attribute, but they happily return another instance of `Mock` itself. Second, the attribute you tried to access has now been created inside the object and accessing it returns the same mock object as before.
 
``` pycon
>>> m.some_attribute
<Mock name='mock.some_attribute' id='140222043808432'>
```

