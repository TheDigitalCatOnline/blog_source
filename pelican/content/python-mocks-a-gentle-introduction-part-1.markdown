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

Mock objects are callables, which means that they may act both as attributes and as methods. If you try to call the mock it just returns you another mock with a name that includes parentheses to remarks its callable nature

``` pycon
>>> m.some_attribute()
<Mock name='mock.some_attribute()' id='140247621475856'>
```

As you can understand, such objects are the perfect tool to mimic other objects or systems, since they may expose any API without raising exceptions. To use them in tests, however, we need them to behave just like the original, which implies returning sensible values or performing operations.
 
## Return value

The simplest thing a mock can do for you is to return a given value every time you call it. This is configured setting the `return_value` attribute of a mock object

``` pycon
>>> m.some attribute.return_value = 42
>>> m.some attribute()
42
```

Now the object does not return a mock object any more, instead it just returns the static value stored in the `return_value` attribute. Obviously you can also store a callable such as a function or an object, and the method will return it, but it will not run it. Let me give you an example

``` pycon
>>> def print_answer():
...  print("42")
... 
>>> 
>>> m.some_attribute.return_value = print_answer
>>> m.some_attribute()
<function print_answer at 0x7f8df1e3f400>
```

As you can see calling `some_attribute()` just returns the value stored in `return_value`, that is the function itself. To return values that come from a function we have to use a slightly more complex attribute of mock objects called `side_effect`.

## Side effect

The `side_effect` parameter of mock objects is a very powerful tool. It accepts three different flavours of objects, callables, iterables, and exceptions, and changes its behaviour accordingly.

If you pass an exception the mock will raise it
 
``` pycon
>>> m.some_attribute.side_effect = ValueError('A custom value error')
>>> m.some_attribute()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/usr/lib/python3.4/unittest/mock.py", line 902, in __call__
    return _mock_self._mock_call(*args, **kwargs)
  File "/usr/lib/python3.4/unittest/mock.py", line 958, in _mock_call
    raise effect
ValueError: A custom value error
```

If you pass an iterable, such as for example a generator, or a plain list, tuple, or similar objects, the mock will yield the values of that iterable, i.e. return every value contained in the iterable on subsequent calls of the mock. Let me give you an example

``` pycon
>>> m.some_attribute.side_effect = range(3)
>>> m.some_attribute()
0
>>> m.some_attribute()
1
>>> m.some_attribute()
2
>>> m.some_attribute()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/usr/lib/python3.4/unittest/mock.py", line 902, in __call__
    return _mock_self._mock_call(*args, **kwargs)
  File "/usr/lib/python3.4/unittest/mock.py", line 961, in _mock_call
    result = next(effect)
StopIteration
```

As promised, the mock just returns every object found in the iterable (in this case a `range` object) once at a time until the generator is exhausted. According to the iterator protocol (see [this post](blog/2013/03/25/python-generators-from-iterators-to-cooperative-multitasking/)) once every item has been returned the object raises the `StopIteration` exception, which means that you can correctly use it in a loop.

The last and perhaps most used case is that of passing a callable to `side_effect`, which shamelessly executes it with its own same parameters. This is very powerful, especially if you stop thinking about "functions" and start considering "callables". Indeed, `side_effect` also accepts a class and calls it, that is it can instantiate objects. Let us consider a simple example with a function without arguments

``` pycon
>>> def print_answer():
...     print("42")       
>>> m.some_attribute.side_effect = print_answer
>>> m.some_attribute.side_effect()
42
```

A slightly more complex example: a function with arguments

``` pycon
>>> def print_number(num):
...     print("Number:", num)
... 
>>> m.some_attribute.side_effect = print_number
>>> m.some_attribute.side_effect(5)
Number: 5
```

And finally an example with a class

``` pycon
>>> class Number(object):
...     def __init__(self, value):
...         self._value = value
...     def print_value(self):
...         print("Value:", self._value)
... 
>>> m.some_attribute.side_effect = Number
>>> n = m.some_attribute.side_effect(26)
>>> n
<__main__.Number object at 0x7f8df1aa4470>
>>> n.print_value()
Value: 26
```

## Testing with mocks

Now we know how to build a mock and how to give it a static return value or make it call a callable object. It is time to see how to use a mock in a test and what facilities do mocks provide. I'm going to use [pytest](http://pytest.org) as a testing framework. You can find a quick introduction to pytest and TDD [here](categories/tdd/)).

If you want to quickly setup a pytest playground, however, you may execute this 

``` sh
mkdir mockplayground
cd mockplayground
virtualenv venv3 -p python3
source venv3/bin/activate
pip install --upgrade pip
pip install pytest
echo "[pytest]" >> pytest.ini
echo "norecursedirs=venv*" >> pytest.ini
mkdir tests
touch tests/test_mock.py
py.test
```

