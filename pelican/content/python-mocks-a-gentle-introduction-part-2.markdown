Title: Python Mocks: a gentle introduction - Part 2
Date: 2016-09-27 09:00:00 +0000
Category: Programming
Tags: decorators, OOP, Python, Python2, Python3, TDD, testing
Authors: Leonardo Giordani
Slug: python-mocks-a-gentle-introduction-part-2
Series: Python Mocks: a gentle introduction
Image: python-mocks
Summary: 

In the [first post]({filename}python-mocks-a-gentle-introduction-part-1.markdown) I introduced you to Python mocks, objects that can imitate other objects and work as placeholders, replacing external systems during unit testing. I described the basic behaviour of mock objects, the `return_value` and `side_effect` attributes, and the `assert_called_with()` method.

In this post I will briefly review the remaining `assert_*` methods and some interesting attributes that allow to check the calls received by the mock object. Then I will introduce and exemplify patching, which is a very important topic in testing.

## Other assertions and attributes

The [official documentation](https://docs.python.org/dev/library/unittest.mock.html) of the mock library lists many other assertion, namely `assert_called_once_with()`, `assert_any_call()`, `assert_has_calls()`, `assert_not_called()`. If you grasped how `assert_called_with()` works, you will have no troubles in understanding how those other behave. Be sure to check the documentation to get a full description of what mock object can assert about their history after being used by your code.

Together with those methods, mock objects also provide some useful attributes, two of which have been already reviewed in the first post. The remaining attributes are as expected mostly related to calls, and are `called`, `call_count`, `call_args`, `call_args_list`, `method_calls`, `mock_calls`. While these also are very well described in the official documentation, I want to point out the two `method_calls` and `mock_calls` attributes, that store the detailed list of methods which are called on the mock, and the `call_args_list` attribute that lists the parameters of every call.

Do not forget that methods called on a mock object are mocks themselves, so you may first access the main mock object to get information about the called methods, and then access those methods to get the arguments they received.

## Patching

Mocks are very simple to introduce in your tests whenever your objects accept classes or instances from outside. In that case, as described, you just have to instantiate the `Mock` class and pass the resulting object to your system. However, when the external classes instantiated by your library are hardcoded this simple trick does not work. In this case you have no chance to pass a mock object instead of the real one.
  
This is exactly the case addressed by patching. Patching, in a testing framework, means to replace a globally reachable object with a mock, thus achieving the target of having the code run unmodified, while part of it has been hot swapped, that is, replaced at run time.

## A warm-up example

Let us start with a very simple example. Patching can be complex to grasp at the beginning so it is better to learn it with trivial code. If you do not have it yet, create the testing environment `mockplayground` with the instruction given in the previous post.

I want to develop a simple class that returns information about a given file. The class shall be instantiated with the file name, which can be a relative path.

For the sake of brevity I will not show you every step of the TDD development of the class. Remember that TDD requires you to write a test and then implement the code, but sometimes this could be too fine grained, so do not use the TDD rules without thinking.

The tests for the initialization of the class are

``` python
from fileinfo import FileInfo

def test_init():
    filename = 'somefile.ext'
    fi = FileInfo(filename)
    assert fi.filename == filename

def test_init():
    filename = 'somefile.ext'
    relative_path = '../{}'.format(filename)
    fi = FileInfo(relative_path)
    assert fi.filename == filename

```

You can put them into the `tests/test_fileinfo.py` file. The code that makes the tests pass could be something like

``` python
import os


class FileInfo:
    def __init__(self, path):
        self.original_path = path
        self.filename = os.path.basename(path)
```

Up to now I didn't introduce any new feature. Now I want the `get_info()` function to return a tuple with the file name, the original path the class was instantiated with, and the absolute path of the file.

You immediately realise that you have an issue in writing the test. There is no way to easily test something as "the absolute path", since the outcome of the function called in the test is supposed to vary with the path of the test itself. Let us write part of the test

``` python
def test_get_info():
    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)
    fi = FileInfo(original_path)
    assert fi.get_info() == (filename, original_path, '???')
```

where the '???' string highlights that I cannot put something sensible to test the absolute path of the file.

Patching is the way to solve this problem. You know that the function will use some code to get the absolute path of the file. So in the scope of the test only you can replace that code with different code and perform the test. Since the replacement code has a known outcome writing the test is now possible.

Patching, thus, means to inform Python that in some scope you want a globally accessible module/object replaced by a mock. Let's see how we can use it in our example

``` python
from unittest.mock import patch

[...]

def test_get_info():
    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)

    with patch('os.path.abspath') as abspath_mock:
        test_abspath = 'some/abs/path'
        abspath_mock.return_value = test_abspath
        fi = FileInfo(original_path)
        assert fi.get_info() == (filename, original_path, test_abspath)
```

Remember that if you are using Python 2 you installed the `mock` module with `pip`, so your import statement becomes `from mock import patch`.

You clearly see the context in which the patching happens, as it is enclosed in a `with` statement. Inside this statement the module `os.path.abspath` will be replaced by a mock created by the function `patch` and called `abspath_mock`. We can now give the function a `return_value` as we did with standard mocks in the first post and run the test.

The code that make the test pass is

``` python
class FileInfo:
    [...]

    def get_info(self):
        return self.filename, self.original_path, os.path.abspath(self.filename)
```

Obviously to write the test you have to know that you are going to use the `os.path.abspath` function, so patching is somehow a "less pure" practice in TDD. In pure OOP/TDD you are only concerned with the external behaviour of the object, and not with its internal structure. This example, however, shows that you have to cope with some real world issues, and patching is a clean way to do it.

# The patching decorator

The `patch` function we imported from the `unittest.mock` module is very powerful, and can be used as a function decorator as well. When used in this fashion you need to change the decorated function to accept a mock as last argument.

``` python
@patch('os.path.abspath')
def test_get_info(abspath_mock):
    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)

    test_abspath = 'some/abs/path'
    abspath_mock.return_value = test_abspath
    fi = FileInfo(original_path)
    assert fi.get_info() == (filename, original_path, test_abspath)
```

As you can see the `patch` decorator works like a big `with` statement for the whole function. Obviously in this way you replace the target function `os.path.abspath` in the scope of the whole function. It is then up to you to decide if you need to use `patch` as a decorator or in a `with` block.

# Multiple patches

We can also patch more that one object. Say for example that we want to change the above test to check that the outcome of the `FileInfo.get_info()` method also contains the size of the file. To get the size of a file in Python we may use the `os.path.getsize()` function, which returns the size of the file in bytes.

So now we have to patch `os.path.getsize` as well, and this can be done with another `patch` decorator.

``` python
@patch('os.path.getsize')
@patch('os.path.abspath')
def test_get_info(abspath_mock, getsize_mock):
    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)

    test_abspath = 'some/abs/path'
    abspath_mock.return_value = test_abspath

    test_size = 1234
    getsize_mock.return_value = test_size

    fi = FileInfo(original_path)
    assert fi.get_info() == (filename, original_path, test_abspath, test_size)
```

Please notice that the decorator which is nearest to the function is applied first. Always remember that the decorator syntax with `@` is a shortcut to replace the function with the output of the decorator, so two decorators result in

``` python
@decorator1
@decorator2
def myfunction():
    pass
```

which is a shorcut for

``` python
def myfunction():
    pass
myfunction = decorator1(decorator2(myfunction))
```

This explains why, in the test code, the function receives first `abspath_mock` and then `getsize_mock`. The first decorator applied to the function is the patch of `os.path.abspath`, which appends the mock that we call `abspath_mock`. Then the patch of `os.path.getsize` is applied and this appends its own mock.

The code that makes the test pass is

``` python
class FileInfo:
    [...]

    def get_info(self):
        return self.filename, self.original_path, os.path.abspath(self.filename), os.path.getsize(self.filename)
```

We can write the above test using two `with` statements as well

``` python
def test_get_info():
    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)

    with patch('os.path.abspath') as abspath_mock:
        test_abspath = 'some/abs/path'
        abspath_mock.return_value = test_abspath

        with patch('os.path.getsize') as getsize_mock:
            test_size = 1234
            getsize_mock.return_value = test_size

            fi = FileInfo(original_path)
            assert fi.get_info() == (filename, original_path, test_abspath, test_size)
```

Using more than one `with` statement, however, makes the code difficult to read, in my opinion, so in general I prefer to avoid complex `with` trees if I do not need a limited scope of the patching.

# Patching immutable objects

The most widespread version of Python is CPython, which is written, as the name suggests, in C. Part of the standard library is also written in C, while the rest is written in Python itself.

The objects (classes, modules, functions, etc) that are implemented in C are shared between interpreters, which is something that you can do embedding the Python interpreter in a C program, for example. This requires those objects to be immutable, so that you cannot alter them at runtime from a single interpreter.

For an example of this immutability just check the following code

``` pycon
>>> a = 1
>>> a.conjugate = 5
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'int' object attribute 'conjugate' is read-only
```

Here I'm trying to replace a method with an integer, which is pointless, but nevertheless shows the issue we are facing.

What has this immutability to do with patching? What `patch` does is actually to temporarily replace an attibute of an object (method of a class, class of a module, etc), so if that object is immutable the patching action fails.

A typical example of this problem is the `datetime` module, which is also one of the best candidates for patching, since the output of time functions is by definition time-varying.

Let me show the problem with a simple class that logs operations. The class is the following (you can put it into a file called `logger.py`)

``` python
import datetime

class Logger:
    def __init__(self):
        self.messages = []

    def log(self, message):
        self.messages.append((datetime.datetime.now(), message))
```

This is pretty simple, but testing this code is problematic, because the `log()` method produces results that depend on the actual execution time.

If we try to write a test patching `datetime.datetime.now` we have a bitter surprise. This is the test code, that you can put in `tests/test_logger.py`

``` python
from unittest.mock import patch

from logger import Logger

def test_init():
    l = Logger()
    assert l.messages == []

@patch('datetime.datetime.now')
def test_log(mock_now):
    test_now = 123
    test_message = "A test message"
    mock_now.return_value = test_now

    l = Logger()
    l.log(test_message)
    assert l.messages == [(test_now, test_message)]
```

and the execution of pytest returns a `TypeError: can't set attributes of built-in/extension type 'datetime.datetime'`, which is exactly a problem of immutability.

There are several ways to address this problem, but all of them leverage the fact that, when you import of subclass an immutable object what you get is a "copy" of that is now mutable.

The easiest example in this case is the module `datetime` itself. In the `test_log` function we try to patch directly the `datetime.datetime.now` object, affecting the builtin module `datetime`. The file `logger.py`, however, does import `datetime`, so that the latter becomes a local symbol in the `logger` module. This is exactly the key for our patching. Let us change the code to

``` python
@patch('logger.datetime.datetime')
def test_log(mock_datetime):
    test_now = 123
    test_message = "A test message"
    mock_datetime.now.return_value = test_now

    l = Logger()
    l.log(test_message)
    assert l.messages == [(test_now, test_message)]
```

As you see running the test now the patching works. What we did was to patch `logger.datetime.datetime` instead of `datetime.datetime.now`. Two things changed, thus, in our test. First, we are patching the module imported in the `logger.py` file and not the module provided globally by the Python interpreter. Second, we have to patch the whole module because this is what is imported by the `logger.py` file. If you try to patch `logger.datetime.datetime.now` you will find that it is still immutable.

Another possible solution to this problem is to create a function that invokes the immutable object and returns its value. This last function can be easily patched, because it just uses the builtin objects and thus is not immutable. This solution, however, requires to change the source code to allow testing, which is far from being desirable. Obviously it is better to introduce a small change in the code and have it tested than to leave it untested, but whenever is possible I avoid solutions that introduce code which wouldn't be required without tests.

## Final words

In this second part of this small series on Python testing we reviewed the patching mechanism and run through some of its subtleties. Patching is a really effective technique, and patch-based tests can be found in many different packages. Take your time to become confident with mocks and patching, since they will be one of your main tools while working with Python and any other object-oriented language.

As always, I strongly recommend finding some time to read the [official documentation](https://docs.python.org/dev/library/unittest.mock.html) of the mock library.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections