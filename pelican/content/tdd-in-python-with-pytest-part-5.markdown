Title: TDD in Python with pytest - Part 5
Date: 2020-09-21 10:30:00 +0200
Category: Programming
Tags: OOP, pytest, Python, Python3, refactoring, TDD, testing
Authors: Leonardo Giordani
Slug: tdd-in-python-with-pytest-part-5
Series: TDD in Python with pytest
Image: tdd-in-python-with-pytest-part-5
Summary: 

This is the fifth and last post in the series "TDD in Python with pytest" where I develop a simple project following a strict TDD methodology. The posts come from my book [Clean Architectures in Python](https://leanpub.com/clean-architectures-in-python) and have been reviewed to get rid of some bad naming choices of the version published in the book.

You can find the first post [here]({filename}tdd-in-python-with-pytest-part-1.markdown).

In this post I will conclude the discussion about mocks introducing patching.

## Patching

Mocks are very simple to introduce in your tests whenever your objects accept classes or instances from outside. In that case, as shown in the previous sections, you just have to instantiate the class `Mock` and pass the resulting object to your system. However, when the external classes instantiated by your library are hardcoded this simple trick does not work. In this case you have no chance to pass a fake object instead of the real one.

This is exactly the case addressed by patching. Patching, in a testing framework, means to replace a globally reachable object with a mock, thus achieving the goal of having the code run unmodified, while part of it has been hot swapped, that is, replaced at run time.

### A warm-up example

Clone the repository `fileinfo` that you can find [here](https://github.com/lgiordani/fileinfo) and move to the branch `develop`. As I did for the project `simple_calculator`, the branch `master` contains the full solution, and I use it to maintain the repository, but if you want to code along you need to start from scratch. If you prefer, you can clearly clone it on GitHub and make your own copy of the repository.

``` sh
git clone https://github.com/lgiordani/fileinfo
cd fileinfo
git checkout --track origin/develop
```

Create a virtual environment following your preferred process and install the requirements

``` sh
pip install -r requirements/dev.txt
```

You should at this point be able to run

``` sh
pytest -svv
```

and get an output like

``` text
=============================== test session starts ===============================
platform linux -- Python XXXX, pytest-XXXX, py-XXXX, pluggy-XXXX --
fileinfo/venv3/bin/python3
cachedir: .cache
rootdir: fileinfo, inifile: pytest.ini
plugins: cov-XXXX
collected 0 items 

============================== no tests ran in 0.02s ==============================
```

Let us start with a very simple example. Patching can be complex to grasp at the beginning so it is better to start learning it with trivial use cases. The purpose of this library is to develop a simple class that returns information about a given file. The class shall be instantiated with the file path, which can be relative.

The starting point is the class with the method `__init__`. If you want you can develop the class using TDD, but for the sake of brevity I will not show here all the steps that I followed. This is the set of tests I have in `tests/test_fileinfo.py`

``` { .python filename="tests/test_fileinfo.py" }
from fileinfo.fileinfo import FileInfo


def test_init():
    filename = 'somefile.ext'
    fi = FileInfo(filename)
    assert fi.filename == filename


def test_init_relative():
    filename = 'somefile.ext'
    relative_path = '../{}'.format(filename)
    fi = FileInfo(relative_path)
    assert fi.filename == filename
```

and this is the code of the class `FileInfo` in the file `fileinfo/fileinfo.py`

``` { .python filename="fileinfo/fileinfo.py" }
import os


class FileInfo:
    def __init__(self, path):
        self.original_path = path
        self.filename = os.path.basename(path)
```

**Git tag:** [first-version](https://github.com/lgiordani/fileinfo/tree/first-version)

As you can see the class is extremely simple, and the tests are straightforward. So far I didn't add anything new to what we discussed in the previous posts.

Now I want the method `get_info` to return a tuple with the file name, the original path the class was instantiated with, and the absolute path of the file. Pretending we are in the directory `/some/absolute/path`, the class should work as shown here

``` python
>>> fi = FileInfo('../book_list.txt')
>>> fi.get_info()
('book_list.txt', '../book_list.txt', '/some/absolute')
```

You can quickly realise that you have a problem writing the test. There is no way to easily test something as "the absolute path", since the outcome of the function called in the test is supposed to vary with the path of the test itself. Let us try to write part of the test

``` python
def test_get_info():
    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)
    fi = FileInfo(original_path)
    assert fi.get_info() == (filename, original_path, '???')
```

where the `'???'` string highlights that I cannot put something sensible to test the absolute path of the file.

Patching is the way to solve this problem. You know that the function will use some code to get the absolute path of the file. So, within the scope of this test only, you can replace that code with something different and perform the test. Since the replacement code has a known outcome writing the test is now possible.

Patching, thus, means to inform Python that during the execution of a specific portion of the code you want a globally accessible module/object replaced by a mock. Let's see how we can use it in our example

``` { .python filename="tests/test_fileinfo.py" }
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

You clearly see the context in which the patching happens, as it is enclosed in a `with` statement. Inside this statement the module `os.path.abspath` will be replaced by a mock created by the function `patch` and called `abspath_mock`. So, while Python executes the lines of code enclosed by the statement `with` any call to `os.path.abspath` will return the object `abspath_mock`.

The first thing we can do, then, is to give the mock a known `return_value`. This way we solve the issue that we had with the initial code, that is using an external component that returns an unpredictable result. The line

``` { .python filename="tests/test_fileinfo.py" hl_lines="11" }
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

instructs the patching mock to return the given string as a result, regardless of the real values of the file under consideration. 

The code that make the test pass is

``` { .python filename="fileinfo/fileinfo.py" }
class FileInfo:
    [...]

    def get_info(self):
        return (
            self.filename,
            self.original_path,
            os.path.abspath(self.original_path)
        )
```

When this code is executed by the test the function `os.path.abspath` is replaced at run time by the mock that we prepared there, which basically ignores the input value `self.filename` and returns the fixed value it was instructed to use.

**Git tag:** [patch-with-context-manager](https://github.com/lgiordani/fileinfo/tree/patch-with-context-manager)

It is worth at this point discussing outgoing messages again. The code that we are considering here is a clear example of an outgoing query, as the method `get_info` is not interested in changing the status of the external component. In the previous post we reached the conclusion that testing the return value of outgoing queries is pointless and should be avoided. With `patch` we are replacing the external component with something that we know, using it to test that our object correctly handles the value returned by the outgoing query. We are thus not testing the external component, as it has been replaced, and we are definitely not testing the mock, as its return value is already known.

Obviously to write the test you have to know that you are going to use the function `os.path.abspath`, so patching is somehow a "less pure" practice in TDD. In pure OOP/TDD you are only concerned with the external behaviour of the object, and not with its internal structure. This example, however, shows that this pure approach has some limitations that you have to cope with, and patching is a clean way to do it.

## The patching decorator

The function `patch` we imported from the module `unittest.mock` is very powerful, as it can temporarily replace an external object. If the replacement has to or can be active for the whole test, there is a cleaner way to inject your mocks, which is to use `patch` as a function decorator.

This means that you can decorate the test function, passing as argument the same argument you would pass if  `patch` was used in a `with` statement. This requires however a small change in the test function prototype, as it has to receive an additional argument, which will become the mock.

Let's change `test_get_info`, removing the statement `with` and decorating the function with `patch`

``` { .python filename="tests/test_fileinfo.py" }
@patch('os.path.abspath')
def test_get_info(abspath_mock):
    test_abspath = 'some/abs/path'
    abspath_mock.return_value = test_abspath

    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)

    fi = FileInfo(original_path)
    assert fi.get_info() == (filename, original_path, test_abspath)
```

**Git tag:** [patch-with-function-decorator](https://github.com/lgiordani/fileinfo/tree/patch-with-function-decorator)

As you can see the decorator `patch` works like a big `with` statement for the whole function. The argument `abspath_mock` passed to the test becomes internally the mock that replaces `os.path.abspath`. Obviously this way you replace `os.path.abspath` for the whole function, so you have to decide case by case which form of the function `patch` you need to use.

## Multiple patches

You can patch more that one object in the same test. For example, consider the case where the method `get_info` calls `os.path.getsize` in addition to `os.path.abspath`m in order to return the size of the file. You have at this point two different outgoing queries, and you have to replace both with mocks to make your class work during the test.

This can be easily done with an additional `patch` decorator

``` { .python filename="tests/test_fileinfo.py" }
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

Please note that the decorator which is nearest to the function is applied first. Always remember that the decorator syntax with `@` is a shortcut to replace the function with the output of the decorator, so two decorators result in

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

``` { .python filename="fileinfo/fileinfo.py" }
class FileInfo:
    [...]

    def get_info(self):
        return (
            self.filename,
            self.original_path,
            os.path.abspath(self.filename),
            os.path.getsize(self.filename)
        )
```

**Git tag:** [multiple-patches](https://github.com/lgiordani/fileinfo/tree/multiple-patches)

We can write the above test using two `with` statements as well

``` { .python filename="tests/test_fileinfo.py" }
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
            assert fi.get_info() == (
                filename,
                original_path,
                test_abspath,
                test_size
            )
```

Using more than one `with` statement, however, makes the code difficult to read, in my opinion, so in general I prefer to avoid complex `with` trees if I do not really need to use a limited scope of the patching.

## Checking call parameters

When you patch, your internal algorithm is not executed, as the patched method just return the values it has been instructed to return. This is connected to what we said about testing external systems, so everything is good, but while we don't want to test the internals of the module `os.path`, we want to be sure that we are passing the correct values to the external methods.

This is why mocks provide methods like `assert_called_with` (and other similar methods), through which we can check the values passed to a patched method when it is called. Let's add the checks to the test

``` { .python filename="tests/test_fileinfo.py" }
@patch('os.path.getsize')
@patch('os.path.abspath')
def test_get_info(abspath_mock, getsize_mock):
    test_abspath = 'some/abs/path'
    abspath_mock.return_value = test_abspath

    filename = 'somefile.ext'
    original_path = '../{}'.format(filename)

    test_size = 1234
    getsize_mock.return_value = test_size

    fi = FileInfo(original_path)
    info = fi.get_info() 

    abspath_mock.assert_called_with(original_path)
    getsize_mock.assert_called_with(original_path)
    assert info == (filename, original_path, test_abspath, test_size)
```

As you can see, I first invoke `fi.get_info` storing the result in the variable `info`, check that the patched methods have been called witht the correct parameters, and then assert the format of its output.

The test passes, confirming that we are passing the correct values.

**Git tag:** [addding-checks-for-input-values](https://github.com/lgiordani/fileinfo/tree/addding-checks-for-input-values)

## Patching immutable objects

The most widespread version of Python is CPython, which is written, as the name suggests, in C. Part of the standard library is also written in C, while the rest is written in Python itself.

The objects (classes, modules, functions, etc.) that are implemented in C are shared between interpreters, and this requires those objects to be immutable, so that you cannot alter them at runtime from a single interpreter.

An example of this immutability can be given easily using a Python console

``` python
>>> a = 1
>>> a.conjugate = 5
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'int' object attribute 'conjugate' is read-only
```

Here I'm trying to replace a method with an integer, which is pointless per se, but clearly shows the issue we are facing.

What has this immutability to do with patching? What `patch` does is actually to temporarily replace an attribute of an object (method of a class, class of a module, etc.), which also means that if we try to replace an attribute in an immutable object the patching action will fail.

A typical example of this problem is the module `datetime`, which is also one of the best candidates for patching, since the output of time functions is by definition time-varying.

Let me show the problem with a simple class that logs operations. I will temporarily break the TDD methodology writing first the class and then the tests, so that you can appreciate the problem.

Create a file called `logger.py` and put there the following code

``` { .python filename="fileinfo/logger.py" }
import datetime


class Logger:
    def __init__(self):
        self.messages = []

    def log(self, message):
        self.messages.append((datetime.datetime.now(), message))
```

This is pretty simple, but testing this code is problematic, because the method `log` produces results that depend on the actual execution time. The call to `datetime.datetime.now` is however an outgoing query, and as such it can be replaced by a mock with `patch`.

If we try to do it, however, we will have a bitter surprise. This is the test code, that you can put in `tests/test_logger.py`

``` { .python filename="tests/test_logger.py" }
from unittest.mock import patch

from fileinfo.logger import Logger


@patch('datetime.datetime.now')
def test_log(mock_now):
    test_now = 123
    test_message = "A test message"
    mock_now.return_value = test_now

    test_logger = Logger()
    test_logger.log(test_message)
    assert test_logger.messages == [(test_now, test_message)]
```

When you try to execute this test you will get the following error

``` txt
TypeError: can't set attributes of built-in/extension type 'datetime.datetime'
```

which is raised because patching tries to replace the function `now` in `datetime.datetime` with a mock, and since the module is immutable this operation fails.

**Git tag:** [initial-logger-not-working](https://github.com/lgiordani/fileinfo/tree/initial-logger-not-working)

There are several ways to address this problem. All of them, however, start from the fact that importing or subclassing an immutable object gives you a mutable "copy" of that object.

The easiest example in this case is the module `datetime` itself. In the function `test_log` we tried to patch directly the object `datetime.datetime.now`, affecting the builtin module `datetime`. The file `logger.py`, however, does import `datetime`, so this latter becomes a local symbol in the module `logger`. This is exactly the key for our patching. Let us change the code to

``` { .python filename="tests/test_logger.py" }
@patch('fileinfo.logger.datetime.datetime')
def test_log(mock_datetime):
    test_now = 123
    test_message = "A test message"
    mock_datetime.now.return_value = test_now

    test_logger = Logger()
    test_logger.log(test_message)
    assert test_logger.messages == [(test_now, test_message)]
```

**Git tag:** [correct-patching](https://github.com/lgiordani/fileinfo/tree/correct-patching)

If you run the test now, you can see that the patching works. What we did was to inject our mock in `fileinfo.logger.datetime.datetime` instead of `datetime.datetime.now`. Two things changed, thus, in our test. First, we are patching the module imported in the file `logger.py` and not the module provided globally by the Python interpreter. Second, we have to patch the whole module because this is what is imported by the file `logger.py`. If you try to patch `fileinfo.logger.datetime.datetime.now` you will find that it is still immutable.

Another possible solution to this problem is to create a function that invokes the immutable object and returns its value. This last function can be easily patched, because it just uses the builtin objects and thus is not immutable. This solution, however, requires changing the source code to allow testing, which is far from being optimal. Obviously it is better to introduce a small change in the code and have it tested than to leave it untested, but whenever is possible I try as much as possible to avoid solutions that introduce code which wouldn't be required without tests.

## Mocks and proper TDD

Following a strict TDD methodology means writing a test before writing the code that passes that test. This can be done because we use the object under test as a black box, interacting with it through its API, and thus not knowing anything of its internal structure.

When we mock systems we break this assumption. In particular we need to open the black box every time we need to patch an hardcoded external system. Let's say, for example, that the object under test creates a temporary directory to perform some data processing. This is a detail of the implementation and we are not supposed to know it while testing the object, but since we need to mock the file creation to avoid interaction with the external system (storage) we need to become aware of what happens internally.

This also means that writing a test for the object before writing the implementation of the object itself is difficult. Pretty often, thus, such objects are built with TDD but iteratively, where mocks are introduced after the code has been written.

While this is a violation of the strict TDD methodology, I don't consider it a bad practice. TDD helps us to write better code consistently, but good code can be written even without tests. The real outcome of TDD is a test suite that is capable of detecting regressions or the removal of important features in the future. This means that breaking strict TDD for a small part of the code (patching objects) will not affect the real result of the process, only change the way we achieve it.

## A warning

Mocks are a good way to approach parts of the system that are not under test but that are still part of the code that we are running. This is particularly true for parts of the code that we wrote, which internal structure is ultimately known. When the external system is complex and completely detached from our code, mocking starts to become complicated and the risk is that we spend more time faking parts of the system than actually writing code.

In this cases we definitely crossed the barrier between unit testing and integration testing. You may see mocks as the bridge between the two, as they allow you to keep unit-testing parts that are naturally connected ("integrated") with external systems, but there is a point where you need to recognise that you need to change approach.

This threshold is not fixed, and I can't give you a rule to recognise it, but I can give you some advice. First of all keep an eye on how many things you need to mock to make a test run, as an increasing number of mocks in a single test is definitely a sign of something wrong in the testing approach. My rule of thumb is that when I have to create more than 3 mocks, an alarm goes off in my mind and I start questioning what I am doing.

The second advice is to always consider the complexity of the mocks. You may find yourself patching a class but then having to create monsters like `cls_mock().func1().func2().func3.assert_called_with(x=42)` which is a sign that the part of the system that you are mocking is deep into some code that you cannot really access, because you don't know it's internal mechanisms.

The third advice is to consider mocks as "hooks" that you throw at the external system, and that break its hull to reach its internal structure. These hooks are obviously against the assumption that we can interact with a system knowing only its external behaviour, or its API. As such, you should keep in mind that each mock you create is a step back from this perfect assumption, thus "breaking the spell" of the decoupled interaction. Doing this makes it increasingly complex to create mocks, and this will contribute to keep you aware of what you are doing (or overdoing).

## Final words

Mocks are a very powerful tool that allows us to test code that contains outgoing messages. In particular they allow us to test the arguments of outgoing commands. Patching is a good way to overcome the fact that some external components are hardcoded in our code and are thus unreachable through the arguments passed to the classes or the methods under analysis.

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

