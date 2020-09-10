Title: Useful pytest command line options 
Date: 2018-07-05 11:00:00 +0100
Category: Programming
Tags: pytest, Python, Python2, Python3, TDD, testing
Authors: Leonardo Giordani
Slug: useful-pytest-command-line-options
Image: useful-pytest-command-line-options
Summary: A curated list of useful command line options of the Python unit testing framework pytest

I recently gave a workshop on "TDD in Python with pytest", where I developed a very simple Python project together with the attendees following a TDD approach. It's a good way to introduce TDD, I think. I wrote each test together with the attendees, and then I left them the task of writing the Python code that passes the test. This way I could show TDD in action, introducing pytest features like the `pytest.raises` context manager or the use of `assert` while they become useful for the actual tests.

This is the approach that I follow in some of my posts on TDD here on the blog, for example [A simple example of Python OOP development (with TDD)]({filename}python-oop-tdd-example-part1.markdown) and [A game of tokens: write an interpreter in Python with TDD - Part 1]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1.markdown).

Part of the workshop was dedicated to pytest command line options and in general to what pytest can do as a testing framework. Unfortunately there was no time to go through this part, so I promised some of the attendees to give them a written version of it. This post is the fulfilment of that promise.

Please remember to `import pytest` before using functions, decorators or attributes prefixed by `pytest.`, as I will not repeat it in each example.

# Run single tests

If you want to run only a specific test you can provide its name on the pytest command line

``` sh
$ pytest -svv tests/test_calc.py::test_addition
```

which for example runs the `tests_addition` test inside the `tests/test_calc.py` file. You can also specify the file name only to run all the tests contained there

``` sh
$ pytest -svv tests/test_calc.py
```

# Skipping tests

Sometimes it is useful to skip tests. The reason might be that some new code broke too many tests, and we want to face them one at a time, or that a specific feature had to be temporarily disabled. In all those cases the `pytest.mark.skip` decorator is your friend. Remember that a decorator is something that changes the way the decorated function works (for the skilled reader: it's a function wrapper). Assuming we are working on a `tests/test_calc.py` file the code might be

``` python
@pytest.mark.skip
def test_test_addition():
    [...]
```

The result on the command line will be (after running `py.test -svv`)

``` sh
tests/test_calc.py::test_addition SKIPPED
```

# Skipping with a reason

The previous solution is good for a temporary skip, but if the test has to remain deactivated for a long time it's better to annotate a specific reason for the exclusion. In my experience 1 day is enough to forget small details like this, so my advice is to always put a well-written reason on skipped tests. To add it you can use the `reason` attribute of the `skip` decorator

``` python
@pytest.mark.skip(reason="Addition has been deactivated because of issue #123")
def test_test_addition():
    [...]
```

Remember to add the `-rs` option to your command line to see the `r`eason behind `s`kipped tests. So after running `py.test -svv -rs` we will get something like

``` sh
tests/test_calc.py::test_addition SKIPPED
[...]
============================= short test summary info =============================
SKIP [1] tests/test_calc.py:5: Addition has been deactivated because of issue #123

====================== 12 passed, 1 skipped in 0.02 seconds =======================
```

# Skipping tests conditionally

Well, most of the time we will skip tests not for a stable reason, but according to some other condition that we can retrieve from the system, like the Python version, or maybe the region in which a server is running. The decorator that we need to use in that case is `skipif`, which accepts a condition (a boolean value) and a reason

``` python
import os


@pytest.mark.skipif(
    os.environ['AWS_REGION'] == 'us-west-2',
    reason="Addition has been deactivated in us-west-2 because of issue #234"
)
def test_addition():
    [...]
```

With this code running `AWS_REGION=eu-west-1 py.test -svv -rs` will run the `test_addition` test, while running `AWS_REGION=us-west-2 py.test -svv -rs` will skip it. The environment variable `AWS_REGION` set in the previous command lines is an example that simulates the presence of the variable in the system.

# Run tests by name

You can selectively run tests by name using `-k`. This option accepts Python expressions that try to match the name of the test with the provided values. So

``` sh
$ pytest -svv -k "test_addition"
```

will run all those tests which name contains the substring `'addition'`, like `test_addiiton`, `test_addition_multiple_inputs`, and `test_complex_addition`. A more complex expression could be for example

``` sh
$ pytest -svv -k "test_addition and not complex"
```

which will run both `test_addition` and `test_addition_multiple_inputs` but not `test_complex_addition`.


# Tagging tests

Tests can be tagged or labelled using `pytest.mark`, and the tag can be used to run or skip sets of tests. Let's say that we identify a set of very slow tests that we don't want to run continuously.

``` python
@pytest.mark.slow
def test_addition():
    [...]


def test_subtraction():
    [...]


@pytest.mark.slow
def test_multiplication():
    [...]
```

In the above example `test_addition` and `test_multiplication` have been decorated with `pytest.mark.slow` which tells pytest to label them with the `slow` identifier. At this point we can run all the tests that are tagged with the `-m` option

``` sh
$ pytest -svv -m slow
```

Tests can be tagged multiple times

``` python
@pytest.mark.complex
@pytest.mark.slow
def test_addition():
    [...]
```

In this case the test will be run both by `pytest -svv -m slow` and by `pytest -svv -m complex`.

The `-m` option supports complex expressions like

``` sh
$ pytest -svv -m 'not slow'
```

which runs all the tests that are not tagged with `slow`, or

``` sh
$ pytest -svv -m 'mac or linux'
```

which runs all the tests tagged with `mac` and all the tests tagged with `linux`. Pay attention that `-m` expressions refer to the tags of each single test, so `slow and complex` will run only those tests that are tagged both with `slow` and with `complex`, and not all the tests marked with the first and all the tests marked with the second.

# Adding a command line option

You can add custom command line options to pytest with the `pytest_addoption` and `pytest_runtest_setup` hooks that allows you to manage the command line parser and the setup for each test.

Let's say, for example, that we want to add a `--runslow` option that runs all the tests marked with `slow`. First, create the file `tests/conftest.py`, which is a file that pytest imports before running your tests, and use the `pytest_addoption` hook

``` python
def pytest_addoption(parser):
    parser.addoption("--runslow", action="store_true",
                     help="run slow tests")
```

The command line parser configuration will be stored into the `config` attribute of the setup of each test. Thus we can use the `pytest_runtest_setup` hook that runs before each test

``` python
def pytest_runtest_setup(item):
    if 'slow' in item.keywords and not item.config.getvalue("runslow"):
        pytest.skip("need --runslow option to run")
```

Here `item` is the single test, so `item.keywords` is the set of tags attached to the test, and `item.config` is the configuration after the parser run on the command line. This makes the previous code match all the tests that are decorated with `@pytest.mark.slow` and only when the `--runslow` option has been specified on the command line. If both those conditions are satisfied the `pytest.skip` function is run, which skips the current test adding the specified string as a reason.

# Coverage

Coverage is a measure of the percentage of code lines are "hit" when running tests. Basically the idea is to discover if there are parts of the code that are not run during the tests, thus being untested.

If you follow a strict TDD methodology your coverage will be 100% always, because the only code you will write is the one that you need to pass the tests. But please, please, please keep this in mind: not everything is easily tested and in front of some complex parts of the code you always have to ask yourself "Is it worth?".

Is it worth spending 3 days to write a test for a feature? Well, if a failure in the new code means a huge financial loss for your company, yes. If you are writing a tool for yourself, and the code you are writing is not dangerous at all, maybe not. With all the shades of grey between these two black and white extreme cases.

So, don't become a slave of the coverage index. A coverage of more than 90% is heaven, and being over 80% is perfectly fine. I would say that, except for specific corner cases being under 80% means that you are not really following a TDD methodology. So, maybe go and review your work flow.

Anyway, pytest gives you a nice way to report the coverage using the `coverage` program. Just install `pytest-cov` with

``` sh
$ pip install pytest-cov
```

and run pytest with

``` sh
$ pytest -svv --cov=<name> --cov-report=term
```

where `<name>` is the name of the Python module you are testing (actually the path where the code you are testing is). This gives you a nice report with the percentage of covered code file by file.

``` sh
$ py.test -svv --cov=mypymodule --cov-report=term

----------- coverage: platform linux, python 3.6.5-final-0 -----------
Name                          Stmts   Miss  Cover
-------------------------------------------------
mypymodule/__init__.py       3      0   100%
mypymodule/calc.py          23      0   100%
-------------------------------------------------
TOTAL                            26      0   100%
```

You may also use the `term-missing` report instad of just `term`, that lists the code blocks that are not covered

``` sh
$ py.test -svv --cov=mypymodule --cov-report=term-missing

----------- coverage: platform linux, python 3.6.5-final-0 -----------
Name                          Stmts   Miss  Cover   Missing
-----------------------------------------------------------
mypymodule/__init__.py       3      0   100%
mypymodule/calc.py          23      2    91%   6, 11
-----------------------------------------------------------
TOTAL                            26      2    92%
```

Here I commented some of the tests to force the coverage percentage to drop. As you can see the report tells us that lines 6 and 11 of the `mypymodule/calc.py` file are not covered by any test.

# Updates

2017-12-24: pytest.org (such an honour!) spotted a misspelled `pytest.mark.skip`. Thanks!

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

