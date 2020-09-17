Title: TDD in Python with pytest - Part 4
Date: 2020-09-17 11:30:00 +0200
Category: Programming
Tags: OOP, pytest, Python, Python3, refactoring, TDD, testing
Authors: Leonardo Giordani
Slug: tdd-in-python-with-pytest-part-4
Series: TDD in Python with pytest
Image: tdd-in-python-with-pytest-part-4
Summary: 

This is the fourth post in the series "TDD in Python with pytest" where I develop a simple project following a strict TDD methodology. The posts come from my book [Clean Architectures in Python](https://leanpub.com/clean-architectures-in-python) and have been reviewed to get rid of some bad naming choices of the version published in the book.

You can find the first post [here]({filename}tdd-in-python-with-pytest-part-1.markdown).

In this post I will discuss a very interesting and useful testing tool: mocks.

## Basic concepts

As we saw in the previous post the relationship between the component that we are testing and other components of the system can be complex. Sometimes idempotency and isolation are not easy to achieve, and testing outgoing commands requires to check the parameters sent to the external component, which is not trivial.

The main difficulty comes from the fact that your code is actually using the external system. When you run it in production the external system will provide the data that your code needs and the whole process can work as intended. During testing, however, you don't want to be bound to the external system, for the reasons explained in the previous post, but at the same time you need it to make your code work.

So, you face a complex issue. On the one hand your code is connected to the external system (be it hardcoded or chosen programmatically), but on the other hand you want it to run without the external system being active (or even present).

This problem can be solved with the use of mocks. A mock, in the testing jargon, is an object that simulates the behaviour of another (more complex) object. Wherever your code connects to an external system, during testing you can replace the latter with a mock, pretending the external system is there and properly checking that your component behaves like intended.

## First steps

Let us try and work with a mock in Python and see what it can do. First of all fire up a Python shell and import the library 

``` python
>>> from unittest import mock
```

The main object that the library provides is `Mock` and you can instantiate it without any argument

``` python
>>> m = mock.Mock()
```

This object has the peculiar property of creating methods and attributes on the fly when you require them. Let us first look inside the object to get an idea of what it provides

``` python
>>> dir(m)
['assert_any_call', 'assert_called_once_with', 'assert_called_with', 'assert_has_calls', 'attach_mock', 'call_args', 'call_args_list', 'call_count', 'called', 'configure_mock', 'method_calls', 'mock_add_spec', 'mock_calls', 'reset_mock', 'return_value', 'side_effect']
```

As you can see there are some methods which are already defined into the object `Mock`. Let's try to read a non-existent attribute

``` python
>>> m.some_attribute
<Mock name='mock.some_attribute' id='140222043808432'>
>>> dir(m)
['assert_any_call', 'assert_called_once_with', 'assert_called_with', 'assert_has_calls', 'attach_mock', 'call_args', 'call_args_list', 'call_count', 'called', 'configure_mock', 'method_calls', 'mock_add_spec', 'mock_calls', 'reset_mock', 'return_value', 'side_effect', 'some_attribute']
```

As you can see this class is somehow different from what you are used to. First of all, its instances do not raise an `AttributeError` when asked for a non-existent attribute, but they happily return another instance of `Mock` itself. Second, the attribute you tried to access has now been created inside the object and accessing it returns the same mock object as before.
 
``` python
>>> m.some_attribute
<Mock name='mock.some_attribute' id='140222043808432'>
```

Mock objects are callables, which means that they may act both as attributes and as methods. If you try to call the mock, it just returns another mock with a name that includes parentheses to signal its callable nature

``` python
>>> m.some_attribute()
<Mock name='mock.some_attribute()' id='140247621475856'>
```

As you can understand, such objects are the perfect tool to mimic other objects or systems, since they may expose any API without raising exceptions. To use them in tests, however, we need them to behave just like the original, which implies returning sensible values or performing real operations.
 
## Simple return values

The simplest thing a mock can do for you is to return a given value every time you call one of its methods. This is configured setting the attribute `return_value` of a mock object

``` python
>>> m.some_attribute.return_value = 42
>>> m.some_attribute()
42
```

Now, as you can see the object does not return a mock object any more, instead it just returns the static value stored in the attribute `return_value`. Since in Python everything is an object you can return here any type of value: simple types like an integer of a string, more complex structures like dictionaries or lists, classes that you defined, instances of those, or functions.

Pay attention that what the mock returns is exactly the object that it is instructed to use as return value. If the return value is a callable such as a function, calling the mock will return the function itself and not the result of the function. Let me give you an example

``` python
>>> def print_answer():
...  print("42")
... 
>>> 
>>> m.some_attribute.return_value = print_answer
>>> m.some_attribute()
<function print_answer at 0x7f8df1e3f400>
```

As you can see calling `some_attribute` just returns the value stored in `return_value`, that is the function itself. This is not exactly what we were aiming for. To make the mock call the object that we use as a return value we have to use a slightly more complex attribute called `side_effect`.

## Complex return values

The `side_effect` parameter of mock objects is a very powerful tool. It accepts three different flavours of objects: callables, iterables, and exceptions, and changes its behaviour accordingly.

If you pass an exception the mock will raise it
 
``` python
>>> m.some_attribute.side_effect = ValueError('A custom value error')
>>> m.some_attribute()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/usr/lib/python3.6/unittest/mock.py", line 939, in __call__
    return _mock_self._mock_call(*args, **kwargs)
  File "/usr/lib/python3.6/unittest/mock.py", line 995, in _mock_call
    raise effect
ValueError: A custom value error
```

If you pass an iterable, such as for example a generator, a plain list, tuple, or similar objects, the mock will yield the values of that iterable, i.e. return every value contained in the iterable on subsequent calls of the mock.

``` python
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
  File "/usr/lib/python3.6/unittest/mock.py", line 939, in __call__
    return _mock_self._mock_call(*args, **kwargs)
  File "/usr/lib/python3.6/unittest/mock.py", line 998, in _mock_call
    result = next(effect)
StopIteration
```

As promised, the mock just returns every object found in the iterable (in this case a `range` object) one at a time until the generator is exhausted. According to the iterator protocol once every item has been returned the object raises the `StopIteration` exception, which means that you can safely use it in a loop.

Last, if you feed `side_effect` a callable, the latter will be executed with the parameters passed when calling the attribute. Let's consider again the simple example given in the previous section

``` python
>>> def print_answer():
...     print("42")       
>>> m.some_attribute.side_effect = print_answer
>>> m.some_attribute()
42
```

A slightly more complex example is that of a function with arguments

``` python
>>> def print_number(num):
...     print("Number:", num)
... 
>>> m.some_attribute.side_effect = print_number
>>> m.some_attribute(5)
Number: 5
```

As you can see the arguments passed to the attribute are directly used as arguments for the stored function. This is very powerful, especially if you stop thinking about "functions" and start considering "callables". Indeed, given the nature of Python objects we know that instantiating an object is not different from calling a function, which means that `side_effect` can be given a class and return a instance of it

``` python
>>> class Number:
...     def __init__(self, value):
...         self._value = value
...     def print_value(self):
...         print("Value:", self._value)
... 
>>> m.some_attribute.side_effect = Number
>>> n = m.some_attribute(26)
>>> n
<__main__.Number object at 0x7f8df1aa4470>
>>> n.print_value()
Value: 26
```

## Asserting calls

As I explained in the previous post outgoing commands shall be tested checking the correctness of the message argument. This can be easily done with mocks, as these objects record every call that they receive and the arguments passed to it.

Let's see a practical example

``` python
from unittest import mock
import myobj


def test_connect():
    external_obj = mock.Mock()

    myobj.MyObj(external_obj)

    external_obj.connect.assert_called_with()
```

Here, the class `myobj.MyObj`  needs to connect to an external object, for example a remote repository or a database. The only thing we need to know for testing purposes is if the class called the method `connect` of the external object without any parameter.
 
So the first thing we do in this test is to instantiate the mock object. This is a fake version of the external object, and its only purpose is to accept calls from the object `MyObj` under test and possibly return sensible values. Then we instantiate the class `MyObj`  passing the external object. We expect the class to call the method `connect` so we express this expectation calling `external_obj.connect.assert_called_with`.

What happens behind the scenes? The class `MyObj` receives the fake external object and somewhere in its initialization process calls the method `connect` of the mock object. This call creates the method itself as a mock object. This new mock records the parameters used to call it and the subsequent call to its method `assert_called_with` checks that the method was called and that no parameters were passed.

In this case an object like

``` python
class MyObj():
    def __init__(self, repo):
        repo.connect()
```

would pass the test, as the object passed as `repo` is a mock that does nothing but record the calls. As you can see, the method `__init__` actually calls `repo.connect`, and `repo` is expected to be a full-featured external object that provides `connect` in its API. Calling `repo.connect` when `repo` is a mock object, instead, silently creates the method (as another mock object) and records that the method has been called once without arguments.

The method `assert_called_with` allows us to also check the parameters we passed when calling. To show this let us pretend that we expect the method `MyObj.setup` to call `setup(cache=True, max_connections=256)` on the external object. Remember that this is an outgoing command, so we are interested in checking the parameters and not the result.

The new test can be something like

``` python
def test_setup():
    external_obj = mock.Mock()
    obj = myobj.MyObj(external_obj)
    obj.setup()
    external_obj.setup.assert_called_with(cache=True, max_connections=256)
```

In this case an object that passes the test can be

``` python
class MyObj():
    def __init__(self, repo):
        self._repo = repo
        repo.connect()

    def setup(self):
        self._repo.setup(cache=True, max_connections=256)
```

If we change the method `setup` to

```
    def setup(self):
        self._repo.setup(cache=True)
```

the test will fail with the following error

``` sh
E           AssertionError: Expected call: setup(cache=True, max_connections=256)
E           Actual call: setup(cache=True)
```

Which I consider a very clear explanation of what went wrong during the test execution.

As you can read in the official documentation, the object `Mock` provides other methods and attributes, like `assert_called_once_with`, `assert_any_call`, `assert_has_calls`, `assert_not_called`, `called`, `call_count`, and many others. Each of those explores a different aspect of the mock behaviour concerning calls. Make sure to read their description and go through the examples.

## A simple example

To learn how to use mocks in a practical case, let's work together on a new module in the `simple_calculator` package. The target is to write a class that downloads a JSON file with data on meteorites and computes some statistics on the dataset using the class `SimpleCalculator`. The file is provided by NASA at [this URL](https://data.nasa.gov/resource/y77d-th95.json).

The class contains a method `get_data` that queries the remote server and returns the data, and a method `average_mass` that uses the method `SimpleCalculator.avg` to compute the average mass of the meteorites and return it. In a real world case, like for example in a scientific application, I would probably split the class in two. One class manages the data, updating it whenever it is necessary, and another one manages the statistics. For the sake of simplicity, however, I will keep the two functionalities together in this example.

Let's see a quick example of what is supposed to happen inside our code. An excerpt of the file provided from the server is

``` json
[
    {
        "fall": "Fell",
        "geolocation": {
            "type": "Point",
            "coordinates": [6.08333, 50.775]
        },
        "id":"1",
        "mass":"21",
        "name":"Aachen",
        "nametype":"Valid",
        "recclass":"L5",
        "reclat":"50.775000",
        "reclong":"6.083330",
        "year":"1880-01-01T00:00:00.000"
    },
    {
        "fall": "Fell",
        "geolocation": {
            "type": "Point",
            "coordinates": [10.23333, 56.18333]
        },
        "id":"2",
        "mass":"720",
        "name":"Aarhus",
        "nametype":"Valid",
        "recclass":"H6",
        "reclat":"56.183330",
        "reclong":"10.233330",
        "year":"1951-01-01T00:00:00.000"
    }
]
```

So a good way to compute the average mass of the meteorites is

``` python
import urllib.request
import json

from simple_calculator.main import SimpleCalculator

URL = ("https://data.nasa.gov/resource/y77d-th95.json")

with urllib.request.urlopen(URL) as url:
    data = json.loads(url.read().decode())

masses = [float(d['mass']) for d in data if 'mass' in d]

print(masses)

calculator = SimpleCalculator()

avg_mass = calculator.avg(masses)

print(avg_mass)
```

Where the list comprehension filters out those elements which do not have a attribute `mass`. This code returns the value 50190.19568930039, so that is the average mass of the meteorites contained in the file.

Now we have a proof of concept of the algorithm, so we can start writing the tests. We might initially come up with a simple solution like

``` python
def test_average_mass():
    metstats = MeteoriteStats()

    data = metstats.get_data()

    assert metstats.average_mass(data) == 50190.19568930039
```

This little test contains, however, two big issues. First of all the method `get_data` is supposed to use the Internet connection to get the data from the server. This is a typical example of an outgoing query, as we are not trying to change the state of the web server providing the data. You already know that you should not test the return value of an outgoing query, but you can see here why you shouldn't use real data when testing either. The data coming from the server can change in time, and this can invalidate your tests. 

Testing such a case becomes very simple with mocks. Since the class has a public method `get_data` that interacts with the external component, it is enough to temporarily replace it with a mock that provides sensible values. Create the file `tests/test_meteorites.py` and put this code in it

``` python
from unittest import mock

from simple_calculator.meteorites import MeteoriteStats


def test_average_mass():
    metstats = MeteoriteStats()

    metstats.get_data = mock.Mock()
    metstats.get_data.return_value = [
        {
            "fall": "Fell",
            "geolocation": {
                "type": "Point",
                "coordinates": [6.08333, 50.775]
            },
            "id":"1",
            "mass":"21",
            "name":"Aachen",
            "nametype":"Valid",
            "recclass":"L5",
            "reclat":"50.775000",
            "reclong":"6.083330",
            "year":"1880-01-01T00:00:00.000"},
        {
            "fall": "Fell",
            "geolocation": {
                "type": "Point",
                "coordinates": [10.23333, 56.18333]
            },
            "id":"2",
            "mass":"720",
            "name":"Aarhus",
            "nametype":"Valid",
            "recclass":"H6",
            "reclat":"56.183330",
            "reclong":"10.233330",
            "year":"1951-01-01T00:00:00.000"
        }
    ]

    result = metstats.average_mass(metstats.get_data())

    assert result == 370.5
```

When we run this test we are not testing that the external server provides the correct data. We are testing the process implemented by `average_mass`, feeding the algorithm some known input. This is not different from the first tests that we implemented: in that case we were testing an addition, here we are testing a more complex algorithm, but the concept is the same.

We can now write a class that passes this test. Put the following code in `simple_calculator/meteorites.py` alongside with `main.py`

``` python
import urllib.request
import json

from simple_calculator.main import SimpleCalculator

URL = ("https://data.nasa.gov/resource/y77d-th95.json")


class MeteoriteStats:
    def get_data(self):
        with urllib.request.urlopen(URL) as url:
            return json.loads(url.read().decode())

    def average_mass(self, data):
        calculator = SimpleCalculator()

        masses = [float(d['mass']) for d in data if 'mass' in d]

        return calculator.avg(masses)
```

As you can see the class contains the code we wrote as a proof of concept, slightly reworked to match the methods we used in the test. Run the test suite now, and you will see that the latest test we wrote passes.

Please note that we are not testing the method `get_data`. That method uses the function `urllib.request.urlopen` that opens an Internet connection without passing through any other public object that we can replace at run time during the test. We need then a tool to replace internal parts of our objects when we run them, and this is provided by patching, which will be the topic of the next post.

**Git tag:** [meteoritestats-class-added](https://github.com/lgiordani/simple_calculator/tree/meteoritestats-class)

## Final words

Mocks are very important, and as a Python programmer you need to know the subtleties of their implementation. Aside from the technical details, however, I believe it is mandatory to master the different types of tests that I discussed in the previous post, and to learn when to use simple assertions and when to pull a bigger gun like a mock object.

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

