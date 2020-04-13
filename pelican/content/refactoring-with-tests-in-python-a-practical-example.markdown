Title: Refactoring with tests in Python: a practical example
Date: 2017-07-21 09:30:00 +0100
Category: Programming
Tags: OOP, Python, Python3, refactoring, TDD, testing
Authors: Leonardo Giordani
Slug: refactoring-with-test-in-python-a-practical-example
Image: refactoring-with-tests-in-python
Summary: A step-by-step review of a refactoring session of Python code, using TDD

This post contains a step-by-step example of a refactoring session guided by tests. When dealing with untested or legacy code refactoring is dangerous and tests can help us do it the right way, minimizing the amount of bugs we introduce, and possibly completely avoiding them.

Refactoring is not easy. It requires a double effort to understand code that others wrote, or that we wrote in the past, and moving around parts of it, simplifying it, in one word **improving** it, is by no means something for the faint-hearted. Like programming, refactoring has its rules and best practices, but it can be described as a mixture of technique, intuition, experience, risk.

Programming, after all, is craftsmanship.

# The starting point

The simple use case I will use for this post is that of a service API that we can access, and that produces data in JSON format, namely a **list** of elements like the one shown here

``` json
{
    "age": 20,
    "surname": "Frazier",
    "name": "John",
    "salary": "£28943"
}
```

Once we convert this to a Python data structure we obtain a list of dictionaries, where `'age'` is an integer, and the remaining fields are strings.

Someone then wrote a class that computes some statistics on the input data. This class, called `DataStats`, provides a single method `stats()`, whose inputs are the data returned by the service (in JSON format), and two integers called `iage` and `isalary`. Those, according to the short documentation of the class, are the initial age and the initial salary used to compute the average yearly increase of the salary on the whole dataset.

The code is the following

``` python
import math
import json


class DataStats:

    def stats(self, data, iage, isalary):
        # iage and isalary are the starting age and salary used to
        # compute the average yearly increase of salary.

        # Compute average yearly increase
        average_age_increase = math.floor(
            sum([e['age'] for e in data])/len(data)) - iage
        average_salary_increase = math.floor(
            sum([int(e['salary'][1:]) for e in data])/len(data)) - isalary

        yearly_avg_increase = math.floor(
            average_salary_increase/average_age_increase)

        # Compute max salary
        salaries = [int(e['salary'][1:]) for e in data]
        threshold = '£' + str(max(salaries))

        max_salary = [e for e in data if e['salary'] == threshold]

        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        min_salary = [e for e in data if e['salary'] ==
                      '£{}'.format(str(min(salaries)))]

        return json.dumps({
            'avg_age': math.floor(sum([e['age'] for e in data])/len(data)),
            'avg_salary': math.floor(sum(
                [int(e['salary'][1:]) for e in data])/len(data)),
            'avg_yearly_increase': yearly_avg_increase,
            'max_salary': max_salary,
            'min_salary': min_salary
        })
```

# The goal

It is fairly easy, even for the untrained eye, to spot some issues in the previous class. A list of the most striking ones is

* The class exposes a single method and has no `__init__()`, thus the same functionality could be provided by a single function.
* The `stats()` method is too big, and performs too many tasks. This makes debugging very difficult, as there is a single inextricable piece of code that does everything.
* There is a lot of code duplication, or at least several lines that are very similar. Most notably the two operations `'£' + str(max(salaries))` and `'£{}'.format(str(min(salaries)))`, the two different lines starting with `salaries = `, and the several list comprehensions.

So, since we are going to use this code in some part of our Amazing New Project™, we want to possibly fix these issues.

The class, however, is working perfectly. It has been used in production for many years and there are no known bugs, so our operation has to be a **refactoring**, which means that we want to write something better, preserving the behaviour of the previous object.

# The path

In this post I want to show you how you can safely refactor such a class using tests. This is different from TDD, but the two are closely related. The class we have has not been created using TDD, as there are no tests, but we can use tests to ensure its behaviour is preserved. This should therefore be called Test Driven Refactoring (TDR).

The idea behind TDR is pretty simple. First, we have to write a test that checks the behaviour of some code, possibly a small part with a clearly defined scope and output. This is a posthumous (or late) unit test, and it simulates what the author of the code should have provided (cough cough, it was you some months ago...).

Once you have you unit test you can go and modify the code, knowing that the behaviour of the resulting object will be the same of the previous one. As you can easily understand, the effectiveness of this methodology depends strongly on the quality of the tests themselves, possibly more than when developing with TDD, and this is why refactoring is hard.

# Caveats

Two remarks before we start our refactoring. The first is that such a class could easily be refactored to some functional code. As you will be able to infer from the final result there is no real reason to keep an object-oriented approach for this code. I decided to go that way, however, as it gave me the possibility to show a design pattern called wrapper, and the refactoring technique that leverages it.

The second remark is that in pure TDD it is strongly advised not to test internal methods, that is those methods that do not form the public API of the object. In general, we identify such methods in Python by prefixing their name with an underscore, and the reason not to test them is that TDD wants you to shape objects according to the object-oriented programming methodology, which considers objects as **behaviours** and not as **structures**. Thus, we are only interested in testing public methods.

It is also true, however, that sometimes even tough we do not want to make a method public, that method contains some complex logic that we want to test. So, in my opinion the TDD advice should sound like "Test internal methods only when they contain some non-trivial logic".

When it comes to refactoring, however, we are somehow deconstructing a previously existing structure, and usually we end up creating a lot of private methods to help extracting and generalising parts of the code. My advice in this case is to test those methods, as this gives you a higher degree of confidence in what you are doing. With experience you will then learn which tests are required and which are not.

# Setup of the testing environment

Clone [this repository](https://github.com/lgiordani/datastats) and create a virtual environment. Activate it and install the required packages with 

``` sh
pip install -r requirements.txt
```

The repository already contains a configuration file for pytest and you should customise it to avoid entering your virtual environment directory. Go and fix the `norecursedirs` parameter in that file, adding the name of the virtual environment you just created; I usually name my virtual environments with a `venv` prefix, and this is why that variable contains the entry `venv*`.

At this point you should be able to run `pytest -svv` in the parent directory of the repository (the one that contains `pytest.ini`), and obtain a result similar to the following

``` sh
========================== test session starts ==========================
platform linux -- Python 3.5.3, pytest-3.1.2, py-1.4.34, pluggy-0.4.0
cachedir: .cache
rootdir: datastats, inifile: pytest.ini
plugins: cov-2.5.1
collected 0 items 

====================== no tests ran in 0.00 seconds ======================
```

The given repository contains two branches. `master` is the one that you are into, and contains the initial setup, while `develop` points to the last step of the whole refactoring process. Every step of this post contains a reference to the commit that contains the changes introduced in that section.

# Step 1 - Testing the endpoints

Commit: [27a1d8c](https://github.com/lgiordani/datastats/commit/27a1d8ccd5b0a57fa6d9d5f3bd80874538f14ed2)

When you start refactoring a system, regardless of the size, you have to test the endpoints. This means that you consider the system as a black box (i.e. you do not know what is inside) and just check the external behaviour. In this case we can write a test that initialises the class and runs the `stats()` method with some test data, possibly **real** data, and checks the output. Obviously we will write the test with the actual output returned by the method, so this test is automatically passing.

Querying the server we get the following data

``` python
test_data = [
    {
        "id": 1,
        "name": "Laith",
        "surname": "Simmons",
        "age": 68,
        "salary": "£27888"
    },
    {
        "id": 2,
        "name": "Mikayla",
        "surname": "Henry",
        "age": 49,
        "salary": "£67137"
    },
    {
        "id": 3,
        "name": "Garth",
        "surname": "Fields",
        "age": 70,
        "salary": "£70472"
    }
]
```

and calling the `stats()` method with that output, with `iage` set to `20`, and `isalary` set to `20000`, we get the following JSON result

``` json
{
    "avg_age": 62,
    "avg_salary": 55165,
    "avg_yearly_increase": 837,
    "max_salary": [{
        "id": 3,
        "name": "Garth",
        "surname": "Fields",
        "age": 70,
        "salary": "£70472"
    }],
    "min_salary": [{
        "id": 1,
        "name": "Laith",
        "surname": "Simmons",
        "age": 68,
        "salary": "£27888"
    }]
}
```

Caveat: I'm using a single very short set of real data, namely a list of 3 dictionaries. In a real case I would test the black box with many different use cases, to ensure I am not just checking some corner case.

The test is the following

``` python
import json

from datastats.datastats import DataStats


def test_json():
    test_data = [
        {
            "id": 1,
            "name": "Laith",
            "surname": "Simmons",
            "age": 68,
            "salary": "£27888"
        },
        {
            "id": 2,
            "name": "Mikayla",
            "surname": "Henry",
            "age": 49,
            "salary": "£67137"
        },
        {
            "id": 3,
            "name": "Garth",
            "surname": "Fields",
            "age": 70,
            "salary": "£70472"
        }
    ]

    ds = DataStats()

    assert ds.stats(test_data, 20, 20000) == json.dumps(
        {
            'avg_age': 62,
            'avg_salary': 55165,
            'avg_yearly_increase': 837,
            'max_salary': [{
                "id": 3,
                "name": "Garth",
                "surname": "Fields",
                "age": 70,
                "salary": "£70472"
            }],
            'min_salary': [{
                "id": 1,
                "name": "Laith",
                "surname": "Simmons",
                "age": 68,
                "salary": "£27888"
            }]
        }
    )
```

As said before, this test is obviously passing, having been artificially constructed from a real execution of the code.

Well, this test is very important! Now we know that if we change something inside the code, altering the behaviour of the class, at least one test will fail.

# Step 2 - Getting rid of the JSON format

Commit: [65e2997](https://github.com/lgiordani/datastats/commit/65e2997d71ade752633229186c6669803a46f185)

The method returns its output in JSON format, and looking at the class it is pretty evident that the conversion is done by `json.dumps()`.

The structure of the code is the following

``` python
class DataStats:

    def stats(self, data, iage, isalary):
        [code_part_1]

        return json.dumps({
            [code_part_2]
        })
```

Where obviously `code_part_2` depends on `code_part_1`. The first refactoring, then, will follow this procedure

1\. We write a test called `test__stats()` for a `_stats()` method that is supposed to return the data as a Python structure. We can infer the latter manually from the JSON or running `json.loads()` from a Python shell. The test fails.

2\. We **duplicate** the code of the `stats()` method that produces the data, putting it in the new `_stats()` method. The test passes.

``` python

class DataStats:

    def _stats(parameters):
        [code_part_1]

        return [code_part_2]

    def stats(self, data, iage, isalary):
        [code_part_1]

        return json.dumps({
            [code_part_2]
        })
```

3\. We remove the duplicated code in `stats()` replacing it with a call to `_stats()`

``` python

class DataStats:

    def _stats(parameters):
        [code_part_1]

        return [code_part_2]

    def stats(self, data, iage, isalary):
        return json.dumps(
            self._stats(data, iage, isalary)
        )
```

At this point we could refactor the initial test `test_json()` that we wrote, but this is an advanced consideration, and I'll leave it for some later notes.

So now the code of our class looks like this

``` python
class DataStats:

    def _stats(self, data, iage, isalary):
        # iage and isalary are the starting age and salary used to
        # compute the average yearly increase of salary.

        # Compute average yearly increase
        average_age_increase = math.floor(
            sum([e['age'] for e in data])/len(data)) - iage
        average_salary_increase = math.floor(
            sum([int(e['salary'][1:]) for e in data])/len(data)) - isalary

        yearly_avg_increase = math.floor(
            average_salary_increase/average_age_increase)

        # Compute max salary
        salaries = [int(e['salary'][1:]) for e in data]
        threshold = '£' + str(max(salaries))

        max_salary = [e for e in data if e['salary'] == threshold]

        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        min_salary = [e for e in data if e['salary'] ==
                      '£{}'.format(str(min(salaries)))]

        return {
            'avg_age': math.floor(sum([e['age'] for e in data])/len(data)),
            'avg_salary': math.floor(sum(
                [int(e['salary'][1:]) for e in data])/len(data)),
            'avg_yearly_increase': yearly_avg_increase,
            'max_salary': max_salary,
            'min_salary': min_salary
        }

    def stats(self, data, iage, isalary):
        return json.dumps(
            self._stats(data, iage, isalary)
        )
```

and we have two tests that check the correctness of it.

# Step 3 - Refactoring the tests

Commit: [d619017](https://github.com/lgiordani/datastats/commit/d61901754b83ccc36fa25bcebf88da7cace28ff2)

It is pretty clear that the `test_data` list of dictionaries is bound to be used in every test we will perform, so it is high time we moved that to a global variable. There is no point now in using a fixture, as the test data is just static data.

We could also move the output data to a global variable, but the upcoming tests are not using the whole output dictionary any more, so we can postpone the decision.

The test suite now looks like 

``` python
import json

from datastats.datastats import DataStats


test_data = [
    {
        "id": 1,
        "name": "Laith",
        "surname": "Simmons",
        "age": 68,
        "salary": "£27888"
    },
    {
        "id": 2,
        "name": "Mikayla",
        "surname": "Henry",
        "age": 49,
        "salary": "£67137"
    },
    {
        "id": 3,
        "name": "Garth",
        "surname": "Fields",
        "age": 70,
        "salary": "£70472"
    }
]


def test_json():

    ds = DataStats()

    assert ds.stats(test_data, 20, 20000) == json.dumps(
        {
            'avg_age': 62,
            'avg_salary': 55165,
            'avg_yearly_increase': 837,
            'max_salary': [{
                "id": 3,
                "name": "Garth",
                "surname": "Fields",
                "age": 70,
                "salary": "£70472"
            }],
            'min_salary': [{
                "id": 1,
                "name": "Laith",
                "surname": "Simmons",
                "age": 68,
                "salary": "£27888"
            }]
        }
    )


def test__stats():

    ds = DataStats()

    assert ds._stats(test_data, 20, 20000) == {
        'avg_age': 62,
        'avg_salary': 55165,
        'avg_yearly_increase': 837,
        'max_salary': [{
            "id": 3,
            "name": "Garth",
            "surname": "Fields",
            "age": 70,
            "salary": "£70472"
        }],
        'min_salary': [{
            "id": 1,
            "name": "Laith",
            "surname": "Simmons",
            "age": 68,
            "salary": "£27888"
        }]
    }
```

# Step 4 - Isolate the average age algorithm

Commit: [9db1803](https://github.com/lgiordani/datastats/commit/9db18036eee2f6712384195fcd970303387291f6)

Isolating independent features is a key target of software design. Thus, our refactoring shall aim to disentangle the code dividing it into small separated functions.

The output dictionary contains five keys, and each of them corresponds to a value computed either on the fly (for `avg_age` and `avg_salary`) or by the method's code (for `avg_yearly_increase`, `max_salary`, and `min_salary`). We can start replacing the code that computes the value of each key with dedicated methods, trying to isolate the algorithms.

To isolate some code, the first thing to do is to duplicate it, putting it into a dedicated method. As we are refactoring with tests, the first thing is to write a test for this method.

``` python
def test__avg_age():

    ds = DataStats()

    assert ds._avg_age(test_data) == 62
```

We know that the method's output shall be `62` as that is the value we have in the output data of the original `stats()` method. Please note that there is no need to pass `iage` and `isalary` as they are not used in the refactored code.

The test fails, so we can dutifully go and duplicate the code we use to compute `'avg_age'`

``` python
    def _avg_age(self, data):
        return math.floor(sum([e['age'] for e in data])/len(data))
```

and once the test passes we can replace the duplicated code in `_stats()` with a call to `_avg_age()`

``` python
        return {
            'avg_age': self._avg_age(data),
            'avg_salary': math.floor(sum(
                [int(e['salary'][1:]) for e in data])/len(data)),
            'avg_yearly_increase': yearly_avg_increase,
            'max_salary': max_salary,
            'min_salary': min_salary
        }
```

Checking after that that no test is failing. Well done! We isolated the first feature, and our refactoring produced already three tests.

# Step 5 - Isolate the average salary algorithm

Commit: [4122201](https://github.com/lgiordani/datastats/commit/412220145ea4d7ef846b1d1f289b4ddefc4fb24b)

The `avg_salary` key works exactly like the `avg_age`, with different code. Thus, the refactoring process is the same as before, and the result should be a new `test__avg_salary()` test

``` python
def test__avg_salary():

    ds = DataStats()

    assert ds._avg_salary(test_data) == 55165
```

a new `_avg_salary()` method

``` python
    def _avg_salary(self, data):
        return math.floor(sum([int(e['salary'][1:]) for e in data])/len(data))
```

and a new version of the final return value

``` python
        return {
            'avg_age': self._avg_age(data),
            'avg_salary': self._avg_salary(data),
            'avg_yearly_increase': yearly_avg_increase,
            'max_salary': max_salary,
            'min_salary': min_salary
        }
```

# Step 6 - Isolate the average yearly increase algorithm

Commit: [4005145](https://github.com/lgiordani/datastats/commit/4005145f39d36fda0519127d57e1b4099d24e72b)

The remaining three keys are computed with algorithms that, being longer than one line, couldn't be squeezed directly in the definition of the dictionary. The refactoring process, however, does not really change; as before, we first test a helper method, then we define it duplicating the code, and last we call the helper removing the code duplication.

For the average yearly increase of the salary we have a new test

``` python
def test__avg_yearly_increase():

    ds = DataStats()

    assert ds._avg_yearly_increase(test_data, 20, 20000) == 837
```

a new method that passes the test

``` python
    def _avg_yearly_increase(self, data, iage, isalary):
        # iage and isalary are the starting age and salary used to
        # compute the average yearly increase of salary.

        # Compute average yearly increase
        average_age_increase = math.floor(
            sum([e['age'] for e in data])/len(data)) - iage
        average_salary_increase = math.floor(
            sum([int(e['salary'][1:]) for e in data])/len(data)) - isalary

        return math.floor(average_salary_increase/average_age_increase)
```

and a new version of the `_stats()` method

``` python
    def _stats(self, data, iage, isalary):
        # Compute max salary
        salaries = [int(e['salary'][1:]) for e in data]
        threshold = '£' + str(max(salaries))

        max_salary = [e for e in data if e['salary'] == threshold]

        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        min_salary = [e for e in data if e['salary'] ==
                      '£{}'.format(str(min(salaries)))]

        return {
            'avg_age': self._avg_age(data),
            'avg_salary': self._avg_salary(data),
            'avg_yearly_increase': self._avg_yearly_increase(
                data, iage, isalary),
            'max_salary': max_salary,
            'min_salary': min_salary
        }
```

Please note that we are not solving any code duplication but the ones that we introduce to refactor. The first achievement we should aim to is to completely isolate independent features.

# Step 7 - Isolate max and min salary algorithms

Commit: [17b2413](https://github.com/lgiordani/datastats/commit/17b24138e712f9174b072a579a2dfc9e2800e6ac)

When refactoring we shall always do one thing at a time, but for the sake of conciseness, I'll show here the result of two refactoring steps at once. I'll recommend the reader to perform them as independent steps, as I did when I wrote the code that I am posting below.

The new tests are

``` python
def test__max_salary():

    ds = DataStats()

    assert ds._max_salary(test_data) == [{
        "id": 3,
        "name": "Garth",
        "surname": "Fields",
        "age": 70,
        "salary": "£70472"
    }]


def test__min_salary():

    ds = DataStats()

    assert ds._min_salary(test_data) == [{
        "id": 1,
        "name": "Laith",
        "surname": "Simmons",
        "age": 68,
        "salary": "£27888"
    }]
```

The new methods in the `DataStats` class are

``` python
    def _max_salary(self, data):
        # Compute max salary
        salaries = [int(e['salary'][1:]) for e in data]
        threshold = '£' + str(max(salaries))

        return [e for e in data if e['salary'] == threshold]

    def _min_salary(self, data):
        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        return [e for e in data if e['salary'] ==
                '£{}'.format(str(min(salaries)))]
```

and the `_stats()` method is now really tiny

``` python
    def _stats(self, data, iage, isalary):
        return {
            'avg_age': self._avg_age(data),
            'avg_salary': self._avg_salary(data),
            'avg_yearly_increase': self._avg_yearly_increase(
                data, iage, isalary),
            'max_salary': self._max_salary(data),
            'min_salary': self._min_salary(data)
        }
```

# Step 8 - Reducing code duplication

Commit: [b559a5c](https://github.com/lgiordani/datastats/commit/b559a5c91ef58e1e734ac97b676468d09a460a45)

Now that we have the main tests in place we can start changing the code of the various helper methods. These are now small enough to allow us to change the code without further tests. While this can be true in this case, however, in general there is no definition of what "small enough" means, as there is no real definition of what "unit test" is. Generally speaking you should be confident that the change that you are doing is covered by the tests that you have. Weren't this the case, you'd better add one or more tests until you feel confident enough.

The two methods `_max_salary()` and `_min_salary()` share a great deal of code, even though the second one is more concise

``` python
    def _max_salary(self, data):
        # Compute max salary
        salaries = [int(e['salary'][1:]) for e in data]
        threshold = '£' + str(max(salaries))

        return [e for e in data if e['salary'] == threshold]

    def _min_salary(self, data):
        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        return [e for e in data if e['salary'] ==
                '£{}'.format(str(min(salaries)))]

```

I'll start by making explicit the `threshold` variable in the second function. As soon as I change something, I'll run the tests to check that the external behaviour did not change.

``` python
    def _max_salary(self, data):
        # Compute max salary
        salaries = [int(e['salary'][1:]) for e in data]
        threshold = '£' + str(max(salaries))

        return [e for e in data if e['salary'] == threshold]

    def _min_salary(self, data):
        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        threshold = '£{}'.format(str(min(salaries)))

        return [e for e in data if e['salary'] == threshold]
```

Now, it is pretty evident that the two functions are the same but for the `min()` and `max()` functions. They still use different variable names and different code to format the threshold, so my first action is to even out them, copying the code of `_min_salary()` to `_max_salary()` and changing `min()` to `max()`

``` python
    def _max_salary(self, data):
        # Compute max salary
        salaries = [int(d['salary'][1:]) for d in data]
        threshold = '£{}'.format(str(max(salaries)))

        return [e for e in data if e['salary'] == threshold]

    def _min_salary(self, data):
        # Compute min salary
        salaries = [int(d['salary'][1:]) for d in data]
        threshold = '£{}'.format(str(min(salaries)))

        return [e for e in data if e['salary'] == threshold]
```

Now I can create another helper called `_select_salary()` that duplicates that code and accepts a function, used instead of `min()` or `max()`. As I did before, first I duplicate the code, and then remove the duplication by calling the new function.

After some passages, the code looks like this

``` python
    def _select_salary(self, data, func):
        salaries = [int(d['salary'][1:]) for d in data]
        threshold = '£{}'.format(str(func(salaries)))

        return [e for e in data if e['salary'] == threshold]

    def _max_salary(self, data):
        return self._select_salary(data, max)

    def _min_salary(self, data):
        return self._select_salary(data, min)
```

I noticed then a code duplication between `_avg_salary()` and `_select_salary()`

``` python
    def _avg_salary(self, data):
        return math.floor(sum([int(e['salary'][1:]) for e in data])/len(data))
```

``` python
    def _select_salary(self, data, func):
        salaries = [int(d['salary'][1:]) for d in data]
```

and decided to extract the common algorithm in a method called `_salaries()`. As before, I write the test first

``` python
def test_salaries():

    ds = DataStats()

    assert ds._salaries(test_data) == [27888, 67137, 70472]
```

then I implement the method

``` python
    def _salaries(self, data):
        return [int(d['salary'][1:]) for d in data]
```

and eventually I replace the duplicated code with a call to the new method

``` python
    def _salaries(self, data):
        return [int(d['salary'][1:]) for d in data]
```

``` python
    def _select_salary(self, data, func):
        threshold = '£{}'.format(str(func(self._salaries(data))))

        return [e for e in data if e['salary'] == threshold]
```

While doing this I noticed that `_avg_yearly_increase()` contains the same code, and fix it there as well.

``` python
    def _avg_yearly_increase(self, data, iage, isalary):
        # iage and isalary are the starting age and salary used to
        # compute the average yearly increase of salary.

        # Compute average yearly increase
        average_age_increase = math.floor(
            sum([e['age'] for e in data])/len(data)) - iage
        average_salary_increase = math.floor(
            sum(self._salaries(data))/len(data)) - isalary

        return math.floor(average_salary_increase/average_age_increase)
```

It would be useful at this point to store the input data inside the class and to use it as `self.data` instead of passing it around to all the class's methods. This however would break the class's API, as `DataStats` is currently initialised without any data. Later I will show how to introduce changes that potentially break the API, and briefly discuss the issue. For the moment, however, I'll keep changing the class without modifying the external interface.

It looks like `age` has the same code duplication issues as `salary`, so with the same procedure I introduce the `_ages()` method and change the `_avg_age()` and `_avg_yearly_increase()` methods accordingly.

Speaking of `_avg_yearly_increase()`, the code of that method contains the code of the `_avg_age()` and `_avg_salary()` methods, so it is worth replacing it with two calls. As I am moving code between existing methods, I do not need further tests.

``` python
    def _avg_yearly_increase(self, data, iage, isalary):
        # iage and isalary are the starting age and salary used to
        # compute the average yearly increase of salary.

        # Compute average yearly increase
        average_age_increase = self._avg_age(data) - iage
        average_salary_increase = self._avg_salary(data) - isalary

        return math.floor(average_salary_increase/average_age_increase)
```

# Step 9 - Advanced refactoring

Commit: [cc0b0a1](https://github.com/lgiordani/datastats/commit/cc0b0a105ebc882cb73831b177e881bb65f4b491)

The initial class didn't have any `__init__()` method, and was thus missing the encapsulation part of the object-oriented paradigm. There was no reason to keep the class, as the `stats()` method could have easily been extracted and provided as a plain function.

This is much more evident now that we refactored the method, because we have 10 methods that accept `data` as a parameter. I would be nice to load the input data into the class at instantiation time, and then access it as `self.data`. This would greatly improve the readability of the class, and also justify its existence.

If we introduce a `__init__()` method that requires a parameter, however, we will change the class's API, breaking the compatibility with the code that imports and uses it. Since we want to keep it, we have to devise a way to provide both the advantages of a new, clean class and of a stable API. This is not always perfectly achievable, but in this case the [Adapter design pattern](https://en.wikipedia.org/wiki/Adapter_pattern) (also known as Wrapper) can perfectly solve the issue.

The goal is to change the current class to match the new API, and then build a class that wraps the first one and provides the old API. The strategy is not that different from what we did previously, only this time we will deal with classes instead of methods. With a stupendous effort of my imagination I named the new class `NewDataStats`. Sorry, sometimes you just have to get the job done.

The first things, as happens very often with refactoring, is to duplicate the code, and when we insert new code we need to have tests that justify it. The tests will be the same as before, as the new class shall provide the same functionalities as the previous one, so I just create a new file, called `test_newdatastats.py` and start putting there the first test `test_init()`.

``` python
import json

from datastats.datastats import NewDataStats


test_data = [
    {
        "id": 1,
        "name": "Laith",
        "surname": "Simmons",
        "age": 68,
        "salary": "£27888"
    },
    {
        "id": 2,
        "name": "Mikayla",
        "surname": "Henry",
        "age": 49,
        "salary": "£67137"
    },
    {
        "id": 3,
        "name": "Garth",
        "surname": "Fields",
        "age": 70,
        "salary": "£70472"
    }
]


def test_init():

    ds = NewDataStats(test_data)

    assert ds.data == test_data
```

This test doesn't pass, and the code that implements the class is very simple

``` python
class NewDataStats:

    def __init__(self, data):
        self.data = data
```

Now I can start an iterative process:

1. I will copy one of the tests of `DataStats` and adapt it to `NewDataStats`
2. I will copy some code from `DataStats` to `NewDataStats`, adapting it to the new API and making it pass the test.

At this point iteratively removing methods from `DataStats` and replacing them with a call to `NewDataStats` would be overkill. I'll show you in the next section why, and what we can do to avoid  that.

An example of the resulting tests for `NewDataStats` is the following

``` python
def test_ages():

    ds = NewDataStats(test_data)

    assert ds._ages() == [68, 49, 70]
```

and the code that passes the test is

``` python
    def _ages(self):
        return [d['age'] for d in self.data]
```

Once finished, I noticed that, as now methods like `_ages()` do not require an input parameter any more, I can convert them to properties, changing the tests accordingly.

``` python
    @property
    def _ages(self):
        return [d['age'] for d in self.data]
```

It is time to replace the methods of `DataStats` with calls to `NewDataStats`. We could do it method by method, but actually the only thing that we really need is to replace `stats()`. So the new code is

``` python
class DataStats:

    def stats(self, data, iage, isalary):
        nds = NewDataStats(data)
        return nds.stats(iage, isalary)
```

And since all the other methods are not used any more we can safely delete them, checking that the tests do not fail. Speaking of tests, removing methods will make many tests of `DataStats` fail, so we need to remove them.

# Step 10 - Still room for improvement

As refactoring is an iterative process it will often happen that you think you did everything was possible, just to spot later that you missed something. In this case the missing step was spotted by Harun Yasar, who noticed another small code duplication.

The two functions

``` python
    def _avg_salary(self):
        return math.floor(sum(self._salaries)/len(self.data))

    def _avg_age(self):
        return math.floor(sum(self._ages)/len(self.data))
```

share the same logic, so we can definitely isolate that and call the common code in each function

``` python
    def _floor_avg(self, sum_of_numbers):
        return math.floor(sum_of_numbers / len(self.data))
    
    def _avg_salary(self):
        return self._floor_avg(sum(self._salaries))
    
    def _avg_age(self):
        return self._floor_avg(sum(self._ages))
```

which passes all the tests and is thus correct.

Whenever I get corrected by someone who read one of my posts and just learned something new I feel so happy, because it means that the message is clear!

# Final words

I hope this little tour of a refactoring session didn't result too trivial, and helped you to grasp the basic concepts of this technique. If you are interested in the subject I'd strongly recommend the classic book by Martin Fowler "Refactoring: Improving the Design of Existing Code", which is a collection of refactoring patterns. The reference language is Java, but the concepts are easily adapted to Python.

# Updates

2017-07-28: [delirious-lettuce](https://github.com/delirious-lettuce) and [Matt Beck](https://github.com/superbeckgit) did a very serious proofread and spotted many typos. Thank you both for reading the post and for taking the time to submit the issues!

2020-02-15: [Harun Yasar](https://github.com/harunyasar) spotted a missing refactoring in two functions. Thanks!

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
