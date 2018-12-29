Title: Clean architectures in Python: a step-by-step example
Date: 2016-11-14 19:00:00 +0000
Modified: 2018-12-29 08:50:00 +0000
Category: Programming
Tags: OOP, Python, Python2, Python3, TDD, architectures
Authors: Leonardo Giordani
Slug: clean-architectures-in-python-a-step-by-step-example
Image: clean-architectures
Summary: 

**Update**: an expanded version of this post may be found in "Clean Architectures in Python", a book I published on LeanPub. The book features 3 chapters on TDD with pytest, mocks and unit testing in general, and 4 chapters on clean architectures. Those chapters are very similar to this post, but they include a discussion and examples of integration between the architecture and real-world databases. The book is free and can be downloaded [here](https://leanpub.com/clean-architectures-in-python).

In 2015 I was introduced by my friend [Roberto Ciatti](https://github.com/gekorob) to the concept of Clean Architecture, as it is called by Robert Martin. The well-known Uncle Bob talks a lot about this concept at conferences and wrote some very interesting posts about it. What he calls "Clean Architecture" is a way of structuring a software system, a set of consideration (more than strict rules) about the different layers and the role of the actors in it.

As he clearly states in a post aptly titled [The Clean Architecture](https://blog.8thlight.com/uncle-bob/2012/08/13/the-clean-architecture.html), the idea behind this design is not new, being built on a set of concepts that have been pushed by many software engineers over the last 3 decades. One of the first implementations may be found in the Boundary-Control-Entity model proposed by Ivar Jacobson in his masterpiece "Object-Oriented Software Engineering: A Use Case Driven Approach" published in 1992, but Martin lists other more recent versions of this architecture.

I will not repeat here what he had already explained better than I can do, so I will just point out some resources you may check to start exploring these concepts:

* [The Clean Architecture](https://blog.8thlight.com/uncle-bob/2012/08/13/the-clean-architecture.html) a post by Robert Martin that concisely describes the goals of the architecture. It also lists resources that describe similar architectures.
* [The Open Closed Principle](https://blog.8thlight.com/uncle-bob/2014/05/12/TheOpenClosedPrinciple.html) a post by Robert Martin not strictly correlated with the Clean Architecture concept but important for the separation concept.
* Hakka Labs: Robert "Uncle Bob" Martin - [Architecture: The Lost Years](https://www.youtube.com/watch?v=HhNIttd87xs) a video of Robert Martin from Hakka Labs.
* [DDD & Testing Strategy](http://www.taimila.com/blog/ddd-and-testing-strategy/) by Lauri Taimila
* (upcoming) [Clean Architecture](https://www.amazon.co.uk/Clean-Architecture-Robert-C-Martin-x/dp/0134494164/ref=la_B000APG87E_1_3?s=books&ie=UTF8&qid=1479146201&sr=1-3) by Robert Martin, published by Prentice Hall.

The purpose of this post is to show how to build a web service in Python from scratch using a clean architecture. One of the main advantages of this layered design is testability, so I will develop it following a TDD approach. The project was initially developed from scratch in around 3 hours of work. Given the toy nature of the project some choices have been made to simplify the resulting code. Whenever meaningful I will point out those simplifications and discuss them.

If you want to know more about TDD in Python read the posts in [this category](/categories/tdd/).
  
# Project overview

The goal of the "Rent-o-matic" project (fans of Day of the Tentacle may get the reference) is to create a simple search engine on top of a dataset of objects which are described by some quantities. The search engine shall allow to set some filters to narrow the search.
 
The objects in the dataset are storage rooms for rent described by the following quantities:
 
* An unique identifier
* A size in square meters
* A renting price in Euro/day
* Latitude and longitude

As pushed by the clean architecture model, we are interested in separating the different layers of the system. The architecture is described by four layers, which however can be implemented by more than four actual code modules. I will give here a brief description of those layers.

## Entities

This is the level in which the domain models are described. Since we work in Python, I will put here the class that represent my storage rooms, with the data contained in the database, and whichever data I think is useful to perform the core business processing.

It is very important to understand that the models in this layer are different from the usual models of framework like Django. These models are not connected with a storage system, so they cannot be directly saved or queried using methods of their classes. They may however contain helper methods that implement code related to the business rules.
  
## Use cases

This layer contains the use cases implemented by the system. In this simple example there will be only one use case, which is the list of storage rooms according to the given filters. Here you would put for example a use case that shows the detail of a given storage room or every business process you want to implement, such as booking a storage room, filling it with goods, etc.

## Interface Adapters

This layer corresponds to the boundary between the business logic and external systems and implements the APIs used to exchange data with them. Both the storage system and the user interface are external systems that need to exchange data with the use cases and this layer shall provide an interface for this data flow. In this project the presentation part of this layer is provided by a JSON serializer, on top of which an external web service may be built. The storage adapter shall define here the common API of the storage systems. 

## External interfaces

This part of the architecture is made by external systems that implement the interfaces defined in the previous layer. Here for example you will find a web server that implements (REST) entry points, which access the data provided by use cases through the JSON serializer. You will also find here the storage system implementation, for example a given database such as MongoDB.

# API and shades of grey

The word API is of uttermost importance in a clean architecture. Every layer may be accessed by an API, that is a fixed collection of entry points (methods or objects). Here "fixed" means "the same among every implementation", obviously an API may change with time. Every presentation tool, for example, will access the same use cases, and the same methods, to obtain a set of domain models, which are the output of that particular use case. It is up to the presentation layer to format data according to the specific presentation media, for example HTML, PDF, images, etc. If you understand plugin-based architectures you already grasped the main concept of a separate, API-driven component (or layer).

The same concept is valid for the storage layer. Every storage implementation shall provide the same methods. When dealing with use cases you shall not be concerned with the actual system that stores data, it may be a MongoDB local installation, a cloud storage system or a trivial in-memory dictionary.
   
The separation between layers, and the content of each layer, is not always fixed and immutable. A well-designed system shall also cope with practical world issues such as performances, for example, or other specific needs. When designing an architecture it is very important to know "what is where and why", and this is even more important when you "bend" the rules. Many issues do not have a black-or-white answer, and many decisions are "shades of grey", that is it is up to you to justify why you put something in a given place.

Keep in mind however, that you should not break the _structure_ of the clean architecture, in particular you shall be inflexible about the data flow (see the "Crossing boundaries" section in the original post of Robert Martin). If you break the data flow, you are basically invalidating the whole structure. Let me stress it again: **never break the data flow**. A simple example of breaking the data flow is to let a use case output a Python class instead of a _representation_ of that class such as a JSON string.

# Project structure

Let us take a look at the final structure of the project

<structure here>

The global structure of the package has been built with Cookiecutter, and I will run quickly through that part. The `rentomatic` directory contains the following subdirectories: `domain`, `repositories`, `REST`, `serializers`, `use_cases`. Those directories reflect the layered structure introduced in the previous section, and the structure of the `tests` directory mirrors this structure so that tests are easily found.

# Source code

You can find the source code in [this GitHub repository](https://github.com/lgiordani/rentomatic). Feel free to fork it and experiment, change it, and find better solutions to the problem I will discuss in this post. The source code contains tagged commits to allow you to follow the actual development as presented in the post. You can find the current tag in the **Git tag: `<tag name>`** label under the section titles. The label is actually a link to the tagged commit on GitHub, if you want to see the code without cloning it.

# Project initialization

#### **Git tag: [step01](https://github.com/lgiordani/rentomatic/tree/step01)**

_Update_: [this](https://github.com/ardydedase/cookiecutter-pypackage) Cookiecutter package creates an environment like the one I am creating in this section. I will keep the following explanation so that you can see how to manage requirements and configurations, but for your next project consider using this automated tool.

I usually like maintaining a Python virtual environment inside the project, so I will create a temporary virtualenv to install cookiecutter, create the project, and remove the virtualenv. Cookiecutter is going to ask you some questions about you and the project, to provide an initial file structure. We are going to build our own testing environment, so it is safe to answer no to `use_pytest`. Since this is a demo project we are not going to need any publishing feature, so you can answer no to `use_pypi_deployment_with_travis` as well. The project does not have a command line interface, and you can safely create the author file and use any license.

``` bash
virtualenv venv3 -p python3
source venv3/bin/activate
pip install cookiecutter
cookiecutter https://github.com/audreyr/cookiecutter-pypackage
```

Now answer the questions, then finish creating the project with the following code

``` bash
deactivate
rm -fR venv3
cd rentomatic
virtualenv venv3 -p python3
source venv3/bin/activate
```

Get rid of the `requirements_dev.txt` file that Cookiecutter created for you. I usually store virtualenv requirements in different hierarchical files to separate production, development and testing environments, so create the `requirements` directory and the relative files

``` bash
mkdir requirements
touch requirements/prod.txt
touch requirements/dev.txt
touch requirements/test.txt
```

The `test.txt` file will contain specific packages used to test the project. Since to test the project you also need to install the packages for the production environment the file will first include the production one.

``` text
-r prod.txt

pytest
tox
coverage
pytest-cov
```

The `dev.txt` file will contain packages used during the development process and shall install also test and production package

``` text
-r test.txt

pip
wheel
flake8
Sphinx
```

(taking advantage of the fact that `test.txt` already includes `prod.txt`).

Last, the main `requirements.txt` file of the project will just import `requirements/prod.txt`

``` text
-r prod.txt
```

Obviously you are free to find the project structure that better suits your need or preferences. This is the structure we are going to use in this project but nothing forces you to follow it in your personal projects.

This separation allows you to install a full-fledged development environment on your machine, while installing only testing tools in a testing environment like the Travis platform and to further reduce the amount of dependencies in the production case. 

As you can see, I am not using version tags in the requirements files. This is because this project is not going to be run in a production environment, so we do not need to freeze the environment.

Remember at this point to install the development requirements in your virtualenv

``` sh
$ pip install -r requirements/dev.txt
```

## Miscellaneous configuration

The `pytest` testing library needs to be configured. This is the `pytest.ini` file that you can create in the root directory (where the `setup.py` file is located)

``` text
[pytest]
minversion = 2.0
norecursedirs = .git .tox venv* requirements*
python_files = test*.py
```

To run the tests during the development of the project just execute

``` sh
$ py.test -sv
```

If you want to check the coverage, i.e. the amount of code which is run by your tests or "covered", execute

``` sh
$ py.test --cov-report term-missing --cov=rentomatic
```

If you want to know more about test coverage check the official documentation of the [Coverage.py](http://coverage.readthedocs.io/en/latest/) and the [pytest-cov](http://pytest-cov.readthedocs.io/en/latest/index.html) packages.

I strongly suggest the use of the `flake8` package to check that your Python code is PEP8 compliant. This is the `flake8` configuration that you can put in your `setup.cfg` file

``` text
[flake8]
ignore = D203
exclude = .git, venv*, docs
max-complexity = 10
```

To check the compliance of your code with the PEP8 standard execute

``` sh
$ flake8
```

Flake8 documentation is available [here](http://flake8.pycqa.org/en/latest/).

Note that every step in this post produces tested code and a of coverage of 100%. One of the benefits of a clean architecture is the separation between layers, and this guarantees a great degree of testability. Note however that in this tutorial, in particular in the REST sections, some tests have been omitted in favour of a simpler description of the architecture. 

# Domain models

**Git tag: [step02](https://github.com/lgiordani/rentomatic/tree/step02)**

Let us start with a simple definition of the `StorageRoom` model. As said before, the clean architecture models are very lightweight, or at least they are lighter than their counterparts in a framework.

Following the TDD methodology the first thing that I write are the tests. Create the `tests/domain/test_storageroom.py` and put this code inside it

``` python
import uuid
from rentomatic.domain.storageroom import StorageRoom


def test_storageroom_model_init():
    code = uuid.uuid4()
    storageroom = StorageRoom(code, size=200, price=10,
                              longitude=-0.09998975,
                              latitude=51.75436293)
    assert storageroom.code == code
    assert storageroom.size == 200
    assert storageroom.price == 10
    assert storageroom.longitude == -0.09998975
    assert storageroom.latitude == 51.75436293


def test_storageroom_model_from_dict():
    code = uuid.uuid4()
    storageroom = StorageRoom.from_dict(
        {
            'code': code,
            'size': 200,
            'price': 10,
            'longitude': -0.09998975,
            'latitude': 51.75436293
        }
    )
    assert storageroom.code == code
    assert storageroom.size == 200
    assert storageroom.price == 10
    assert storageroom.longitude == -0.09998975
    assert storageroom.latitude == 51.75436293
```

With these two tests we ensure that our model can be initialized with the correct values and that can be created from a dictionary. In this first version all the parameters of the model are required. Later we could want to make some of them optional, and in that case we will have to add the relevant tests.

Now let's write the `StorageRoom` class in the `rentomatic/domain/storageroom.py` file. Do not forget to create the `__init__.py` file in the subdirectories of the project, otherwise Python will not be able to import the modules.

``` python
from rentomatic.shared.domain_model import DomainModel


class StorageRoom(object):

    def __init__(self, code, size, price, latitude, longitude):
        self.code = code
        self.size = size
        self.price = price
        self.latitude = latitude
        self.longitude = longitude

    @classmethod
    def from_dict(cls, adict):
        room = StorageRoom(
            code=adict['code'],
            size=adict['size'],
            price=adict['price'],
            latitude=adict['latitude'],
            longitude=adict['longitude'],
        )

        return room


DomainModel.register(StorageRoom)
```

The model is very simple, and requires no further explanation. One of the benefits of a clean architecture is that each layer contains small pieces of code that, being isolated, shall perform simple tasks. In this case the model provides an initialization API and stores the information inside the class.

The `from_dict` method comes in handy when we have to create a model from data coming from another layer (such as the database layer or the query string of the REST layer).

One could be tempted to try to simplify the `from_dict` function, abstracting it and providing it through a `Model` class. Given that a certain level of abstraction and generalization is possible and desirable, the initialization part of the models shall probably deal with various different cases, and thus is better off being implemented directly in the class.

The `DomainModel` abstract base class is an easy way to categorize the model for future uses like checking if a class is a model in the system. For more information about this use of Abstract Base Classes in Python see [this post](/blog/2016/04/03/abstract-base-classes-in-python/).

Since we have a method creates an object form a dictionary it is useful to have a method that returns a dictionary version of the object. This allows us to easily write a comparison operator between objects, that we will use later in some tests.

The new tests in `tests/domain/test_storageroom.py` are

``` python
def test_storageroom_model_to_dict():
    storageroom_dict = {
        'code': uuid.uuid4(),
        'size': 200,
        'price': 10,
        'longitude': -0.09998975,
        'latitude': 51.75436293
    }

    storageroom = StorageRoom.from_dict(storageroom_dict)

    assert storageroom.to_dict() == storageroom_dict


def test_storageroom_model_comparison():
    storageroom_dict = {
        'code': uuid.uuid4(),
        'size': 200,
        'price': 10,
        'longitude': -0.09998975,
        'latitude': 51.75436293
    }
    storageroom1 = StorageRoom.from_dict(storageroom_dict)
    storageroom2 = StorageRoom.from_dict(storageroom_dict)

    assert storageroom1 == storageroom2
```

and the new methods of the object in `rentomatic/domain/storageroom.py` are

``` python
    def to_dict(self):
        return {
            'code': self.code,
            'size': self.size,
            'price': self.price,
            'latitude': self.latitude,
            'longitude': self.longitude,
        }

    def __eq__(self, other):
        return self.to_dict() == other.to_dict()
```

# Serializers

**Git tag: [step03](https://github.com/lgiordani/rentomatic/tree/step03)**

Our model needs to be serialized if we want to return it as a result of an API call. The typical serialization format is JSON, as this is a broadly accepted standard for web-based API. The serializer is not part of the model, but is an external specialized class that receives the model instance and produces a representation of its structure and values.

To test the JSON serialization of our `StorageRoom` class put in the `tests/serializers/test_storageroom_serializer.py` file the following code

``` python
import datetime
import json
import uuid

import pytest

from rentomatic.serializers import storageroom_serializer as srs
from rentomatic.domain.storageroom import StorageRoom


def test_serialize_domain_storageroom():
    code = uuid.uuid4()

    room = StorageRoom(
        code=code,
        size=200,
        price=10,
        longitude=-0.09998975,
        latitude=51.75436293
    )

    expected_json = """
        {{
            "code": "{}",
            "size": 200,
            "price": 10,
            "longitude": -0.09998975,
            "latitude": 51.75436293
        }}
    """.format(code)

    json_storageroom = json.dumps(room, cls=srs.StorageRoomEncoder)

    assert json.loads(json_storageroom) == json.loads(expected_json)


def test_serialize_domain_storageruum_wrong_type():
    with pytest.raises(TypeError):
        json.dumps(datetime.datetime.now(), cls=srs.StorageRoomEncoder)
```

Put in the `rentomatic/serializers/storageroom_serializer.py` file the code that makes the test pass

``` python
import json


class StorageRoomEncoder(json.JSONEncoder):

    def default(self, o):
        try:
            to_serialize = {
                'code': str(o.code),
                'size': o.size,
                'price': o.price,
                "latitude": o.latitude,
                "longitude": o.longitude,
            }
            return to_serialize
        except AttributeError:
            return super().default(o)

```

Providing a class that inherits from `json.JSONEncoder` let us use the `json.dumps(room, cls=StorageRoomEncoder)` syntax to serialize the model.

There is a certain degree of repetition in the code we wrote, and this is the annoying part of a clean architecture. Since we want to isolate layers as much as possible and create lightweight classes we end up somehow repeating certain types of actions. For example the serialization code that assigns attributes of a `StorageRoom` to JSON attributes is very similar to that we use to create the object from a dictionary. Not exactly the same, obviously, but the two functions are very close.

# Use cases (part 1)

**Git tag: [step04](https://github.com/lgiordani/rentomatic/tree/step04)**

It's time to implement the actual business logic our application wants to expose to the outside world. Use cases are the place where we implement classes that query the repository, apply business rules, logic, and whatever transformation we need for our data, and return the results.

With those requirements in mind, let us start to build a use case step by step. The simplest use case we can create is one that fetches all the storage rooms from the repository and returns them. Please note that we did not implement any repository layer yet, so our tests will mock it.

This is the skeleton for a basic test of a use case that lists all the storage rooms. Put this code in the `tests/use_cases/test_storageroom_list_use_case.py`

``` python
import uuid

import pytest
from unittest import mock

from rentomatic.domain.storageroom import StorageRoom
from rentomatic.use_cases import storageroom_use_cases as uc


@pytest.fixture
def domain_storagerooms():
    storageroom_1 = StorageRoom(
        code=uuid.uuid4(),
        size=215,
        price=39,
        longitude=-0.09998975,
        latitude=51.75436293,
    )

    storageroom_2 = StorageRoom(
        code=uuid.uuid4(),
        size=405,
        price=66,
        longitude=0.18228006,
        latitude=51.74640997,
    )

    storageroom_3 = StorageRoom(
        code=uuid.uuid4(),
        size=56,
        price=60,
        longitude=0.27891577,
        latitude=51.45994069,
    )

    storageroom_4 = StorageRoom(
        code=uuid.uuid4(),
        size=93,
        price=48,
        longitude=0.33894476,
        latitude=51.39916678,
    )

    return [storageroom_1, storageroom_2, storageroom_3, storageroom_4]


def test_storageroom_list_without_parameters(domain_storagerooms):
    repo = mock.Mock()
    repo.list.return_value = domain_storagerooms

    storageroom_list_use_case = uc.StorageRoomListUseCase(repo)
    result = storageroom_list_use_case.execute()

    repo.list.assert_called_with()
    assert result == domain_storagerooms
```

The test is straightforward. First we mock the repository so that is provides a `list()` method that returns the list of models we created above the test. Then we initialize the use case with the repo and execute it, collecting the result. The first thing we check is if the repository method was called without any parameter, and the second is the effective correctness of the result.

This is the implementation of the use case that makes the test pass. Put the code in the `rentomatic/use_cases/storageroom_use_case.py`

``` python
class StorageRoomListUseCase(object):

    def __init__(self, repo):
        self.repo = repo

    def execute(self):
        return self.repo.list()
```

With such an implementation of the use case, however, we will soon experience issues. For starters, we do not have a standard way to transport the call parameters, which means that we do not have a standard way to check for their correctness either. The second problem is that we miss a standard way to return the call results and consequently we lack a way to communicate if the call was successful of if it failed, and in the latter case what are the reasons of the failure. This applies also to the case of bad parameters discussed in the previous point.

We want thus to introduce some structures to wrap input and outputs of our use cases. Those structures are called request and response objects.

# Requests and responses

**Git tag: [step05](https://github.com/lgiordani/rentomatic/tree/step05)**

Request and response objects are an important part of a clean architecture, as they transport call parameters, inputs and results from outside the application into the use cases layer.

More specifically, requests are objects created from incoming API calls, thus they shall deal with things like incorrect values, missing parameters, wrong formats, etc. Responses, on the other hand, have to contain the actual results of the API calls, but shall also be able to represent error cases and to deliver rich information on what happened.

The actual implementation of request and response objects is completely free, the clean architecture says nothing about them. The decision on how to pack and represent data is up to us.

For the moment we just need a `StorageRoomListRequestObject` that can be initialized without parameters, so let us create the file `tests/use_cases/test_storageroom_list_request_objects.py` and put there a test for this object.

``` python
from rentomatic.use_cases import request_objects as ro


def test_build_storageroom_list_request_object_without_parameters():
    req = ro.StorageRoomListRequestObject()

    assert bool(req) is True


def test_build_file_list_request_object_from_empty_dict():
    req = ro.StorageRoomListRequestObject.from_dict({})

    assert bool(req) is True
```

While at the moment this request object is basically empty, it will come in handy as soon as we start having parameters for the list use case. The code of the `StorageRoomListRequestObject` is the following and goes into the `rentomatic/use_cases/request_objects.py` file

``` python
class StorageRoomListRequestObject(object):
    @classmethod
    def from_dict(cls, adict):
        return StorageRoomListRequestObject()

    def __nonzero__(self):
        return True
```

The response object is also very simple, since for the moment we just need a successful response. Unlike the request, the response is not linked to any particular use case, so the test file can be named `tests/shared/test_response_object.py`

``` python
from rentomatic.shared import response_object as ro


def test_response_success_is_true():
    assert bool(ro.ResponseSuccess()) is True
```

and the actual response object is in the file `rentomatic/shared/response_object.py`

``` python
class ResponseSuccess(object):

    def __init__(self, value=None):
        self.value = value

    def __nonzero__(self):
        return True

    __bool__ = __nonzero__
```

# Use cases (part 2)

**Git tag: [step06](https://github.com/lgiordani/rentomatic/tree/step06)**

Now that we have implemented the request and response object we can change the test code to include those structures. Change the `tests/use_cases/test_storageroom_list_use_case.py` to contain this code

``` python
import uuid

import pytest
from unittest import mock

from rentomatic.domain.storageroom import StorageRoom
from rentomatic.use_cases import request_objects as ro
from rentomatic.use_cases import storageroom_use_cases as uc


@pytest.fixture
def domain_storagerooms():
    storageroom_1 = StorageRoom(
        code=uuid.uuid4(),
        size=215,
        price=39,
        longitude=-0.09998975,
        latitude=51.75436293,
    )

    storageroom_2 = StorageRoom(
        code=uuid.uuid4(),
        size=405,
        price=66,
        longitude=0.18228006,
        latitude=51.74640997,
    )

    storageroom_3 = StorageRoom(
        code=uuid.uuid4(),
        size=56,
        price=60,
        longitude=0.27891577,
        latitude=51.45994069,
    )

    storageroom_4 = StorageRoom(
        code=uuid.uuid4(),
        size=93,
        price=48,
        longitude=0.33894476,
        latitude=51.39916678,
    )

    return [storageroom_1, storageroom_2, storageroom_3, storageroom_4]


def test_storageroom_list_without_parameters(domain_storagerooms):
    repo = mock.Mock()
    repo.list.return_value = domain_storagerooms

    storageroom_list_use_case = uc.StorageRoomListUseCase(repo)
    request_object = ro.StorageRoomListRequestObject.from_dict({})

    response_object = storageroom_list_use_case.execute(request_object)

    assert bool(response_object) is True
    repo.list.assert_called_with()

    assert response_object.value == domain_storagerooms

```

The new version of the `rentomatic/use_case/storageroom_use_cases.py` file is the following

``` python
from rentomatic.shared import response_object as ro


class StorageRoomListUseCase(object):

    def __init__(self, repo):
        self.repo = repo

    def execute(self, request_object):
        storage_rooms = self.repo.list()
        return ro.ResponseSuccess(storage_rooms)
```

Let us consider what we have achieved with our clean architecture up to this point. We have a very lightweight model that can be serialized to JSON and which is completely independent from other parts of the system. The code also contains a use case that, given a repository that exposes a given API, extracts all the models and returns them contained in a structured object.

We are missing some objects, however. For example, we have not implemented any unsuccessful response object or validated the incoming request object.

To explore these missing parts of the architecture let us improve the current use case to accept a `filters` parameter that represents some filters that we want to apply to the extracted list of models. This will generate some possible error conditions for the input, forcing us to introduce some validation for the incoming request object.

# Requests and validation

**Git tag: [step07](https://github.com/lgiordani/rentomatic/tree/step07)**

I want to add a `filters` parameter to the request. Through that parameter the caller can add different filters by specifying a name and a value for each filter (for instance `{'price_lt': 100}` to get all results with a price lesser than 100).

The first thing to do is to change the request object, starting from the test. The new version of the `tests/use_cases/test_storageroom_list_request_objects.py` file is the following

``` python
from rentomatic.use_cases import request_objects as ro


def test_build_storageroom_list_request_object_without_parameters():
    req = ro.StorageRoomListRequestObject()

    assert req.filters is None
    assert bool(req) is True


def test_build_file_list_request_object_from_empty_dict():
    req = ro.StorageRoomListRequestObject.from_dict({})

    assert req.filters is None
    assert bool(req) is True


def test_build_storageroom_list_request_object_with_empty_filters():
    req = ro.StorageRoomListRequestObject(filters={})

    assert req.filters == {}
    assert bool(req) is True


def test_build_storageroom_list_request_object_from_dict_with_empty_filters():
    req = ro.StorageRoomListRequestObject.from_dict({'filters': {}})

    assert req.filters == {}
    assert bool(req) is True


def test_build_storageroom_list_request_object_with_filters():
    req = ro.StorageRoomListRequestObject(filters={'a': 1, 'b': 2})

    assert req.filters == {'a': 1, 'b': 2}
    assert bool(req) is True


def test_build_storageroom_list_request_object_from_dict_with_filters():
    req = ro.StorageRoomListRequestObject.from_dict({'filters': {'a': 1, 'b': 2}})

    assert req.filters == {'a': 1, 'b': 2}
    assert bool(req) is True


def test_build_storageroom_list_request_object_from_dict_with_invalid_filters():
    req = ro.StorageRoomListRequestObject.from_dict({'filters': 5})

    assert req.has_errors()
    assert req.errors[0]['parameter'] == 'filters'
    assert bool(req) is False
```

As you can see I added the `assert req.filters is None` check to the original two tests, then I added 5 tests to check if filters can be specified and to test the behaviour of the object with an invalid filter parameter.

To make the tests pass we have to change our `StorageRoomListRequestObject` class. There are obviously multiple possible solutions that you can come up with, and I recommend you to try to find your own. This is the one I usually employ. The file `rentomatic/use_cases/request_object.py` becomes

``` python
import collections


class InvalidRequestObject(object):

    def __init__(self):
        self.errors = []

    def add_error(self, parameter, message):
        self.errors.append({'parameter': parameter, 'message': message})

    def has_errors(self):
        return len(self.errors) > 0

    def __nonzero__(self):
        return False

    __bool__ = __nonzero__


class ValidRequestObject(object):

    @classmethod
    def from_dict(cls, adict):
        raise NotImplementedError

    def __nonzero__(self):
        return True

    __bool__ = __nonzero__


class StorageRoomListRequestObject(ValidRequestObject):

    def __init__(self, filters=None):
        self.filters = filters

    @classmethod
    def from_dict(cls, adict):
        invalid_req = InvalidRequestObject()
        
        if 'filters' in adict and not isinstance(adict['filters'], collections.Mapping):
            invalid_req.add_error('filters', 'Is not iterable')

        if invalid_req.has_errors():
            return invalid_req

        return StorageRoomListRequestObject(filters=adict.get('filters', None))
```

Let me review this new code bit by bit.

First of all, two helper objects have been introduced, `ValidRequestObject` and `InvalidRequestObject`. They are different because an invalid request shall contain the validation errors, but both can be converted to booleans. 

Second, the `StorageRoomListRequestObject` accepts an optional `filters` parameter when instantiated. There are no validation checks in the `__init__()` method because this is considered to be an internal method that gets called when the parameters have already been validated.

Last, the `from_dict()` method performs the validation of the `filters` parameter, if it is present. I leverage the `collections.Mapping` abstract base class to check if the incoming parameter is a dictionary-like object and return either an `InvalidRequestObject` or a `ValidRequestObject` instance.

Since we can now tell bad requests from good ones we need to introduce a new type of response as well, to manage bad requests or other errors in the use case.

# Responses and failures

**Git tag: [step08](https://github.com/lgiordani/rentomatic/tree/step08)**

What happens if the use case encounter an error? Use cases can encounter a wide set of errors: validation errors, as we just discussed in the previous section, but also business logic errors or errors that come from the repository layer. Whatever the error, the use case shall always return an object with a known structure (the response), so we need a new object that provides a good support for different types of failures.

As happened for the requests there is no unique way to provide such an object, and the following code is just one of the possible solutions.

The first thing to do is to expand the `tests/shared/test_response_object.py` file, adding tests for failures.

``` python
import pytest

from rentomatic.shared import response_object as res
from rentomatic.use_cases import request_objects as req


@pytest.fixture
def response_value():
    return {'key': ['value1', 'value2']}


@pytest.fixture
def response_type():
    return 'ResponseError'


@pytest.fixture
def response_message():
    return 'This is a response error'
```

This is some boilerplate code, basically pytest fixtures that we will use in the following tests.

``` python
def test_response_success_is_true(response_value):
    assert bool(res.ResponseSuccess(response_value)) is True


def test_response_failure_is_false(response_type, response_message):
    assert bool(res.ResponseFailure(response_type, response_message)) is False
```

Two basic tests to check that both the old `ResponseSuccess` and the new `ResponseFailure` objects behave consistently when converted to boolean.

``` python
def test_response_success_contains_value(response_value):
    response = res.ResponseSuccess(response_value)

    assert response.value == response_value
```

The `ResponseSuccess` object contains the call result in the `value` attribute.

``` python
def test_response_failure_has_type_and_message(response_type, response_message):
    response = res.ResponseFailure(response_type, response_message)

    assert response.type == response_type
    assert response.message == response_message


def test_response_failure_contains_value(response_type, response_message):
    response = res.ResponseFailure(response_type, response_message)

    assert response.value == {'type': response_type, 'message': response_message}
```

These two tests ensure that the `ResponseFailure` object provides the same interface provided by the successful one and that the `type` and `message` parameter are accessible.

``` python
def test_response_failure_initialization_with_exception():
    response = res.ResponseFailure(response_type, Exception('Just an error message'))

    assert bool(response) is False
    assert response.type == response_type
    assert response.message == "Exception: Just an error message"


def test_response_failure_from_invalid_request_object():
    response = res.ResponseFailure.build_from_invalid_request_object(req.InvalidRequestObject())

    assert bool(response) is False


def test_response_failure_from_invalid_request_object_with_errors():
    request_object = req.InvalidRequestObject()
    request_object.add_error('path', 'Is mandatory')
    request_object.add_error('path', "can't be blank")

    response = res.ResponseFailure.build_from_invalid_request_object(request_object)

    assert bool(response) is False
    assert response.type == res.ResponseFailure.PARAMETERS_ERROR
    assert response.message == "path: Is mandatory\npath: can't be blank"
```

We sometimes want to create responses from Python exceptions that can happen in the use case, so we test that `ResponseFailure` objects can be initialized with a generic exception.

And last we have the tests for the `build_from_invalid_request_object()` method that automate the initialization of the response from an invalid request. If the request contains errors (remember that the request validates itself), we need to put them into the response message.

The last test uses a class attribute to classify the error. The `ResponseFailure` class will contain three predefined errors that can happen when running the use case, namely `RESOURCE_ERROR`, `PARAMETERS_ERROR`, and `SYSTEM_ERROR`. This categorization is an attempt to capture the different types of issues that can happen when dealing with an external system through an API. `RESOURCE_ERROR` contains all those errors that are related to the resources contained in the repository, for instance when you cannot find an entry given its unique id. `PARAMETERS_ERROR` describes all those errors that occur when the request parameters are wrong or missing. `SYSTEM_ERROR` encompass the errors that happen in the underlying system at operating system level, such as a failure in a filesystem operation, or a network connection error while fetching data from the database.

The use case has the responsibility to manage the different error conditions arising from the Python code and to convert them into an error description made of one of the three types I just described and a message.

Let's write the `ResponseFailure` class that makes the tests pass. This can be the initial definition of the class. Put it in `rentomatic/shared/response_object.py`

``` python
class ResponseFailure(object):
    RESOURCE_ERROR = 'ResourceError'
    PARAMETERS_ERROR = 'ParametersError'
    SYSTEM_ERROR = 'SystemError'

    def __init__(self, type_, message):
        self.type = type_
        self.message = self._format_message(message)

    def _format_message(self, msg):
        if isinstance(msg, Exception):
            return "{}: {}".format(msg.__class__.__name__, "{}".format(msg))
        return msg
```

Through the `_format_message()` method we enable the class to accept both string messages and Python exceptions, which is very handy when dealing with external libraries that can raise exceptions we do not know or do not want to manage.

``` python
    @property
    def value(self):
        return {'type': self.type, 'message': self.message}
```

This property makes the class comply with the `ResponseSuccess` API, providing the `value` attribute, which is an aptly formatted dictionary.

```python
    def __nonzero__(self):
        return False

    __bool__ = __nonzero__

    @classmethod
    def build_from_invalid_request_object(cls, invalid_request_object):
        message = "\n".join(["{}: {}".format(err['parameter'], err['message'])
                             for err in invalid_request_object.errors])
        return cls(cls.PARAMETERS_ERROR, message)
```

As explained before, the `PARAMETERS_ERROR` type encompasses all those errors that come from an invalid set of parameters, which is the case of this function, that shall be called whenever the request is wrong, which means that some parameters contain errors or are missing.

Since building failure responses is a common activity it is useful to have helper methods, so I add three tests for the building functions to the `tests/shared/test_response_object.py` file

``` python
def test_response_failure_build_resource_error():
    response = res.ResponseFailure.build_resource_error("test message")

    assert bool(response) is False
    assert response.type == res.ResponseFailure.RESOURCE_ERROR
    assert response.message == "test message"


def test_response_failure_build_parameters_error():
    response = res.ResponseFailure.build_parameters_error("test message")

    assert bool(response) is False
    assert response.type == res.ResponseFailure.PARAMETERS_ERROR
    assert response.message == "test message"


def test_response_failure_build_system_error():
    response = res.ResponseFailure.build_system_error("test message")

    assert bool(response) is False
    assert response.type == res.ResponseFailure.SYSTEM_ERROR
    assert response.message == "test message"
```

We add the relevant methods to the class and change the `build_from_invalid_request_object()` method to leverage the `build_parameters_error()` new method. Change the `rentomatic/shared/response_object.py` file to contain this code

```python
    @classmethod
    def build_resource_error(cls, message=None):
        return cls(cls.RESOURCE_ERROR, message)

    @classmethod
    def build_system_error(cls, message=None):
        return cls(cls.SYSTEM_ERROR, message)

    @classmethod
    def build_parameters_error(cls, message=None):
        return cls(cls.PARAMETERS_ERROR, message)

    @classmethod
    def build_from_invalid_request_object(cls, invalid_request_object):
        message = "\n".join(["{}: {}".format(err['parameter'], err['message'])
                             for err in invalid_request_object.errors])
        return cls.build_parameters_error(message)
```

# Use cases (part 3)

**Git tag: [step09](https://github.com/lgiordani/rentomatic/tree/step09)**

Our implementation of responses and requests is finally complete, so now we can implement the last version of our use case. The use case correctly returns a `ResponseSuccess` object but is still missing a proper validation of the incoming request.

Let's change the test in the `tests/use_cases/test_storageroom_list_use_case.py` file and add two more tests. The resulting set of tests (after the `domain_storagerooms` fixture) is the following

``` python
import pytest
from unittest import mock

from rentomatic.domain.storageroom import StorageRoom
from rentomatic.shared import response_object as res
from rentomatic.use_cases import request_objects as req
from rentomatic.use_cases import storageroom_use_cases as uc


@pytest.fixture
def domain_storagerooms():
    [...]


def test_storageroom_list_without_parameters(domain_storagerooms):
    repo = mock.Mock()
    repo.list.return_value = domain_storagerooms

    storageroom_list_use_case = uc.StorageRoomListUseCase(repo)
    request_object = req.StorageRoomListRequestObject.from_dict({})

    response_object = storageroom_list_use_case.execute(request_object)

    assert bool(response_object) is True
    repo.list.assert_called_with(filters=None)

    assert response_object.value == domain_storagerooms
```

This is the test we already wrote, but the `assert_called_with()` method is called with `filters=None` to reflect the added parameter. The import line has slightly changed as well, given that we are now importing both `response_objects` and `request_objects`. The `domain_storagerooms` fixture has not changed and has been omitted from the code snippet to keep it short.

``` python
def test_storageroom_list_with_filters(domain_storagerooms):
    repo = mock.Mock()
    repo.list.return_value = domain_storagerooms

    storageroom_list_use_case = uc.StorageRoomListUseCase(repo)
    qry_filters = {'a': 5}
    request_object = req.StorageRoomListRequestObject.from_dict({'filters': qry_filters})

    response_object = storageroom_list_use_case.execute(request_object)

    assert bool(response_object) is True
    repo.list.assert_called_with(filters=qry_filters)
    assert response_object.value == domain_storagerooms
```

This test checks that the value of the `filters` key in the dictionary used to create the request is actually used when calling the repository.

``` python
def test_storageroom_list_handles_generic_error():
    repo = mock.Mock()
    repo.list.side_effect = Exception('Just an error message')

    storageroom_list_use_case = uc.StorageRoomListUseCase(repo)
    request_object = req.StorageRoomListRequestObject.from_dict({})

    response_object = storageroom_list_use_case.execute(request_object)

    assert bool(response_object) is False
    assert response_object.value == {
        'type': res.ResponseFailure.SYSTEM_ERROR,
        'message': "Exception: Just an error message"
    }


def test_storageroom_list_handles_bad_request():
    repo = mock.Mock()

    storageroom_list_use_case = uc.StorageRoomListUseCase(repo)
    request_object = req.StorageRoomListRequestObject.from_dict({'filters': 5})

    response_object = storageroom_list_use_case.execute(request_object)

    assert bool(response_object) is False
    assert response_object.value == {
        'type': res.ResponseFailure.PARAMETERS_ERROR,
        'message': "filters: Is not iterable"
    }
```

This last two tests check the behaviour of the use case when the repository raises an exception or when the request is badly formatted.

Change the file `rentomatic/use_cases/storageroom_use_cases.py` to contain the new use case implementation that makes all the test pass

``` python
from rentomatic.shared import response_object as res


class StorageRoomListUseCase(object):

    def __init__(self, repo):
        self.repo = repo

    def execute(self, request_object):
        if not request_object:
            return res.ResponseFailure.build_from_invalid_request_object(request_object)

        try:
            storage_rooms = self.repo.list(filters=request_object.filters)
            return res.ResponseSuccess(storage_rooms)
        except Exception as exc:
            return res.ResponseFailure.build_system_error(
                "{}: {}".format(exc.__class__.__name__, "{}".format(exc)))
```

As you can see the first thing that the `execute()` method does is to check if the request is valid, otherwise returns a `ResponseFailure` build with the same request object. Then the actual business logic is implemented, calling the repository and returning a success response. If something goes wrong in this phase the exception is caught and returned as an aptly formatted `ResponseFailure`.

# Intermezzo: refactoring

**Git tag: [step10](https://github.com/lgiordani/rentomatic/tree/step10)**

A clean architecture is _not_ a framework, so it provides very few generic features, unlike products like for example Django, which provide models, ORM, and all sorts of structures and libraries. Nevertheless, some classes can be isolated from our code and provided as a library, so that we can reuse the code. In this section I will guide you through a refactoring of the code we already have, during which we will isolate common features for requests, responses, and use cases.

We already isolated the response object. We can move the `test_valid_request_object_cannot_be_used` from `tests/use_cases/test_storageroom_list_request_objects.py` to `tests/shared/test_response_object.py` since it tests a generic behaviour and not something related to the `StorageRoom` model and use cases.

Then we can move the `InvalidRequestObject` and `ValidRequestObject` classes from `rentomatic/use_cases/request_objects.py` to `rentomatic/shared/request_object.py`, making the necessary changes to the `StorageRoomListRequestObject` class that now inherits from an external class.

The use case is the class that undergoes the major changes. The `UseCase` class is tested by the following code in the `tests/shared/test_use_case.py` file

``` python
from unittest import mock

from rentomatic.shared import request_object as req, response_object as res
from rentomatic.shared import use_case as uc


def test_use_case_cannot_process_valid_requests():
    valid_request_object = mock.MagicMock()
    valid_request_object.__bool__.return_value = True

    use_case = uc.UseCase()
    response = use_case.execute(valid_request_object)

    assert not response
    assert response.type == res.ResponseFailure.SYSTEM_ERROR
    assert response.message == \
        'NotImplementedError: process_request() not implemented by UseCase class'
```

This test checks that the `UseCase` class cannot be actually used to process incoming requests.

``` python
def test_use_case_can_process_invalid_requests_and_returns_response_failure():
    invalid_request_object = req.InvalidRequestObject()
    invalid_request_object.add_error('someparam', 'somemessage')

    use_case = uc.UseCase()
    response = use_case.execute(invalid_request_object)

    assert not response
    assert response.type == res.ResponseFailure.PARAMETERS_ERROR
    assert response.message == 'someparam: somemessage'
```

This test runs the use case with an invalid request and check if the response is correct. Since the request is wrong the response type is `PARAMETERS_ERROR`, as this represents an issue in the request parameters.

``` python
def test_use_case_can_manage_generic_exception_from_process_request():
    use_case = uc.UseCase()

    class TestException(Exception):
        pass

    use_case.process_request = mock.Mock()
    use_case.process_request.side_effect = TestException('somemessage')
    response = use_case.execute(mock.Mock)

    assert not response
    assert response.type == res.ResponseFailure.SYSTEM_ERROR
    assert response.message == 'TestException: somemessage'
```

This test makes the use case raise an exception. This type of error is categorized as `SYSTEM_ERROR`, which is a generic name for an exception which is not related to request parameters or actual entities.

As you can see in this last test the idea is that of exposing the `execute()` method in the `UseCase` class and to call the `process_request()` method defined by each child class, which is the actual use case we are implementing.

The `rentomatic/shared/use_case.py` file contains the following code that makes the test pass

``` python
from rentomatic.shared import response_object as res


class UseCase(object):

    def execute(self, request_object):
        if not request_object:
            return res.ResponseFailure.build_from_invalid_request_object(request_object)
        try:
            return self.process_request(request_object)
        except Exception as exc:
            return res.ResponseFailure.build_system_error(
                "{}: {}".format(exc.__class__.__name__, "{}".format(exc)))

    def process_request(self, request_object):
        raise NotImplementedError(
            "process_request() not implemented by UseCase class")
```

While the `rentomatic/use_cases/storageroom_use_cases.py` now contains the following code

``` python
from rentomatic.shared import use_case as uc
from rentomatic.shared import response_object as res


class StorageRoomListUseCase(uc.UseCase):

    def __init__(self, repo):
        self.repo = repo

    def process_request(self, request_object):
        domain_storageroom = self.repo.list(filters=request_object.filters)
        return res.ResponseSuccess(domain_storageroom)
```

# The repository layer

**Git tag: [step11](https://github.com/lgiordani/rentomatic/tree/step11)**

The repository layer is the one in which we run the data storage system. As you saw when we implemented the use case we access the data storage through an API, in this case the `list()` method of the repository. The level of abstraction provided by a repository level is higher than that provided by an ORM or by a tool like SQLAlchemy. The repository layer provides only the endpoints that the application needs, with an interface which is tailored on the specific business problems the application implements.

To clarify the matter in terms of concrete technologies, SQLAlchemy is a wonderful tool to abstract the access to an SQL database, so the internal implementation of the repository layer could use it to access a PostgreSQL database. But the external API of the layer is not that provided by SQLAlchemy. The API is a (usually reduced) set of functions that the use cases call to get the data, and indeed the internal implementation could also use raw SQL queries on a proprietary network interface. The repository does not even need to be based on a database. We can have a repository layer that fetches data from a REST service, for example, or that makes remote procedure calls through a RabbitMQ network.

A very important feature of the repository layer is that it always returns domain models, and this is in line with what framework ORMs usually do.

I will not deploy a real database in this post. I will address that part of the application in a future post, where there will be enough space to implement two different solutions and show how the repository API can mask the actual implementation.

Instead, I am going to create a very simple memory storage system with some predefined data. I think this is enough for the moment to demonstrate the repository concept.

The first thing to do is to write some tests that document the public API of the repository. The file containing the tests is `tests/repository/test_memrepo.py`.

First we add some data that we will be using in the tests. We import the domain model to check if the results of the API calls have the correct type

``` python
import pytest

from rentomatic.domain.storageroom import StorageRoom
from rentomatic.shared.domain_model import DomainModel

from rentomatic.repository import memrepo


@pytest.fixture
def storageroom_dicts():
    return [
        {
            'code': 'f853578c-fc0f-4e65-81b8-566c5dffa35a',
            'size': 215,
            'price': 39,
            'longitude': -0.09998975,
            'latitude': 51.75436293,
        },
        {
            'code': 'fe2c3195-aeff-487a-a08f-e0bdc0ec6e9a',
            'size': 405,
            'price': 66,
            'longitude': 0.18228006,
            'latitude': 51.74640997,
        },
        {
            'code': '913694c6-435a-4366-ba0d-da5334a611b2',
            'size': 56,
            'price': 60,
            'longitude': 0.27891577,
            'latitude': 51.45994069,
        },
        {
            'code': 'eed76e77-55c1-41ce-985d-ca49bf6c0585',
            'size': 93,
            'price': 48,
            'longitude': 0.33894476,
            'latitude': 51.39916678,
        }
    ]
```

Since the repository object will return domain models, we need a helper function to check the correctness of the results. The following function checks the length of the two lists, ensures that all the returned elements are domain models and compares the codes. Note that we can safely employ the `isinstance()` built-in function since `DomainModel` is an abstract base class and our models are registered (see the `rentomatic/domain/storagerooms.py`)

``` python
def _check_results(domain_models_list, data_list):
    assert len(domain_models_list) == len(data_list)
    assert all([isinstance(dm, DomainModel) for dm in domain_models_list])
    assert set([dm.code for dm in domain_models_list]
               ) == set([d['code'] for d in data_list])
```

We need to be able to initialize the repository with a list of dictionaries, and the `list()` method without any parameter shall return the same list of entries.

``` python
def test_repository_list_without_parameters(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    _check_results(
        repo.list(),
        storageroom_dicts
    )
```

The `list()` method shall accept a `filters` parameter, which is a dictionary. The dictionary keys shall be in the form `<attribute>__<operator>`, similar to the syntax used by the Django ORM. So to express that the price shall be less than 65 we can write `filters={'price__lt': 60}`.

A couple of error conditions shall be checked: using an unknown key shall raise a `KeyError` exception, and using a wrong operator shall raise a `ValueError` exception.

``` python
def test_repository_list_with_filters_unknown_key(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    with pytest.raises(KeyError):
        repo.list(filters={'name': 'aname'})


def test_repository_list_with_filters_unknown_operator(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    with pytest.raises(ValueError):
        repo.list(filters={'price__in': [20, 30]})
```

Let us then test that the filtering mechanism actually works. We want the default operator to be `__eq`, which means that if we do not put any operator an equality check shall be performed.

``` python
def test_repository_list_with_filters_price(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    _check_results(
        repo.list(filters={'price': 60}),
        [storageroom_dicts[2]]
    )


def test_repository_list_with_filters_price_eq(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    _check_results(
        repo.list(filters={'price__eq': 60}),
        [storageroom_dicts[2]]
    )


def test_repository_list_with_filters_price_lt(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    _check_results(
        repo.list(filters={'price__lt': 60}),
        [storageroom_dicts[0], storageroom_dicts[3]])


def test_repository_list_with_filters_price_gt(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)
    _check_results(
        repo.list(filters={'price__gt': 60}),
        [storageroom_dicts[1]]
    )


def test_repository_list_with_filters_size(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    _check_results(
        repo.list(filters={'size': 93}),
        [storageroom_dicts[3]]
    )


def test_repository_list_with_filters_size_eq(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)
    _check_results(
        repo.list(filters={'size__eq': 93}),
        [storageroom_dicts[3]]
    )


def test_repository_list_with_filters_size_lt(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)
    _check_results(
        repo.list(filters={'size__lt': 60}),
        [storageroom_dicts[2]]
    )


def test_repository_list_with_filters_size_gt(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)
    _check_results(
        repo.list(filters={'size__gt': 400}),
        [storageroom_dicts[1]]
    )


def test_repository_list_with_filters_code(storageroom_dicts):
    repo = memrepo.MemRepo(storageroom_dicts)

    _check_results(
        repo.list(filters={'code': '913694c6-435a-4366-ba0d-da5334a611b2'}),
        [storageroom_dicts[2]]
    )
```

The implementation of the `MemRepo` class is pretty simple, and I will not dive into it line by line.

``` python
from rentomatic.domain import storageroom as sr


class MemRepo:

    def __init__(self, entries=None):
        self._entries = []
        if entries:
            self._entries.extend(entries)

    def _check(self, element, key, value):
        if '__' not in key:
            key = key + '__eq'

        key, operator = key.split('__')

        if operator not in ['eq', 'lt', 'gt']:
            raise ValueError('Operator {} is not supported'.format(operator))

        operator = '__{}__'.format(operator)

        if key in ['size', 'price']:
            return getattr(element[key], operator)(int(value))
        elif key in ['latitude', 'longitude']:
            return getattr(element[key], operator)(float(value))

        return getattr(element[key], operator)(value)

    def list(self, filters=None):
        if not filters:
            result = self._entries
        else:
            result = []
            result.extend(self._entries)

            for key, value in filters.items():
                result = [e for e in result if self._check(e, key, value)]

        return [sr.StorageRoom.from_dict(r) for r in result]

```

# The REST layer (part1)

**Git tag: [step12](https://github.com/lgiordani/rentomatic/tree/step12)**

This is the last step of our journey into the clean architecture. We created the domain models, the serializers, the use cases and the repository. We are actually missing an interface that glues everything together, that is gets the call parameters from the user, initializes a use case with a repository, runs the use case that fetches the domain models from the repository and converts them to a standard format. This layer can be represented by a wide number of interfaces and technologies. For example a command line interface (CLI) can implement exactly those steps, getting the parameters via command line switches, and returning the results as plain text on the console. The same underlying system, however, can be leveraged by a web page that gets the call parameters from a set of widgets, perform the steps described above, and parses the returned JSON data to show the result on the same page.

Whatever technology we want to use to interact with the user to collect inputs and provide results we need to interface with the clean architecture we just built, so now we will create a layer to expose an HTTP API. This can be done with a server that exposes a set of HTTP addresses (API endpoints) that once accessed return some data. Such a layer is commonly called a REST layer, because usually the semantic of the addresses comes from the REST recommendations.

Flask is a lightweight web server with a modular structure that provides just the parts that the user needs. In particular, we will not use any database/ORM, since we already implemented our own repository layer.

Please keep in mind that this part of the project, together with the repository layer, is usually implemented as a separate package, and I am keeping them together just for the sake of this introductory tutorial.

Let us start updating the requirements files. The `dev.txt` file shall contain Flask

``` text
-r test.txt

pip
wheel
flake8
Sphinx
Flask
```

And the `test.txt` file will contain the pytest extension to work with Flask (more on this later)

``` text
-r prod.txt

pytest
tox
coverage
pytest-cov
pytest-flask
```

Remember to run `pip install -r requirements/dev.txt` again after those changes to actually install the new packages in your virtual environment.

The setup of a Flask application is not complex, but a lot of concepts are involved, and since this is not a tutorial on Flask I will run quickly through these steps. I will however provide links to the Flask documentation for every concept.

I usually define different configurations for my testing, development, and production environments. Since the Flask application can be configured using a plain Python object ([documentation](http://flask.pocoo.org/docs/latest/api/#flask.Config.from_object)), I created the file `rentomatic/settings.py` to host those objects

``` python
import os


class Config(object):
    """Base configuration."""

    APP_DIR = os.path.abspath(os.path.dirname(__file__))  # This directory
    PROJECT_ROOT = os.path.abspath(os.path.join(APP_DIR, os.pardir))


class ProdConfig(Config):
    """Production configuration."""
    ENV = 'prod'
    DEBUG = False


class DevConfig(Config):
    """Development configuration."""
    ENV = 'dev'
    DEBUG = True


class TestConfig(Config):
    """Test configuration."""
    ENV = 'test'
    TESTING = True
    DEBUG = True
```

Read [this page](http://flask.pocoo.org/docs/latest/config/) to know more about Flask configuration parameters. Now we need a function that initializes the Flask application ([documentation](http://flask.pocoo.org/docs/latest/patterns/appfactories/)), configures it and registers the blueprints ([documentation](http://flask.pocoo.org/docs/latest/blueprints/)). The file `rentomatic/app.py` contains the following code

``` python
from flask import Flask

from rentomatic.rest import storageroom
from rentomatic.settings import DevConfig


def create_app(config_object=DevConfig):
    app = Flask(__name__)
    app.config.from_object(config_object)
    app.register_blueprint(storageroom.blueprint)
    return app
```

The application endpoints need to return a Flask `Response` object, with the actual results and an HTTP status. The content of the response, in this case, is the JSON serialization of the use case response.

Let us write a test step by step, so that you can perfectly understand what is going to happen in the REST endpoint. The basic structure of the test is

``` text
[SOME PREPARATION]
[CALL THE API ENDPOINT]
[CHECK RESPONSE DATA]
[CHECK RESPONSE STATUS CODE]
[CHECK RESPONSE MIMETYPE]
```

So our first test `tests/rest/test_get_storagerooms_list.py` is made of the following parts

``` python
@mock.patch('rentomatic.use_cases.storageroom_use_cases.StorageRoomListUseCase')
def test_get(mock_use_case, client):
    mock_use_case().execute.return_value = res.ResponseSuccess(storagerooms)
```

Remember that we are not testing the use case here, so we can safely mock it. Here we make the use case return a `ResponseSuccess` instance containing a list of domain models (that we didn't define yet).

``` python
    http_response = client.get('/storagerooms')
```

This is the actual API call. We are exposing the endpoint at the `/storagerooms` address. Note the use of the `client` fixture provided by `pytest-flask`.

``` python
    assert json.loads(http_response.data.decode('UTF-8')) == [storageroom1_dict]
    assert http_response.status_code == 200
    assert http_response.mimetype == 'application/json'
```

These are the three checks previously mentioned. The second and the third ones are pretty straightforward, while the first one needs some explanations. We want to compare `http_response.data` with `[storageroom1_dict]`, which is a list with a Python dictionary containing the data of the `storageroom1_domain_model` object. Flask `Response` objects contain a binary representation of the data, so first we decode the bytes using UTF-8, then convert them in a Python object. It is much more convenient to compare Python objects, since pytest can deal with issues like the unordered nature of dictionaries, while this is not possible when comparing two strings.  

The final test file, with the test domain model and its dictionary is

``` python
import json
from unittest import mock

from rentomatic.domain.storageroom import StorageRoom
from rentomatic.shared import response_object as res

storageroom1_dict = {
    'code': '3251a5bd-86be-428d-8ae9-6e51a8048c33',
    'size': 200,
    'price': 10,
    'longitude': -0.09998975,
    'latitude': 51.75436293
}

storageroom1_domain_model = StorageRoom.from_dict(storageroom1_dict)

storagerooms = [storageroom1_domain_model]


@mock.patch('rentomatic.use_cases.storageroom_use_cases.StorageRoomListUseCase')
def test_get(mock_use_case, client):
    mock_use_case().execute.return_value = res.ResponseSuccess(storagerooms)

    http_response = client.get('/storagerooms')

    assert json.loads(http_response.data.decode('UTF-8')) == [storageroom1_dict]
    assert http_response.status_code == 200
    assert http_response.mimetype == 'application/json'
```

If you run pytest you'll notice that the test suite fails because of the `app` fixture, which is missing. The `pytest-flask` plugin provides the `client` fixture, but relies on the `app` fixture which has to be provided. The best place to define it is in `tests/conftest.py`

``` python
import pytest


from rentomatic.app import create_app
from rentomatic.settings import TestConfig


@pytest.yield_fixture(scope='function')
def app():
    return create_app(TestConfig)
```

It's time to write the endpoint, where we will finally see all the pieces of the architecture working together. 

The minimal Flask endpoint we can put in `rentomatic/rest/storageroom.py` is something like

``` python
blueprint = Blueprint('storageroom', __name__)


@blueprint.route('/storagerooms', methods=['GET'])
def storageroom():
    [LOGIC]
    return Response([JSON DATA],
                    mimetype='application/json',
                    status=[STATUS])
```

The first part of our logic is the creation of a `StorageRoomListRequestObject`. For the moment we can ignore the optional querystring parameters and use an empty dictionary

``` python
def storageroom():
    request_object = ro.StorageRoomListRequestObject.from_dict({})
```

As you can see I'm creating the object from an empty dictionary, so querystring parameters are not taken into account for the moment. The second thing to do is to initialize the repository

``` python
    repo = mr.MemRepo()
```

The third thing the endpoint has to do is the initialization of the use case

``` python
    use_case = uc.StorageRoomListUseCase(repo)
```

And finally we run the use case passing the request object

``` python
    response = use_case.execute(request_object)
```

This response, however, is not yet an HTTP response, and we have to explicitly build it. The HTTP response will contain the JSON representation of the `response.value` attribute.

``` python
    return Response(json.dumps(response.value, cls=ser.StorageRoomEncoder),
                    mimetype='application/json',
                    status=200)
```

Note that this function is obviously still incomplete, as it returns always a successful response (code 200). It is however enough to pass the test we wrote. The whole file is the following

``` python
import json
from flask import Blueprint, Response

from rentomatic.use_cases import request_objects as req
from rentomatic.repository import memrepo as mr
from rentomatic.use_cases import storageroom_use_cases as uc
from rentomatic.serializers import storageroom_serializer as ser

blueprint = Blueprint('storageroom', __name__)


@blueprint.route('/storagerooms', methods=['GET'])
def storageroom():
    request_object = req.StorageRoomListRequestObject.from_dict({})

    repo = mr.MemRepo()
    use_case = uc.StorageRoomListUseCase(repo)

    response = use_case.execute(request_object)

    return Response(json.dumps(response.value, cls=ser.StorageRoomEncoder),
                    mimetype='application/json',
                    status=200)
```

This code demonstrates how the clean architecture works in a nutshell. The function we wrote is however not complete, as it doesn't consider querystring parameters and error cases.

# The server in action

**Git tag: [step13](https://github.com/lgiordani/rentomatic/tree/step13)**

Before I fix the missing parts of the endpoint let us see the server in action, so we can finally enjoy the product we have been building during this long post.

To actually see some results when accessing the endpoint we need to fill the repository with some data. This part is obviously required only because of the ephemeral nature of the repository we are using. A real repository would wrap a persistent source of data and providing data at this point wouldn't be necessary. To initialize the repository we have to define some data, so add these dictionaries to the `rentomatic/rest/storageroom.py` file

``` python
storageroom1 = {
    'code': 'f853578c-fc0f-4e65-81b8-566c5dffa35a',
    'size': 215,
    'price': 39,
    'longitude': -0.09998975,
    'latitude': 51.75436293,
}

storageroom2 = {
    'code': 'fe2c3195-aeff-487a-a08f-e0bdc0ec6e9a',
    'size': 405,
    'price': 66,
    'longitude': 0.18228006,
    'latitude': 51.74640997,
}

storageroom3 = {
    'code': '913694c6-435a-4366-ba0d-da5334a611b2',
    'size': 56,
    'price': 60,
    'longitude': 0.27891577,
    'latitude': 51.45994069,
}
```

And then use them to initialise the repository

``` python
    repo = mr.MemRepo([storageroom1, storageroom2, storageroom3])
```

To run the web server we need to create a `wsgi.py` file in the main project folder (the folder where `setup.py` is stored)

``` python
from rentomatic.app import create_app


app = create_app()
```

Now we can run the Flask development server

``` sh
$ flask run
```

At this point, if you open your browser and navigate to [http://localhost:5000/storagerooms](http://localhost:5000/storagerooms), you can see the API call results. I recommend installing a formatter extension for the browser to better check the output. If you are using Chrome try [JSON Formatter](https://github.com/callumlocke/json-formatter).

# The REST layer (part2)

**Git tag: [step14](https://github.com/lgiordani/rentomatic/tree/step14)**

Let us cover the two missing cases in the endpoint. First I introduce a test to check if the endpoint correctly handles querystring parameters. Add it to the `tests/rest/test_get_storagerooms_list.py` file

``` python
@mock.patch(
    'rentomatic.use_cases.storageroom_use_cases.StorageRoomListUseCase')
def test_get_failed_response(mock_use_case, client):
    mock_use_case().execute.return_value = \
        res.ResponseFailure.build_system_error('test message')

    http_response = client.get('/storagerooms')

    assert json.loads(http_response.data.decode('UTF-8')) == \
        {'type': 'SYSTEM_ERROR', 'message': 'test message'}
    assert http_response.status_code == 500
    assert http_response.mimetype == 'application/json'
```

This makes the use case return a failed response and check that the HTTP response contains a formatted version of the error. To make this test pass we have to introduce a proper mapping between domain responses codes and HTTP codes in the `rentomatic/rest/storageroom.py` file

``` python
from rentomatic.shared import response_object as res

STATUS_CODES = {
    res.ResponseSuccess.SUCCESS: 200,
    res.ResponseFailure.RESOURCE_ERROR: 404,
    res.ResponseFailure.PARAMETERS_ERROR: 400,
    res.ResponseFailure.SYSTEM_ERROR: 500
}
```

Then we need to create the Flask response with the correct code in the definition of the endpoint

``` python
    return Response(json.dumps(response.value, cls=ser.StorageRoomEncoder),
                    mimetype='application/json',
                    status=STATUS_CODES[response.type])
```

The second and last test is a bit more complex. As before we will mock the use case, but this time we will also patch `StorageRoomListRequestObject`. We do this because we need to know if the request object is initialized with the correct parameters from the command line. So, step by step

``` python
@mock.patch('rentomatic.use_cases.storageroom_use_cases.StorageRoomListUseCase')
def test_request_object_initialisation_and_use_with_filters(mock_use_case, client):
    mock_use_case().execute.return_value = res.ResponseSuccess([])
```

This is, like, before, a patch of the use case class that ensures the use case will return a `ResponseSuccess` instance.

``` python
    internal_request_object = mock.Mock()
```

The request object will be internally created with `StorageRoomListRequestObject.from_dict`, and we want that function to return a known mock object, which is the one we initialized here.

``` python
    request_object_class = 'rentomatic.use_cases.request_objects.StorageRoomListRequestObject'
    with mock.patch(request_object_class) as mock_request_object:
        mock_request_object.from_dict.return_value = internal_request_object
        client.get('/storagerooms?filter_param1=value1&filter_param2=value2')
```

Here we patch `StorageRoomListRequestObject` and we assign a known output to the `from_dict()` method. Then we call the endpoint with some querystring parameters. What should happen is that the `from_dict()` method of the request is called with the filter parameters and that the `execute()` method of the use case instance is called with the `internal_request_object`.

``` python
    mock_request_object.from_dict.assert_called_with(
        {'filters': {'param1': 'value1', 'param2': 'value2'}}
    )
    mock_use_case().execute.assert_called_with(internal_request_object)
```

The endpoint function shall be changed somehow to reflect this new behaviour and to make the test pass. The whole code of the new `storageroom()` Flask method is the following

``` python
import json
from flask import Blueprint, request, Response

from rentomatic.use_cases import request_objects as req
from rentomatic.shared import response_object as res
from rentomatic.repository import memrepo as mr
from rentomatic.use_cases import storageroom_use_cases as uc
from rentomatic.serializers import storageroom_serializer as ser

blueprint = Blueprint('storageroom', __name__)

STATUS_CODES = {
    res.ResponseSuccess.SUCCESS: 200,
    res.ResponseFailure.RESOURCE_ERROR: 404,
    res.ResponseFailure.PARAMETERS_ERROR: 400,
    res.ResponseFailure.SYSTEM_ERROR: 500
}

storageroom1 = {
    'code': 'f853578c-fc0f-4e65-81b8-566c5dffa35a',
    'size': 215,
    'price': 39,
    'longitude': '-0.09998975',
    'latitude': '51.75436293',
}

storageroom2 = {
    'code': 'fe2c3195-aeff-487a-a08f-e0bdc0ec6e9a',
    'size': 405,
    'price': 66,
    'longitude': '0.18228006',
    'latitude': '51.74640997',
}

storageroom3 = {
    'code': '913694c6-435a-4366-ba0d-da5334a611b2',
    'size': 56,
    'price': 60,
    'longitude': '0.27891577',
    'latitude': '51.45994069',
}


@blueprint.route('/storagerooms', methods=['GET'])
def storageroom():
    qrystr_params = {
        'filters': {},
    }

    for arg, values in request.args.items():
        if arg.startswith('filter_'):
            qrystr_params['filters'][arg.replace('filter_', '')] = values

    request_object = req.StorageRoomListRequestObject.from_dict(qrystr_params)

    repo = mr.MemRepo([storageroom1, storageroom2, storageroom3])
    use_case = uc.StorageRoomListUseCase(repo)

    response = use_case.execute(request_object)

    return Response(json.dumps(response.value, cls=ser.StorageRoomEncoder),
                    mimetype='application/json',
                    status=STATUS_CODES[response.type])
```

Note that we extract the querystring parameters from the global `request` object provided by Flask. Once the querystring parameters are in a dictionary, we just need to create the request object from it.

## Conclusions

Well, that's all! Some tests are missing in the REST part, but as I said I just wanted to show a working implementation of a clean architecture and not a fully developed project. I suggest that you try to implement some changes, for example:

* another endpoint like the access to a single resource (`/storagerooms/<code>`)
* a different repository, connected to a real DB (you can use [SQLite](https://sqlite.org/), for example)
* implement a new querystring parameter, for example the distance from a given point on the map (use [geopy](https://github.com/geopy/geopy) to easily compute distances)

While you develop your code always try to work following the TDD approach. Testability is one of the main features of a clean architecture, so don't ignore it.

Whether you decide to use a clean architecture or not, I really hope this post helped you to get a fresh view on software architectures, as happened to me when I first discovered the concepts exemplified here.

## Updates

2016-11-15: Two tests contained variables with a wrong name (artist), which came from an initial version of the project. The name did not affect the tests. Added some instructions on the virtual environment and the development requirements.

2016-12-12: Thanks to [Marco Beri](https://twitter.com/Taifu) who spotted a typo in the code of step 6, which was already correct in the GitHub repository. He also suggested using the Cookiecutter package by [Ardy Dedase](https://github.com/ardydedase). Thanks to Marco and to Ardy!

2018-11-18 Two years have passed since I wrote this post and I found some errors that I fixed, like `longitude` and `latitude` passed as string instead of floats. I also moved the project from Flask-script to the Flask development server and added a couple of clarifications here and there.

2018-11-28 Fixed a typo. Thanks [Seth Mason](https://github.com/slackorama)!

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

