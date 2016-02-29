Title: Advanced use of Python decorators and metaclasses
Date: 2014-10-14 09:43:08 +0200
Category: Programming
Tags: Python3, Python, OOP, decorators, metaclasses, metaprogramming
Authors: Leonardo Giordani
Slug: decorators-and-metaclasses
Summary:

## Abstract

While introducing people to Python metaclasses I realized that sometimes the big problem of the most powerful Python features is that programmers do not perceive how they may simplify their usual tasks. Therefore, features like metaclasses are considered a fancy but rather unuseful addition to a standard OOP language, instead of a real game changer.

This post wants to show how to use metaclasses and decorators to create a powerful class that can be inherited and customized by easily adding decorated methods.

## Metaclasses and decorators: a match made in space

Metaclasses are a complex topic, and most of the times even advanced programmers do not see a wide range of practical uses for them. Chances are that this is the part of Python (or other languages that support metaclasses, like Smalltalk and Ruby) that fits the least the "standard" object-oriented patterns or solutions found in C++ and Java, just to mention two big players.

Indeed metaclasess usually come in play when programming advanced libraries or frameworks, where a lot of automation must be provided. For example, Django Forms system heavily relies on metaclasses to provide all its magic.

We also have to note, however, that we usually call "magic" or "tricks" all those techniques we are not familiar with, and as a result in Python many things are called this way, being its implementation often peculiar compared to other languages.

Time to bring some spice into your programming: let's practice some Python wizardry and exploit the power of the language!

In this post I want to show you an interesting joint use of decorators and metaclasses. I will show you how to use decorators to mark methods so that they can be automatically used by the class when performing a given operation.

More in detail, I will implement a class that can be called on a string to "process" it, and show you how to implement different "filters" through simple decorated methods. What I want to obtain is something like this:

``` python
class MyStringProcessor(StringProcessor):
    @stringfilter
    def capitalize(self, str):
        [...]

    @stringfilter
    def remove_double_spaces(self, str):
        [...]

msp = MyStringProcessor()
"A test string" == msp("a test  string")
```

The module defines a `StringProcessor` class that I can inherit and customize adding methods that have a standard signature `(self, str)` and are decorated with `@stringfilter`. This class can later be instantiated and the instance used to directly process a string and return the result. Internally the class automatically executes all the decorated methods in succession. I also would like the class to obey the order I defined the filters: first defined, first executed.

## The Hitchhiker's Guide To Metaclasses

How can metaclasses help to reach this target?

Simply put, metaclasses are classes that are instantiated to get classes. That means that whenever I use a class, for example to instantiate it, first Python _builds_ that class using the metaclass and the class definition we wrote. For example, you know that you can find the class members in the `__dict__` attribute: this attribute is created by the standard metaclass, which is `type`.

Given that, a metaclass is a good starting point for us to insert some code to identify a subset of functions inside the definition of the class. In other words, we want the output of the metaclass (that is, the class) be built exactly as happens in the standard case, but with an addition: a separate list of all the methods decorated with `@stringfilter`.

You know that a class has a _namespace_, that is a dictionary of what was defined inside the class. So, when the standard `type` metaclass is used to create a class, the class body is parsed and a `dict()` object is used to collect the namespace.

We are however interested in preserving the order of definition and a Python dictionary is an unordered structure, so we take advantage of the `__prepare__` hook introduced in the class creation process with Python 3. This function, if present in the metaclass, is used to preprocess the class and to return the structure used to host the namespace. So, following the example found in the official documentation, we start defining a metaclass like

``` python
class FilterClass(type):
    def __prepare__(name, bases, **kwds):
        return collections.OrderedDict()
```

This way, when the class will be created, an `OrderedDict` will be used to host the namespace, allowing us to keep the definition order. Please note that the signature `__prepare__(name, bases, **kwds)` is enforced by the language. If you want the method to get the metaclass as a first argument (because the code of the method needs it) you have to change the signature to `__prepare__(metacls, name, bases, **kwds)` and decorate it with `@classmethod`.

The second function we want to define in our metaclass is `__new__`. Just like happens for the instantiation of classes, this method is invoked by Python to get a new instance of the metaclass, and is run before `__init__`. Its signature has to be `__new__(metacls, name, bases, namespace, **kwds)` and the result shall be an instance of the metaclass. As for its normal class counterpart (after all a metaclass is a class), `__new__()` usually wraps the same method of the parent class, `type` in this case, adding its own customizations.

The customization we need is the creation of a list of methods that are marked in some way (the decorated filters). Say for simplicity's sake that the decorated methods have an attribute `_filter`.

The full metaclass is then

``` python
class FilterClass(type):
    @classmethod
    def __prepare__(name, bases, **kwds):
        return collections.OrderedDict()

    def __new__(metacls, name, bases, namespace, **kwds):
        result = type.__new__(metacls, name, bases, dict(namespace))
        result._filters = [
            value for value in namespace.values() if hasattr(value, '_filter')]
        return result
```

Now we have to find a way to mark all filter methods with a `_filter` attribute.

## The Anatomy of Purple Decorators

**decorate**: _to add something to an object or place, especially in order to make it more attractive_ (Cambridge Dictionary)

Decorators are, as the name suggests, the best way to augment functions or methods. Remember that a decorator is basically a callable that accepts another callable, processes it, and returns it.

Used in conjunction with metaclasses, decorators are a very powerful and expressive way to implement advanced behaviours in our code. In this case we may easily use them to add an attribute to decorated methods, one of the most basic tasks for a decorator.

I decided to implement the `@stringfilter` decorator as a function, even if I usually prefer implementing them as classes. The reason is that decorator classes behave differently when used to implement decorators without arguments rather than decorators with arguments. In this case this difference would force to write some complex code and an explanation of that would be overkill now. In a future post on dectorators you will find all the gory details, but in the meantime you may check the three Bruce Eckel posts listed in the references section.

The decorator is very simple:

``` python
def stringfilter(func):
    func._filter = True
    return func
```

As you can see the decorator just creates an attribute called `_filter` into the function (remember that functions are objects). The actual value of this attribute is not important in this case, since we are just interested in telling apart class members that contain it.

## The Dynamics of a Callable Object

We are used to think about functions as special language components that may be "called" or executed. In Python functions are objects, just like everything else, and the feature that allows them to be executed comes from the presence of the `__call__()` method. Python is polymorphic by design and based on delegation, so (almost) everything that happens in the code relies on some features of the target object.

The result of this generalization is that every object that contains the `__call__()` method may be executed like a function, and gains the name of _callable object_.

The `StringProcessor` class shall thus contain this method and perform there the string processing with all the contained filters. The code is

``` python
class StringProcessor(metaclass=FilterClass):

    def __call__(self, string):
        _string = string
        for _filter in self._filters:
            _string = _filter(self, _string)

        return _string
```

A quick review of this simple function shows that it accepts the string as an argument, stores it in a local variable and loops over the filters, executing each of them on the local string, that is on the result of the previous filter.

The filter functions are extracted from the `self._filters` list, that is compiled by the `FilterClass` metaclass we already discussed.

What we need to do now is to inherit from `StringProcessor` to get the metaclass machinery and the `__call__()` method, and to define as many methods as needed, decorating them with the `@stringfilter` decorator.

Note that, thanks to the decorator and the metaclass, you may have other methods in your class that do not interfere with the string processing as long as they are not decorated with the decorator under consideration.

An example derived class may be the following

``` python
class MyStringProcessor(StringProcessor):

    @stringfilter
    def capitalize(self, string):
        return string.capitalize()

    @stringfilter
    def remove_double_spaces(self, string):
        return string.replace('  ', ' ')
```

The two `capitalize()` and `remove_double_spaces()` methods have been decorated, so they will be applied in order to any string passed when calling the class. A quick example of this last class is

``` pycon
>>> import strproc
>>> msp = strproc.MyStringProcessor()
>>> input_string = "a test  string"
>>> output_string = msp(input_string)
>>> print("INPUT STRING:", input_string)
INPUT STRING: a test  string
>>> print("OUTPUT STRING:", output_string)
OUTPUT STRING: A test string
>>> 
```

That's it!

## Final words

There are obviously other ways to accomplish the same task, and this post wanted just to give a practical example of what metaclasses are good for, and why I think that they should be part of any Python programmer's arsenal.

[Update] Some developers [on Reddit](http://www.reddit.com/r/Python/comments/2jbi2f/advanced_use_of_python_decorators_and_metaclasses/) and Linkedin raised objections to the content of the post mainly about the fact that the example may be perfectly implemented without metaclasses and about the dangerous nature of metaclasses. Since I try to learn from everyone, I thank them for their suggestions.

It is especially interesting to know that some developers consider the use of metaclasses a risky business, because they hide a lot of the structure of the class and the underlying machinery. This is true, so (as you should do for other technologies), think carefully about the reasons that drive you to use metaclasses, and be sure you know them well.

## Book Trivia

Section titles come from the following books: _A Match Made in Space - George McFly_, _The Hitchhiker's Guide To the Galaxy - Various Authors_, _The Anatomy of Purple Dragons - Unknown_, _The Dynamics of an Asteroid - James Moriarty_.

## Source code

The [strproc.py](/code/metaclasses/strproc.py) file contains the full source code used in this post.

## Online resources

The following resources may be useful.

#### Metaclasses

* Python 3 official documentation: [customizing class creation](https://docs.python.org/3.4/reference/datamodel.html#customizing-class-creation).
* Python 3 OOP Part 5 - Metaclasses [on this blog](/blog/2014/09/01/python-3-oop-part-5-metaclasses).
* [Metaprogramming examples and patterns](http://python-3-patterns-idioms-test.readthedocs.org/en/latest/Metaprogramming.html) (still using some Python 2 code but useful).

#### Decorators

* [Bruce Eckel](http://www.artima.com/weblogs/viewpost.jsp?thread=240808) on decorators (series of three posts, 6 years old but still valid).
* [A different approach](http://simeonfranklin.com/blog/2012/jul/1/python-decorators-in-12-steps/) on explaining decorators.
* [Jeff Knupp](http://www.jeffknupp.com/blog/2013/11/29/improve-your-python-decorators-explained/) goes deep inside the concept of function.

#### Callable objects

* [Rafe Kettler](http://www.rafekettler.com/magicmethods.html#callable) provides a very detaild guide on Python "magic" methods.

## Updates

2014-10-17: [Matthew Dillon](https://github.com/thermokarst) and [Damon McDougall](https://github.com/dmcdougall) spotted two typos. Thank you!

2014-10-17: [ionelmc](http://www.reddit.com/user/ionelmc) suggested two corrections [here](http://www.reddit.com/r/Python/comments/2jbi2f/advanced_use_of_python_decorators_and_metaclasses/cla696y) and [here](http://www.reddit.com/r/Python/comments/2jbi2f/advanced_use_of_python_decorators_and_metaclasses/cla6o77). Both are correct so I implemented them. The second one is more about style, but fits well the introductory purpose of the post. Thanks!

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
