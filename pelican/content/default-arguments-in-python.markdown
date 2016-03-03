Title: Default arguments in Python
Date: 2015-02-11 10:00:00 +0100
Category: Programming
Tags: Python
Authors: Leonardo Giordani
Slug: default-arguments-in-python
Summary: How to deal with default arguments in Python functions and methods

Recently I found a nasty bug in some Python code due to a misuse of default arguments. If you already know everything about default arguments and just want to laugh at the funny error feel free to jump to the end of the article. Alas, the code was written by me, but I'm pretty sure that day I was replaced by an evil clone of myself. You know, it happens sometimes. =)

This post just summarizes the basics about standard and default arguments in Python functions and warns against possible pitfalls in your code. If you are just approaching Python and started writing functions I warmly recommend checking what the official Python manual says about them at the following links: [Defining Functions](https://docs.python.org/3/tutorial/controlflow.html#defining-functions) and [More on Defining Functions](https://docs.python.org/3/tutorial/controlflow.html#more-on-defining-functions).

## A quick review of functions

Python is a strongly object-oriented language, pushing this programming paradigm to its maximum. But object-oriented programming still relies on the _function_ concept, that is something you can use to process data. Python has a more general concept of _callable_ object, that is every object that can be called, which means _applied_ on some data.

Functions in Python are callable objects and at a first glance they behave the same as functions in other languages. They just get some data, called _arguments_, process them, and return a _result_ (which is `None` if no `return` statement is issued).

Arguments are declared (when the function is defined) as placeholders for actual objects that will be _passed_ when the function will be called. In Python you do not declare the type of an argument (like you do in C or Java, for example) since Python philosophy relies on polymorphism.

Remember that Python variables are _references_, that is **memory addresses of actual variables**. This means that Python functions always work like being "passed by address" (using a C/C++ nomenclature). When you call a function you are not _copying_ the value of the arguments to the function placeholders. Instead, you are pointing the placeholder to the variable itself. This has a very important consequence: you may alter the value of the variable from inside the function. A good visual explanation of the reference mechanism in Python is [this](http://python.net/~goodger/projects/pycon/2007/idiomatic/handout.html#other-languages-have-variables).

References play a very big role in Python, being the bare bones of its fully-polymorphic approach. Check [this link](/blog/2014/08/21/python-3-oop-part-4-polymorphism) for a better explanation of this very important subject.

To check your understanding of this basic behaviour of the language try to follow this simple code (where the variable _ph_ stands for "placeholder")

``` pycon
>>> def print_id(ph):
...  print(hex(id(ph)))
... 
>>> a = 5
>>> print(hex(id(a)))
0x84ab460
>>> print_id(a)
0x84ab460
>>> 
>>> def alter_value(ph):
...  ph = ph + 1
...  return ph
... 
>>> b = alter_value(a)
>>> b
6
>>> a
5
>>> hex(id(a))
'0x84ab460'
>>> hex(id(b))
'0x84ab470'
>>> 
>>> def alter_value(ph):
...  ph.append(1)
...  return ph
... 
>>> a = [1,2,3]
>>> b = alter_value(a)
>>> a
[1, 2, 3, 1]
>>> b
[1, 2, 3, 1]
>>> hex(id(a))
'0xb701f72c'
>>> hex(id(b))
'0xb701f72c'
>>>
```

If you haven't been surprised by what's happening here you already grasped one of the most important things in Python and can safely skip the following explanation.

The `print_id()` function demonstrates that placeholders inside functions are the very same variable passed at runtime (their memory addresses match).

The two versions of `alter_value()` are meant to be functions that try to change the value of the passed argument. As you can see the first version of `alter_value()` does not succeed in changing the value of the variable `a`, while the second version does. Why? Indeed both act the same, trying to modify the value of the original variable passed, but some types in Python are _immutable_, and integers are among them. On the other hand, lists are not immutable, so the function does what its name pledges. ([here](https://docs.python.org/3.4/reference/datamodel.html) you find more details on immutable types).

There is a lot more to say about functions in Python, but these are the basic bricks about standard arguments.

## Default argument values

Every now and then you need to define a function that _may_ accept an argument and shall behave differently whether or not the argument is present. If a language does not provide support for such cases you only have two choices: the first one is to define two different functions and to decide which is the most suitable one to call every time, the second one is to force the presence of the argument, selecting a "null" value that signals that the argument must not be used (such as `0` or `None`, for example). Both solutions are viable but suboptimal.

Python, like other languages, provides support for default argument values, that is function arguments that can either be specified by the caller or left blank to automatically receive a predefined value.

A very simple (and rather useless) example of default value is the following:

```python
def log(message=None):
    if message:
        print("LOG: {0}".format(message))
```

This function may be run with an argument (which can be `None`)

``` pycon
>>> log("File closed")
LOG: File closed
>>> log(None)
>>>
```

but can also be called _without_ arguments, in which case it will receive the default value set in its prototype (`None`, in this case)

``` pycon
>>> log()
>>> 
```

You can find more interesting examples in the standard library, for example in the `open()` function (check the [official documentation](https://docs.python.org/3.4/library/functions.html#open))

``` python
open(file, mode='r', buffering=-1, encoding=None, errors=None, newline=None, closefd=True, opener=None)
```

It's evident from the prototype that a call like `f = open('/etc/hosts')` hides a lot of arguments by passing them default values (`mode`, `buffering`, `encoding`, etc), and making the function very simple to use in its standard use case.

As you see from the `open()` builtin function we can use both standard and default arguments in a function, but the order in which they appear in the function prototype is fixed: all standard arguments first, then the default ones.

``` python
def a_rich_function(a, b, c, d=None, e=0):
    pass
```

The reason is easy to figure out: if we put an argument with a default value before a standard one, the language has no way to understand if the argument  with default value has been initialized or not. For example consider the following function definition

``` python
def a_rich_function(a, b, d=None, c, e=0):
    pass
```

when calling the function `a_rich_function(1, 2, 4, 5)` what arguments are we passing? Is `d=4, c=5` or `c=4, e=5` since `d` has a default value? That order of definition is thus forbidden, and if you try Python will raise a `SyntaxError`.

``` pycon
>>> def a_rich_function(a, b, d=None, c, e=0):
...  pass
... 
  File "<stdin>",  line 1
SyntaxError: non-default argument follows default argument
>>>
```

## Default arguments evaluation

Default arguments may be provided as plain values or as the result of a function call, but this latter technique need a **very big warning**.

While plain values are hardcoded, thus needing no evaluation except that made at compilation time, function calls are expected to be executed at run time (check [this comment on Reddit](http://www.reddit.com/r/Python/comments/2viygh/default_arguments_in_python/coii8bn?context=3) for a better explanation of this matter). So we could write

``` python
import datetime as dt

def log_time(message, time=dt.datetime.now()):
    print("{0}: {1}".format(time.isoformat(), message))
```

expecting the `log_time()` function to correctly provide the current time each time we call it. This unfortunately does not work: default arguments are evaluated at definition time (for example when you first import the module), and the result of successive calls is

``` pycon
>>> log_time("message 1")
2015-02-10T21:20:32.998647: message 1
>>> log_time("message 2")
2015-02-10T21:20:32.998647: message 2
>>> log_time("message 3")
2015-02-10T21:20:32.998647: message 3
```

If you set the default value to a class instance the result may be even stranger, as you can read in [The Hitchhiker’s Guide to Python!](http://docs.python-guide.org/en/latest/writing/gotchas/). As suggested by this latter resource the usual solution is to replace the default value with `None` and to check the value of the argument inside the function

``` python
import datetime as dt

def log_time(message, time=None):
    if time is None:
        time=dt.datetime.now()
    print("{0}: {1}".format(time.isoformat(), message))
```

## Conclusions

Default arguments may vastly simplify APIs, provided that you pay attention to their only "failure point", the evaluation time. Surprisingly, one of the most basic things in Python, function arguments and references, are one of the biggest source of errors, sometimes for experienced programmers too. I recommend giving references and polymorphism some study time.

## Updates

2015-06-10: [brandjon](http://www.reddit.com/user/brandjon) added some useful information [here](http://www.reddit.com/r/Python/comments/2viygh/default_arguments_in_python/coii8bn?context=3), explaining how CPython deals with plain values and functions. I added the link to the commented section.