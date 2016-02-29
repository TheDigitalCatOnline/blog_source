Title: Python 3 OOP Part 1 - Objects and types
Date: 2014-08-20 13:00:00 +0200
Category: Programming
Tags: Python, Python3, OOP
Authors: Leonardo Giordani
Slug: python-3-oop-part-1-objects-and-types
Series: "Python 3 OOP"
Summary:

This post is available as an **IPython Notebook** [here](/notebooks/Python_3_OOP_Part_1__Objects_and_types.ipynb)

## About this series

Object-oriented programming (OOP) has been the leading programming paradigm for several decades now, starting from the initial attempts back in the 60s to some of the most important languages used nowadays. Being a set of programming concepts and design methodologies, OOP can never be said to be "correctly" or "fully" implemented by a language: indeed there are as many implementations as languages.

So one of the most interesting aspects of OOP languages is to understand how they implement those concepts. In this post I am going to try and start analyzing the OOP implementation of the Python language. Due to the richness of the topic, however, I consider this attempt just like a set of thoughts for Python beginners trying to find their way into this beautiful (and sometimes peculiar) language.

This series of posts wants to introduce the reader to the Python 3 implementation of Object Oriented Programming concepts. The content of this and the following posts will not be completely different from that of the previous "OOP Concepts in Python 2.x" series, however. The reason is that while some of the internal structures change a lot, the global philosophy doesn't, being Python 3 an _evolution_ of Python 2 and not a new language.

So I chose to split the previous series and to adapt the content to Python 3 instead of posting a mere list of corrections. I find this way to be more useful for new readers, that otherwise sould be forced to read the previous series.

### Print

One of the most noticeable changes introduced by Python 3 is the transformation of the `print` keyword into the `print()` function. This is indeed a very small change, compared to other modifications made to the internal structures, but is the most visual-striking one, and will be the source of 80% of your syntax errors when you will start writing Python 3 code.

Remember that print is now a function so write `print(a)` and not `print a`.

## Back to the Object

Computer science deals with data and with procedures to manipulate that data. Everything, from the earliest Fortran programs to the latest mobile apps is about data and their manipulation. 

So if data are the ingredients and procedures are the recipes, it seems (and can be) reasonable to keep them separate.

Let's do some procedural programming in Python

``` python
# This is some data
data = (13, 63, 5, 378, 58, 40)

# This is a procedure that computes the average
def avg(d):
    return sum(d)/len(d)
    
print(avg(data))
```

As you can see the code is quite good and general: the procedure (function) operates on a sequence of data, and it returns the average of the sequence items. So far, so good: computing the average of some numbers leaves the numbers untouched and creates new data.

The observation of the everyday world, however, shows that _complex data mutate_: an electrical device is on or off, a door is open or closed, the content of a bookshelf in your room changes as you buy new books.

You can still manage it keeping data and procedures separate, for example

``` python
# These are two numbered doors, initially closed
door1 = [1, 'closed']
door2 = [2, 'closed']

# This procedure opens a door
def open_door(door):
    door[1] = 'open'
    
open_door(door1)
print(door1)
```

I described a door as a structure containing a number and the status of the door (as you would do in languages like LISP, for example). The procedure knows how this structure is made and may alter it.

This also works like a charm. Some problems arise, however, when we start building specialized types of data. What happens, for example, when I introduce a "lockable door" data type, which can be opened only when it is not locked? Let's see

``` python
# These are two standard doors, initially closed
door1 = [1, 'closed']
door2 = [2, 'closed']

# This is a lockable door, initially closed and unlocked
ldoor1 = [1, 'closed', 'unlocked']

# This procedure opens a standard door
def open_door(door):
    door[1] = 'open'

# This procedure opens a lockable door
def open_ldoor(door):
    if door[2] == 'unlocked':
        door[1] = 'open'

open_door(door1)
print(door1)

open_ldoor(ldoor1)
print(ldoor1)
```

Everything still works, no surprises in this code. However, as you can see, I had to find a different name for the procedure that opens a locked door since its implementation differs from the procedure that opens a standard door. But, wait... I'm still opening a door, the action is the same, and it just changes the status of the door itself. So why shall I remember that a locked door shall be opened with `open_ldoor()` instead of `open_door()` if the verb is the same?

Chances are that this separation between data and procedures doesn't perfectly fit some situations. The key problem is that the "open" action is not actually _using_ the door; rather it is _changing its state_. So, just like the volume control buttons of your phone, which are _on_ your phone, the "open" procedure should stick to the "door" data.

This is exactly what leads to the concept of _object_: an object, in the OOP context, is a structure holding data _and_ procedures operating on them.

## What About Type?

When you talk about data you immediately need to introduce the concept of _type_. This concept may have two meanings that are worth being mentioned in computer science: the _behavioural_ and the _structural_ one.

The behavioural meaning represents the fact that you know what something is by describing how it acts. This is the foundation of the so-called "duck typing" (here "typing" means "to give a type" and not "to type on a keyboard"): if it <del>types</del> acts like a duck, it _is_ a duck.

The structural meaning identifies the type of something by looking at its internal structure. So two things that act in the same way but are internally different are of different type.

Both points of view can be valid, and different languages may implement and emphasize one meaning of type or the other, and even both.

## Class Games

Objects in Python may be built describing their structure through a _class_. A class is the programming representation of a generic object, such as "a book", "a car", "a door": when I talk about "a door" everyone can understand what I'm saying, without the need of referring to a specific door in the room.

In Python, the type of an object is represented by the class used to build the object: that is, in Python the word _type_ has the same meaning of the word _class_.

For example, one of the built-in classes of Python is `int`, which represents an integer number

``` pycon
>>> a = 6
>>> print(a)
6
>>> print(type(a))
<class 'int'>
>>> print(a.__class__)
<class 'int'>
```

As you can see, the built-in function `type()` returns the content of the _magic attribute_ `__class__` (magic here means that its value is managed by Python itself offstage). The type of the variable `a`, or its class, is `int`. (This is a very inaccurate description of this rather complex topic, so remember that at the moment we are just scratching the surface).

Once you have a class you can _instantiate_ it to get a concrete object (an _instance_) of that type, i.e. an object built according to the structure of that class. The Python syntax to instantiate a class is the same of a function call

``` pycon
>>> b = int()
>>> type(b)
<class 'int'>
```

When you create an instance, you can pass some values, according to the class definition, to _initialize_ it.

``` pycon
>>> b = int()
>>> print(b)
0
>>> c = int(7)
>>> print(c)
7
```

In this example, the `int` class creates an integer with value 0 when called without arguments, otherwise it uses the given argument to initialize the newly created object.

Let us write a class that represents a door to match the procedural examples done in the first section

``` python
class Door:
    def __init__(self, number, status):
        self.number = number
        self.status = status
        
    def open(self):
        self.status = 'open'
        
    def close(self):
        self.status = 'closed'
```

The `class` keyword defines a new class named `Door`; everything indented under `class` is part of the class. The functions you write inside the object are called _methods_ and don't differ at all from standard functions; the nomenclature changes only to highlight the fact that those functions now are part of an object.

Methods of a class must accept as first argument a special value called `self` (the name is a convention but please never break it).

The class can be given a special method called `__init__()` which is run when the class is instantiated, receiving the arguments passed when calling the class; the general name of such a method, in the OOP context, is _constructor_, even if the `__init__()` method is not the only part of this mechanism in Python.

The `self.number` and `self.status` variables are called _attributes_ of the object. In Python, methods and attributes are both _members_ of the object and are accessible with the dotted syntax; the difference between attributes and methods is that the latter can be called (in Python lingo you say that a method is a _callable_).

As you can see the `__init__()` method shall create and initialize the attributes since they are not declared elsewhere. This is very important in Python and is strictly linked with the way the language handles the type of variables. I will detail those concepts when dealing with polymorphism in a later post.

The class can be used to create a concrete object

``` pycon
>>> door1 = Door(1, 'closed')
>>> type(door1)
<class '__main__.Door'>
>>> print(door1.number)
1
>>> print(door1.status)
closed
```

Now `door1` is an instance of the `Door` class; `type()` returns the class as `__main__.Door` since the class was defined directly in the interactive shell, that is in the current main module.

To call a method of an object, that is to run one of its internal functions, you just access it as an attribute with the dotted syntax and call it like a standard function.

``` pycon
>>> door1.open()
>>> print(door1.number)
1
>>> print(door1.status)
open
```

In this case, the `open()` method of the `door1` instance has been called. No arguments have been passed to the `open()` method, but if you review the class declaration, you see that it was declared to accept an argument (`self`). When you call a method of an instance, Python automatically passes the instance itself to the method as the first argument.

You can create as many instances as needed and they are completely unrelated each other. That is, the changes you make on one instance do not reflect on another instance of the same class.

## Recap

Objects are described by a _class_, which can generate one or more _instances_, unrelated each other. A class contains _methods_, which are functions, and they accept at least one argument called `self`, which is the actual instance on which the method has been called. A special method, `__init__()` deals with the initialization of the object, setting the initial value of the _attributes_.

## Movie Trivia

Section titles come from the following movies: _Back to the Future (1985)_ , _What About Bob? (1991)_, _Wargames (1983)_.

## Sources

You will find a lot of documentation in [this Reddit post](http://www.reddit.com/r/Python/comments/226ahl/some_links_about_python_oop/). Most of the information contained in this series come from those sources.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Next post

[Python 3 OOP Part 2 - Classes and members](/blog/2014/08/20/python-3-oop-part-2-classes-and-members)
