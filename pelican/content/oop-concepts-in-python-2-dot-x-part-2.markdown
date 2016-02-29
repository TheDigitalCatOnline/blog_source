Title: OOP concepts in Python 2.x - Part 2
Date: 2014-03-10 18:56:56 +0100
Category: Programming
Tags: Python2, Python, OOP
Authors: Leonardo Giordani
Slug: oop-concepts-in-python-2-dot-x-part-2
Series: "OOP concepts in Python 2.x"
Summary:

## Abstract

This post continues the analysis of the Python OOP implementation started with [this post](/blog/2014/03/05/oop-concepts-in-python-2-dot-x-part-1), which I recommend reading before taking on this new one.

This second post discusses the following OOP features in Python:

* Polymorphism
* Classes and instances (again)
* Metaclasses
* Object creation

This post refers to the internals of Python 2.x - please note that Python 3.x changes (improves!) some of the features shown here. You can find the **updated version** [here](/categories/python3/).

## Good Morning, Polymorphism

The term _polymorphism_, in the OOP lingo, refers to the ability of an object to adapt the code to the type of the data it is processing.

Polymorphism has two major applications in an OOP language. The first is that an object may provide different implementations of one of its methods depending on the type of the input parameters. The second is that code written for a given type of data may be used on data with a derived type, i.e. methods understand the class hierarchy of a type.

In Python polymorphism is one of the key concepts, and we can say that it is a built-in feature. Let us deal with it step by step.

First of all, you know that in Python the type of a variable is not explicitly declared. Beware that this does not mean that Python variables are _untyped_. On the contrary, everything in Python has a type, it just happens that the type is implicitly assigned. If you remember the last paragraph of the previous post, I stated that in Python variables are just pointers (using a C-like nomenclature), in other words they just tell the language _where_ in memory a variable has been stored. What is stored at that address is not a business of the variable.

``` pycon
>>> a = 5
>>> a
5
>>> type(a)
<type 'int'>
>>> hex(id(a))
'0x89812b0'
>>> a = "five"
>>> a
'five'
>>> type(a)
<type 'str'>
>>> hex(id(a))
'0xb74bb280L'
```

This little example shows a lot about the Python typing system. The variable `a` is not statically declared, after all it can contain only one type of data: a memory address. When we assign the number 5 to it, Python stores in `a` the _address_ of the number 5 (`0x89812b0` in my case, but your result will be different). The `type()` built-in function is smart enough to understand that we are not asking about the type of `a` (which is always a reference), but about the type of the content. When you store another value in `a`, the string `"five"`, Python shamelessly replaces the previous content of the variable with the new address.

So, thanks to the reference system, Python type system is both _strong_ and _dynamic_. The exact definition of those two concepts is not universal, so if you are interested be ready to dive into a broad matter. However, in Python, the meaning of those two words is the following:

* type system is _strong_ because everything has a well-defined type, that you can check with the `type()` built-in
* type system is _dynamic_ since the type of a variable is not explicitly declared, but changes with the content

Onward! We just scratched the surface of the whole thing.

To explore the subject a little more, try to define the simplest function in Python (apart from an empty function)

``` python
def echo(a):
    return a
```

Pretty straightforward, isn't it? Well, if you come from a statically compiled language such as C or C++ you should be at least puzzled. What is `a`? I mean: what type of data does it contain? Moreover, how can Python know what it is returning if there is no type specification?

Again, if you recall the references stuff everything becomes clear: that function accepts a reference and returns a reference. In other words we just defined a sort of universal function, that does the same thing regardless of the input.

This is exactly the problem that polymorphism wants to solve. We want to describe an action regardless of the type of objects, and this is what we do when we talk among humans. When you describe how to move an object by pushing it, you may explain it using a box, but you expect the person you are addressing to be able to repeat the action even if you need to move a pen, or a book, or a bottle.

There are two main strategies you can apply to get code that performs the same operation regardless of the input types.

The first approach is to cover all cases, and this is a typical approach of procedural languages. If you need to sum two numbers that can be integers, float or complex, you just need to write three `sum()` functions, one bound to the integer type, the second bound to the float type and the third bound to the complex type, and to have some language feature that takes charge of choosing the correct implementation depending on the input type. This logic can be implemented by a compiler (if the language is statically typed) or by a runtime environment (if the language is dynamically typed) and is the approach chosen by C++. The disadvantage of this solution is that it requires the programmer to forecast all the possible situations: what if I need to sum an integer with a float? What if I need to sum two lists? (Please note that C++ is not so poorly designed, and the operator overloading technique allows to manage such cases, but the base polymorphism strategy of that language is the one exposed here).

The second strategy, the one implemented by Python, is simply to require the input objects to solve the problem for you. In other words you _ask the data itself to perform the operation_, reversing the problem. Instead of writing a bunch on functions that sum all the possible types in every possible combination you just write one function that requires the input data to sum, trusting that they know how to do it. Does it sound complex? It is not.

Let's look at the Python implementation of the `+` operator. When we write `c = a + b`, Python actually executes `c = a.__add__(b)`. As you can see the sum operation is delegated to the first input variable. So if we write

``` python
def sum(a, b):
    return a + b
```

there is no need to specify the type of the two input variables. The object `a` (the object contained in the variable `a`) shall be able to sum with the object `b`. This is a very beautiful and simple implementation of the polymorphism concept. Python functions are polymorphic simply because they accept everything and trust the input data to be able to perform some actions.

Let us consider another simple example before moving on. The built-in `len()` function returns the length of the input object. For example

``` pycon
>>> l = [1, 2, 3]
>>> len(l)
3
>>> s = "Just a sentence"
>>> len(s)
15
```

As you can see it is perfectly polymorphic: you can feed both a list or a string to it and it just computes its length. Does it work with any type? let's check

``` pycon
>>> d = {'a': 1, 'b': 2}
>>> len(d)
2
>>> i = 5
>>> len(i)
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: object of type 'int' has no len()
```

Ouch! Seems that the `len()` function is smart enough to deal with dictionaries, but not with integers. Well, after all, the length of an integer is not defined.

Indeed this is exactly the point of Python polymorphism: _the integer type does not define a length operation_. While you blame the `len()` function, the `int` type is at fault. The `len()` function just calls the `__len__()` method of the input object, as you can see from this code

``` pycon
>>> l.__len__()
3
>>> s.__len__()
15
>>> d.__len__()
2
>>> i.__len__()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'int' object has no attribute '__len__'
```

but the `'int' object` does not define any `__len__()` method.

So, to sum up what we discovered until here, I would say that _Python polymorphism is based on delegation_. In the following sections we will talk about the [EAFP](http://docs.python.org/2/glossary.html#term-eafp) Python principle, and you will see that the delegation principle is somehow ubiquitous in this language.

## Type Hard

Another real-life concept that polymorphism wants to bring into a programming language is the ability to walk the class hierarchy, that is _to run code on specialized types_. This is a complex sentence to say something we are used to do every day, and an example will clarify the matter.

You know how to open a door, it is something you learned in your early years. Under an OOP point of view you are an object (sorry, no humiliation intended) which is capable of interacting with a wood rectangle rotating on hinges. When you can open a door, however, you can also open a window, which, after all, is a specialized type of wood-rectangle-with-hinges, hopefully with some glass in it too. You are also able to open the car door, which is also a specialized type (this one is a mix between a standard door and a window). This shows that, once you know how to interact with the most generic type (basic door) you can also interact with specialized types (window, car door) as soon as they act like the ancestor type (e.g. as soon as they rotate on hinges).

This directly translates into OOP languages: polymorphism requires that _code written for a given type may also be run on derived types_. For example, a list (a generic list object, not a Python one) that can contain "numbers" shall be able to accept integers because they _are_ numbers. The list could specify an ordering operation which requires the numbers to be able to compare each other. So, as soon as integers specify a way to compare each other they can be inserted into the list and ordered.

Statically compiled languages shall provide specific language features to implement this part of the polymorphism concept. In C++, for example, the language needs to introduce the concept of pointer compatibility between parent and child classes.

In Python there is no need to provide special language features to implement subtype polymorphism. As we already discovered Python functions accept any variable without checking the type and rely on the variable itself to provide the correct methods. But you already know that a subtype must provide the methods of the parent type, either redefining them or through implicit delegation, so as you can see Python implements subtype polymorphism from the very beginning.

I think this is one of the most important things to understand when working with this language. Python is not really interested in the actual type of the variables you are working with. It is interested in how those variables act, that is it just wants the variable _to provide the right methods_. So, if you come from statically typed languages, you need to make a special effort to think about _acting like_ instead of _being_. This is what we called "duck typing".

Time to do an example. Let us define a `Room` class

``` python
class Room(object):
    def __init__(self, door):
        self.door = door
        
    def open(self):
        self.door.open()

    def close(self):
        self.door.close()

    def is_open(self):
        return self.door.is_open()
```

A very simple class, as you can see, just enough to exemplify polymorphism. The `Room` class accepts a `door` variable, and the type of this variable is not specified. Duck typing in action: the actual type of `door` is not declared, there is no "acceptance test" built in the language. Indeed, the incoming variable shall export the following methods that are used in the `Room` class: `open()`, `close()`, `is_open()`. So we can build the following classes

``` python
class Door(object):
    def __init__(self):
        self.status = "closed"

    def open(self):
        self.status = "open"

    def close(self):
        self.status = "closed"

    def is_open(self):
        return self.status == "open"


class BooleanDoor(object):
    def __init__(self):
        self.status = True

    def open(self):
        self.status = True

    def close(self):
        self.status = False

    def is_open(self):
        return self.status
```

Both represent a door that can be open or closed, and they implement the concept in two different ways: the first class relies on strings, while the second leverages booleans. Despite _being_ two different types, both _act_ the same way, so both can be used to build a `Room` object.

``` pycon
>>> door = Door()
>>> bool_door = BooleanDoor()
>>> room = Room(door)
>>> bool_room = Room(bool_door)

>>> room.open()
>>> print room.is_open()
True
>>> room.close()
>>> print room.is_open()
False

>>> bool_room.open()
>>> print bool_room.is_open()
True
>>> bool_room.close()
>>> print bool_room.is_open()
False
```

#### File Like Us

File-like objects are a concrete and very useful example of polymorphism in Python. A file-like object is a class (or the instance of a class) that acts like a file, i.e. it provides those methods a file object exposes.

Say for example that you code a class that parses an XML tree, and that you expect the XML code to be contained in a file. So your class accepts a file in its `__init__()` method, and reads the content from it

``` python
class XMLReader(object):
    def __init__(xmlfile):
        xmlfile.open()
        self.content = xmlfile.read()
        xmlfile.close()

[...]
```

The class works well until your application shall be modified to receive XML content from a network stream. To use the class without modifying it you shall write the stream in a temporary file and load this latter, but this sounds a little overkill. So you plan to change the class to accept a string, but this way you shall change every single code that uses the class to read a file, since now you shall open, read and close the file on your own, outside the class.

Polymorphism offers a better way. Why not store the incoming stream inside an object that _acts like_ a file, even if it is not an actual one? If you check the StringIO module you will find that such an object has been already invented and provided in the standard Python library.

Other very useful file-like classes are those contained in the `gzip`, `bz2`, and `zipfile` modules (just to name some of the most used), which provide objects that allow you to manage compressed files just like plain files, hiding the decompression/compression machinery.


## Unforgiveness

EAFP is a Python acronym that stands for _easier to ask for forgiveness than permission_. This coding style is highly pushed in the Python community because it completely relies on the duck typing concept, thus fitting well with the language philosophy.

The concept behind EAFP is fairly easy: instead of checking if an object has a given attribute or method before actually accessing or using it, just trust the object to provide what you need and manage the error case. This can be probably better understood by looking at some code. According to EAFP, instead of writing

``` python
if hasattr(someobj, 'open'):
    [...]
else:
    [...]
```

you shall write

``` python
try:
    someobj.open()
    [...]
except AttributeError:
    [...]
```

As you can see, the second snippet directly uses the method and deals with the possible `AttributeError` exception (by the way: managing exceptions is one of the top Black Magic Topics in Python, more on it in a future post. A very quick preview: I think we may learn something from Erlang - check [this](/blog/2013/05/30/error-handling-in-erlang-a-primer/)).

Why is this coding style pushed so much in the Python community? I think the main reason is that through EAFP you _think_ polymorphically: you are not interested in knowing if the object _has_ the `open` attribute, you are interested in knowing if the object can satisfy your request, that is to perform the `open()` method call.

## Intermezzo

Are you still with me? Good. Now go, make yourself a cup of tea and fasten belts: the Python roller coaster is about to start.

We will leave the polymorphism palace for a while to explore other parts of the Python OOP world. Don't worry, however, it is just to lay some foundation before diving another time into the matter.

## The Type Brothers

The first step into the most intimate secrets of Python objects comes from two components we already met in the first post: `type` and `object`. These two things are the very fundamental elements of Python OOP system, so it is worth spending some time to understand how they work and relate each other.

First of all recall that in Python _everything is an object_, that is everything inherits from `object`. Thus, `object` seems to be the deepest thing you can find digging into Python variables. Let's check this

``` pycon
>>> a = 5
>>> type(a)
<type 'int'>
>>> a.__class__
<type 'int'>
>>> a.__class__.__bases__
(<type 'object'>,)
>>> object.__bases__
()
```

The variable `a` is an instance of the `int` class, and this latter inherits from `object`, which inherits from nothing. This demonstrates that `object` is at the top of the class hierarchy. However, as you can see, both `int` and `object` are called _types_ (`<type 'int'>`, `<type 'object'>`), which in Python is a pure alias of the word _class_. Indeed, while `a` is an instance of the `int` class, `int` itself is an instance of another class, _a class that is instanced to build classes_

``` pycon
>>> type(a)
<type 'int'>
>>> type(int)
<type 'type'>
>>> type(float)
<type 'type'>
>>> type(dict)
<type 'type'>
```

Since in Python everything is an object, everything is the instance of a class, even classes. Well, `type` is the class that is instanced to get classes. So remember this: `object` is the base of every object, `type` is the class of every type. Sounds puzzling? It is not your fault, don't worry. However, just to strike you with the finishing move, this is what Python is built on

``` pycon
>>> type(object)
<type 'type'>
>>> type.__bases__
(<type 'object'>,)
```

If you are not about to faint at this point chances are that you are Guido van Rossum of one of his friends down at the Python core development team (in this case let me thank you for your beautiful creation). You may get another cup of tea, if you need it.

Jokes apart, at the very base of Python type system there are two things, `object` and `type`, which are inseparable. The previous code shows that `object` is an instance of `type`, and `type` inherits from `object`. Take your time to understand this subtle concept, as it is very important for the upcoming discussion about metaclasses.

When you think you grasped the `type`/`object` matter read this and start thinking again

``` pycon
>>> type(type)
<type 'type'>
```

Hint: type is a class (i.e. a type). =)

## The Metaclasses Take Python

You are now familiar with Python classes. You know that a class is used to create an instance, and that the structure of this latter is ruled by the source class and all its parent classes (until you reach `object`).

Since classes are objects too, you know that a class itself is an instance of a (super)class, and this class is `type`. That is, as already stated, `type` is the class that is used to build classes.

So for example you know that a class may be instanced, i.e. it can be called and by calling it you obtain another object that is linked with the class. What prepares the class for being called? What gives the class all its methods? In Python the class in charge of performing such tasks is called _metaclass_, and `type` is the default metaclass of all classes.

The point of exposing this structure of Python objects is that you may change the way classes are built. As you know, `type` is an object, so it can be subclassed just like any other class. Once you get a subclass of `type` you need to instruct your class to use it as the metaclass instead of type, and you can do this by setting the `__metaclass__` attribute.

``` pycon
>>> class MyType(type):
...  pass
...
>>> class MySpecialClass(object):
...  __metaclass__ = MyType
... 
>>> msp = MySpecialClass()
>>> type(msp)
<class '__main__.MySpecialClass'>
>>> type(MySpecialClass)
<class '__main__.MyType'>
>>> type(MyType)
<type 'type'>
```

#### Metaclasses 2: Singleton Day

Metaclasses are a very advanced topic in Python, but they have many practical uses. For example, by means of a custom metaclass you may log any time a class is instanced, which can be important for applications that shall keep a low memory usage or have to monitor it.

I am going to show here a very simple example of metaclass, the Singleton. Singleton is a well known design pattern, and many description of it may be found on the Internet. It has also been heavily criticized mostly because its bad behaviour when subclassed, but here I do not want to introduce it for its technological value, but for its simplicity (so please do not question the choice, it is just an example). Check the links at the bottom if you are interested in this topic.

Singleton has one purpose: to return the same instance every time it is instanced, like a sort of object-oriented global variable. So we need to build a class that does not work like standard classes, which return a new instance every time they are called.

"Build a class"? This is a task for metaclasses. The following implementation comes from [Python 3 Patterns, Recipes and Idioms](http://python-3-patterns-idioms-test.readthedocs.org/en/latest/Metaprogramming.html#intercepting-class-creation), but is also valid for Python 2.x

``` python
class Singleton(type):
    instance = None
    def __call__(cls, *args, **kw):
        if not cls.instance:
             cls.instance = super(Singleton, cls).__call__(*args, **kw)
        return cls.instance
```

We are defining a new type, which inherits from `type` to provide all bells and whistles of Python classes. We override the `__call__` method, that is a special method invoked when we call the class, i.e. when we instance it. The new method wraps the original method of `type` by calling it only when the `instance` attribute is not set, i.e. the first time the class is instanced, otherwise it just returns the recorded instance. As you can see this is a very basic cache class, the only trick is that it is applied to the creation of instances.

To test the new type we need to define a new class that uses it as its metaclass

``` pycon
>>> class ASingleton(object):
...     __metaclass__ = Singleton
... 
>>> a = ASingleton()
>>> b = ASingleton()
>>> a is b
True
>>> hex(id(a))
'0xb6aae28cL'
>>> hex(id(b))
'0xb6aae28cL'
```

By using the `is` operator we test that the two objects are the very same structure in memory, that is their ids are the same, as explicitly shown. What actually happens is that when you issue `a = ASingleton()` the `ASingleton` class runs its `__call__()` method, which is taken from the `Singleton` type behind the class. That method recognizes that no instance has been created (`Singleton.instance` is `None`) and acts just like any standard class does. When you issue `b = ASingleton()` the very same things happen, but since `Singleton.instance` is now different from `None` its value (the previous instance) is directly returned.

Metaclasses are a very powerful programming tool and leveraging them you can achieve very complex behaviours with a small effort. Their use is a must every time you are actually metaprogramming, that is you are writing code that has to drive the way your code works. Good examples are creational patterns (injecting custom class attributes depending on some configuration), testing, debugging, and performance monitoring.

## Coming to Instance

Before introducing you to a very smart use of metaclasses by talking about Abstract Base Classes (read: to save some topics for the third part of this series), I want to dive into the object creation procedure in Python, that is what happens when you instance a class. In the previous post this procedure was described only partially, by looking at the `__init_()` method.

In the first post I recalled the object-oriented concept of _constructor_, which is a special method of the class that is automatically called when the instance is created. The class may also define a destructor, which is called when the object is destroyed. In languages without a garbage collection mechanism such as C++ the destructor shall be carefully designed. In Python the destructor may be defined through the `__del__()` method, but it is hardly used.

The constructor mechanism in Python is on the contrary very important, and it is implemented by two methods, instead of just one: `__new__()` and `__init__()`. The tasks of the two methods are very clear and distinct: `__new__()` shall perform actions needed when _creating_ a new instance while `__init__` deals with object _initialization_.

Since in Python you do not need to declare attributes due to its dynamic nature, `__new__()` is rarely defined by programmers, who may rely on `__init__` to perform the majority of the usual tasks. Typical uses of `__new__()` are very similar to those listed in the previous section, since it allows to trigger some code whenever your class is instanced.

The standard way to override `__new__()` is 

``` python
class MyClass(object):
    def __new__(cls, *args, **kwds):
        obj = super(MyClass, cls).__new__(cls, *args, **kwds)
        [put your code here]
        return obj
```

just like you usually do with `__init__()`. When your class inherits from `object` you do not need to call the parent method (`object.__init__()`) because it is empty.

Remember that `__new__()` is not forced to return an instance of the class in which it is defined, even if you shall have very good reasons to break this behaviour. Anyway, `__init__()` will be called only if you return an instance of the container class. Please also note that `__new__()`, unlike `__init__()`, accepts the class as its first parameter. The name is not important in Python, and you can also call it `self`, but it is worth using `cls` to remember that it is not an instance.

## Final words

Next post will introduce Abstract Base Classes as the major topic. Slots are another topic which for many people is buried into the Tome of Python Secrets and is worth uncovering. Stay tuned! You can [follow me on Twitter](https://twitter.com/tw_lgiordani/) if you want to get the latest news from the blog.

## Movie Trivia

This time section titles come from the following movies: _Good Morning, Vietnam_, _Die Hard_, _Spies Like Us_, _Unforgiven_, _The Blues Brothers_, _The Muppets Take Manhattan_, _Terminator 2: Judgement Day_, _Coming to America_

## Sources

Some sources for the content of this post. Thank you authors!

* [The official Python documentation](http://docs.python.org/2/tutorial/classes.html) and the Python source code.
* [Python Types and Objects - by Shalabh Chaturvedi](http://www.cafepy.com/article/python_types_and_objects/)
* [Python in a Nutshell - by Alex Martelli](http://www.amazon.com/Python-Nutshell-Second-Edition-In/dp/0596100469/)
* [Design Patterns: Elements of Reusable Object-Oriented Software - by Gamma, Helm, Johnson, Vlissides](http://www.amazon.com/Design-Patterns-Elements-Reusable-Object-Oriented/dp/0201633612/)
* [Python 3 Patterns, Recipes and Idioms](http://python-3-patterns-idioms-test.readthedocs.org/en/latest/)
* [Singleton and Borg patterns - by Alex Martelli](http://www.aleax.it/5ep.html)
* Many [Stackoverflow](http://stackoverflow.com/questions/tagged/python) questions and answers

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Previous articles

* [OOP Concepts in Python 2.x - Part 1](/blog/2014/03/05/oop-concepts-in-python-2-dot-x-part-1)

## Next articles

* [OOP Concepts in Python 2.x - Part 3](/blog/2014/07/04/oop-concepts-in-python-2-dot-x-part-3)


