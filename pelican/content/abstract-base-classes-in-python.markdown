Title: Abstract Base Classes in Python
Date: 2016-04-03 11:00:00 +0100
Category: Programming
Tags: decorators, metaclasses, metaprogramming, Notebook, OOP, Python, Python2, Python3
Authors: Leonardo Giordani
Slug: abstract-base-classes-in-python
Image: abstract-base-classes
Summary: What are Abstract Base Classes in Python and why are they useful?

With the introduction of Abstract Base Classes, Python once again shows its nature of a very innovative and flexible language. It is interesting to see how such a remarkable feature has been introduced into the language by a pure Python module. This demonstrates that Python is built in a way that is very open to changes, thanks to its foundations in pure polymorphism based on delegation.

Many Python programmers overlooked Abstract Base Classes and the classes in the `collections` module, which are one of the simplest and useful applications of the concept. Sure enough, this is not a feature that you will use every day or that will change the way you are programming in Python. But neither is it something you shall discard before understanding what it brings into the language, and what sort of problems it can solve for you.

## EAFP

Python is a dynamically-typed object-oriented language strongly based on delegation, so its approach to problems is intrinsically polymorphic. This means that Python deals mostly with the behaviour of objects and not with their structure. The well-known EAFP protocol (it's Easier to Ask Forgiveness than Permission) comes from this approach. This code

``` python
try:
	someobj[1]
except TypeError:
	# object is not subscriptable
	...
```

does not check if the object is a `list` or a `dictionary` (both would allow the `[1]` notation), but if the object can be accessed by key (or index). When you accept a parameter in a function Python does not specify the type (leaving aside type hints) because you are not interested in accepting a given type or one of its derived types. You are interested in accepting something that provides the methods you will use.

The behaviour, in an object-oriented environment, is the run-time interface of the object. This is different from the static interface, which is the collection of the methods provided by the object. The run-time interface is the actual interface the object shows when it is used, and this encompasses the methods provided by its class, but also methods provided by parent classes, the metaclass and other entry points provided by `__getattr__`.

## Complex checks

Sometimes, however, you need to perform complex checks, such as "it behaves like a list". How can you test this condition? You could test if the incoming object has some standard methods, but this is not only incomplete but also wrong. For example, I could write the following test

``` python
try:
    obj.append
    obj.count
    obj.extend
    obj.index
    obj.insert
    [...]
except AttributeError:
    [...]
```

which tries to cover the methods a list-like object shall provide. This test, however, accepts objects that do not really behave like a list such as

``` python
class FakeList:
    def fakemethod(self):
        pass
    
    def __getattr__(self, name):
        if name in ['append', 'count', 'extend', 'index', 'insert', ...]:
            return self.fakemethod
```

It is unlikely that you will write such a class, but this shows you one of the potential pitfalls of the previous test, which is wrong because it tries to rely on the structure instead of testing the behaviour. The temptation to rely on `isinstance()` is big

``` python
if isinstance(someobj, list):
	...
```

If possible this approach is worse than before, since it tests the exact type. Even if `isinstance()` and `issubclass()` are smart enough to walk through the hierarchy of parent classes, this excludes evary class that behaves like a list does not inherit from it.

## Back to delegation

The idea proposed in [PEP 3119](http://legacy.python.org/dev/peps/pep-3119/) to face this problem is very elegant, and leverages the very nature of Python: that of being strongly based on delegation. The solution, implemented in Python 3 and backported to Python 2.7, changes the nature of the two `isinstance()` and `issubclass()` builtins. Now the first thing that `isinstance()` does is to call the `__instancecheck__()` method of the queried class, basically giving it the chance to answer the call with a different algorithm than the standard one. The same happens for `issubclass()`, which becomes `__subclasscheck__()`. So the following code

``` python
issubclass(myclass, someclass)
```

does no more perform a pure external check of the relationship between `someclass` and `myclass`. The first thing that `issubclass()` does now is the following

``` python
someclass.__subclasscheck__(myclass)
```

This is very natural because, after all, `someclass` is the best source of judgement about being a subclass of itself.

## A new type of subclass

With the introduction of delegation-based instance and subclass checks, Python provides a new type of subclass, and thus a new way to relate classes together. Now a subclass may be a real subclass, obtained using inheritance

``` python
class ChildClass(ParentClass):
	pass
```

or can be a _virtual_ subclass, obtained through registration

``` python
ParentClass.register(ChildClass)
```

The difference between a real and a virtual subclass is very simple: a real subclass knows its relationship with the parent class through its `__bases__` attribute, and can thus implicitly delegate the resolution of missing methods. A virtual subclass knows nothing about the class that registered it, and nowhere in the subclass will you find something that links it to the parent class. Thus, a virtual parent class is useful only as a categorization.

## Abstract Base Classes

Classes that can register other classes, thus becoming virtual parents of those, are called in Python Abstract Base Classes, or ABCs.

The name of this new language element is important. ABCs are first of all classes, just like any other class you can create in Python, and they can be subclassed in the usual way to create taxonomies. They are also meant to be base classes, that is classes that represent fundamental behaviours or categories. Last, they are abstract. This has a very precise meaning in Python and will be the subject of the last part of this post.

The classes provided by the `collections` module are Abstract Base Classes, and they set themselves as virtual parents of some base types in the same module. If you check the `_collections_abc.py` file in your Python 3 installation (for example in `/usr/lib/python3.4/_collections_abc.py`) you will find code like this

``` python
[...]

Sequence.register(tuple)
Sequence.register(str)
Sequence.register(range)

[...]

MutableSequence.register(list)

[...]
```

Where the `Sequence` and the `MutableSequence` ABCs register some built-in types of Python.

It is very important to understand that registering a class does not imply any form of check about methods or attributes. Registering is just the _promise_ that a given behaviour is provided by the registered class.

To demonstrate this let me provide you a very simple example made using one of the `collections` classes

``` pycon
>>> import collections
>>> class MyClass():
...  pass
...
>>> issubclass(MyClass, collections.Sequence)
False
>>> collections.Sequence.register(MyClass)
<class '__main__.MyClass'>
>>> issubclass(MyClass, collections.Sequence)
True
>>>
```

As you can see, the `MyClass` class is initially not recognized as a subclass of `collections.Sequence`, but after the registration `issubclass()` returns `True`, even if the class is still empty.

## How to create ABCs

The example given by the official documentation is very simple and clear

``` python
from abc import ABCMeta

class MyABC(metaclass=ABCMeta):
	pass

MyABC.register(tuple)

assert issubclass(tuple, MyABC)
assert isinstance((), MyABC)
```

All you need to do is to create a class and use the `ABCMeta` metaclass provided by the `abc` module and you will obtain a class that has the `register()` method and a suitable implementation of `__subclasscheck__()` and `__instancecheck__()`. Checking again the `_collections_abc.py` file you can see that this is exactly the way the `collections` classes are implemented

``` python
[...]

class Hashable(metaclass=ABCMeta):
	...

class Iterable(metaclass=ABCMeta):
	...

[...]
```

## Are you scared of metaclasses?

Metaclasses are a strange topic in Python. Most of the times the advice given to the novice is "Don't use them", like they were an error of the language and something that shall be avoided.

I don't think so. As a matter of facts I definitely disagree with such position, for many reasons.

First of all, if you are programming in Python its better for you to understand everything Python provides you, both the good and the bad parts. Programming languages are tools, and you shall know their strengths and their limitations. Most of the times what we call "limitations" are just features that become a restraint just because we are not aware of them. The C language, for example, is not object-oriented. Is this a strength or a limitation? Python provides you a very powerful inspection mechanism. Is this a strength or a limitations? I could give countless other examples.

Second, powerful features are the one you should know better. After all, we use a language for the unique features it provides, not for the features it shares with other languages. I use Python because of its powerful polymorphism implementation, not because of loops and inheritance. Those are provided by Java and C++, too, for example. I write a device driver in C because of the closeness to the machine language and its speed, not because of the `int` and `float` types, which are provided by many other languages. So, since powerful features are what let the language do what others cannot, those are the ones you have to master.

Third, if a feature of a language is a design error, and it can be, you need to understand why it is an error, and how you can avoid using it. Before ES6, JavaScript had some issues with scopes, given by the behaviour of the `var` keyword. You cannot afford being ignorant about those limitations, otherwise your software will be buggy. So, while studying the `for` loop in JavaScript took me a couple of minutes (it is a C-like for loop, after all), I spent a lot of time in dealing with `var`, which is the dangerous button of the whole device.

Back to Python. Metaclasses are not a last-minute feature put into the language just for fun. They are the foundation of Python itself, and the relationship between `object` and `type` is something so beautiful that it is a pity that basically no one talks about it. So, plase stop complaining against metaclasses and telling people that they are dangerous or complex.

Metaclasses are part of the language. And they are not complex to understand.

## Why metaclasses for ABCs?

If you program in Python you should be somehow familiar with classes and instances. You know that when you build an instance you use a class (like a blueprint) and that the class can put things into the instance. For example

``` python
# Class definition
class Child():
	def __init__(self):
		self.answer = 42

# Link instance and class
c = Child()

# Use the instance
assert c.answer == 42
```

Now, when you build a class you use a metaclass (like a blueprint) and the metaclass can put things into the class.

``` python
# Metaclass definition
class NewType(type):
	def __init__(self, name, bases, namespace):
		self.answer = 42

# Link class and metaclass
class Child(metaclass=NewType): pass

# Use the class
assert Child.answer == 42
```

Sounds complex? Not at all, in my opinion. If you check the two examples you will see that they are exactly the same thing, the first referring to the instance-class relationship, the second to the class-metaclass one.

This is all you need to understand metaclasses. When you build a class you need to put things into it, for example you need to put the `__getattribute__` or the `__new__()` methods. This is done by the metaclass, which is usually `type` for every class. Indeed, if you check the `__class__` attribute into a class you get exactly this

``` pycon
>>> int
<class 'int'>
>>> int.__class__
<class 'type'>
>>>
```

## Metaclasses and MRO

A slightly advanced annotation: when I say that the metaclass _puts_ the methods into the class I'm simplifying the whole thing. As a matter of fact, like a class provides methods to the instance at runtime through the `__class__` attribute and the MRO protocol, the metaclass provides methods for the class. Attributes, instead, are put inside the class by the `__new__` or `__init__` methods of the metaclass.

Let us review the MRO mechanism for instances and classes first. When you call a method on an instance Python automatically looks for that method in the instance first, then in the parent class and in every class in its hierarchy.

So in this example

``` python
class GrandParent(): pass

class Parent(GrandParent): pass

class Child(Parent): pass
```

calling the method `get_name()` on an instance of `Child` will look for it first into the `Child` class, then into `Parent` and `GrandParent`, in this order. Finally, it will check `object`.

What happens to the MRO when a class of this hierarchy defines a different metaclass? For example

``` python
class NewType(type): pass

class GrandParent(): pass

class Parent(GrandParent): pass

class Child(Parent, metaclass=NewType): pass
```

In this case everything works as usual, but after checking `object` the MRO will also check the `NewType` metaclass (and its ancestors).

So, metaclasses can act as mixins, and they are queried only at the end of the usual MRO. This is exactly what happens using multiple inheritance if `NewType` were a standard parent class that does not have `Parent` or `GrandParent` as ancestors.

Metaclasses are not part of the MRO however, since the MRO just deals with standard inheritance. If you check the MRO of the `Child` class, you will see that the metaclass is not included

``` pycon
>>> Child.mro()
[<class '__main__.Child'>, <class '__main__.Parent'>, <class '__main__.GrandParent'>, <class 'object'>]
>>> Child.__class__
<class '__main__.NewType'>
>>> 
```

## Abstract methods

Why are ABC called `abstract`? ABCs can be instantiated, so they are after all not pure interfaces (like Java ones, for example)

``` pycon
>>> import abc
>>> class MyABC(metaclass=abc.ABCMeta):
...  pass
... 
>>> m = MyABC()
>>> 
```

They may however define some methods as _abstract_, using the `abc.abstractmethod` decorator, which prevents the class from being instantiated if the method is not implemented. Let me give you a simple example: I define an Abstract Base Class with and abstract method

``` python
class MyABC(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def get(self):
        pass
```

and try to instantiate it. Python complains

``` pycon
>>> m = MyABC()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: Can't instantiate abstract class MyABC with abstract methods get
```

I am forced to create a new class that inherits from `MyABC` and implements the method

``` python
class Concrete(MyABC):
    def get(self):
        return 1
```

Now I can instantiate the class.

``` pycon
>>> c = Concrete()
>>> 
```

Check the official documentation of Abstract Base Classes for a complete description of `@abstractmethod` and `@abstractproperty`.

## What about the behaviour?

So, what happened to the Python tenet "Check the behaviour and not the structure"? With collections, after all, we dropped the EAFP protocol, going back to a Look Before You Leap approach. Are we going against the philosophy at the very base of the language?

It is very interesting to see what Guido van Rossum, creator of the Python language, says about this in [PEP 3119](http://legacy.python.org/dev/peps/pep-3119/): _Invocation means interacting with an object by invoking its methods. Usually this is combined with polymorphism, so that invoking a given method may run different code depending on the type of an object. Inspection means the ability for external code (outside of the object's methods) to examine the type or properties of that object, and make decisions on how to treat that object based on that information. [...] In classical OOP theory, invocation is the preferred usage pattern, and inspection is actively discouraged, being considered a relic of an earlier, procedural programming style. However, in practice this view is simply too dogmatic and inflexible, and leads to a kind of design rigidity that is very much at odds with the dynamic nature of a language like Python._

So the point is that forcing the use of a pure polymorphic approach sometimes can lead to solutions that are too complex or even incorrect. The key words here, in my opinion, are "dogmatic", "inflexible", and "rigidity", opposed to "dynamic nature". I really like this flexibility in a language and in its author.

Writing `if isinstance(obj, collections.Sequence)` is not EAFP, neither is any conditional test you may write. Nevertheless, no one would replace conditional tests with a pure EAFP approach, simply because sometimes those tests are more readable. This is the exact purpose of collections in Python and ABCs in general: to allow parts of the code to be simpler.

## Final words

I hope this post helped you understand that Abstract Base Classes, and in particular the standard collections, are useful and easy to understand. Metaclasses are also not that scary and dangerous, even if using them obviously requires some skill.

## Sources

The official documentation of the `abc` module is very well written. [Here](https://docs.python.org/3.5/library/abc.html) you find the version for Python 3.5. I also suggest to read the original [PEP 3119](https://www.python.org/dev/peps/pep-3119) and the related [PEP 3141](https://www.python.org/dev/peps/pep-3141) for a deeper understanding of the topic.

## Feedback

The [GitHub issues](https://github.com/TheDigitalCatOnline/blog_source/issues) page is the best place to submit corrections.

