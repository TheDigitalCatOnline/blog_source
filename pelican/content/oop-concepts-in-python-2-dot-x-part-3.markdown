Title: OOP concepts in Python 2.x - Part 3
Date: 2014-07-04 13:00:00 +0200
Category: Programming
Tags: Python2, Python, OOP
Authors: Leonardo Giordani
Slug: oop-concepts-in-python-2-dot-x-part-3
Series: "OOP concepts in Python 2.x"
Summary:

## Abstract

Welcome to the third installment of this little series of posts about Python 2.x OOP implementation. The [first](/blog/2014/03/05/oop-concepts-in-python-2-dot-x-part-1) and [second](/blog/2014/03/10/oop-concepts-in-python-2-dot-x-part-2) issues introduced the most important concepts at the basis of Python as an object-oriented language.

This post will continue the discussion about metaclasses, introducing Abstract Base Classes, and give some insights on callable objects.

This post refers to the internals of Python 2.x - please note that Python 3.x changes (improves!) some of the features shown here. You can find the **updated version** [here](/categories/python3/).

## The Inspection Club

As you know, Python leverages polymorphism at its maximum by dealing only with generic references to objects. This makes OOP not an addition to the language but part of its structure from the ground up. Moreover, Python pushes the EAFP appoach, which tries to avoid direct inspection of objects as much as possible.

It is however very interesting to read what Guido van Rossum says in [PEP 3119](http://legacy.python.org/dev/peps/pep-3119/): _Invocation means interacting with an object by invoking its methods. Usually this is combined with polymorphism, so that invoking a given method may run different code depending on the type of an object. Inspection means the ability for external code (outside of the object's methods) to examine the type or properties of that object, and make decisions on how to treat that object based on that information. [...] In classical OOP theory, invocation is the preferred usage pattern, and inspection is actively discouraged, being considered a relic of an earlier, procedural programming style. However, in practice this view is simply too dogmatic and inflexible, and leads to a kind of design rigidity that is very much at odds with the dynamic nature of a language like Python._

The author of Python recognizes that forcing the use of a pure polymorphic approach leads sometimes to solutions that are too complex or even incorrect. In this section I want to show some of the problems that can arise from a pure polymorphic approach and introduce Abstract Base Classes, which aim to solve them. I strongly suggest to read [PEP 3119](http://legacy.python.org/dev/peps/pep-3119/) (as for any other PEP) since it contains a deeper and better explanation of the whole matter. Indeed I think that this PEP is so well written that any further explanation is hardly needed. I am however used to write explanations to check how much I understood about the topic, so I am going to try it this time too.

#### E.A.F.P the Extra Test Trial

The EAFP coding style requires you to trust the incoming objects to provide the attributes and methods you need, and to manage the possible exceptions, if you know how to do it. Sometimes, however, you need to test if the incoming object matches a complex behaviour. For example, you could be interested in testing if the object _acts_ like a list, but you quickly realize that the amount of methods a `list` provides is very big and this could lead to odd EAFP code like

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

where the methods of the `list` type are accessed (not called) just to force the object to raise the `AttributeError` exception if they are not present. This code, however, is not only ugly but also wrong. If you recall the "Enter the Composition" section of the [first post](/blog/2014/03/05/oop-concepts-in-python-2-dot-x-part-1), you know that in Python you can always customize the `__getattr__()` method, which is called whenever the requested attribute is not found in the object. So I could write a class that passes the test but actually does not act like a list

``` python
class FakeList(object):
    def fakemethod(self):
        pass
    
    def __getattr__(self, name):
        if name in ['append', 'count', 'extend', 'index', 'insert', ...]:
            return self.fakemethod
```

This is obviously just an example, and no one will ever write such a class, but this demonstrates that just accessing methods does not guarantee that a class _acts_ like the one we are expecting.

There are many examples that could be done leveraging the highly dynamic nature of Python and its rich object model. I would summarize them by saying that sometimes you'd better to check the type of the incoming object.

In Python you can obtain the type of an object using the `type()` built-in function, but to check it you'd better use `isinstance()`, which returns a boolean value. Let us see an example before moving on

``` pycon
>>> isinstance([], list)
True
>>> isinstance(1, int)
True
>>> class Door(object):
...  pass
... 
>>> d = Door()
>>> isinstance(d, Door)
True
>>> class EnhancedDoor(Door):
...  pass
... 
>>> ed = EnhancedDoor()
>>> isinstance(ed, EnhancedDoor)
True
>>> isinstance(ed, Door)
True
```

As you can see the function can also walk the class hierarchy, so the check is not so trivial like the one you would obtain by directly using `type()`.

The `isinstance()` function, however, does not completely solve the problem. If we write a class that actually _acts_ like a `list` but does not inherit from it, `isinstance()` does not recognize the fact that the two may be considered the same thing. The following code returns `False` regardless the content of the `MyList` class

``` pycon
>>> class MyList(object):
...  pass
... 
>>> ml = MyList()
>>> isinstance(ml, list)
False
```

since `isinstance()` does not check the content of the class or its behaviour, it just consider the class and its ancestors.

The problem, thus, may be summed up with the following question: what is the best way to test that an object exposes a given interface? Here, the word _interface_ is used for its natural meaning, without any reference to other programming solutions, which however address the same problem.

A good way to address the problem could be to write inside an attribute of the object the list of interfaces it promises to implement, and to agree that any time we want to test the behaviour of an object we simply have to check the content of this attribute. This is exactly the path followed by Python, and it is very important to understand that the whole system is just about a promised behaviour.

The solution proposed through PEP 3119 is, in my opinion, very simple and elegant, and it perfectly fits the nature of Python, where things are usually agreed rather than being enforced. Not only, the solution follows the spirit of polymorphism, where information is provided by the object itself and not extracted by the calling code.

In the next sections I am going to try and describe this solution in its main building blocks. The matter is complex so my explanation will lack some details: please refer to the forementioned PEP 3119 for a complete description.

#### Who Framed the Metaclasses

As already described, Python provides two built-ins to inspect objects and classes, which are `isinstance()` and `issubclass()` and it would be desirable that a solution to the inspection problem allows the programmer to go on with using those two functions.

This means that we need to find a way to inject the "behaviour promise" into both classes and instances. This is the reason why metaclasses come in play. If you recall what we said about them in the second issue of this series, metaclasses are the classes used to build classes, which means that they are the preferred way to change the structure of a class, and, in consequence, of its instances.

Another way to do the same job would be to leverage the inheritance mechanism, injecting the behaviour through a dedicated parent class. This solution has many downsides, which I'm am not going to detail. It is enough to say that affecting the class hierarchy may lead to complex situations or subtle bugs. Metaclasses may provide here a different entry point for the introduction of a "virtual base class" (as PEP 3119 specifies, this is not the same concept as in C++).

#### Overriding Places

As said, `isinstance()` and `issubclass()` are built-in functions, not object methods, so we cannot simply override them providing a different implementation in a given class. So the first part of the solution is to change the behaviour of those two functions to first check if the class or the instance contain a special method, which is `__instancecheck__()` for `isinstance()` and `__subclasscheck__()` for `issubclass()`. So both built-ins try to run the respective special method, reverting to the standard algorithm if it is not present.

A note about naming. Methods must accept the object they belong to as the first argument, so the two special methods shall have the form

``` python
def __instancecheck__(cls, inst):
   [...]
   
def __subclasscheck__(cls, sub):
   [...]
```

where `cls` is the class where they are injected, that is the one representing the promised behaviour. The two built-ins, however, have a reversed argument order, where the behaviour comes after the tested object: when you write `isinstance([], list)` you want to check if the `[]` instance has the `list` behaviour. This is the reason behind the name choice: just calling the methods `__isinstance__()` and `__issubclass__()` and passing arguments in a reversed order would have been confusing.

#### This is ABC

The proposed solution is thus called Abstract Base Classes, as it provides a way to attach to a concrete class a virtual class with the only purpose of signaling a promised behaviour to anyone inspecting it with `isinstance()` or `issubclass()`.

To help programmers implement Abstract Base Classes, the standard library has been given an `abc` module, thet contains the `ABCMeta` class (and other facilities). This class is the one that implements `__instancecheck__()` and `__subclasscheck__()` and shall be used as a metaclass to augment a standard class. This latter will then be able to register other classes as implementation of its behaviour.

Sounds complex? An example may clarify the whole matter. The one from the official documentation is rather simple:

``` python
from abc import ABCMeta

class MyABC:
    __metaclass__ = ABCMeta

MyABC.register(tuple)

assert issubclass(tuple, MyABC)
assert isinstance((), MyABC)
```

Here, the `MyABC` class is provided the `ABCMeta` metaclass. This puts the two `__instancecheck__()` and `__subclasscheck__()` behaviours inside `MyABC`. Please note however that those methods are not actually put into the object so that you can use them like

``` pycon
>>> d = {'a': 1}
>>> MyABC.__metaclass__.__instancecheck__(MyABC, d)
```

that returns `True` if the dictionary `d` is an instance of the Abstract Base Class `MyABC`. In other words if the dictionary `d` implements the behaviour promised by the `MyABC` class.

After the definition of `MyABC` we need a way to signal that a given class is an instance of the Abstract Base Class and this happens through the `register()` method, provided by the `ABCMeta` metaclass. Calling `MyABC.register(tuple)` we record inside `MyABC` the fact that the `tuple` class shall be identified as a subclass of `MyABC` itself. This is analogous to saying that `tuple` inherits from `MyABC` but not quite the same. As already said registering a class in an Abstract Base Class with `register()` does not affect the class hierarchy. Indeed, the whole `tuple` class is unchanged.

## Little Shop of Collections

In addition to the `abc` module, the standard library now provides a `collections` module that, besides some interesting container datatypes like `namedtuple` and `OrderedDict`, supplies a remarkable number of ABCs that represent container behaviours. An example is `collections.Sized` that pledges that the registered class will contain the `__len__()` method, enabling the code to pass it to the `len()` builtin. Let us exemplify that:

``` pycon
>>> class Snake(object):
...   def __init__(self, meters):
...     self.len = meters
...   def __len__(self):
...     return self.len
... 
>>> 
>>> s = Snake(5)
>>> len(s)
5
>>> 
>>> import collections
>>> collections.Sized.register(Snake)
>>> 
>>> issubclass(Snake, collections.Sized)
True
```

If not stressed enough, ABCs assure that a given behaviour will be implemented but there is no actual check of this. For example:

``` pycon
>>> class FakeSnake(object):
...   def __init__(self, meters):
...     pass
... 
>>> 
>>> collections.Sized.register(FakeSnake)
>>> issubclass(FakeSnake, collections.Sized)
True
>>> f = FakeSnake(6)
>>> len(f)
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: object of type 'FakeSnake' has no len()
```

Remember that ABCs are classes just like any other standard Python class, which means that they can be inherited to build more specialized ABCs.

#### The Naked Class

The `abc` module provides two decorators `@abstractmethod` and `@abstractproperty` to help the design of Abstract Base Classes. The first decorator, `@abstractmethod`, may only be used in a class which metaclass is `ABCMeta` (just like the example `MyABC` class described above). Decorating a method with it blocks the instantiation of the class itself until that method is overridden. That enables to write classes that may inherited only if the child class provides an implementation of some specific methods. The `@abstractproperty` decorator provides the same service for properties.

Let us run through a very simple example:

``` python
import abc

class AbstractCalculator(object):
    __metaclass__ = abc.ABCMeta
   
    @abc.abstractmethod
    def sum(a, b):
        pass

    @abc.abstractmethod
    def mutiply(a, b):
        pass
```

This is a class with `ABCMeta` as its metaclass, which makes `AbstractCalculator` an Abstract Base Class. The two `sum()` and `multiply()` methods are decorated with `@abstractmethod`, that raises an exception when we try to instance the class:

``` pycon
>>> ac = AbstractCalculator()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: Can't instantiate abstract class AbstractCalculator with abstract methods mutiply, sum
>>> 
```

Moreover, any class that derives from `AbstractCalculator` and does not override the two methods (implementing them), cannot be instantiated:

``` pycon
>>> class Calculator(AbstractCalculator):
...  pass
... 
>>> c = Calculator()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: Can't instantiate abstract class Calculator with abstract methods mutiply, sum
>>> 
```

These two decorators are the standard way to enforce a check about a behaviour or an interface, and are the closest thing to a "strict interface check".

## A Fish Called Object

In the first post, when I discussed for the first time the concept of instantiation, I stated that an instance of a class may be obtained by calling the class itself just like a function. Let me recall you the example

``` pycon
>>> b = int()
>>> type(b)
<type 'int'>
```

Later, speaking about methods and attributes, I defined _callable_ something that can be invoked with that syntax. Thus, we can now say that classes are callables, just like methods. What makes an object callable, anyway? It turns out that, as many other things in Python, the solution is pretty straightforward: an object is callable if it contains the `__call__()` method. Simple, isn't it?

When we execute a syntax like

``` pycon
>>> some_object()
```

Python executes under the hood the following code

``` pycon
>>> some_object.__call__()
```

where any parameter passed to the class is obviously passed to `__call__()`.

For standard classes the `__call__()` method is provided by `type`, which is the standard metaclass. So when we write

``` pycon
>>> b = int()
```

python actually executes this code

``` pycon
>>> b = int.__class__.__call__(int)
```

This standard implementation of `__call__()` runs the constructor mechanism as depicted in the second post, executing `__new__()` and `__init__()` to get a new instance and initialize it. 

The definition of callable object is very powerful, since it allows to flatten the difference between classes and functions. In OOP many times the two are presented as two completely separated concepts, but in Python it is usually more convenient to talk about callables. Here, Python shows its polymorphic nature at its maximum: if I expect a function and what is given to me is something that acts like a function everything is fine. Functions, however, are themselves simple callable objects. Remember: in Python everything is an object.

Since `__call__()` is a method we can redefine it in any class, let us try and see what happens

``` python
class CallMe(object):
    def __call__(self, *args, **kwds):
        return 1
```

Class instances usually contain no definition of `__call__()` and the implementation provided in the class hierarchy prevents them from being called

``` pycon
>>> a = int()
>>> a()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'int' object is not callable
```

Nothing forbids us to define a custom `__call__()` method inside the instance and make it callable. This allows us to pass the instance where Python expects a function or, better, a callable. A classical example is the `sort()` method of `list` objects, which accepts an optional callable `f` that shall compare objects. Here, a callable instance is accepted just like a plain function.

## Final words

There is still a lot to say about Python OOP. Just to name one topic, functions are a very interesting and wide subject. I would prefer now to update the current material to Python 3, so this is the last post in this small series. Stay tuned [on Twitter](https://twitter.com/tw_lgiordani/) if you want to get the latest news from the blog!

## Movie Trivia

Section titles of this issue come from the following movies: _The Breakfast Club_, _E.T. the Extra-Terrestrial_, _Who Framed Roger Rabbit_, _Trading Places_, _This is Spinal Tap_, _Little Shop of Horrors_, _The Naked Gun_, _A Fish Called Wanda_.

## Sources

Some sources for the content of this post. Thank you authors!

* [PEP 3119](http://legacy.python.org/dev/peps/pep-3119/) and the [ABC module documentation](https://docs.python.org/2/library/abc.html#module-abc)
* [Understanding Python's Execution Model](http://www.jeffknupp.com/blog/2013/02/14/drastically-improve-your-python-understanding-pythons-execution-model/) and [Python Classes and Object Oriented Programming](http://jeffknupp.com/blog/2014/06/18/improve-your-python-python-classes-and-object-oriented-programming/) by Jeff Knupp
* [A Guide to Python's Magic Methods](http://www.rafekettler.com/magicmethods.html) by Rafe Kettler
* Many [Stackoverflow](http://stackoverflow.com/questions/tagged/python) questions and answers

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Previous articles

* [OOP Concepts in Python 2.x - Part 1](/blog/2014/03/05/oop-concepts-in-python-2-dot-x-part-1)
* [OOP Concepts in Python 2.x - Part 2](/blog/2014/03/10/oop-concepts-in-python-2-dot-x-part-2)

