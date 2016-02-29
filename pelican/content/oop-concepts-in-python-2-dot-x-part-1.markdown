Title: OOP concepts in Python 2.x - Part 1
Date: 2014-03-05 12:46:06 +0100
Category: Programming
Tags: Python2, Python, OOP
Authors: Leonardo Giordani
Slug: oop-concepts-in-python-2-dot-x-part-1
Series: "OOP concepts in Python 2.x"
Summary:

## Abstract

Object-oriented programming (OOP) has been the leading programming paradigm for several decades now, starting from the initial attempts back in the 60s to some of the most important languages used nowadays. Being a set of programming concepts and design methodologies, OOP can never be said to be "correctly" or "fully" implemented by a language: indeed there are as many implementations as languages.

So one of the most interesting aspects of OOP languages is to understand how they implement those concepts. In this post I am going to try and start analyzing the OOP implementation of the Python language. Due to the richness of the topic, however, I consider this attempt just like a set of thoughts for Python beginners trying to find their way into this beautiful (and sometimes peculiar) language.

This first post covers the following topics:

* Objects and types
* Classes and instances
* Object members: methods and attributes
* Delegation: inheritance and composition

This post refers to the internals of Python 2.x - please note that Python 3.x changes (improves!) some of the features shown here. You can find the **updated version** [here](/categories/python3/).

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
    
print avg(data)
```

As you can see the procedure is quite good and general: the procedure is run on a sequence of data, and it returns the average of the sequence items. So far, so good: computing the average of some numbers leaves the numbers untouched and creates new data.

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
print door1
```

I described a door as a structure containing a number and the status of the door. The procedure knows how this structure is made and may alter it.

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
    if door[2] == 'unlocked`:
        door[1] = `open`

open_door(door1)
print door1

open_ldoor(ldoor1)
print ldoor1

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
>>> print a
6
>>> print type(a)
<type 'int'>
>>> print a.__class__
<type 'int'>
```

As you can see, the built-in function `type()` returns the content of the _magic attribute_ `__class__` (magic here means that its value is managed by Python itself offstage). The type of the variable `a`, or its class, is `int`. (This is a very inaccurate description of this rather complex topic, so remember that we are just scratching the surface).

Once you have a class you can _instantiate_ it to get a concrete object (an _instance_) of that type, i.e. an object built according to the structure of that class. The Python syntax to instantiate a class is the same of a function call

``` pycon
>>> b = int()
>>> type(b)
<type 'int'>
```

When you create an instance, you can pass some values, according to the class definition, to _initialize_ it.

``` pycon
>>> b = int()
>>> print b
0
>>> c = int(7)
>>> print c
7
```

In this example, the `int` class creates an integer with value 0 when called without arguments, otherwise it uses the given argument to initialize the newly created object.

Let us write a class that represents a door to match the procedural examples done in the first section

``` python
class Door(object):
    def __init__(self, number, status):
        self.number = number
        self.status = status
        
    def open(self):
        self.status = 'open'
        
    def close(self):
        self.status = 'closed'
```

The `class` keyword defines a new class named `Door`; everything indented under `class` is part of the class. The functions you write inside the object are called _methods_ and don't differ at all from standard functions; the name changes only to highlight the fact that those functions now are part of an object.

Methods of a class must accept as first argument a special value called `self` (the name is a convention but please never break it).

The class can be given a special method called `__init__()` which is run when the class is instantiated, receiving the arguments passed when calling the class; the general name of such a method, in the OOP context, is _constructor_, even if the `__init__()` method is not the only part of this mechanism in Python.

The `self.number` and `self.status` variables are called _attributes_ of the object. In Python, methods and attributes are both _members_ of the object and are accessible with the dotted syntax; the difference between attributes and methods is that the latter can be called (in Python lingo you say that a method is a _callable_).

As you can see the `__init__()` method shall create and initialize the attributes since they are not declared elsewhere.

The class can be used to create a concrete object

``` pycon
>>> door1 = Door(1, 'closed')
>>> type(door1)
<class '__main__.Door'>
>>> print door1.number
1
>>> print door1.status
closed
```

Now `door1` is an instance of the `Door` class; `type()` returns the class as `__main__.Door` since the class was defined directly in the interactive shell, that is in the current main module. To call a method of an object, that is to run one of its internal functions, you just access it as an attribute with the dotted syntax and call it like a standard function.

``` pycon
>>> door1.open()
>>> print door1.number
1
>>> print door1.status
open
```

In this case, the `open()` method of the `door1` instance has been called. No arguments have been passed to the `open()` method, but if you review the class declaration, you see that it was declared to accept an argument (`self`). When you call a method of an instance, Python automatically passes the instance itself to the method as the first argument.

You can create as many instances as needed and they are completely unrelated each other. That is the changes you make on one instance do not reflect on another instance of the same class.

#### Recap

Objects are described by a _class_, which can generate one or more _instances_, unrelated each other. A class contains _methods_, which are functions, and they accept at least one argument called `self`, which is the actual instance on which the method has been called. A special method, `__init__()` deals with the initialization of the object, setting the initial value of the _attributes_.

## Python Classes Strike Again

The Python implementation of classes has some peculiarities. The bare truth is that in Python the class of an object is an object itself. You can check this by issuing `type()` on the class

``` pycon
>>> type(door1)
<class '__main__.Door'>
>>> print type(Door)
<type 'type'>
```

This shows that the `Door` class is an object, an instance of the `type` class.

This concept is not so difficult to grasp as it can seem at first sight: in the real world we deal with concepts using them like _things_: for example we can talk about the concept of "door", telling people how a door looks like and how it works. So in our everyday experience the _type_ of an object is an object itself. In Python this can be expressed by saying that _everything is an object_.

If the class is an instance it is a concrete object and is stored somewhere in memory. Let us leverage the _inspection_ capabilities of Python and its `id()` function to check the status of our objects. The `id()` built-in function returns the memory position of an object.

First of all, let's check that the two objects are stored at different addresses

``` pycon
>>> hex(id(door1))
'0x7fa4c818bad0'
>>> hex(id(door2))
'0x7fa4c818b890'
```

This confirms that the two instances are separate and unrelated.
**Please note that your values are very likely to be different from the ones I got.**

However if we use `id()` on the class of the two instances we discover that the class is _exactly_ the same

``` pycon
>>> hex(id(door1.__class__))
'0x766800'
>>> hex(id(door2.__class__))
'0x766800'
```

Well this is very important. In Python, a class is not just the schema used to build an object. Rather, the class is a shared living object, which code is accessed at run time.

As we already tested, however, attributes are not stored in the class but in every instance, due to the fact that `__init__()` works on `self` when creating them. Classes, however, can be given attributes like any other object; with a terrific effort of imagination, let's call them _class attributes_.

As you can expect, class attributes are shared among the class instances just like their container

``` python
class Door(object):
    colour = 'brown'

    def __init__(self, number, status):
        self.number = number
        self.status = status
        
    def open(self):
        self.status = 'open'
        
    def close(self):
        self.status = 'closed'
```

The `colour` attribute here is not created using `self`, and any change of its value reflects on all instances

``` pycon
>>> door1 = Door(1, 'closed')
>>> door2 = Door(2, 'closed')
>>> Door.colour
'brown'
>>> door1.colour
'brown'
>>> door2.colour
'brown'
>>> Door.colour = 'white'
>>> Door.colour
'white'
>>> door1.colour
'white'
>>> door2.colour
'white'
>>> hex(id(Door.colour))
'0xb74c5420L'
>>> hex(id(door1.colour))
'0xb74c5420L'
>>> hex(id(door2.colour))
'0xb74c5420L'
```

## Raiders of the Lost Attribute

Any Python object is automatically given a `__dict__` attribute, which contains its list of attributes. Let's investigate what this dictionary contains for our example objects:

``` pycon
>>> Door.__dict__
dict_proxy({'__module__': '__main__',
    'colour': 'brown',
    '__weakref__': <attribute '__weakref__' of 'Door' objects>,
    '__dict__': <attribute '__dict__' of 'Door' objects>,
    'close': <function close at 0xb6a8a56c>,
    'open': <function open at 0xb6a8a534>,
    '__doc__': None,
    '__init__': <function __init__ at 0xb6a8a48c>})
>>> door1.__dict__
{'status': 'closed', 'number': 1}
```

Leaving aside the difference between a dictionary and a `dict_proxy` object, you can see that the `colour` attribute is listed among the `Door` class attributes, while `status` and `number` are listed for the instance.

How comes that we can call `door1.colour`, if that attribute is not listed for that instance? This is a job performed by the magic `__getattribute__()` method; in Python the dotted syntax automatically invokes this method so when we write `door1.colour`, Python executes `door1.__getattribute__('colour')`. That method performs the  _attribute  lookup_ action, i.e. finds the value of the attribute by looking in different places.

The standard implementation of `__getattribute__()` searches first the internal dictionary (`__dict__`) of an object, then the type of the object itself; in this case `door1.__getattribute__('colour')` executes first `door1.__dict__['colour']` and then `door1.__class__.__dict__['colour']`

``` pycon
>>> door1.__dict__['colour']
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'colour'
>>> door1.__class__.__dict__['colour']
'brown'
```

Indeed, if we check the objects equality through the `is` operator we can confirm that both `door1.colour` and `Door.colour` are exactly the same object

``` pycon
>>> door1.colour is Door.colour
True
```

When we try to assign a value to a class attribute directly on an instance, we just put in the `__dict__` of the instance a value with that name, and this value masks the class attribute since it is found first by `__getattribute__()`. As you can see from the examples of the previous section, this is different from changing the value of the attribute on the class itself.

``` pycon
>>> door1.colour = 'white'
>>> door1.__dict__['colour']
'white'
>>> door1.__class__.__dict__['colour']
'brown'
>>> Door.colour = 'red'
>>> door1.__dict__['colour']
'white'
>>> door1.__class__.__dict__['colour']
'red'
```

## Revenge of the Methods

Let's play the same game with methods. First of all you can see that, just like class attributes, methods are listed only in the class `__dict__`. Chances are that they behave the same as attributes when we get them

``` pycon
>>> door1.open is Door.open
False
```

Whoops. Let us further investigate the matter

``` pycon
>>> Door.__dict__['open']
<function open at 0xb73ee10c>
>>> Door.open
<unbound method Door.open>
>>> door1.open
<bound method Door.open of <__main__.Door object at 0xb73f956c>>
```

So, the class method is listed in the members dictionary as _function_. So far, so good. The same method, taken directly from the class is returned as _unbound method_, while taking it from the instance it changes to _bound method_. Well, a _function_ is a procedure you named and defined with the `def` statement. When you refer to a function as part of a class you get an unbound method. The name _method_ simply means "a function inside a class", according to the usual OOP definitions, while _unbound_ signals that the method is not bound to any instance. As you can see, as soon as you access the method from an instance, the method becomes _bound_.

Why does Python bother with methods being bound or unbound? And how does Python transform an unbound method into a bound one?

First of all, if you try to call an unbound method you get an error

``` pycon
>>> Door.open()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unbound method open() must be called with
    Door instance as first argument (got nothing instead)
```

so by default Python considers methods as functions that shall operate on instances, and calling them from the class leaves the interpreter puzzled. Let us try to pass the instance as suggested by the exception message

``` pycon
>>> Door.open(door1)
>>> door1.status
'open'
```

Python does not complain here, and the method works as expected. So `Door.open(door1)` is the same as `door1.open()`. Again, under the hood, `__getattribute__()` is working to make everything work and when we call `door1.open()`, Python actually calls `door1.__class__.open(door1)`. However, `door1.__class__.open` is an unbound method, so there is something more that converts it into a bound method that Python can safely call.

When you access a member of an object, Python calls `__getattribute__()` to satisfy the request. This magic method, however, conforms to a procedure known as _descriptor protocol_. For the read access `__getattribute__()` checks if the object has a `__get__()` method and calls this latter. So for function the conversion from an unbound into a bound method is made by such a mechanism. Let us review it by means of an example.

``` pycon
>>> door1.__class__.__dict__['open']
<function open at 0xb73ee10c>
```

This syntax retrieves the function defined in the class; the function knows nothing about objects, but it _is_ an object (remember "everything is an object"). So we can look inside it with the `dir()` built-in function

``` pycon
>>> dir(door1.__class__.__dict__['open'])
['__call__', '__class__', '__closure__', '__code__', '__defaults__',
 '__delattr__', '__dict__', '__doc__', '__format__', '__get__',
 '__getattribute__', '__globals__', '__hash__', '__init__',
 '__module__', '__name__', '__new__', '__reduce__', '__reduce_ex__',
 '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__',
 'func_closure', 'func_code', 'func_defaults', 'func_dict', 'func_doc',
 'func_globals', 'func_name']
>>> door1.__class__.__dict__['open'].__get__
<method-wrapper '__get__' of function object at 0xb73ee10c>
```

As you can see, a `__get__` method is listed among the members of the function, and Python recognizes it as a _method-wrapper_. This method shall connect the `open` function to the `door1` instance, so we can call it passing the instance alone

``` pycon
>>> door1.__class__.__dict__['open'].__get__(door1)
<bound method ?.open of <__main__.Door object at 0xb73f956c>>
```

Ok, something is missing. Indeed `__get__()` in this case also accepts the _owner_ class, i.e. the class we are trying to get the attribute from. So the correct form is

``` pycon
>>> door1.__class__.__dict__['open'].__get__(door1, Door)
<bound method Door.open of <__main__.Door object at 0xb73f956c>>
```

## When methods met classes

If we use `type()` on an unbound method, we get an interesting result

``` pycon
>>> door1.__class__.__dict__['open']
<function open at 0xb6aa548c>
>>> type(door1.__class__.open)
<type 'instancemethod'>
```

The method is an _instance method_; as we discovered, it shall be bound to an instance to work, so the name is a good choice. Does this imply that we can also define a _class method_? Indeed we can, through the `classmethod` decorator

``` python
class Door(object):
    colour = 'brown'

    def __init__(self, number, status):
        self.number = number
        self.status = status

    @classmethod
    def knock(cls):
        print "Knock!"

    def open(self):
        self.status = 'open'
        
    def close(self):
        self.status = 'closed'
```

Such a definition makes the method callable on both the instance and the class

``` pycon
>>> door1.knock()
Knock!
>>> Door.knock()
Knock!
>>> door1.knock
<bound method type.knock of <class '__main__.Door'>>
>>> Door.knock
<bound method type.knock of <class '__main__.Door'>>
```

And this is possible since, as you can see, both the class method and the instance method are bound. The class method is bound to the class itself, while the instance method is bound to the instance of the class. What about the type of the method?

``` pycon
>>> door1.__class__.__dict__['knock']
<classmethod object at 0xb6a8db6c>
>>> type(door1.__class__.knock)
<type 'instancemethod'>
```

Puzzled? Don't be confused! When you look in the `__dict__` you are not going through the `__getattribute__()` and `__get__()` machinery, so you get the plain unprocessed attribute. With standard methods you find `function` objects in the members dictionary, while for class methods you find `classmethod` objects.

On the other side, when you check the type of `door1.__class__.knock` you implicitly invoke `__get__()`, which binds the method to the class.

``` pycon
>>> type(door1.__class__.__dict__['knock'].__get__(door1, Door))
<type 'instancemethod'>
>>> type(door1.__class__.__dict__['knock'])
<type 'classmethod'>
```

After all, it is no surprise that a class method is bound since a class is an instance of `type`.

## Intermezzo

I realize now that I introduced this post as "a set of thoughts for Python beginners". Sorry. If you are reading this, however, chances are that you are still alive. Calm down. Breathe. Relaxed? Ready to go? Let's dive into delegation!

## The Delegation Run

If classes are objects what is the difference between types and instances? When I talk about "my cat" I am referring to a concrete instance of the "cat" concept, which is a _subtype_ of "animal". So, despite being both objects, while types can be _specialized_, instances cannot.

Usually an object B is said to be a specialization of an object A when:

* B has all the features of A
* B can provide new features
* B can perform some or all the tasks performed by A in a different way

Those targets are very general and valid for any system and the key to achieve them with the maximum reuse of already existing components is _delegation_. Delegation means that an object shall perform only what it knows best, and leave the rest to other objects.

Delegation can be implemented with two different mechanisms: _composition_ and _inheritance_. Sadly, very often inheritance is listed among the pillars of OOP techniques, forgetting that it is an implementation of the more generic and fundamental mechanism of delegation; perhaps a better nomenclature for the two techniques could be _explicit delegation_ (composition) and _implicit delegation_ (inheritance). Please note that, again, when talking about composition and inheritance we are talking about focusing on a behavioural or structural delegation. Another way to think about the difference between composition and inheritance is to consider if the object _knows_ who can satisfy your request or if the object _is_ the one that satisfy the request.

Please, please, please do not forget composition: in many cases, composition can lead you to a simpler system, with benefits on maintainability and changeability. 

Usually composition is said to be a very generic technique that needs no special syntax, while inheritance and its rules are strongly dependent on the language of choice. Actually, the strong dynamic nature of Python softens the boundary line between the two techniques.

## Inheritance Now

In Python a class can be declared as an _extension_ of one or more different classes, through the _class inheritance_ mechanism. The child class (the one that inherits) internally has the same structure of the parent class (the one that is inherited), and for the case of multiple inheritance the language has very specific rules to manage possible conflicts or redefinitions among the parent classes. A very simple example of inheritance is

``` python
class SecurityDoor(Door):
    pass
```

where we declare a new class `SecurityDoor` that, at the moment, is a perfect copy of the `Door` class. Let us investigate what happens when we access attributes and methods. First we instance the class

``` pycon
>>> sdoor = SecurityDoor(1, 'closed')
```

The first check we can do is that class attributes are still global and shared

``` pycon
>>> SecurityDoor.colour is Door.colour
True
>>> sdoor.colour is Door.colour
True
```

This shows us that Python tries to resolve instance members not only looking into the class the instance comes from, but also investigating the parent classes. In this case `sdoor.colour` becomes `SecurityDoor.colour`, that in turn becomes `Door.colour`. `SecurityDoor` _is_ a `Door`.

If we investigate the content of `__dict__` we can catch a glimpse of the inheritance mechanism in action

``` pycon
>>> sdoor.__dict__
{'status': 'closed', 'number': 1}
>>> sdoor.__class__.__dict__
dict_proxy({'__module__': '__main__', '__doc__': None})
>>> Door.__dict__
dict_proxy({'knock': <classmethod object at 0xb6a8db6c>,
    '__module__': '__main__',
    '__weakref__': <attribute '__weakref__' of 'Door' objects>,
    '__dict__': <attribute '__dict__' of 'Door' objects>,
    'close': <function close at 0xb6aa5454>,
    'colour': 'brown',
    'open': <function open at 0xb6aa53e4>,
    '__doc__': None,
    '__init__': <function __init__ at 0xb6aa51ec>})
```

As you can see the content of `__dict__` for `SecurityDoor` is very narrow compared to that of `Door`. The inheritance mechanism takes care of the missing elements by climbing up the classes tree. Where does Python get the parent classes? A class always contains a `__bases__` tuple that lists them

``` pycon
>>> SecurityDoor.__bases__
(<class '__main__.Door'>,)
```

So an example of what Python does to resolve a class method call through the inheritance tree is

``` pycon
>>> sdoor.__class__.__bases__[0].__dict__['knock'].__get__(sdoor, SecurityDoor)
<bound method type.knock of <class '__main__.SecurityDoor'>>
>>> sdoor.knock
<bound method type.knock of <class '__main__.SecurityDoor'>>
```

Please note that this is just an example that does not consider multiple inheritance.

Let us try now to override some methods and attributes. In Python you can _override_ (redefine) a parent class member simply by redefining it in the child class.

``` python
class SecurityDoor(Door):
    colour = 'gray'
    locked = True
    
    def open(self):
        if not self.locked:
            self.status = 'open'
```

As you can forecast, the overridden members now are present in the `__dict__` of the `SecurityDoor` class

``` pycon
>>> SecurityDoor.__dict__
dict_proxy({'locked': True,
    '__module__': '__main__',
    'open': <function open at 0xb73d8844>,
    'colour': 'gray',
    '__doc__': None})
```

So when you override a member, the one you put in the child class is used instead of the one in the parent class simply because the former is found before the latter while climbing the class hierarchy. This also shows you that Python does not implicitly call the parent implementation when you override a method. So, overriding is a way to block implicit delegation.

If we want to call the parent implementation we have to do it explicitly. In the former example we could write

``` python
class SecurityDoor(Door):
    colour = 'gray'
    locked = True
    
    def open(self):
        if self.locked:
            return
        Door.open(self)
```

You can easily test that this implementation is working correctly. This form of explicit parent delegation is heavily discouraged, however.

The first reason is because of the very high coupling that results from explicitly naming the parent class again when calling the method; if you decide to use a new parent class you have to manually propagate the change to every method that calls it. Moreover, since in Python the class hierarchy can be dynamically changed (i.e. at runtime), this form of explicit delegation could be not only annoying but wrong.

The second reason is that in general you need to deal with multiple inheritance, where you do not know a priori which parent class implements the original form of the method you are overriding.

To solve these issues, Python supplies the `super()` built-in function, that climbs the class hierarchy and returns the correct class that shall be called. The syntax for calling `super()` is

``` python
class SecurityDoor(Door):
    colour = 'gray'
    locked = True
    
    def open(self):
        if self.locked:
            return
        super(SecurityDoor, self).open(self)
```

As you can see you have to explicitly pass the class you are in and the current instance. This is a (indeed very light) form of repetition that has been fixed in Python 3.x.

## Enter the Composition

Composition means that an object knows another object, and explicitly delegates some tasks to it. While inheritance is implicit, composition is explicit: in Python, however, things are far more interesting than this =).

First of all let us implement classic composition, which simply makes an object part of the other as an attribute

``` python
class SecurityDoor(object):
    colour = 'gray'
    locked = True
    
    def __init__(self, number, status):
        self.door = Door(number, status)
        
    def open(self):
        if self.locked:
            return
        self.door.open()
        
    def close(self):
        self.door.close()
```

The primary goal of composition is to relax the coupling between objects. This little example shows that now `SecurityDoor` is an `object` and no more a `Door`, which means that the internal structure of `Door` is not copied. For this very simple example both `Door` and `SecurityDoor` are not big classes, but in a real system objects can very complex; this means that their allocation consumes a lot of memory and if a system contains thousands or millions of objects that could be an issue.

As you can see, however, our solution is far from perfect.

The composed `SecurityDoor` has to redefine the `colour` attribute since the concept of delegation applies only to methods and not to attributes, doesn't it? Well, no. Python provides a very high degree of indirection for objects manipulation and attribute access is one of the most useful. As you already discovered, accessing attributes is ruled by a special method called `__getattribute__()` that is called whenever an attribute of the object is accessed. Overriding `__getattribute__()`, however, is overkill; it is a very complex method, and, being called on every attribute access, any change makes the whole thing slower.

The method we have to leverage to delegate attribute access is `__getattr__()`, which is a special method that is called whenever the requested attribute is not found in the object. So basically it is the right place to dispatch all attribute and method access our object cannot handle. In the previous example

``` python
class SecurityDoor(object):
    locked = True
    
    def __init__(self, number, status):
        self.door = Door(number, status)
        
    def open(self):
        if self.locked:
            return
        self.door.open()
        
    def __getattr__(self, attr):
        return getattr(self.door, attr)
```

Using `__getattr__()` blends the separation line between inheritance and composition since after all the former is a form of automatic delegation of every member access.

``` python
class ComposedDoor(object):
    def __init__(self, number, status):
        self.door = Door(number, status)
        
    def __getattr__(self, attr):
        return getattr(self.door, attr)
```

As this last example shows, delegating every member access through `__getattr__()` is very simple. Pay attention to `getattr()` which is different from `__getattr__()`. The former is a built-in that is equivalent to the dotted syntax, i.e. `getattr(obj, 'someattr')` is the same as `obj.someattr`, but you have to use it since the name of the attribute is contained in a string.

Composition provides a superior way to manage delegation since it can selectively delegate the access, even mask some attributes or methods, while inheritance cannot. In Python you also avoid the memory problems that might arise when you put many objects inside another; Python handles everything through its reference, i.e. through a pointer to the memory position of the thing, so the size of an attribute is constant and very limited.

## Final words

There is much more that can be said about the Python implementation of objects, classes, and friends. Just to name a couple of things, the object creation mechanism and the relationship between `object` and `type` have not been explained. Hopefully, there will be space in a later post. In the second post, however, I'll start talking about polymorphism, Abstract Base Classes, and metaclasses. Stay tuned!

## Movie Trivia

If you are wondering why section titles are so weird, chances are that you missed some good movies to watch while you are not coding =). The movies are: _Back to the Future_, _What About Bob?_, _Wargames_, _The Empire Strikes Back_, _Raiders of the Lost Ark_, _Revenge of the Nerds_, _When Harry Met Sally_, _The Cannonball Run_, _Apocalypse Now_, _Enter the Dragon_.

## Sources

Some sources for the content of this post. Thank you authors!

* [The official Python documentation](http://docs.python.org/2/tutorial/classes.html) and the Python source code.
* [Python Types and Objects - by Shalabh Chaturvedi](http://www.cafepy.com/article/python_types_and_objects/)
* [Python’s super() considered super! - by Raymond Hettinger](http://rhettinger.wordpress.com/2011/05/26/super-considered-super/)
* [Design Patterns in Python - by Alex Martelli](http://www.aleax.it/gdd_pydp.pdf)
* Many [Stackoverflow](http://stackoverflow.com/questions/tagged/python) questions and answers

## Updates

2014-03-08: "When methods met classes" section had a typo: the sentence "The class method is bound to the class itself, while the instance method is bound to the class of the instance." is "while the instance method is bound to instance of the class.". Thanks [Mohcin Shah](https://plus.google.com/u/0/116309787480950087834/posts) for spotting it!

2014-03-10: Fixed link to Alex Martelli's presentation.

2014-04-07: A typo when discussing `door1.colour` has been fixed. Thanks to [pujuma](http://www.reddit.com/user/pujuma).

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Next articles

* [OOP Concepts in Python 2.x - Part 2](/blog/2014/03/10/oop-concepts-in-python-2-dot-x-part-2)
* [OOP Concepts in Python 2.x - Part 3](/blog/2014/07/04/oop-concepts-in-python-2-dot-x-part-3)

