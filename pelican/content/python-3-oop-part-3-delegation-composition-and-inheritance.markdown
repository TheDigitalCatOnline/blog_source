Title: Python 3 OOP Part 3 - Delegation: composition and inheritance
Date: 2014-08-20 15:00:00 +0200
Category: Programming
Tags: Python, Python3, OOP
Authors: Leonardo Giordani
Slug: python-3-oop-part-3-delegation-composition-and-inheritance
Series: "Python 3 OOP"
Summary:

This post is available as an **IPython Notebook** [here](/notebooks/Python_3_OOP_Part_3__Delegation__composition_and_inheritance.ipynb)

## Previous post

[Python 3 OOP Part 2 - Classes and members](/blog/2014/08/20/python-3-oop-part-2-classes-and-members)

## The Delegation Run

If classes are objects what is the difference between types and instances?

When I talk about "my cat" I am referring to a concrete instance of the "cat" concept, which is a _subtype_ of "animal". So, despite being both objects, while types can be _specialized_, instances cannot.

Usually an object B is said to be a specialization of an object A when:

* B has all the features of A
* B can provide new features
* B can perform some or all the tasks performed by A in a different way

Those targets are very general and valid for any system and the key to achieve them with the maximum reuse of already existing components is _delegation_. Delegation means that an object shall perform only what it knows best, and leave the rest to other objects.

Delegation can be implemented with two different mechanisms: _composition_ and _inheritance_. Sadly, very often only inheritance is listed among the pillars of OOP techniques, forgetting that it is an implementation of the more generic and fundamental mechanism of delegation; perhaps a better nomenclature for the two techniques could be _explicit delegation_ (composition) and _implicit delegation_ (inheritance).

Please note that, again, when talking about composition and inheritance we are talking about focusing on a behavioural or structural delegation. Another way to think about the difference between composition and inheritance is to consider if the object _knows_ who can satisfy your request or if the object _is_ the one that satisfy the request.

**Please, please, please do not forget composition**: in many cases, composition can lead to simpler systems, with benefits on maintainability and changeability. 

Usually composition is said to be a very generic technique that needs no special syntax, while inheritance and its rules are strongly dependent on the language of choice. Actually, the strong dynamic nature of Python softens the boundary line between the two techniques.

## Inheritance Now

In Python a class can be declared as an _extension_ of one or more different classes, through the _class inheritance_ mechanism. The child class (the one that inherits) has the same internal structure of the parent class (the one that is inherited), and for the case of multiple inheritance the language has very specific rules to manage possible conflicts or redefinitions among the parent classes. A very simple example of inheritance is

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
{'number': 1, 'status': 'closed'}
>>> sdoor.__class__.__dict__
mappingproxy({'__doc__': None, '__module__': '__main__'})
>>> Door.__dict__
mappingproxy({'__dict__': <attribute '__dict__' of 'Door' objects>,
    'colour': 'yellow',
    'open': <function Door.open at 0xb687e224>,
    '__init__': <function Door.__init__ at 0xb687e14c>,
    '__doc__': None,
    'close': <function Door.close at 0xb687e1dc>,
    'knock': <classmethod object at 0xb67ff6ac>,
    '__weakref__': <attribute '__weakref__' of 'Door' objects>,
    '__module__': '__main__',
    'paint': <classmethod object at 0xb67ff6ec>})
```

As you can see the content of `__dict__` for `SecurityDoor` is very narrow compared to that of `Door`. The inheritance mechanism takes care of the missing elements by climbing up the classes tree. Where does Python get the parent classes? A class always contains a `__bases__` tuple that lists them

``` pycon
>>> SecurityDoor.__bases__
(<class '__main__.Door'>,)
```

So an example of what Python does to resolve a class method call through the inheritance tree is

``` pycon
>>> sdoor.__class__.__bases__[0].__dict__['knock'].__get__(sdoor)
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
mappingproxy({'__doc__': None,
    '__module__': '__main__',
    'open': <function SecurityDoor.open at 0xb6fcf89c>,
    'colour': 'gray',
    'locked': True})
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

You can easily test that this implementation is working correctly.

``` pycon
>>> sdoor = SecurityDoor(1, 'closed')
>>> sdoor.status
'closed'
>>> sdoor.open()
>>> sdoor.status
'closed'
>>> sdoor.locked = False
>>> sdoor.open()
>>> sdoor.status
'open'
```

This form of explicit parent delegation is heavily discouraged, however.

The first reason is because of the very high coupling that results from explicitly naming the parent class again when calling the method. _Coupling_, in the computer science lingo, means to link two parts of a system, so that changes in one of them directly affect the other one, and is usually avoided as much as possible. In this case if you decide to use a new parent class you have to manually propagate the change to every method that calls it. Moreover, since in Python the class hierarchy can be dynamically changed (i.e. at runtime), this form of explicit delegation could be not only annoying but also wrong.

The second reason is that in general you need to deal with multiple inheritance, where you do not know a priori which parent class implements the original form of the method you are overriding.

To solve these issues, Python supplies the `super()` built-in function, that climbs the class hierarchy and returns the correct class that shall be called. The syntax for calling `super()` is

``` python
class SecurityDoor(Door):
    colour = 'gray'
    locked = True
    
    def open(self):
        if self.locked:
            return
        super().open()
```

The output of `super()` is not exactly the `Door` class. It returns a `super` object which representation is `<super: <class 'SecurityDoor'>, <SecurityDoor object>>`. This object however acts like the parent class, so you can safely ignore its custom nature and use it just like you would do with the `Door` class in this case.

## Enter the Composition

Composition means that an object knows another object, and explicitly delegates some tasks to it. While inheritance is implicit, composition is explicit: in Python, however, things are far more interesting than this =).

First of all let us implement classic composition, which simply makes an object part of the other as an attribute

``` python
class SecurityDoor:
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

The composed `SecurityDoor` has to redefine the `colour` attribute since the concept of delegation applies only to methods and not to attributes, doesn't it?

Well, no. Python provides a very high degree of indirection for objects manipulation and attribute access is one of the most useful. As you already discovered, accessing attributes is ruled by a special method called `__getattribute__()` that is called whenever an attribute of the object is accessed. Overriding `__getattribute__()`, however, is overkill; it is a very complex method, and, being called on every attribute access, any change makes the whole thing slower.

The method we have to leverage to delegate attribute access is `__getattr__()`, which is a special method that is called whenever the requested attribute is not found in the object. So basically it is the right place to dispatch all attribute and method access our object cannot handle. The previous example becomes

``` python
class SecurityDoor:
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
class ComposedDoor:
    def __init__(self, number, status):
        self.door = Door(number, status)
        
    def __getattr__(self, attr):
        return getattr(self.door, attr)
```

As this last example shows, delegating every member access through `__getattr__()` is very simple. Pay attention to `getattr()` which is different from `__getattr__()`. The former is a built-in that is equivalent to the dotted syntax, i.e. `getattr(obj, 'someattr')` is the same as `obj.someattr`, but you have to use it since the name of the attribute is contained in a string.

Composition provides a superior way to manage delegation since it can selectively delegate the access, even mask some attributes or methods, while inheritance cannot. In Python you also avoid the memory problems that might arise when you put many objects inside another; Python handles everything through its reference, i.e. through a pointer to the memory position of the thing, so the size of an attribute is constant and very limited.

## Movie Trivia

Section titles come from the following movies: _The Cannonball Run (1981)_, _Apocalypse Now (1979)_, _Enter the Dragon (1973)_.

## Sources

You will find a lot of documentation in [this Reddit post](http://www.reddit.com/r/Python/comments/226ahl/some_links_about_python_oop/). Most of the information contained in this series come from those sources.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Next post

[Python 3 OOP Part 4 - Polymorphism](/blog/2014/08/21/python-3-oop-part-4-polymorphism)