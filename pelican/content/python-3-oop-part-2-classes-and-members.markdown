Title: Python 3 OOP Part 2 - Classes and members
Date: 2014-08-20 14:00:00 +0200
Category: Programming
Tags: Python, Python3, OOP
Authors: Leonardo Giordani
Slug: python-3-oop-part-2-classes-and-members
Series: "Python 3 OOP"
Summary:

This post is available as an **IPython Notebook** [here](/notebooks/Python_3_OOP_Part_2__Classes_and_members.ipynb)

## Previous post

[Python 3 OOP Part 1 - Objects and types](/blog/2014/08/20/python-3-oop-part-1-objects-and-types)

## Python Classes Strike Again

The Python implementation of classes has some peculiarities. The bare truth is that in Python the class of an object is an object itself. You can check this by issuing `type()` on the class

``` pycon
>>> a = 1
>>> type(a)
<class 'int'>
>>> type(int)
<class 'type'>
```

This shows that the `int` class is an object, an instance of the `type` class.

This concept is not so difficult to grasp as it can seem at first sight: in the real world we deal with _concepts_ using them like _things_: for example we can talk about the concept of "door", telling people how a door looks like and how it works. In this case the concept of door is the topic of our discussion, so in our everyday experience the _type_ of an object is an object itself. In Python this can be expressed by saying that _everything is an object_.

If the class of an object is itself an instance it is a concrete object and is stored somewhere in memory. Let us leverage the _inspection_ capabilities of Python and its `id()` function to check the status of our objects. The `id()` built-in function returns the memory position of an object.

In the first post we defined this class

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

First of all, let's create two instances of the `Door` class and check that the two objects are stored at different addresses

``` pycon
>>> door1 = Door(1, 'closed')
>>> door2 = Door(1, 'closed')
>>> hex(id(door1))
'0xb67e148c'
>>> hex(id(door2))
'0xb67e144c'
```

This confirms that the two instances are separate and unrelated.
**Please note that your values are very likely to be different from the ones I got. Being memory addresses they change at every execution.** The second instance was given the same attributes of the first instance to show that the two are different objects regardless of the value of the attributes.

However if we use `id()` on the class of the two instances we discover that the class is _exactly_ the same

``` pycon
>>> hex(id(door1.__class__))
'0xb685f56c'
>>> hex(id(door2.__class__))
'0xb685f56c'
```

Well this is very important. In Python, a class is not just the schema used to build an object. Rather, the class is a shared living object, which code is accessed at run time.

As we already tested, however, attributes are not stored in the class but in every instance, due to the fact that `__init__()` works on `self` when creating them. Classes, however, can be given attributes like any other object; with a terrific effort of imagination, let's call them _class attributes_.

As you can expect, class attributes are shared among the class instances just like their container

``` python
class Door:
    colour = 'brown'

    def __init__(self, number, status):
        self.number = number
        self.status = status
        
    def open(self):
        self.status = 'open'
        
    def close(self):
        self.status = 'closed'
```

Pay attention: the `colour` attribute here is not created using `self`, so it is contained in the class and shared among instances

``` pycon
>>> door1 = Door(1, 'closed')
>>> door2 = Door(2, 'closed')
>>> Door.colour
'brown'
>>> door1.colour
'brown'
>>> door2.colour
'brown'
```

Until here things are not different from the previous case. Let's see if changes of the shared value reflect on all instances

``` pycon
>>> Door.colour = 'white'
>>> Door.colour
'white'
>>> door1.colour
'white'
>>> door2.colour
'white'
>>> hex(id(Door.colour))
'0xb67e1500'
>>> hex(id(door1.colour))
'0xb67e1500'
>>> hex(id(door2.colour))
'0xb67e1500'
```

## Raiders of the Lost Attribute

Any Python object is automatically given a `__dict__` attribute, which contains its list of attributes. Let's investigate what this dictionary contains for our example objects:

``` pycon
>>> Door.__dict__
mappingproxy({'open': <function Door.open at 0xb68604ac>,
    'colour': 'brown',
    '__dict__': <attribute '__dict__' of 'Door' objects>,
    '__weakref__': <attribute '__weakref__' of 'Door' objects>,
    '__init__': <function Door.__init__ at 0xb7062854>,
    '__module__': '__main__',
    '__doc__': None,
    'close': <function Door.close at 0xb686041c>})
>>> door1.__dict__
{'number': 1, 'status': 'closed'}
```

Leaving aside the difference between a dictionary and a `mappingproxy` object, you can see that the `colour` attribute is listed among the `Door` class attributes, while `status` and `number` are listed for the instance.

How comes that we can call `door1.colour`, if that attribute is not listed for that instance? This is a job performed by the magic `__getattribute__()` method; in Python the dotted syntax automatically invokes this method so when we write `door1.colour`, Python executes `door1.__getattribute__('colour')`. That method performs the  _attribute  lookup_ action, i.e. finds the value of the attribute by looking in different places.

The standard implementation of `__getattribute__()` searches first the internal dictionary (`__dict__`) of an object, then the type of the object itself; in this case `door1.__getattribute__('colour')` executes first `door1.__dict__['colour']` and then, since the latter raises a `KeyError` exception, `door1.__class__.__dict__['colour']`

``` pycon
>>> door1.__dict__['colour']
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'colour'
>>> door1.__class__.__dict__['colour']
'brown'
```

Indeed, if we compare the objects' equality through the `is` operator we can confirm that both `door1.colour` and `Door.colour` are exactly the same object

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
<function Door.open at 0xb68604ac>
>>> Door.open
<function Door.open at 0xb68604ac>
>>> door1.open
<bound method Door.open of <__main__.Door object at 0xb67e162c>>
```

So, the class method is listed in the members dictionary as _function_. So far, so good. The same happens when taking it directly from the class; here Python 2 needed to introduce _unbound methods_, which are not present in Python 3. Taking it from the instance returns a _bound method_.

Well, a _function_ is a procedure you named and defined with the `def` statement. When you refer to a function as part of a class in Python 3 you get a plain function, without any difference from a function defined outside a class.

When you get the function from an instance, however, it becomes a bound method. The name _method_ simply means "a function inside an object", according to the usual OOP definitions, while _bound_ signals that the method is linked to that instance. Why does Python bother with methods being bound or not? And how does Python transform a function into a bound method?

First of all, if you try to call a class function you get an error

``` pycon
>>> Door.open()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: open() missing 1 required positional argument: 'self'
```

Yes. Indeed the function was defined to require an argument called 'self', and calling it without an argument raises an exception. This perhaps means that we can give it one instance of the class and make it work

``` pycon
>>> Door.open(door1)
>>> door1.status
'open'
```

Python does not complain here, and the method works as expected. So `Door.open(door1)` is the same as `door1.open()`, and this is the difference between a plain function coming from a class an a bound method: the bound method automatically passes the instance as an argument to the function.

Again, under the hood, `__getattribute__()` is working to make everything work and when we call `door1.open()`, Python actually calls `door1.__class__.open(door1)`. However, `door1.__class__.open` is a plain function, so there is something more that converts it into a bound method that Python can safely call.

When you access a member of an object, Python calls `__getattribute__()` to satisfy the request. This magic method, however, conforms to a procedure known as _descriptor protocol_. For the read access `__getattribute__()` checks if the object has a `__get__()` method and calls this latter. So the converstion of a function into a bound method happens through such a mechanism. Let us review it by means of an example.

``` pycon
>>> door1.__class__.__dict__['open']
<function Door.open at 0xb68604ac>
```

This syntax retrieves the function defined in the class; the function knows nothing about objects, but it _is_ an object (remember "everything is an object"). So we can look inside it with the `dir()` built-in function

``` pycon
>>> dir(door1.__class__.__dict__['open'])
['__annotations__', '__call__', '__class__', '__closure__', '__code__',
 '__defaults__', '__delattr__', '__dict__', '__dir__', '__doc__', '__eq__',
 '__format__', '__ge__', '__get__', '__getattribute__', '__globals__',
 '__gt__', '__hash__', '__init__', '__kwdefaults__', '__le__', '__lt__',
 '__module__', '__name__', '__ne__', '__new__', '__qualname__', '__reduce__',
 '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__',
 '__subclasshook__']
>>> door1.__class__.__dict__['open'].__get__
<method-wrapper '__get__' of function object at 0xb68604ac>
```

As you can see, a `__get__` method is listed among the members of the function, and Python recognizes it as a _method-wrapper_. This method shall connect the `open` function to the `door1` instance, so we can call it passing the instance alone

``` pycon
>>> door1.__class__.__dict__['open'].__get__(door1)
<bound method Door.open of <__main__.Door object at 0xb67e162c>>
```

and we get exactly what we were looking for. This complex syntax is what happens behind the scenes when we call a method of an instance.

## When Methods met Classes

Using `type()` on functions defined inside classes reveals some other details on their internal representation

``` pycon
>>> Door.open
<function Door.open at 0xb687e074>
>>> door1.open
<bound method Door.open of <__main__.Door object at 0xb6f9834c>>
>>> type(Door.open)
<class 'function'>
>>> type(door1.open)
<class 'method'>
```

As you can see, Python tells the two apart recognizing the first as a _function_ and the second as a _method_, where the second is a function bound to an instance.

What if we want to define a function that operates on the class instead of operating on the instance? As we may define class attributes, we may also define _class methods_ in Python, through the `classmethod` decorator. Class methods are functions that are bound to the class and not to an instance.

``` python
class Door:
    colour = 'brown'

    def __init__(self, number, status):
        self.number = number
        self.status = status

    @classmethod
    def knock(cls):
        print("Knock!")

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
```

and Python identifies both as (bound) methods

``` pycon
>>> door1.__class__.__dict__['knock']
<classmethod object at 0xb67ff6ac>
>>> door1.knock
<bound method type.knock of <class '__main__.Door'>>
>>> Door.knock
<bound method type.knock of <class '__main__.Door'>>
>>> type(Door.knock)
<class 'method'>
>>> type(door1.knock)
<class 'method'>
```

As you can see the `knock()` function accepts one argument, which is called `cls` just to remember that it is not an instance but the class itself. This means that inside the function we can operate on the class, and the class is shared among instances.

``` python
class Door:
    colour = 'brown'

    def __init__(self, number, status):
        self.number = number
        self.status = status

    @classmethod
    def knock(cls):
        print("Knock!")

    @classmethod
    def paint(cls, colour):
        cls.colour = colour

    def open(self):
        self.status = 'open'
        
    def close(self):
        self.status = 'closed'
```

The `paint()` classmethod now changes the class attribute `colour` which is shared among instances. Let's check how it works

``` pycon
>>> door1 = Door(1, 'closed')
>>> door2 = Door(2, 'closed')
>>> Door.colour
'brown'
>>> door1.colour
'brown'
>>> door2.colour
'brown'
>>> Door.paint('white')
>>> Door.colour
'white'
>>> door1.colour
'white'
>>> door2.colour
'white'
```

The class method can be called on the class, but this affects both the class and the instances, since the `colour` attribute of instances is taken at runtime from the shared class.

``` pycon
>>> door1.paint('yellow')
>>> Door.colour
'yellow'
>>> door1.colour
'yellow'
>>> door2.colour
'yellow'
```

Class methods can be called on instances too, however, and their effect is the same as before. The class method is bound to the class, so it works on this latter regardless of the actual object that calls it (class or instance).

## Movie Trivia

Section titles come from the following movies: _The Empire Strikes Back (1980)_, _Raiders of the Lost Ark (1981)_, _Revenge of the Nerds (1984)_, _When Harry Met Sally (1989)_.

## Sources

You will find a lot of documentation in [this Reddit post](http://www.reddit.com/r/Python/comments/226ahl/some_links_about_python_oop/). Most of the information contained in this series come from those sources.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.

## Next post

[Python 3 OOP Part 3 - Delegation: composition and inheritance](/blog/2014/08/20/python-3-oop-part-3-delegation-composition-and-inheritance)