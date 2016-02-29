Title: Accessing attributes in Python
Date: 2015-01-12 17:00:00 +0100
Category: Programming
Tags: Python, Python3, OOP
Authors: Leonardo Giordani
Slug: accessing-attributes-in-python
Summary: A comprehensive review of the attribute management techniques in Python

Python is a language that tries to push the object-oriented paradigm to its maximum. This means that its object model is very powerful compared to that of other languages, but also that the behaviour of Python code may result surprising to new programmers.

In this post I want to review the methods that Python provides to access object attributes, trying to provide a comprehensive overview of the matter to everyone wants to start programming in this beautiful language.

## What are attributes

Since the nomenclature may vary from language to language, let me name things. In Python we call _attribute_ everything is contained inside an object. In Python there is no real distinction between plain data and functions, being both objects, so what I will say about attributes is perfectly valid even for methods.

As a working example, in this post I will use the following class. It represents a book with a title and an author. It also provides a `get_entry()` method which returns a string representation of the book.

``` python
class Book:
    def __init__(self, title, author):
        self.title = title
        self.author = author

    def get_entry(self):
        return "{0} by {1}".format(self.title, self.author)
```

Every instance of this class will contain three attributes, namely `title`, `author`, and `get_entry`, in addition to the standard attributes provided by the `object` ancestor.

**Python 2 users**: remember that in Python 2 you have to specify that `Book` is a new-style class writing `class Book(object):`

## Basic attribute access

In Python you may call an attribute of an object using the widely accepted dotted-syntax

``` pycon
>>> b = Book(title="Pawn of Prophecy", author="David Eddings")
>>> b.title
'Pawn of Prophecy'
```

As already mentioned, this mechanism works with methods too.

``` pycon
>>> b.get_entry
<bound method Book.get_entry of <__main__.Book object at 0xb703952c>>
```

Here I intentionally omitted the calling brackets (parentheses) to show what happens when accessing the method. For a in-depth explanation of the difference between functions and bound methods read [this post](/blog/2014/08/20/python-3-oop-part-2-classes-and-members).

When an object does not contain the attribute we are looking for, Python raises an `AttributeError` exception

``` pycon
>>> b.publisher
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'Book' object has no attribute 'publisher'
>>>
```

Remember also that Python objects provide a wide range of automatically created attributes such as `__class__` or `__doc__`. These attributes can be read exactly like standard attributes, since in Python the double underscore prefix and suffix is just a gentleman agreement between programmers.

``` pycon
>>> b.__class__
<class '__main__.Book'>
```

When you try to change the value of an attribute (you _write_ the attribute) the syntax does not change.

``` pycon
>>> b = Book(title="Pawn of Prophecy", author="David Eddings")
>>> b.author = "David Carroll Eddings"
>>> b.author
'David Carroll Eddings'
```

## Properties

Sometimes you want to have an attribute which value comes from other attributes or, in general, which value shall be computed at the moment. The standard way to deal with this situation is to create a method, called _getter_, just like I did with `get_entry()`. 

In Python you can "mask" the method, aliasing it with a data attribute, which in this case is called _property_.

``` python
class Book(object):
    def __init__(self, title, author):
        self.title = title
        self.author = author

    def get_entry(self):
        return "{0} by {1}".format(self.title, self.author)

    entry = property(get_entry)
```

The above syntax defines an `entry` attribute which automatically calls `self.get_entry()` when read.

``` pycon
>>> b = Book(title="Pawn of Prophecy", author="David Eddings")
>>> b.entry
'Pawn of Prophecy by David Eddings'
```

Properties allow to specify also a write method (a _setter_), that is automatically called when you try to change the value of the property itself.

``` python
class Book(object):
    def __init__(self, title, author):
        self.title = title
        self.author = author

    def _get_entry(self):
        return "{0} by {1}".format(self.title, self.author)

    def _set_entry(self, value):
        if " by " not in value:
            raise ValueError("Entries shall be formatted as '<title> by <author>'")
        self.title, self.author = value.split(" by ")

    entry = property(_get_entry, _set_entry)
```

Please note that the set method shall accept the value as its second parameter. When getters and setters are masked by a property it may be a good thing to make their name start with an underscore, just to signal that they are not intended to be used directly. Remember however that this has no special meaning for the language, being just a convention between programmers.

``` pycon
>>> b = Book(title="Pawn of Prophecy", author="David Eddings")
>>> b.entry
'Pawn of Prophecy by David Eddings'
>>> b.entry = "Queen of Sorcery by David Carroll Eddings"
>>> b.title
'Queen of Sorcery'
>>> b.author
'David Carroll Eddings'
>>> b.entry = "Magician's Gambit, David Carroll Eddings"
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 9, in _set_entry
ValueError: Entries shall be formatted as '<title> by <author>'
```

While attributes are usually defined in the instance through the `__init__()` method (part of the constructor mechanism), properties are part of the class itself.

``` pycon
>>> Book.title
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: type object 'Book' has no attribute 'title'
>>> Book.entry
<property object at 0xb676a374>
```

Remember however that all instances of that class share the same _property object_, which is a sort of automatic binding between attributes and methods, and not the value of the attribute itself.

## Softcoding attribute access

When you write a basic attribute access like I did in the first section you are _hardcoding_ the attribute name. The attribute you are looking for is part of the source code itself, and shall be known at coding time.

What happens if you want to access an attribute whose name is contained in a variable as a string? This usually happens when writing debuggers or inspection tools that let the user interactively specify the attributes they want to see.

To perform this "indirect" access Python provides the `getattr()` builtin function, which accepts an object and the name of an attribute.

``` pycon
>>> getattr(b, 'title')
'Pawn of Prophecy'
>>> getattr(b, 'get_entry')
<bound method Book.get_entry of <__main__.Book object at 0xb703952c>>
```

This type of attribute access may be useful even when accessing a lot of attributes, allowing to write simple for loops or list comprehensions

``` pycon
>>> for attr in ['title', 'author']:
...  getattr(b, attr)
... 
'Pawn of Prophecy'
'David Eddings'
```

While this type of access is perfectly valid in Python you should not use it as long as you can use the direct dotted access. Writing tricky code may seem very smart, but when it comes to debugging "the simpler the better".

## Avoiding to fail

When you try to access an attribute that does not exist, either with the dotted syntax or with `getattr()`, Python gives you a last chance before raising the `AttributeError` exception. It calls the `__getattr__()` special method (pay attention to the double underscores) passing it the name of the attribute: in the previous example, when accessing `b.publisher`, Python calls `b.__getattr__('publisher')`. 

In this case, since the `Book` class or its ancestors do not define the `__getattr__()` method, the attribute access fails and Python raises the exception.

Let us try to define the method, just to show that it actually works

``` python
class Book(object):
    def __init__(self, title, author):
        self.title = title
        self.author = author

    def get_entry(self):
        return "{0} by {1}".format(self.title, self.author)

    def __getattr__(self, attr):
        return None
```

**WARNING**: this is just an example to show how `__getattr__()` works. The code presented here shall _not_ be considered a good example of Python programming.

``` pycon
>>> b = Book(title="Pawn of Prophecy", author="David Eddings")
>>> b.title
'Pawn of Prophecy'
>>> b.publisher
>>> b.somename
```

As you can see the `publisher` and `somename` attributes are correctly accessed even if they do not actually exist inside the object.

To find a good use for `__getattr__()` and `getattr()` we need to step into something a bit more complex, which is the explicit delegation mechanism involved in a composition between objects. You may find more details on this topic in [this post](/blog/2014/08/20/python-3-oop-part-3-delegation-composition-and-inheritance). 

Both `getattr()` and `__getattr__()` have their counterpart to manage writing access, namely `setattr()` and `__setattr__()`.

## The deepest secret

Every Python object (starting from `object` itself) contains a very special method called `__getattribute__()` which should never be called explicitly or overridden. This method implements the attribute resolution inside the object, provides the attribute lookup through the inheritance hierarchy, resolves properties, calls `__getattr__()` and if needed raises the `AttributeError` exception.

Due to the very complex nature of this method, and the uttermost importance of its role in making Python objects run, you shall never, never try to override it. If you find yourself in a situation where dealing explicitly with `__getattribute__()` is needed, you may be pretty sure that you did something wrong.

## Conclusions

As you can figure, 95% of your Python code will be based on standard dotted attribute access. Knowing other ways to manage attributes (properties) and being aware of what happens behind the scenes (`getattr`, `__getattr__()`, and `__getattribute__()`) is however essential to master the whole power of Python, which can sometimes lead to very elegant solutions that are otherwise very difficult to achieve.

## Updates

2015-01-16: Thanks to [Jamie](https://twitter.com/astronouth7303) who spotted a wrong `__setattribute__()` method presented in the last section.