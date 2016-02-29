Title: Method overriding in Python
Date: 2014-05-19 13:51:26 +0200
Category: Programming
Tags: Python, OOP
Authors: Leonardo Giordani
Slug: method-overriding-in-python
Summary:

What is overriding? Overriding is the ability of a class to change the implementation of a method provided by one of its ancestors.

Overriding is a very important part of OOP since it is the feature that makes inheritance exploit its full power. Through method overriding a class may "copy" another class, avoiding duplicated code, and at the same time enhance or customize part of it. Method overriding is thus a strict part of the inheritance mechanism.


## A quick glance to inheritance

As for most OOP languages, in Python inheritance works through implicit delegation: when the object cannot satisfy a request, it first tries to forward the request to its ancestors, following the specific language rules in the case of multiple inheritance.

An exemple:

``` python
class Parent(object):
    def __init__(self):
        self.value = 5

    def get_value(self):
        return self.value

class Child(Parent):
    pass
```

As you can see the `Child` class is empty, but since it inherits from `Parent` Python takes charge of routing all method calls. So you may use the `get_value()` method of `Child` objects and exerything works as expected.

``` pycon
>>> c = Child()
>>> c.get_value()
5
```

Indeed `get_value()` is not exactly part of the `Child` class as if it were defined in it

``` pycon
>>> p = Parent()
>>> c = Child()
>>>
>>> dir(p)
['__class__', '__delattr__', '__dict__', '__doc__', '__format__',
 '__getattribute__', '__hash__', '__init__', '__module__', '__new__',
 '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__',
 '__str__', '__subclasshook__', '__weakref__', 'get_value', 'value']
>>>
>>> dir(c)
['__class__', '__delattr__', '__dict__', '__doc__', '__format__',
 '__getattribute__', '__hash__', '__init__', '__module__', '__new__',
 '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__',
 '__str__', '__subclasshook__', '__weakref__', 'get_value', 'value']
>>>
>>> dir(Parent)
['__class__', '__delattr__', '__dict__', '__doc__', '__format__',
 '__getattribute__', '__hash__', '__init__', '__module__', '__new__',
 '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__',
 '__str__', '__subclasshook__', '__weakref__', 'get_value']
>>>
>>> dir(Child)
['__class__', '__delattr__', '__dict__', '__doc__', '__format__',
 '__getattribute__', '__hash__', '__init__', '__module__', '__new__',
 '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__',
 '__str__', '__subclasshook__', '__weakref__', 'get_value']
>>>
>>> Parent.__dict__
dict_proxy({'__module__': '__main__',
            'get_value': <function get_value at 0xb69a656c>,
            '__dict__': <attribute '__dict__' of 'Parent' objects>,
            '__weakref__': <attribute '__weakref__' of 'Parent' objects>,
            '__doc__': None,
            '__init__': <function __init__ at 0xb69a6534>})
>>>
>>> Child.__dict__
dict_proxy({'__module__': '__main__', '__doc__': None})
```

This shows that the `Child` class does not actually contain the `get_value()` method and that a mechanism of automatical delegation is active under the hood. For an insight on this mechanism check [this post](/blog/2014/03/05/oop-concepts-in-python-2-dot-x-part-1/).

## Method overriding in action

In Python method overriding occours simply defining in the child class a method with the same name of a method in the parent class. When you define a method in the object you make this latter able to satisfy that method call, so the implementations of its ancestors do not come in play.

``` python
class Parent(object):
    def __init__(self):
        self.value = 5

    def get_value(self):
        return self.value

class Child(Parent):
    def get_value(self):
        return self.value + 1
```

Now `Child` objects behave differently

``` pycon
>>> c = Child()
>>> c.get_value()
6
```

and taking a look inside the class we spot a difference

```
>>> Parent.__dict__
dict_proxy({'__module__': '__main__',
            'get_value': <function get_value at 0xb69a656c>,
            '__dict__': <attribute '__dict__' of 'Parent' objects>,
            '__weakref__': <attribute '__weakref__' of 'Parent' objects>,
            '__doc__': None,
            '__init__': <function __init__ at 0xb69a6534>})
>>>
>>> Child.__dict__
dict_proxy({'__module__': '__main__',
            'get_value': <function get_value at 0xb69a65a4>,
            '__doc__': None})
```

since now the `Child` class actually contains a `get_value()` method with a different implementation (the id of the two functions are different).

This is of uttermost importance in Python. Inheritance delegation occours automatically, but if a method is overridden the implementation of the ancestors is not considered at all. So, if you want to run the implementation of one or more of the ancestors of your class, you have to call them explicitly.

Why should you want to call the implementation of objects that are deeper in the class hierarchy?

You may want to call it because many times you override a method to enhance its nature, that is to improve the "quality" of the result, and to improve something you need to first access it. So, by calling the original implementation, you get the result you later want to improve.

There is however a well defined reason why **you must always call the original implementation**. This reason may be called "hidden side effects".

When you inherit from a class, you are actually inheriting a whole class hierarchy which internal structure is (or shall be considered) unknown. This means that any method call may hide a complex set of operations on the whole class hierarchy, and some of them may be vital for the library or the framework you are using.

Python makes you call the original implementation of an overridden method explicitly (not differently from other object-oriented languages). This surely follows the Python idea that "Explicit is better than implicit" ([The Zen of Python](http://legacy.python.org/dev/peps/pep-0020/)), but this advice is not just a matter of taste or some sort of programming mannerism.

When you override you have to think if you want to filter the arguments for the original implementation, if you want to filter its results, or both. You typically want to filter arguments (pre-filter) if you want to change the data that the parent implementation shall process while you filter the results (post-filter) if you want to add an additional processing layer. Obviously both things may be done together in the same method. Since you have to explicitly call the parent implementation you are free to do it where you want in the code of the new method: the decision about the type of filtering you want to achieve affects the position of the call.

#### An example of pre-filtering

``` python
import datetime

class Logger(object):
    def log(self, message):
        print message

class TimestampLogger(Logger):
    def log(self, message):
        message = "{ts} {msg}".format(ts=datetime.datetime.now().isoformat(),
                                      msg=message)
        super(TimestampLogger, self).log(message)
```

The `TimestampLogger` object adds some information to the message string before calling the original implementation of its `log()` method.

``` pycon
>>> l = Logger()
>>> l.log('hi!')
hi!
>>>
>>> t = TimestampLogger()
>>> t.log('hi!')
2014-05-19T13:18:53.402123 hi!
>>> 
```

#### An example of post-filtering

``` python
import os

class FileCat(object):
    def cat(self, filepath):
        f = file(filepath)
        lines = f.readlines()
        f.close()
        return lines

class FileCatNoEmpty(FileCat):
    def cat(self, filepath):
        lines = super(FileCatNoEmpty, self).cat(filepath)
        nonempty_lines = [l for l in lines if l != '\n']
        return nonempty_lines
```

When you use the `FileCatNoEmpty` object you get the result of the `FileCat` object with the empty lines stripped.

As you can see while in the first example the original implementation has been called as the last thing, in the second one it is called before everything else. There is therefore no fixed position for the call of the original method, and it depends on what you want to do.

## Always call super()?

Shall we *always* call the original method implementation? In theory a well designed API should make it always possible but we know that boundary cases exist: the original method may have side effect that you want to avoid and sometimes the API cannot be refactored to avoid them. In those cases you may prefer to skip the call to the original implementation of the method; Python does not make it mandatory, so feel free to walk that path if you think the situation requires it. Be sure to know what you are doing, however, and document why you are completely overwriting the method.

## Summary

* Call the original implementation of a method you are overriding whenever possible. This meakes the underlying API work as expected. When in need of skipping the call be sure to document the reasons.
* Always use `super(cls, self)` for Python 2.x or `super()` for Python 3.x to call the original implementation of a method. This respects the resolution order in case of multiple inheritance and, for Python 3.x, protects from changes in the class hierarchy.
* If you call to the original implementation of a method do it as soon as you have all the data you need to run it.

## Updates

2014-05-21 As suggested by [caseyweb](http://www.reddit.com/user/caseyweb) in [this comment](http://www.reddit.com/r/Python/comments/25y4hu/method_overriding_in_python/chn5v1q) the conclusions of the post were too strong, so I updated the post with a new section ("Always call super()?") and the summary. Many thanks caseyweb!

## Links

* [Python official documentation on classes](https://docs.python.org/3/tutorial/classes.html)

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
