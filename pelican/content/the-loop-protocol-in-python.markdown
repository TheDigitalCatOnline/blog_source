Title: The loop protocol in Python
Date: 2019-06-06 21:00:00 +0100
Category: Programming
Tags: Python, Python3
Authors: Leonardo Giordani
Slug: the-loop-protocol-in-python
Summary: 

# Iterations

**Iteration** in Python, like in other languages, is a process ruled by the **for** statement and allows to repeatedly execute a block of code, assigning to a variable a value extracted on each cycle from a given ordered set (if not specified otherwise, _set_ in this post is used in the broad sense of a collection of values). The simplest case of iteration is the processing of a list of values

``` python
for i in [0, 1, 2, 3]:
    print(i)
```

In Python, however, iteration is more than simple loop over the elements of an array. The for statement implements a well-defined protocol, which allows to build very complex objects.

To understand the structure of iteration in Python we have to clarify the difference between _iterable_ and _iterator_ objects.

## Iterators

In Python an iterator is an object with the following properties:

* it contains a set of data.
* it exposes the `__next__` magic method, which returns one of the contained elements at each call. Each element is returned only one time. This method goes through the whole set of data the iterator incorporates.
* after the `__next__` method returns the last element any successive call of this method raises the `StopIteration` exception. This signals that the iterator is exhausted.
* it exposes the `__iter__` method that returns the iterator itself.

## Iterables

The definition of iterable is more generic: an iterable is a container of data that exposes either the `__getitem__` or the `__iter__` methods (or both):

* `__getitem__(i)` shall return the value at the given position `i` or raise the `IndexError` exception if there is no data at that position.
* `__iter__()` shall return an iterator on the data contained in the iterable

As you can see the `__getitem__` method considers the data as an ordered set, which is not always the case; for this reason an iterable may define the two different methods, or both.

From the previous definitions you see that an iterator is also automatically an iterable, since it exposes the `__iter__` method that returns an iterator (itself).

# Loop protocol

Back to the loop syntax, we can clarify the matter saying that in Python **the for statement expects an iterable as argument**. This means that we can make any object usable in a for loop, simply adding one of the two previously mentioned methods, `__getitem__` or `__iter__`, with a suitable implementation.

Let’s look at an example:

``` python
class AnIterator:
    def __init__(self, value):
        self.value = value

    def __next__(self):
        if self.value <= 0:
            raise StopIteration
        tmp = self.value
        self.value = self.value - 1
        return tmp

    def __iter__():
        return self
```

This object is an iterator since it exposes `__iter__()`. Its `next()` method returns the decreasing sequence of integer numbers starting from a given number. Testing it we obtain

``` pycon
>>> iterator = AnIterator(3)
>>> print iterator.next()
3
>>> print iterator.next()
2
>>> print iterator.next()
1
>>> print iterator.next()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 7, in next
StopIteration
>>>
>>> iterator = AnIterator(3)
>>> for i in iterator:
...     print i
...
3
2
1
```

This execution shows that the iterator can be used in a for loop. Pay attention to the fact that I had to instance twice the class, since the first three calls of next() exhausted the first instance.

Let’s dive a little more inside what happens when the for loop runs. The Python code

``` python
for i in iterable:
    some_code
```

is equivalent to

``` python
_iter = iterable.__iter__()
while 1:
    try:
        i = _iter.next()
    except StopIteration:
        break
    some_code
```

The for construct receives here an **iterable** object and calls its `__iter__()` method, obtaining an **iterator** object; then it calls the `next()` method of the latter until the `StopIteration` exception is raised. The actual code is a little different, here simplified for clarity’s sake; if you want to learn more about it check the following addresses

* [Python Glossary](http://docs.python.org/2/glossary.html)
* [Built-in iter()](http://docs.python.org/2/library/functions.html#iter)
* [Iterators and sequences](http://docs.python.org/2/library/stdtypes.html#typeiter)

## Conclusion

This first post tried to summarize the loop protocol implemented by the for statement, which is in Python very different from many classic languages. Next post will explore the concept of generator and its Python implementation.

