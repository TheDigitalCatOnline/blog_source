Title: Python generators - from iterators to cooperative multitasking
Date: 2013-03-25 10:41 +0100
Category: Programming
Tags: Python, generators
Authors: Leonardo Giordani
Slug: python-generators-from-iterators-to-cooperative-multitasking
Series: "Python generators - from iterators to cooperative multitasking"
Summary:

## Introduction

Python is a language that in 11 years of life has been through a very remarkable development and the introduction of several new features, sometimes borrowed from other languages, sometimes arisen from the needs of developers and heavily discussed before being officially implemented. One of these improvements concerns generators, a concept which can be found in the computer science environment since the 70s; it has been implemented in Python from version 2.2 (2001) and became popular from version 2.3 (2003).

Generators are a generalization of functions that allow to deal in a more complete and rich way with iterations, repeated executions and in general with everything concerns the program flow. In the last years a concept which was considered obsolete started to spread again, namely that of cooperative multitasking. This concept has been shadowed for some years by the advent of multiprocessing and multithreading but, as happened to interpreted languages and virtual machines, as time passes and contexts change good ideas rise again and prove to be anything but dead.

In the Python world, in particular, numerous solutions have appeared which endorse the use of microthreads: these are parallel execution flows without implicit scheduling as opposed to what happens with traditional processes and threads. The big advantage of such objects is the ease of implementation and management of the multiprogramming code, since all synchronization and data protection problems simply do not exist. On the other hand, their use requires a voluntary scheduling, in other words a system that explicitly acquires and releases system resources.

To start talking about cooperative multitasking in Python, thus, it is imperative to understand generators. This first post reviews the concept of iteration and its implementation.

## Iterations

**Iteration** in Python, like in other languages, is a process ruled by the **for** statement and allows to repeatedly execute a block of code, assigning to a variable a value extracted at each execution from a given ordered set. The simplest case of iteration is the processing of a list of values

``` python
for i in [0,1,2,3]:
	print i
```

In Python, however, iteration is more than simple loop over the elements of an array. The for statement implements a well-defined and nontrivial protocol, which allows to build very complex objects.

To understand the structure of iteration in Python we have to clarify what is the difference between iterable and iterator objects.

#### Iterators

In Python jargon an iterator is an object with the following properties:

* it contains a **set of data**
* it exposes the `next()` method, which returns one of the contained elements at each call. Each element is returned only one time. This method goes through the whole set of data the iterator incorporates. In Python 3 this method has been renamed `__next__()`.
* after the `next()` method returns the last element any successive call of this method raises the `StopIteration` exception. This signals that the iterator is exhausted.
* it exposes the `__iter__()` method that returns the iterator itself.

#### Iterables

The definition of iterable, on the other hand, is more generic: an iterable is a container of data that exposes either the `__getitem__()` or the `__iter__()` methods (or both):

* `__getitem__(i)` shall return the value at the given position `i` or raise the `IndexError` exception if there is no data at that position.
* `__iter__()` shall return an iterator on the data contained in the iterable

As you can see the `__getitem__()` method considers the data as an ordered set, which is not always the case; for this reason an iterable may define the two different methods, or both.

From the previous definitions you see that an iterator is also automatically an iterable, since it exposes the `__iter__()` method that returns an iterator (itself).

#### Loop protocol

Back to the loop syntax from above we can clarify the matter saying that in Python **the for statement expects an iterable as argument**. This means that we can give any object the capability of being used in a for loop, simply exposing one of the two previously mentioned methods, `__getitem__()` or `__iter__()`.

Let’s look at an example:

``` python
class AnIterator(object):
	def __init__(self, value):
		self.value = value
		
	def next(self):
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

The for construct receives here an **iterable** object and calls its `__iter__()` method, obtaining an **iterator** object; then it calls the `next()` method of this latter until the `StopIteration` exception is raised. The actual code is a little different, here simplified for clarity’s sake; if you want to learn more about it check the following addresses

* [Python Glossary](http://docs.python.org/2/glossary.html)
* [Built-in iter()](http://docs.python.org/2/library/functions.html#iter)
* [Iterators and sequences](http://docs.python.org/2/library/stdtypes.html#typeiter)

## Conclusion

This first post tried to summarize the loop protocol implemented by the for statement, which is in Python very different from many classic languages. Next post will explore the concept of generator and its Python implementation.

## Next articles

* [Python Generators - From Iterators to Cooperative Multitasking 2](/blog/2013/03/26/python-generators-from-iterators-to-cooperative-multitasking-2)
* [Python Generators - From Iterators to Cooperative Multitasking 3](/blog/2013/03/29/python-generators-from-iterators-to-cooperative-multitasking-3)
