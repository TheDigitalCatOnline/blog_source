Title: Python 3 generators in depth
Date: 2016-12-06 23:00:00 +0100
Category: Programming
Tags: concurrent programming, generators, Python, Python3
Authors: Leonardo Giordani
Slug: python3-generators-in-depth
Summary: 

This post is an update of a [series of posts](blog/2013/03/25/python-generators-from-iterators-to-cooperative-multitasking) I wrote in 2013. In that series of 3 posts I tried to explain what iterators and generators are in Python and how you could use them to achieve cooperative multitasking.

Python, however, is an evolving language, and its third version is continuously shipping new features. Even generators have received new features and the new asyncio framework is definitely worth being understood and used.

In the first part of this post I will mostly discuss again the contents of the older series, so that this post can be read independently. In the second part of the post I will discuss some new features of generators that have been introduced with Python 2.5 and 3.3, and in the third part I will briefly discuss asyncio.

## Iterations

**Iteration** in Python, like in other languages, is a process that extracts values from an ordered set (in the broader sense or an ordered collection of elements) and executes a given block of code for each of these values.

The simplest case of iteration in Python is a `for` loop over the elements of a list:

``` python
for i in [0,1,2,3]:
    print(i)
```

This type of loop is present in many procedural and object-oriented programming languages and can be easily implemented on every machine that provides accumulators and jumps. In Python, however, an iteration is more than simple loop over the elements of an array. The for statement implements a well-defined and nontrivial protocol, which allows to extend the simple concept of loop and build very complex and interesting objects.

To understand the structure of an iteration in Python I have first to clarify what is the difference between iterable and iterator objects.

#### Iterators

In Python jargon an iterator is an object with the following properties:

* it contains a **set of data**
* it exposes the `__next__()` method, which returns one of the contained elements at each call. Each element is returned only one time. This method goes through the whole set of data the iterator incorporates. The elements are thus ordered, but their order doesn't need to be fixed between different readings of the whole set.
* after the `__next__()` method returns the last element any successive call of this method raises the `StopIteration` exception. This signals that the iterator is exhausted.
* it exposes the `__iter__()` method that returns the iterator itself (more on this later).

#### Iterable

The definition of iterable, on the other hand, is more generic: an iterable is a container of data that exposes either the `__getitem__()` or the `__iter__()` methods (or both):

* `__getitem__(i)` shall return the value at the given position `i` or raise the `IndexError` exception if there is no data at that position.
* `__iter__()` shall return an iterator on the data contained in the iterable

As you can see the `__getitem__()` method considers the data as an ordered set, which is not always the case; for this reason an iterable may define the two different methods, or both.
