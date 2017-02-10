Title: Python 3 generators in depth
Date: 2016-12-06 23:00:00 +0100
Category: Programming
Tags: concurrent programming, generators, Python, Python3
Authors: Leonardo Giordani
Slug: python3-generators-in-depth
Summary: 

This post is an update of a series of three posts I wrote in 2013. In those posts I tried to explain what iterators and generators are in Python and how you could use them to achieve cooperative multitasking.

Python, however, is an evolving language, and its third version is continuously shipping new features. Even generators have received new features and the new asyncio framework is definitely worth being understood and used. As a matter of fact those posts are now severely out of date.

So I decided to move the content of those posts to this new one, updating it with the new features Python implemented both in the 2.x and in the 3.x versions. The proposed implementation of cooperative multitasking taken from the Kamaelia project will then be moved to a stand-alone post.

## Introduction

Python is a language that in 26 years of life has been through a very remarkable development and the introduction of several new features, sometimes borrowed from other languages, sometimes arisen from the needs of developers and heavily discussed before being officially implemented. One of these improvements concerns generators, a concept which can be found in the computer science environment since the 70s; it has been implemented in Python from version 2.2 (2001) and became popular from version 2.3 (2003).

Generators are a generalization of functions that allow to deal in a more complete and rich way with iterations, repeated executions and in general with everything concerns the program flow. In the last years a concept which was considered obsolete started to spread again, namely that of cooperative multitasking. This concept has been shadowed for some years by the advent of multiprocessing and multithreading but, as happened to interpreted languages and virtual machines, as time passes and contexts change good ideas rise again and prove to be anything but dead.

After their introduction, Python generators have been furtherly enhanced with new methods and allowed to implement coroutines, which again are a concept that has been introduced back in the old days when assembly language was the main tool of software developers. Python then just recently introduced coroutines as a proper language feature with a new syntax.

To get a clear understanding of what coroutines are and how asyncio can simplify writing asyncrhronous code, however, we have to start from scratch, learning something new about loops.

## Iterations

**Iteration** in Python, like in other languages, is a process ruled by the **for** statement and allows to repeatedly execute a block of code, assigning to a variable a value extracted at each execution from a given ordered set. The simplest case of iteration is the processing of a list of values

``` python
for i in [0,1,2,3]:
    print i
```

In Python, however, iteration is more than simple loop over the elements of an array. The for statement implements a _well-defined and nontrivial protocol_, which allows to build very complex objects.

To understand the structure of iteration in Python we have to clarify what the difference is between iterable and iterator objects.

#### Iterators

In Python jargon an iterator is an object with the following properties:

* it contains a **set of data**
* it exposes the `__next__()` method, which returns one of the contained elements at each call. Each element is returned only one time. This method goes through the whole set of data the iterator incorporates.
* after the `__next__()` method returns the last element any successive call of this method raises the `StopIteration` exception. This signals that the iterator is exhausted.
* it exposes the `__iter__()` method that returns the iterator itself.

Basically the iterator is a software component that performs the action of iterating over a set of data. In Python the iterator can actually contain data instead of referencing another object, and indeed if you try to iterate on its data set with `__iter__()` you get the iterator itself in return.

#### Iterables

The definition of iterable, on the other hand, is more generic: an iterable is a container of data that exposes either the `__getitem__()` or the `__iter__()` methods (or both):

* `__getitem__(i)` shall return the value at the given position `i` or raise the `IndexError` exception if there is no data at that position.
* `__iter__()` shall return an iterator on the data contained in the iterable

As you can see the `__getitem__()` method considers the data as an ordered set, which is not always the case; for this reason an iterable may define just one of the two methods or both.

From the previous definitions you see that an iterator is also automatically an iterable, since it exposes the `__iter__()` method that returns an iterator (itself).

#### Loop protocol

Back to the loop syntax from above we can clarify the matter saying that in Python **the for statement expects an iterable as argument**. This means that we can give any object the property of being usable in a for loop simply exposing one of the two previously mentioned methods, `__getitem__()` or `__iter__()`.

Let’s look at an example:

``` python
class AnIterator(object):
    def __init__(self, value):
        self.value = value

    def __next__(self):
        if self.value <= 0:
            raise StopIteration
        tmp = self.value
        self.value = self.value - 1
        return tmp

    def __iter__(self):
        return self
```

This object is an iterator since it exposes `__iter__()`. Its `__next__()` method returns the decreasing sequence of integer numbers starting from a given number. Testing it we obtain

``` pycon
>>> iterator = AnIterator(3)
>>> print(iterator.__next__())
3
>>> print(iterator.__next__())
2
>>> print(iterator.__next__())
1
>>> print(iterator.__next__())
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 6, in __next__
StopIteration
>>> 
>>> iterator = AnIterator(3)
>>> for i in iterator:
...     print(i)
... 
3
2
1
```

This execution shows that the iterator can be used in a for loop. Pay attention to the fact that I had to instance twice the class, since the first three calls of `__next__()` exhausted the first instance. Please note that usually you do not call `__next__()` directly, instead you use the built-in function `next()`.

Let’s dive a little more inside what happens when the for loop runs. The Python code

``` python
for i in iterable:
    some_code
```

is equivalent to

``` python
iterator = iterable.__iter__()
while True:
    try:
        i = iterator.__next__()
    except StopIteration:
        break
    some_code
```

The for construct receives here an **iterable** object and calls its `__iter__()` method, obtaining an **iterator** object; then it calls the `__next__()` method of this latter until the `StopIteration` exception is raised. The actual code is a little different, here simplified for clarity’s sake; if you want to learn more about it check the following addresses

* [Python Glossary](https://docs.python.org/3/glossary.html)
* [Built-in iter()](https://docs.python.org/3/library/functions.html#iter)
* [Built-in next()](https://docs.python.org/3/library/functions.html#next)
* [Iterators and sequences](https://docs.python.org/3/library/stdtypes.html#typeiter)

## Generators

The `for` construct is generally simple to use and such a loop can be found in almost all programming languages. Its implementation, however, can be problematic in some cases. Let’s look at an example:

``` python
def sequence(num):
    s = []
    i = 0
    while i != num:
        s.append(i)
        i += 1
    return s

for i in sequence(5):
    print(i)
```

At first `sequence()` seems to be a good function without big defects (for the sake of simplicity error checking has been intentionally omitted - it does not work with negative numbers for example - and the code is intentionally non-pythonic).

The problem concealed in such code is that the function does not execute return until the whole list has been built and until the function returns the loop does not start. Thus when the loop begins the function already processed the whole data set.

While you are building normal sequences of numbers this can be considered irrelevant, even for rather long ones. The problem gets worse when the data set becomes very big or when the creation of an element is very demanding process; in the first case the function might fill the memory, whereas in the second one the whole execution can last for a very long time. Both conditions block the loop before it even produces the first element.

The solution can be found in the generation concept: generating, in this context, means producing only one element of the sequence at each function call. This way each call will take the minimum amount of memory and CPU time needed to create the element, and the loop will not wait for the whole sequence to be generated to produce the first element.

To allow the implementation of such a solution without using global variables generators have been introduced in Python. A **generator** is a special type of iterator, its peculiarity being the way it is built. Aside from this, generators behave the same exact way as iterators.

A generator is built from every function that contains the **yield** statement; `yield`’s behaviour follows that of the `return` statement, i.e. it terminates the function returning a value to the caller. But whereas return permanently terminates the function, giving up local variables to the garbage collector, `yield` freezes the function’s code, allowing a later call to resume execution immediately after `yield`, with all local variables initialized as they were during the previous execution.

Pay attention to the fact that the first call of a function that contains an `yield` statement returns a generator immediately, without executing a single line of the function’s code. Since a generator is an iterator it automatically exposes the `__next__()` method that, when called, will actually continue the execution of the frozen function.

A simple example of generator is the following:

``` python
def dec(num):
    x = num
    while 1:
        x -= 1
        yield x
```

``` pycon
>>> g = dec(8)
>>> g
<generator object dec at 0xb6abbf2c>
```

When executed, the `dec()` function returns a **generator object** and no lines of code have been executed (i.e. `x` has not yet been initialized).

As soon as `__next__()` is called for the first time `x` will be initialized with the value passed to the function (`8` in the example above) and the infinite loop will start. Inside it the local variable `x` will be decremented and returned to the caller by `yield`. This latter will also freeze again the code, holding the internal state, in this case the value of the variable `x`.

``` pycon
>>> g.__next__()
7
>>> g.__next__()
6
>>> g.__next__()
5
>>> g.__next__()
4
```

As you can see the generator `g` remembers its previous state and acts accordingly.

Since generators are iterators they are allowed to raise the `StopIteration` exception to signal their exhaustion. In the previous example the `dec()` function never raises it, thus providing an infinite generator (or infinite iterator).

Another caveat: as it happens with `return`, a function is not limited to one single statement, but can contain more than one of them; obviously the peculiarity of `yield` makes this scenario rather complex, opening at the same time remarkable possibilities such as that of easily build state machines.

What happens if we call the function again, maybe with a different value of the parameter? We simply obtain a new generator object that is completely independent from the first one, although it behaves the same way.

``` pycon
>>> f = dec(12)
>>> f
<generator object dec at 0xb72e1cac>
>>> f.__next__()
11
>>> g.__next__()
3
>>> f.__next__()
10
>>> g.__next__()
2
```

Let’s recap the whole concept. A function containing the `yield` statement is called _generator function_ and when executed it returns a _generator object_; this is nothing more than a simple iterator, which automatically implements the freeze and resume of the function’s code.

Pay attention to the fact that a generator is an iterator (it exposes `__next__()` and `__iter__()` methods and may raise `StopIteration`) but the opposite is not always true. A generator is an iterator built by a generator function, i.e. through the use of the `yield` statement; as explained there are other ways to build iterators.

Back to the `sequence()` function showed above we can now write it as a generator function and use it to build arbitrarily long sequences and, at the limit, infinite.

``` python
def sequence(num):
    i = 0
    while 1:
        if i == num:
            raise StopIteration
        yield i
        i += 1
```

``` pycon
>>> s = sequence(3)
>>> s.__next__()
0
>>> s.__next__()
1
>>> s.__next__()
2
>>> s.__next__()
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 5, in sequence
StopIteration
>>>
>>> for i in sequence(3):
...  print i
...
0
1
2
```

## Generators and iterators

A question might arise: if generators are iterators why should we use the `yield`-based method to create them? After all, generator functions are simple to write but not so simple to manage, due to the radically different behaviour from that of standard functions. Wouldn’t it be enough to create an iterable object that at each call of the `__next__()` method creates the correct element of the sequence?

The answer is certainly affirmative: everything you can do with generators can be done with standard iterators. There are two caveats, however.

First consideration is about performances: instancing an object and calling its methods is slower than calling a function. Every time you hear talking about performance problems always think about Web services, administration or scientific tools and in general about big amounts of data. Obviously a script that manages a dozen or so files is not noticeably affected by such problems.

Second consideration is about code complexity, since writing a function is simpler than writing an object. Take into account, however, that an iterable object can be enriched by custom methods that make it more flexible than a generator; for instance a `reset()` method that can restore the object to its initial state.
