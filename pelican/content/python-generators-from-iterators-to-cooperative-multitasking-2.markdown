Title: Python Generators - From Iterators to Cooperative Multitasking - 2
Date: 2013-03-26 14:05 +0100
Category: Programming
Tags: Python, generators
Authors: Leonardo Giordani
Slug: python-generators-from-iterators-to-cooperative-multitasking-2
Series: "Python generators - from iterators to cooperative multitasking"
Summary:

## Introduction

After the recap of the iteration process in Python, in this post we will introduce the concept of generator, which aims to solve some problems that arise from the use of iterators.

## Generators

The for construct is generally simple to use and such a loop can be found in almost all programming languages. Its implementation, however, can be problematic in some cases. Let’s look at an example:

``` python
def sequence(num):
	s = []
	i = 0
	while i != num:
		s.append(i)
		i += 1
	return s

for i in sequence(5):
	print i
```

At  first sight `sequence()` seems to be a good function without big defects (for the sake of simplicity error checking has been intentionally omitted - it does not work with negative numbers for example - and the code is intentionally non-pythonic).

The problem concealed in such code is that the function does not execute return until the whole list has been built and until the function returns the loop does not start. Thus when the loop begins the function already processed the whole data set.

While you are building normal sequences of numbers this can be considered irrelevant, even for rather long ones. The problem gets worse when the data set becomes very big or when the creation of an element is very demanding process; in the first case the function might fill the memory, whereas in the second one the whole execution can last a very long time. Both conditions happen even before the loop produces the first element.

The solution can be found in the generation concept: generating, in this context, means producing only one element of the sequence at each function call. This way each call will take the minimum amount of memory and CPU time needed to create the element and the loop will start immediately.

To allow the implementation of such a solution without using global variables generators have been introduced in Python. A **generator** is a special type of iterator, its peculiarity being the way it is built. Aside from this, generators behave the same exact way as iterators.

A generator is built from every function that contains the **yield** statement; `yield`’s behaviour follows that of the `return` statement, i.e. it terminates the function returning a value to the caller. But whereas return permanently terminates the function, giving up local variables to the garbage collector, `yield` freezes the function’s code, allowing a later call to resume execution immediately after `yield`, with all local variables initialized as they were during the previous execution.

Pay attention to the fact that the first call of a function that contains an `yield` statement returns a generator immediately, without executing a single line of the function’s code. Since a generator is an iterator it automatically exposes a next() method that, when called, will actually continue the execution of the frozen function.

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

As soon as `next()` is called for the first time `x` will be initialized with the value passed to the function (8 in the example above) and the infinite loop will start. Inside it the local variable `x` will be decremented and returned to the caller by `yield`. This latter will also freeze again the code, holding the internal state, in this case the value of the variable `x`.

``` pycon
>>> g.next()
7
>>> g.next()
6
>>> g.next()
5
>>> g.next()
4
```

As you can see the generator `g` remembers its previous state and acts accordingly.

Since generators are iterators they are allowed to raise the `StopIteration` exception to signal their exhaustion. In the previous example the `dec()` function never raises it, thus providing an infinite generator (or infinite iterator).

Another caveat: as for return a function is not limited to one statement, but can contain more than one; obviously the peculiarity of yield makes this scenario rather complex, opening at the same time remarkable possibilities such as that of easily build state machines.

What happens if we call the function again, maybe with a different value of the parameter? We simply obtain a new generator object that is completely independent from the first one, although it behaves the same way.

``` pycon
>>> f = dec(12)
>>> f
<generator object dec at 0xb72e1cac>
>>> f.next()
11
>>> g.next()
3
>>> f.next()
10
>>> g.next()
2
```

Let’s recap the whole concept. A function containing the `yield` statement is called generator function and when executed returns a generator object; this is nothing more than a simple iterator, which automatically implements the freeze and resume of the function’s code.

Pay attention to the fact that a generator is an iterator (it exposes `next()` and `__iter__()` methods and may raise `StopIteration`) but the opposite is not always true. A generator is an iterator built by a generator function, i.e. through the use of the `yield` statement; as explained there are other ways to build iterators.

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
>>> s.next()
0
>>> s.next()
1
>>> s.next()
2
>>> s.next()
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

A question might arise: if generators are iterators why should we use the `yield`-based method to create them? After all, generator functions are simple to write but not so simple to manage, due to the radically different behaviour from that of standard functions. Wouldn’t it be enough to create an iterable object that at each call of the `next()` method creates the correct element of the sequence?

The answer is certainly affirmative: everything you can do with generators can be done with standard iterators. There are two caveats, however.

First consideration is about performances: instancing an object and calling its methods is slower than calling a function. Every time you hear talking about performance problems always think about Web services, administration or scientific tools and in general about big amounts of data. Obviously a script that manages a dozen or so files is not noticeably affected by such problems.

Second consideration is about code complexity, since writing a function is simpler than writing an object. Take into account, however, that an iterable object can be enriched by custom methods that make it more flexible than a generator; for instance a `reset()` method that can restore the object to its initial state.

## Generator expressions

The two observations above identify the problems that generators can solve in a simpler way than iterators do; one of those problems concerns the processing of long arrays of data.

Every Python programmer uses and hopefully appreciates the elegance of list comprehension. The following code instances with a single line 100 objects of the `MyObject` class and puts them in a list

``` python
object_list = [MyObject() for i in range(100)]
```

where the classic code would be

``` python
object_list = []
for i in range(100):
	object_list.append(MyObject())
```

Surely it is not a lot of code, but it is less easy to understand immediately and less elegant; less pythonic, in a word. List comprehension, however, being nothing more than an alternative syntax to express the above for loop, suffers from the same problems, particularly from performance issues we talked about earlier. Bitter enemies of list comprehensions are long lists and objects which creation is expensive.

Could we take advantage of generators in this case? Yes, with **generator expressions**. The syntax of such expressions is identical to that of list comprehension, except for the use of round brackets instead of square ones. While list comprehension returns a list, however, a generator expression return a generator, as the name implies. The previous code could thus be written this way

``` python
object_generator = (MyObject() for i in range(100))
```

where `object_generator` is a generator like one of those returned by a generator function. This latter form of the code would be

``` python
def object_generator_function():
	for i in range(100):
		yield MyObject()

object_generator = object_generator_function()
```

Which is, as happens for list comprehension and for loops, less elegant than its equivalent shortcut syntax. Obviously, returning a generator has all the advantages we described above: after the generator expression has been executed no element has yet been created. That will happen when the generator is consumed by a for loop or a similar construct.

As for list comprehension, generator expressions can encompass a condition in the form

``` python
generator = (expression for i in s if condition).
```

and can also be directly used as arguments for single-argument functions, using function call brackets to mark the expression.

``` python
afunction(expression for i in s)
```

There is however no real performance improvement using such a syntax, since the generator is exhausted before passing it to the function, but the syntax is very elegant and compact.

A typical example of this use is that of the so-called **dictionary comprehension**; from two lists of the same length, one of keys and one of values, we can obtain a dictionary with

``` python
d = dict(z for z in zip(keys, values))
```

since `dict()` accepts an iterable of `(key, value)` tuples, which is what the generator with `zip()` returns.

## Conclusions

Generators are a very powerful tool, not only because they simplify the creation of iterators, but also for the advantage of delaying the creation of the objects in a list and for the capability of generator functions to interrupt and resume the execution. As we will see in the third post this last feature is the foundation stone of an easy approach to cooperative multitasking.

## Past articles

* [Python Generators - From Iterators to Cooperative Multitasking](/blog/2013/03/25/python-generators-from-iterators-to-cooperative-multitasking)

## Next articles

* [Python Generators - From Iterators to Cooperative Multitasking 3](/blog/2013/03/29/python-generators-from-iterators-to-cooperative-multitasking-3)
