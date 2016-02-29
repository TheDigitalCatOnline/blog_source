Title: Python Generators - From Iterators to Cooperative Multitasking - 3
Date: 2013-03-29 13:25 +0100
Category: Programming
Tags: Python, generators
Authors: Leonardo Giordani
Slug: python-generators-from-iterators-to-cooperative-multitasking-3
Series: "Python generators - from iterators to cooperative multitasking"
Summary:

## Introduction

In this third issue we move on uncovering how generators can be the foundation of a cooperative multitasking system and show some code that implements it. Before we face this topic we will talk shortly about another interesting use of generators, namely generator expressions chains.

## Chaining generator expressions
At PyCon 2008 David M. Beazley, author of “Python Essential Reference”, made a very interesting speech about the use of generators in system administration, in other words where usually more or less complex bash scripts are involved and in particular where long pipe sequences are used.

David starts from the consideration that generators, producing one element at a time, are chainable, that is a generator expression can encompass another generator expression and so on. This way he shows how to write in a very compact and reusable way components that can act as “filters” on a data set, thus following the Unix philosophy of building tools that “do one thing and do it well”, chaining them afterwards to get the needed behaviour.

The slides of this presentation are freely downloadable, so I suggest the interested reader to take a look at it at the following address: [Generator Tricks for Systems Programmers](http://www.dabeaz.com/generators-uk/).

## Microthread: cooperative multitasking

_Disclaimer: the concepts and code presented here have been heavily influenced by the Kamaelia project. You can find it [here](http://www.kamaelia.org)._

Let us move forward to see how (Python) generators allow us to easily build applications based on the concept of cooperative multitasking. I assume the reader is familiar with the concepts of preemption and thread-based multitasking and is aware of the pro and cons of such solutions.

**Cooperative multitasking** allows an application to hold the control of the CPU for an arbitrary time lapse, waiting for it to voluntarily release the resource to the scheduler. This is a major break with the modern approach to multitasking, where the scheduler is in charge of stopping and resuming applications without any previous agreement with them.

Since application can now stop on their own initiative every issue related to shared data protection, atomicity and synchronization is greatly simplified if not removed. Applications need however a mechanism to stop running, save their internal state and later resume from the same point.

Generators, indeed, through the `yield` statement implement this very behaviour, thus they may be used to create a system based on cooperative multitasking, where processes are now called **microthreads** to highlight that they are a lightweight form of thread.

#### Microthreads

Let’s look at a simple implementation of such a system. First of all we need a `MicroThread` object, i.e. an object that can run simultaneously with other similar objects, but in a cooperative way.

``` python
class MicroThread(object):
	def main(self):
		while 1:
			yield 1
```

An instance of this object exposes a `main()` method that, when called, returns a generator. This latter, at each call of its `next()` method simply returns `1`, freezing at the same time its execution just after the `yield` statement, still inside the infinite while loop.

The object can be directly tested

``` bash
>>> mt = MicroThread()
>>> g = mt.main()
>>> g
<generator object main at 0xb74331e4>
>>> g.next()
1
```

To make the object more easily inheritable and extendable we can refactor it a little

``` python
class MicroThread(object):
    def step(self):
        pass

    def create(self):
        pass

    def main(self):
        self.create()
        yield 1
        while 1:
            self.step()
            yield 1
```
[source code](/code/python-generators/mthread.py)

Such changes let us inherit the class and extend it simply by overriding the `create()` and `step()` methods; the first is called as soon as `main()` is called, acting as a delayed initializer, while the second is executed at each call of `next()`, just before freezing the code with `yield`. Pay attention that since `create()` is called inside the generator function, you have to call `next()` once to run it after the genrator has been created. So the standard workflow with this object is

``` python
# Instance the object
mt = MicroThread()
 
# Create the generator
g = mt.main()
 
# Initialize it
g.next()
 
# Loop over it
g.next()
g.next()
...
```

Since `main()` is a generator function it must act as any generator and signal its exhaustion rising a `StopIteration` exception. The overridden `step()` method, thus, may raise this exception at any point (even multiple ones) to terminate the microthread.

#### Scheduler

Now we need a scheduler, i.e. the system component that manages running tasks. While in a true multitasking system the scheduler is a big and complex component, in a cooperative environment it can be rather simple: its job is to execute each task and wait till they give control back. In between a task and the following the scheduler can execute other functions, but its basic workflow is very straightforward. Obviously the scheduler shall handle the `StopIteration` exception possibly raised by a task, removing it from the list of running microthreads.

The core of the scheduler will be something like the following:

``` python
for thread in active_microthreads:
	try:
		thread.next()
		scheduled_microthreads.append(thread)
	except StopIteration:
		pass
	
active_microthreads = scheduled_microthreads
scheduled_microthreads = []
```

This snippet encompasses the behaviour described above. We have two lists, `active_microthreads` with all the tasks that shall be executed in the current loop and `scheduled_microthreads` with all the tasks that are goig to be executed in the next loop. At each loop of the scheduler all microthreads in `active_microthreads` are executed, that is they are granted one execution of their `next()` function. After this the thread is scheduled again, i.e. it is appended to the `scheduled_microthreads` list. If the thread raises the `StopIteration` exception during its execution it is simply not scheduled again. When the `active_microthreads` list is exhausted the loop ends and the scheduled threads list is transferred in the `active_threads` one; after this the loop starts again.

So the first implementation of the scheduler is the following:

``` python
class Scheduler(object):
    def __init__(self):
        self.active_microthreads = []
        self.scheduled_microthreads = []

    def add_microthread(self, mthread):
        g = mthread.main()
        g.next()
        self.active_microthreads.append(g)

    def run(self):
        while 1:
            for thread in self.active_microthreads:
                try:
                    thread.next()
                    self.scheduled_microthreads.append(thread)
                except StopIteration:
                    pass
        
            self.active_microthreads = self.scheduled_microthreads
            self.scheduled_microthreads = []
```
[source code](/code/python-generators/scheduler.py)

The `__init__()` method initializes the two internal lists we talked about above. The `add_microthread()` method allows us to add a microthread to the scheduler; the method calls `main()` on each microthread we add to obtain its generator, then calls `next()` once on this latter to initialize it and finally adds it to the list of scheduled tasks.

The scheduler logic is then implemented in the `run()` method, which executes the above core code in an infinite while loop.

We can test the microthreads and the scheduler with this simple code

``` python
import mthread
import scheduler
import time

class TestMicroThread(mthread.MicroThread):
    def __init__(self, number):
        self.num = number

    def step(self):
        print "Number:", self.num
        time.sleep(1)

mt1 = TestMicroThread(1)
mt2 = TestMicroThread(2)
mt3 = TestMicroThread(3)

ms = scheduler.Scheduler()
ms.add_microthread(mt1)
ms.add_microthread(mt2)
ms.add_microthread(mt3)

ms.run()
```
[source code](/code/python-generators/test_scheduler.py)

Here the `TestMicroThread` is a microthread but the `step()` method was reimplemented to print a number and wait 1 second. Three microthreads are instanced and added to the scheduler and the `run()` method of the scheduler is executed. Not surprisingly the result is the following

``` bash
$ python test_scheduler.py
Number: 1
Number: 2
Number: 3
Number: 1
Number: 2
Number: 3
[...]
```

The three microthreads are executed in a round-robin fashion, as expected from a cooperative multitasking system.

Note: while all microthreads showed in this article just execute `yield 1` to freeze the code, `yield` can return any object, just like the `return` statement does, and this could be exploited to enhance the communication between microthreads and scheduler.

#### Microschedulers

The scheduler could however be more flexible, specifically it could be converted to a microthread itself. The scheduler, when executed, will return a generator, and each call of its `next()` method will run one of its microthreads. After this the scheduler will freeze and give control back.

``` python
class MicroScheduler(object):
    def __init__(self):
        self.active_microthreads = []
        self.scheduled_microthreads = []

    def add_microthread(self, mthread):
        g = mthread.main()
        g.next()
        self.active_microthreads.append(g)

    def main(self):
        yield 1
        while 1:
            if len(self.active_microthreads) == 0:
                yield 1
            for thread in self.active_microthreads:
                try:
                    thread.next()
                    self.scheduled_microthreads.append(thread)
                except StopIteration:
                    pass
                yield 1
        
            self.active_microthreads = self.scheduled_microthreads
            self.scheduled_microthreads = []
```
[source code](/code/python-generators/mscheduler.py)

It is sufficient to rename `run()` to `main()`, to match our arbitrary microthread interface, and add some `yield` statements. The first `yield` at the beginning of `main()` terminates the creation part: this scheduler has no `create()` method, but if present it should be called here. The second `yield` is called if the scheduler contains no microthreads, since it has nothing to do. The third `yield` is called after each loop of the microthread running part.

These little changes allow the scheduler to be run into another scheduler, thus enabling us to create a hierarchy to easily build complex systems. At the same time the scheduler can be used as usual simply calling its `next()` method in a for loop.

``` python
import mthread
import mscheduler
import time

class TestMicroThread(mthread.MicroThread):
    def __init__(self, number):
        self.num = number

    def step(self):
        print "Number:", self.num
        time.sleep(1)

mt1 = TestMicroThread(1)
mt2 = TestMicroThread(2)
mt3 = TestMicroThread(3)

ms = mscheduler.MicroScheduler()
ms.add_microthread(mt1)
ms.add_microthread(mt2)
ms.add_microthread(mt3)

for i in ms.main():
    pass
```
[source code](/code/python-generators/test_mscheduler.py)

This example is obviously very simple. However it shows how simple it is to build components of a cooperative system and let them live together in an execution space. Executing the scheduler in a different way, for example inside another generator, new microthreads can also be added live. This allows to instance system components on the fly to manage specific needs, such as incoming service requests.

## Conclusions

Obviously this sort of multitasking cannot provide an interactive execution like that used on our desktop OS or on a Web server, where human users must receive an immediate feedback of their actions. But for systems where task must simply be simultaneously executed without timing needs cooperative multitasking is a valuable solution, due to its simplicity.

Another interesting scenario is that of a real multitasking system (ruled by threaded code or by the OS itself) where each task is made of small cooperating components. This way putting multiple functionalities inside a single component becomes a breeze; the code of each functionality could also be splitted in several plugins and loaded on demand.

A package that implements cooperative multitasking with generator based microthreads is [Kamaelia](http://www.kamaelia.org), and this article has been heavily inspired by it. Other solutions you can find interesting are [greenlet](http://pypi.python.org/pypi/greenlet), presently the most used microthread Python library that runs on the standard unmodified Python interpreter and [Stackless Python](http://www.stackless.com/), a fork of Python that natively implements microthreads.

## Updates

2014-02-17: [Riccardo](https://twitter.com/entropiae) spotted an error in the example code after `mthread.py` and submitted the correct version. Thanks!

## Past articles

* [Python Generators - From Iterators to Cooperative Multitasking](/blog/2013/03/25/python-generators-from-iterators-to-cooperative-multitasking)
* [Python Generators - From Iterators to Cooperative Multitasking 2](/blog/2013/03/26/python-generators-from-iterators-to-cooperative-multitasking-2)

