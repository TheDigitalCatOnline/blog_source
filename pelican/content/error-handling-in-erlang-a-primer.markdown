Title: Error handling in Erlang - a primer
Date: 2013-05-30 11:41 +0100
Category: Programming
Tags: Erlang, concurrent programming
Authors: Leonardo Giordani
Slug: error-handling-in-erlang-a-primer
Summary:

## Abstract

This article aims to summarize Erlang error handling both in sequential and in concurrent environments. The targets of this article are novices that, like me, make their first steps into the beautiful world of Erlang. Moreover, I always find that trying to explain things makes me understand them better.

Disclaimer (for Erlang gurus): I'm a complete Erlang novice so please be indulgent with me while you read my thoughts. Corrections and suggestions are welcome!

## Introduction

Recently I started studying Erlang; coming from a pure imperative background (C, C++, and Python) I have to say that Erlang has been a surprise and a joy. I did not find something so innovative since long, even if the pure functional part of the language was not totally new since it is available in Python too.

The concept of runtime _system_, with a support for concurrency built in the language itself, the pattern matching idea and the recursion as a way to implement loops are all very intriguing, so learning them is fun (pun intended, if you do not get it review Erlang anonymous functions).

One of the innovative concepts that ploughed through my imperative mind was that of _defensive programming_ under its formulation in the Erlang tenet "Let it crash". This was something new, partly because I rarely found advice on system organization while learning the foundations of a programming language and partly because about 80% of the code I write has the task of avoiding programs to crash.

I found [this very interesting post](http://mazenharake.wordpress.com/2009/09/14/let-it-crash-the-right-way/) of Mazen Harake on the subject. Basically, he clarifies that the Erlang philosophy is not that of just let errors happen and propagate: the point is that the programmer should deal only with errors that are _documented_. This means that the code specification includes that error as a possibility. Well, it is not my intention to state something already well explained: go and read Mazen's post.

Anyway, before discussing the Erlang way of dealing with code errors, it is necessary to firmly grasp syntax and structures that the language provides.

## Exceptions in Erlang

The simplest way to make something go wrong when dealing with a computer is to treat it like a sentient being. Joking apart, a good way to crash a program is to execute a division by zero.

``` erlang
1> 1/0.
** exception error: bad argument in an arithmetic expression
     in operator  '/'/2
        called as 1 / 0
2> 
```

As you can see Erlang does not get mad at your provocation and simply **raises and exception**, i.e. signals that something went wrong, giving some details about why and where it happened. This is not different from what other languages, like Python, do.

``` pycon
>>> 1/0
Traceback  (most recent call last):
  File "<stdin>", line 1, in <module>
ZeroDivisionError: integer division or modulo by zero
>>> 
```

So exceptions are in Erlang, as in other languages, a "reserved channel" the language uses to propagate errors. If you code in C, you cannot leverage something like exceptions and must rely on return values. This means that you have to format the results of your functions so that they can host a wrong result too and end up returning an `int` with the error code while functions results are managed by reference.

Back to Erlang. Exceptions crash you program, i.e. they make your program immediately stop, reporting the error to the system process that executed it (usually the OS GUI or a textual shell). Indeed the Erlang shell crashed when you tried to reach for the infinity.

``` erlang
1> self().
<0.32.0>
2> 1/0.
** exception error: bad argument in an arithmetic expression
     in operator  '/'/2
        called as 1 / 0
3> self().
<0.35.0>
4> 
```
Remember that in Erlang `self()` gives you the Erlang PID of the process (not the operating system's one). As you can see the Eshell process crashed and was restarted by some magic behind the scenes.

I hear you mumble that, well, world is not perfect, and errors happen. So how do we deal with this? Shall we crash every time something wrong occurs in our code or is there some way to unravel the knot and happily continue running the code?

This seems to be a question that even Erlang creators wanted to answer, and their answer was: you can, but you shouldn't always. By now, let us drop the "you shouldn't always" part and learn the basics; you can stop an exception before it crashes your pretty program. Before diving into stopping exceptions techniques, let me review the types of exception you may encounter.

## Exception types

There are three types (or classes) of exceptions in Erlang: _throw_, _error_, and _exit_.

The first, throw, identifies an exception that a called function voluntarily raises (throwing it at you); such exceptions shall be documented, i.e. the documentation of the function you are calling shall state that this exception may be raised and specify under what conditions this may happen. "Shall" here means that if the programmer does not document the exception all sorts of curses will be casted on his or her code forever. As a Python programmer I strongly advice you to read "shall" as "must".

The second exception type, error, signals that something very bad happened in the system, something that was unexpected to the author of the code raising the exception. Even if this type of exception can be raised explicitly, it is usually raised by the Erlang run-time system. If you recall the first example of this post, the division by zero, you can now understand why the shell printed "exception error"; that exception has not been raised by an instruction in the code of the Erlang shell, but from the run-time system itself. This type of exception also contains a stack trace, but I will not cover it in this article.

The third and last exception type, exit, means that your code is being told to stop immediately.

As you can see the real difference between the three types is the communication intent, not a special behaviour. So from the pure theoretical point of view an error exception can be replaced by a throw exception without any side effect. Obviously, the communication intent is not negligible: indeed, as explained before, throw exceptions are usually documented while errors are not intended for being formalized.

In addition to a class, exceptions encompass a _reason_, that is a valid Erlang item (an atom, an integer, a pid, ...). The reason carries the explanation of the exception, i.e. a detailed insight in what really happened.

## Dealing with exceptions

Now that we got acquainted with Erlang exception types we may step further into exception management structures and learn how to stop exceptions from crashing our programs. The way of managing exceptions raised by a function called in our code should be familiar to Python, C++, java and Ruby developers (and to many others, probably); the basic Erlang syntax is

``` erlang
try <expressions> of
  <result_pattern_matching>
catch
  <exception_pattern_matching>
after
  <after_expressions>
end
```

The large part of this structure is well known. Here, `<expressions>` is a series of Erlang expressions, comma-separated as usual; the `<result_pattern_matching>` part is a classical Erlang pattern matching structure, just like that you write in a case construct; last, the `<after_expressions>` is a series of Erlang expressions. The `<exception_pattern_matching>` part has a slightly new syntax we will cover in a moment.

The structure works like in other languages: the `<expressions>` code is evaluated and the result is pattern matched against `<result_pattern_matching>` and the result is returned by the whole `try` statement. If an exception is raised when evaluating `<expressions>`, it is pattern matched against the code listed in `<exception_pattern_matching>` and the relative code is executed. Regardless of what happens in the try/catch part the code in `<after_expressions>` is executed, and its result is not returned.

The exception matching code has a syntax that is very similar to that of the usual pattern matching, but exceptions are listed in the new form `ExceptionType:Reason`, where type and reason have been already described in the previous section. When the exception is a run-time error the reason is one of the values listed [here](http://erlang.org/doc/reference_manual/errors.html#exit_reasons).

So the complete form of a try/catch statement in Erlang is the following, where `Expressions` is always a comma-separated list of Erlang expressions.


``` erlang
try Expression1,...,ExpressionN of
  Pattern1 [when Guard1] -> PatternExpressions1;
  Pattern2 [when Guard2] -> PatternExpressions2;
  ...
  PatternN [when GuardN] -> PatternExpressionN
catch
  ExceptionType:Reason1 [when ExceptionGuard1] -> ExceptionExpressions1;
  ExceptionType:Reason2 [when ExceptionGuard2] -> ExceptionExpressions2;
  ...
  ExceptionType:ReasonN [when ExceptionGuardN] -> ExceptionExpressionsN
after
  AfterExpressions
end.
```

Exceptions pattern matching allows the use of the do-not-care variable `_` not only for reasons but for also for types. So the following syntax catches all exceptions of type ExceptionType

``` erlang
...
catch
  ExceptionType: _ -> ExceptionExpressions1;
end
```

while the following catches all exceptions

``` erlang
...
catch
  _:_ -> ExceptionExpressions1;
end
```

I will not cover here the old-style error handling mechanism with `catch`; the interested reader can find it documented [here](http://erlang.org/doc/reference_manual/expressions.html#id79206).

## Returning values from try/catch statements

Try/catch statements return the value of the last expression executed, that is one of `PatternExpression1`,...,`PatternExpressionN` if no exception is raised, or one of `ExceptionExpressions1`,...,`ExceptionExpressionsN`. This means that we can assign the value of the whole expression to a variable 

``` erlang
Result = try Expression of ... end.
```

Remember that `AfterExpressions` are always executed, but their final value is not returned by the statement.

Since a lot of times you want to return the result of the expression after the `try` keyword, you can omit the `of` part

``` erlang
try Expressions
catch
 ...
end.
```

## Raising exceptions

Erlang provides three different BIFs to raise exceptions, profitably called like the exception type they raise: `throw/1`, `erlang:error/1`, and `exit/1`. As you see, `error/1` is not automatically imported by the system and must be called in its full form. This is a hint for us programmers: `error/1` is there and can be used, but it is not something you should need often; otherwise, you misunderstood what an error exception is in Erlang.

So in most cases, if your code encounters an error condition and you need to raise an exception, you end up using `throw/1` or `exit/1`. The argument of these functions is the reason of the exception: remember that you can format the reason you attach to your exception to match your needs, you only need to document it.

``` erlang
my_function(Somebadvalue) -> throw({badvalue, Somebadvalue}).
```

Pay attention, however, that exceptions are a double-edged sword; the fact that they return values through a reserved channel is powerful, but can lead to subtle bugs and to long debug sessions. The advice in Erlang is to spare throw() for some special cases and to try always to communicate the failure through standard function results. This is, however, part of a coding philosophy that cannot be examined in depth here.

## Exceptions and exit signals

Erlang is a run-time system, not just a language; as such, it has built-in structures and concepts that are usually provided by libraries in other languages. One of these concepts is the dependency between processes, which may be realized through _links_ (and _monitors_, but I am not going to introduce them in this article). Process linking in Erlang means a very simple thing: when two processes are linked they die together, i.e. when one of the two terminates abnormally the other one is terminated too. A process can link to more than one other process, and the dying behaviour is propagated among all them.

What is the point of this structure? In Erlang, you are encouraged to spawn processes to accomplish tasks, even the simplest ones. Thus, you can easily end up with a multitude of processes working together to perform some action, and if one of them crashes it is likely that others should exit too, being them dependent from it. This is not mandatory; it is all up to you to decide what processes have to be linked, but if they are they must die together.

How is this accomplished? Linked processes are connected by a hidden communication channel, which carries information about their termination with so-called _exit signals_. Exit signals are invisible to the programmer, and when a process receives one of them it simply terminates, spreading the news under the form of other exit signals.

#### Process termination and reasons

Exit signals are always sent when a process dies, but they carry a reason for its termination, just like exceptions. This reason is very important for the subsequent events concerning processes linked to the dying one.

First of all let us look at process termination. A process in Erlang can terminate normally or abnormally: the former happens when it has no more code to execute or when it raises an exception passing as reason the atom `normal`; the latter occurs when a raised exception has a reason different from the atom `normal`.

So an exit signal process contains either the atom `normal` or another reason, and it travels from the terminating process to each linked process. When it hits one of them, if the reason is not `normal` the process is terminated and sends its own exit signals to its linked processes with the same reason of the incoming one. The result is that the entire network of linked processes terminates automatically.

The best way to terminate a process with a reason is to execute the BIF `exit(Reason)`. This BIF has also the form of arity 2 where you pass the pid of a process `exit(Pid, Reason)`: the addressed process will get an exit signal with the given reason. A caveat: when using `exit/1` the exit signal will contain the pid of the terminating process, when using `exit/2` the exit signal will contain the pid of the target process.

#### Stopping exit signals

Having a way to stop an entire group of processes when one of them crashes is a big benefit, but it could be a good thing to be able to stop the propagation somewhere. Obviously, if processes are not linked they do not influence each other when terminating, but this also means that no one notices that a process terminated, which in turn means that no one will be restarting it.

The whole point of linking processes is indeed the control over terminating processes. If a process runs there is a reason and if it crashes the system should investigate why it crashed and possibly restart it.

Erlang gives a process the chance to receive an abnormal exit signal from a process it is linked to, without forcing it to terminate: in Erlang speech this is called _trapping exit signals_ or _trapping exits_. When a process traps exits the incoming exit signals coming from linked processes are converted by the run-time system into messages that the process can fetch with a `receive` construct. Thus, a process trapping exits can be notified that a linked process died without being affected by this.

A process can start trapping exits by executing the BIF `process_flag(trap_exit, true)`. It is a best practice to call it at the beginning of the process and to avoid turning it off during the execution since it makes the system difficult to debug.

Once the BIF has been executed, an exit signal with the reason `Reason` coming from another process is converted in an incoming message under the form `{'EXIT', Pid, Reason}`, where Pid is the pid of the terminated process.

#### Unstoppable exit signals

Now we can convert a process so that it does not terminate with its linked processes. This has a downside: if the process contains errors such as infinite loops or if for some reason we need to stop the entire system, the processes that trap exits cannot be stopped. For this reason, Erlang provides the special atom `kill` as a reason for an exit signal. 

An exit signal containing the reason `kill` cannot be trapped; thus the exit signal is unstoppable. Unconditionally terminating the entire network is however something dangerous, so when a process terminates because of an incoming `kill` exit signal it will send to its linked processes a `killed` exit signal, which can be possibly trapped.

## Conclusions

The aim of the article was to give an overview of error handling in Erlang: not everything has been covered, but a novice should find here almost everything he or she needs to step into this part of the language.

There is obviously much more than this in the Erlang treasure chest. If the whole link and exit signals stuff thrilled you like it did with me, I promise you that OTP behavious (sorry, behaviors) will take your breath away.

Keep in touch for other Erlang articles on [this page](/categories/erlang/).









