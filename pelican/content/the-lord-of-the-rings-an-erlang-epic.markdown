Title: The Lord of the Rings: an Erlang epic
Date: 2013-06-20 17:49 +0100
Category: Programming
Tags: Erlang, concurrent programming
Authors: Leonardo Giordani
Slug: the-lord-of-the-rings-an-erlang-epic
Summary:

## Abstract

One of the first really challenging problems an Erlang novice must face is the classical process ring, which can be found around the Internet and most notably in "Erlang Programming" by Cesarini and Thompson (page 115).

Its formulation may vary, but the core of it requires the programmer to design and implement a closed ring of processes, to make them pass a given number of messages each other and then terminate gracefully.

I try here to give an in-depth analysis of the matter, to point out some of the most interesting issues of this exercise. I strongly suggest the Erlang novice to try and solve the exercise before looking at the solutions proposed here.

## Linking processes

The aim of the exercise is to build a chain of processes that exchange messages. This latter specification is important since we are required to connect our processes with the only purpose of sending messages and we know that, in Erlang, a process may send a message to another process simply by knowing its pid.

Actual linking between processes, in Erlang, has the aim of making them exchange _exit signals_; links are thus a mean to control the way the system collapses (or not) when a process crashes. The processes in the ring will thus be linked to ensure that when one of them exits all other processes will be terminated, but this has nothing to do with the ring itself.

The point is to let a process know how to send a message to the next one, forming a chain. This chain, then, is closed, i.e. the last process sends messages to the first one.

There are two main strategies that can be leveraged to get the ring build and behave in a correct way: the **master process** approach and the **recursive** one. For each of the two, I am going to present and analyze the following steps: a ring of processes with **no message** exchange, a ring with **a single message** travelling through it, a ring with **multiple messages** and a ring that exposes a **functional interface** to send messages.

## Debugging

Debugging concurrent applications is everything but simple. For this exercise, it is however enough to get simple information about which process is forwarding which message. To allow a simple removal of debug prints a very small macro has been included in the file `debug.hrl`, activated by the definition of `debug`.

``` erlang
-ifdef(debug).
-define(DEBUG(Format, Args),
        io:format("~p: " ++ Format, [self()] ++ Args)).
-else.
-define(DEBUG(Format, Args), true).
-endif.
```

This converts the line

``` erlang
?DEBUG("A message~n", []).
```

to

``` bash
<0.44.0>: A message
```

where `<0.44.0>` is the pid of the message calling `?DEBUG` (only if `-define(debug, true).` is in the file).


## The “master process” solution

A good way to face the problem of building the ring is to spawn a _master process_, which will then spawn all other processes. This solution keeps the creation phase simple since everything happens between the master process and the process that is currently being spawned.

A small caveat: the creation of the ring must be performed **backwards**. When the master process spawns another process the only other process it knows is the process spawned in the previous loop. This means that the master process spawns a process that connects backwards to it, then spawns a process that connects backward to the second one, and so on.

#### Building the ring

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes without message passing
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_master_no_messages).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/1]).

%% Private exports
-export([create/1, create/2, loop/1]).

%% This version of the processes ring program uses the first process as a
%% master that spawns all other processes. It links to each process it spawns,
%% but processes are not linked each other.
%% Processes send no messages.

%% This spawns the first process. There is no need to register the process.
start(NumberProcesses) ->
    spawn(?MODULE, create, [NumberProcesses]).

%% Function create/1 needs to be defined so that the first process can call
%% create/2 passing its pid through self().
create(NumberProcesses) ->
    create(NumberProcesses, self()).

%% Function create/2 manages the single process creation; in this version the
%% master process just spawns each process and links to it. The exit clause
%% is when the proceses counter reaches 1 (that is, we completed the ring).
create(1, NextProcess) ->
    ?DEBUG("Process ~p connected with ~p~n", [self(), NextProcess]),
    loop(NextProcess);
create(NumberProcesses, NextProcess) ->
    Prev = spawn_link(?MODULE, loop, [NextProcess]),
    ?DEBUG("Process ~p created and connected with ~p~n", [Prev, NextProcess]),
    create(NumberProcesses - 1, Prev).

%% In this version function loop/1 sits down and does nothing.
loop(NextProcess) ->
    loop(NextProcess).
```
[source code](/code/erlang-rings/ring_master_no_messages.erl)

Note that the first spawn cannot directly call `create/2` passing `self()` since at that moment `self()` is the pid of the calling process (e.g. the Erlang shell) and not the pid of the first process of the ring, which is what we want. So we have to bypass this by spawning a process which executes `create/1`, which in turn calls `create/2` with the pid the process extracted with `self()`.

This first program spawns a set of processes that sit down and do nothing. Since they are all linked together (through the link with the master process), you can terminate the whole set by sending an exit signal to one of them. You can get the list of pid and names through the shell function `i()`. Otherwise, you can get and use the pid returned by the function `start/1`.

``` erlang
1> i().
[...]
2> exit(pid(0,44,0), boom).
```

#### Sending a single message

Now I am going to develop further the solution by making a single message travel the whole ring.

The code undergoes the following modifications:

* function `start()` must accept the message and pass it to `create()`.
* function `create()` must accept the message
* when the ring is ready the master process has to send the message to the second process; then it terminates
* each node just forwards the incoming message and terminates

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes with a single
%%%              message traveling once through the ring.
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_master_single_message).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/2]).

%% Private exports
-export([create/2, create/3, loop/1]).

%% This version of the processes ring program uses the first process as a
%% master that spawns all other processes. It links to each process it spawns,
%% but processes are not linked each other.
%% The first process then injects a single message in the ring, sending it to
%% the second process.

%% This spawns the first process. There is no need to register the process.
start(NumberProcesses, Message) ->
    spawn(?MODULE, create, [NumberProcesses, Message]).

%% Function create/2 needs to be defined so that the first process can call
%% create/3 passing its pid through self().
create(NumberProcesses, Message) ->
    create(NumberProcesses, self(), Message).

%% Function create/3 manages the single process creation; in this version the
%% master process spawns each process and links to it. Then each process starts
%% a loop. When the ring is completed the first process injects the message and
%% terminates.
create(1, NextProcess, Message) ->
    ?DEBUG("Process ~p connected with ~p~n", [self(), NextProcess]),
    ?DEBUG("Process ~p injects message ~p~n", [self(), Message]),
    NextProcess ! Message;
create(NumberProcesses, NextProcess, Message) ->
    Prev = spawn_link(?MODULE, loop, [NextProcess]),
    ?DEBUG("Process ~p created and connected with ~p~n", [Prev, NextProcess]),
    create(NumberProcesses - 1, Prev, Message).

%% Now loop/1 blocks each process making it wait for a message to pass along.
loop(NextProcess) ->
    receive
        Msg ->
            ?DEBUG("Got message ~p, passing it to ~p~n", [Msg, NextProcess]),
            NextProcess ! Msg,
            ok
    end.
```
[source code](/code/erlang-rings/ring_master_single_message.erl)

Beware that a subtle mechanism conceals here: in Erlang, you can always send a message to a pid, even if it the relative process does not exist; in this latter case the message is simply discarded. This feature allows us to make the first process send the message and terminate without having the last process crash by sending a message to it. Remember that you cannot do this with registered processes alias, only with pids.

#### Sending multiple messages

The exercise requests that a message can travel more than once through the whole ring. Adding this possibility is pretty straightforward: it simply requires to add a parameter to `start()` and `create()` and to change the loop clauses.

Function `loop/1` now becomes `loop/2` and has the following clauses:

* when the number of remaining travels is 1 the process forwards the message and then terminates.
* when the number of remaining travels is more than 1 the process loops again with a decremented value of travels.

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes with a single
%%%              message traveling multiple times through the ring.
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_master_multiple_messages).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/3]).

%% Private exports
-export([create/3, create/4, loop/2]).

%% This version of the processes ring program uses the first process as a
%% master that spawns all other processes. It links to each process it spawns,
%% but processes are not linked each other.
%% The first process then injects a message in the ring, sending it to the
%% second process and when getting the message from the last process, it
%% injects it again in the ring a given number of times.

%% This spawns the first process. There is no need to register the process.
start(NumberProcesses, Message, NumberMessages) ->
    spawn(?MODULE, create, [NumberProcesses, Message, NumberMessages]).

%% Function create/3 needs to be defined so that the first process can call
%% create/4 passing its pid through self().
create(NumberProcesses, Message, NumberMessages) ->
    create(NumberProcesses, self(), Message, NumberMessages).

%% Function create/4 manages the single process creation; in this version the
%% master process spawns each process and links to it. Then each process starts
%% a loop. When the ring is completed the first process injects the message and
%% starts looping.
create(1, NextProcess, Message, NumberMessages) ->
    ?DEBUG("Process ~p connected with ~p~n", [self(), NextProcess]),
    ?DEBUG("Process ~p injects message ~p~n", [self(), Message]),
    NextProcess ! Message,
    loop(NextProcess, NumberMessages - 1);
create(NumberProcesses, NextProcess, Message, NumberMessages) ->
    Prev = spawn_link(?MODULE, loop, [NextProcess, NumberMessages]),
    ?DEBUG("Process ~p created and connected with ~p~n", [Prev, NextProcess]),
    create(NumberProcesses - 1, Prev, Message, NumberMessages).

loop(NextProcess, 1) ->
    receive
        Msg ->
            ?DEBUG("Got message ~p, passing it to ~p and terminating~n",
                   [Msg, NextProcess]),
            NextProcess ! Msg,
            ok
    end;
loop(NextProcess, NumberMessages) ->
    receive
        Msg ->
            ?DEBUG("Got message ~p, passing it to ~p (still ~p time(s))~n",
                   [Msg, NextProcess, NumberMessages]),
            NextProcess ! Msg,
            loop(NextProcess, NumberMessages - 1)
    end.
```
[source code](/code/erlang-rings/ring_master_multiple_messages.erl)

As before, we are here relying on the possibility of sending a message to a non existent process.

#### Exposing a functional interface

An Erlang best practice is to expose a functional interface to the user that hides the underlying message passing. We are going to convert our ring program to expose the following functions:

* `start/1` - starts a ring with the given number of processes
* `stop/0` - terminates all processes in the ring
* `send_message/1` - sends a message that travels once through the ring
* `send_message/2` - sends a message that travels the given number of times through the ring

To expose a functional interface you need to register one or more processes, to get a global access point for your functions, so the first change to the code is that the spawned process is registered. Note that `Message` and `NumberProcesses` are no more passed to the `create()` function.

``` erlang
register(ring_master_functional, spawn(?MODULE, create, [NumberProcesses])),
```

This is not enough. We have to be sure that the whole ring is ready before giving control back to the user issuing `start()`. Until now the message was sent by the master process just after the creation of the last ring process, so there was no need to synchronize the return of the start function with the spawned processes. Now we need it, so just after registering the master process we wait for a ready message coming from the ring. To allow the ring to send the message to the initial caller we have to pass `self()` to `create()`. Pay attention that `self()` passed to the spawned process is the pid of the external process (e.g. the Erlang shell) while `self()` passed to `create()` is the pid of the master ring process.

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes that exposes a
%%%              functional interface to send messages.
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_master_functional).

%% Include some debug macros
%%-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/1, stop/0, send_message/1, send_message/2]).

%% Private exports
-export([create/2, loop/1]).

%% This version of the processes ring program uses the first process as a
%% master that spawns all other processes. It links to each process it spawns,
%% but processes are not linked each other.
%% The module exposes a functional interface to send messages in the ring
%% and to terminate it.

%% This spawns the first process. The process is registered to allow the
%% functional interface to work with it.
start(NumberProcesses) ->
    register(ring_master_functional, spawn(?MODULE, create,
                                           [NumberProcesses, self()])),
    receive
        ready ->
            ok
    after 5000 ->
            {error, timeout}
    end.

%% Functional interface: terminate the ring
stop() ->
    ring_master_functional ! {command, stop}.

%% Functional interface: send a message that travels through the ring once
send_message(Message) ->
    send_message(Message, 1).

%% Functional interface: send a message that travels through the ring
%% Times times
send_message(Message, Times) ->
    ring_master_functional ! {command, message, [Message, Times]}.

%% Function create/2 needs to be defined so that the first process can call
%% create/3 passing its pid through self(). The Starter process is passed to
%% allow the ring to send a ready message.
create(NumberProcesses, Starter) ->
    create(NumberProcesses, self(), Starter).

%% Function create/3 manages the single process creation; in this version the
%% master process spawns each process and links to it. Then each process starts
%% a loop. When the ring is completed the first process injects the message and
%% terminates.
create(1, NextProcess, Starter) ->
    ?DEBUG("Process ~p connected with ~p~n", [self(), NextProcess]),
    Starter ! ready,
    loop_master(NextProcess);
create(NumberProcesses, NextProcess, Starter) ->
    Prev = spawn_link(?MODULE, loop, [NextProcess]),
    ?DEBUG("Process ~p created and connected with ~p~n", [Prev, NextProcess]),
    create(NumberProcesses - 1, Prev, Starter).

%% Function loop_master/1 rules the behaviour of the master process (the first
%% of the whole ring). When it receives a stop command it forwards it and
%% terminates. When it receives a message command if Times is 0 the message is
%% thrown away, otherwise is is injected again in the ring with a decremented
%% Times value.
loop_master(NextProcess) ->
    receive
        Msg = {command, stop} ->
            NextProcess ! Msg,
            ok;
        {command, message, [_Message, 0]} ->
            loop_master(NextProcess);
        {command, message, [Message, Times]} ->
            ?DEBUG("Got message ~p, passing it to ~p (still ~p time(s)~n",
                   [Message, NextProcess, Times]),
            NextProcess ! {command, message, [Message, Times - 1]},
            loop_master(NextProcess)
    end.

%% Function loop/1 rules the behaviour of the standard processes.
%% When a process receives a stop command it forwards it and
%% terminates. When it receives a message command it just forwards it.
loop(NextProcess) ->
    receive
        Msg = {command, stop} ->
            NextProcess ! Msg,
            ok;
        Msg = {command, message, [_Message, _]} ->
            ?DEBUG("Got message ~p, passing it to ~p~n",
                   [_Message, NextProcess]),
            NextProcess ! Msg,
            loop(NextProcess)
    end.
```
[source code](/code/erlang-rings/ring_master_functional.erl)

The exposed functions are very simple. As you can see they use two different messages: `{command, stop}` and `{command, message, [Message, Times]}`. One of the advantages of exposing a functional API is that you are free to format your messages according to the current status of the software and change the format if it does no more suit the application needs.

The `loop()` function has been splitted in two different functions now: `loop/1` rules the behaviour of the standard ring process, while `loop_master/1` rules the behaviour of the master process.

The standard process has to react to a stop command, forwarding it and terminating, and to a message command, simply forwarding it. The master process has to check an incoming message to decide if it shall be injected again in the ring.

## A recursive solution

The simplest way to make a process know the pid of another process is to let the first spawn the second, and this gives us a hint about another way the process ring can be built. Like many things in Erlang, this can be solved recursively by saying:

In a process do:

* spawn another process and store its pid, call recursively on the spawned process
* if you are the last process just connect to the first

This solution has the advantage of being very straightforward for an Erlang programmer since it implements the standard recursion pattern. However, it forces the programmer to deal most of the time with the last node, which is a little counterintuitive.

The following programs are the recursive version of the four presented in the previous section. Once grasped the two main differences, building the ring forwards instead of backwards and dealing with the last node instead of the first, the two solutions present pretty much the same evolution steps.

The recursive ring construction.

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes without message passing
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_recursion_no_messages).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/1]).

%% Private exports
-export([create/1, loop/1]).

%% This version of the processes ring program starts the processes in a
%% recursive way. Each process spawns the next one and links to it.
%% Processes send no messages.

%% Please remember that io:format() does not always respect processes order
%% and that as a general rule printing on the standard output from concurrent
%% jobs is not very useful. Here I do this just to show that processes have been
%% spawned. Remember to define debug false if you create a ring of more than
%% 30-40 processes.

%% This spawns the first process, registering it under the name
%% 'ring_recursion_no_messages'.
%% Note that spawn() does never fail, but register may; if spawning the
%% first process fails an exception is raised and the process calling
%% start/1 is terminated.
start(NumberProcesses) ->
    register(ring_recursion_no_messages, spawn(?MODULE, create,
                                               [NumberProcesses])).

%% Function create/1 manages the single process creation; in this version the
%% only thing a process shall do is to spawn another process. The exit clause
%% is when the proceses counter reaches 1 (that is, the current process is the
%% last one): the last process uses 'ring_recursion_no_messages' as its next
%% process, thus closing the ring.
%% Here we use the only parameter(NumberProcesses) as the index for the current
%% process, so we end when the parameter is 1. If we want to think of it as the
%% number of REMAINING processes the only changes are that the exit clause is
%% create(0) instead of create(1), and that the first process is spawned in
%% start/1 with [NumberProcesses - 1] as argument. It is only a matter of taste.
%% Since all processes in the ring are meant to work together we are linking
%% them together to ensure that if one fails all others are terminated.
create(1) ->
    ?DEBUG("Process ~p created and connected with ~p (last)~n",
           [self(), whereis(ring_recursion_no_messages)]),
    loop(ring_recursion_no_messages);
create(NumberProcesses) ->
    Next = spawn_link(?MODULE, create, [NumberProcesses -  1]),
    ?DEBUG("Process ~p created and connected with ~p ~n", [self(), Next]),
    loop(Next).

%% In this version function loop/1 sits down and does nothing.
loop(NextProcess) ->
    loop(NextProcess).
```
[source code](/code/erlang-rings/ring_recursion_no_messages.erl)

The recursive ring with a single message travelling.

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes with a single
%%%              message traveling once through the ring.
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_recursion_single_message).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/2]).

%% Private exports
-export([create/2, loop/1]).

%% This version of the processes ring program starts the processes in a
%% recursive way. Each process spawns the next one and links to it.
%% The last process then injects a single message in the ring, sending it to the
%% first process.

%% This spawns the first process passing the requested message to it
start(NumberProcesses, Message) ->
    register(ring_recursion_single_message, spawn(?MODULE, create,
                                                  [NumberProcesses, Message])).

%% Function create/2 manages the single process creation; in this version each
%% process just spawns another process, except the last one, which shall inject
%% the message in the ring and terminate.
create(1, Message) ->
    ?DEBUG("Process ~p created and connected with ~p (last)~n",
           [self(), whereis(ring_recursion_single_message)]),
    ?DEBUG("Process ~p injects message ~p~n", [self(), Message]),
    ring_recursion_single_message ! Message;
create(NumberProcesses, Message) ->
    Next = spawn_link(?MODULE, create, [NumberProcesses -  1, Message]),
    ?DEBUG("Process ~p created and connected with ~p ~n", [self(), Next]),
    loop(Next).

%% Now loop/1 blocks each process making it wait for a message to pass along.
loop(NextProcess) ->
    receive
        Msg ->
            ?DEBUG("Got message ~p, passing it to ~p~n", [Msg, NextProcess]),
            NextProcess ! Msg,
            ok
    end.
```
[source code](/code/erlang-rings/ring_recursion_single_message.erl)

The recursive ring with support for multiple message travels.

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes with a single
%%%              message traveling multiple times through the ring.
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_recursion_multiple_messages).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/3]).

%% Private exports
-export([create/3, loop/2]).

%% This version of the processes ring program starts the processes in a
%% recursive way. Each process spawns the next one and links to it.
%% The last process then injects a message in the ring, sending it to the
%% first process and when getting the message from the second to last, it
%% injects it again in the ring a given number of times.

%% This spawns the first process passing it the requested message and the number
%% of times the message shall travel through the ring.
start(NumberProcesses, Message, NumberMessages) ->
    register(ring_recursion_multiple_messages, spawn(?MODULE, create,
                          [NumberProcesses, Message, NumberMessages])).

%% Function create/3 manages the single process creation; in this version each
%% process spawns another process and then loops, except the last one, which
%% shall inject the message in the ring before looping. Since the last process
%% starts the message passing it loops an already decremented number of times.
create(1, Message, NumberMessages) ->
    ?DEBUG("Process ~p created and connected with ~p (last)~n",
           [self(), whereis(ring_recursion_multiple_messages)]),
    ?DEBUG("Process ~p injects message ~p~n", [self(), Message]),
    ring_recursion_multiple_messages ! Message,
    loop(ring_recursion_multiple_messages, NumberMessages - 1);
create(NumberProcesses, Message, NumberMessages) ->
    Next = spawn_link(?MODULE, create,
                      [NumberProcesses -  1, Message, NumberMessages]),
    ?DEBUG("Process ~p created and connected with ~p ~n", [self(), Next]),
    loop(Next, NumberMessages).

%% Function loop/2 needs two clauses now. The first one is the exit condition,
%% when the forward counter reaches 1: the process forwards the message and
%% terminates. The second clause is the standard behaviour: the process forwards
%% the incoming message and loops again with a decremented counter.
loop(NextProcess, 1) ->
    receive
        Msg ->
            ?DEBUG("Got message ~p, passing it to ~p and terminating~n",
                   [Msg, NextProcess]),
            NextProcess ! Msg,
            ok
    end;
loop(NextProcess, NumberMessages) ->
    receive
        Msg ->
            ?DEBUG("Got message ~p, passing it to ~p (still ~p time(s))~n",
                   [Msg, NextProcess, NumberMessages]),
            NextProcess ! Msg,
            loop(NextProcess, NumberMessages - 1)
    end.
```
[source code](/code/erlang-rings/ring_recursion_multiple_messages.erl)

The recursive ring exposing the functional API.

``` erlang
%%% Author:  Leonardo Giordani <giordani.leonardo@gmail.com>
%%% Description: Implements a simple ring of processes that exposes a
%%%              functional interface to send messages.
%%% Created: Jun 2013 by Leonardo Giordani

-module(ring_recursion_functional).

%% Include some debug macros
-define(debug, true).
-include("debug.hrl").

%% User interface
-export([start/1, stop/0, send_message/1, send_message/2]).

%% Private exports
-export([create/2, loop/1]).

%% This version of the processes ring program starts the processes in a
%% recursive way. Each process spawns the next one and links to it.
%% The module exposes a functional interface to send messages in the ring
%% and to terminate it.

%% This spawns the first process. The process is registered as in the previous
%% recursive versions to allow the last process to connect with the first.
%% Moreover, exposing a functional interface needs a global entry point, that is
%% the name of the first process.
start(NumberProcesses) ->
    register(ring_recursion_functional, spawn(?MODULE, create,
                                              [NumberProcesses, self()])),
    receive
        ready ->
            ok
    after 5000 ->
            {error, timeout}
    end.

%% Functional interface: terminate the ring
stop() ->
    ring_master_functional ! {command, stop}.

%% Functional interface: send a message that travels through the ring once
send_message(Message) ->
    send_message(Message, 1).

%% Functional interface: send a message that travels through the ring
%% Times times
send_message(Message, Times) ->
    ring_recursion_functional ! {command, message, [Message, Times]}.

%% Function create/2 manages the single process creation; in this version each
%% process just spawns another process, except the last one, which connects with
%% the first one. The Starter process is passed to allow the ring to send a
%% ready message.
create(1, Starter) ->
    ?DEBUG("Process ~p created and connected with ~p (last)~n",
           [self(), whereis(ring_recursion_functional)]),
    Starter ! ready,
    loop_last(ring_recursion_functional);
create(NumberProcesses, Starter) ->
    Next = spawn_link(?MODULE, create, [NumberProcesses -  1, Starter]),
    ?DEBUG("Process ~p created and connected with ~p ~n", [self(), Next]),
    loop(Next).

%% Function loop_last/1 rules the behaviour of the last process of the whole
%% ring). When it receives a stop command it just terminates since the message
%% has already been received by all processes in the ring. When it receives a
%% message command if Times is 1 the message is thrown away (it just travelled
%% through the ring for the last time), otherwise is is injected again in the
%% ring with a decremented Times value.
loop_last(NextProcess) ->
    receive
        {command, stop} ->
            ok;
        {command, message, [_Message, 1]} ->
            loop_last(NextProcess);
        {command, message, [Message, Times]} ->
            ?DEBUG("Got message ~p, passing it to ~p (still ~p time(s)~n",
                   [Message, NextProcess, Times]),
            NextProcess ! {command, message, [Message, Times - 1]},
            loop_last(NextProcess)
    end.

%% Function loop/1 rules the behaviour of the standard processes.
%% When a process receives a stop command it forwards it and
%% terminates. When it receives a message command it just forwards it.
loop(NextProcess) ->
    receive
        Msg = {command, stop} ->
            NextProcess ! Msg,
            ok;
        Msg = {command, message, [_Message, _]} ->
            ?DEBUG("Got message ~p, passing it to ~p~n",
                   [_Message, NextProcess]),
            NextProcess ! Msg,
            loop(NextProcess)
    end.
```
[source code](/code/erlang-rings/ring_recursion_functional.erl)

## Conclusions

The process ring is an exercise that can be solved in many ways (I just presented the two more straightforward ones) but makes the programmer face problems that may later rise in real-world applications. For this reason, it is an invaluable sandbox where the Erlang programmer can try different approaches to solve both the concurrency and the topology problems.

Keep in touch for other Erlang articles on [this page](/categories/erlang/).


