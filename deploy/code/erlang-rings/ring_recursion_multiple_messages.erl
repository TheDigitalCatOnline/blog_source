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
