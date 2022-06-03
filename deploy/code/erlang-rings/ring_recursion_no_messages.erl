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
