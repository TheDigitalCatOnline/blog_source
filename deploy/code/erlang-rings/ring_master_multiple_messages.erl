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
