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
