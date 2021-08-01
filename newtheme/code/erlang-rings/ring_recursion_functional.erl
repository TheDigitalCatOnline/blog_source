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
