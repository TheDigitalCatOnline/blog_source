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
