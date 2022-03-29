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
