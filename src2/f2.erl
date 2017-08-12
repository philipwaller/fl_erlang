%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    io:format("Call: ~p(~p)~n", [?FUNCTION_NAME, none]),
    register(?MODULE,
	     spawn(?MODULE, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  io:format("Call: ~p(~p)~n", [?FUNCTION_NAME, Frequencies]),
  receive
    Message = {request, Pid, allocate} ->
    %{request, Pid, allocate} ->
      io:format("~p:Rx:~p~n", [Pid, Message]),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    Message = {request, Pid , {deallocate, Freq}} ->
    %{request, Pid , {deallocate, Freq}} ->
      io:format("~p:Rx:~p~n", [Pid, Message]),
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} = Message ->
    %{request, Pid, stop} ->
      io:format("~p:Rx:~p~n", [Pid, Message]),
      Pid ! {reply, stopped};
    Message ->
      io:format("~p:Rx:~p~n", ["<0.---.0>", Message]),
      loop(Frequencies)
  end.

%% Functional interface

allocate() -> 
    io:format("Call: ~p(~p)~n", [?FUNCTION_NAME, none]),
    ?MODULE ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    io:format("Call: ~p(~p)~n", [?FUNCTION_NAME, Freq]),
    ?MODULE ! {request, self(), {deallocate, Freq}},
    io:format("Call: ~p(~p)~n", ["deallocate", Freq]),
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    io:format("Call: ~p(~p)~n", [?FUNCTION_NAME, none]),
    ?MODULE ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
