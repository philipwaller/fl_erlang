%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).  
-export([allocate/2,deallocate/2,loop/1]).          %%% Added loop/1,allocate/2,deallocate/2 to exports
-export([inject/1,inject/2]).                       %%% Added inject/1,inject/2 to exports
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  Frequencies = {[10,11,12,13,14,15], []},
  loop(Frequencies).                           


%% The Main Loop

loop(Frequencies) ->
  io:format("~w:~w(~w)~n",[?MODULE,?FUNCTION_NAME,Frequencies]),
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),   
      Pid ! {reply, Reply},
      frequency:loop(NewFrequencies);                           %%% Changed to fully-qualified call.
    {request, Pid, {inject,Freqs}} ->                           %%% Added new type of message
      NewFrequencies = inject(Frequencies, Freqs),              
      Pid ! {reply, injected},
      frequency:loop(NewFrequencies);                           %%% NB fully-qualified call.
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq), 
      Pid ! {reply, ok},
      frequency:loop(NewFrequencies);                           %%% Changed to fully-qualified call.
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

inject(Freqs) ->                                                   %%% Added new API function.
    frequency ! {request, self(), {inject, Freqs}},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

inject({Free, Allocated}, Freqs) ->
  {Free++Freqs,Allocated}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

