%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(f2).
-export([
		 start/0,allocate/0,deallocate/1,stop/0,frequencies/0
	]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() -> 
	register(?MODULE, spawn(?MODULE, init, [])).

init() ->
	Frequencies = {get_frequencies(), []},
	loop(Frequencies).

% Hard Coded
get_frequencies() -> 
	[10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
	io:format("Call: ~p(~p).~n", [?FUNCTION_NAME,Frequencies]),
	receive
		{request, Pid, allocate} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			{NewFrequencies, Reply} = allocate(Frequencies, Pid),
			Pid ! {reply, Reply},
			loop(NewFrequencies);

		{request, Pid , {deallocate, Freq}} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			NewFrequencies = deallocate(Frequencies, Freq),
			Pid ! {reply, ok},
			loop(NewFrequencies);

		{request, Pid, stop} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			Pid ! {reply, stopped};

		{request, Pid, frequencies} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			Pid ! {reply, Frequencies},
			loop(Frequencies);

		Message ->
			io:format("ERROR: unhandled Message: ~p~n",[Message])

	after(200) -> 
		clear(0)

	end.

%% Functional interface

allocate() -> 
	%io:format("Call: ~p().~n", [?FUNCTION_NAME]),
	?MODULE ! {request, self(), allocate},
	receive 
		{reply, Reply} = Message -> 
			io:format("~p:Rx:~p~n", [self(), Message]),
			%timer:sleep(1000),
			Reply
	after(200) -> 
		clear(0)
	end.

deallocate(Freq) -> 
	io:format("Call: ~p(~p).~n", [?FUNCTION_NAME,Freq]),
	?MODULE ! {request, self(), {deallocate, Freq}},
	receive 
		{reply, Reply} = Message -> 
			io:format("~p:Rx:~p~n", [self(), Message]),
			%timer:sleep(1000),
			Reply
	after(200) -> 
		clear(0)
	end.

stop() -> 
	%io:format("Call: ~p().~n", [?FUNCTION_NAME]),
	?MODULE ! {request, self(), stop},
	receive 
		{reply, Reply} = Message -> 
			%io:format("~p:Rx:~p~n", [self(), Message]),
			Reply
	end.

frequencies() ->
	%io:format("Call: ~p().~n", [?FUNCTION_NAME]),
	%?MODULE ! {request, self(), frequencies},
	?MODULE ! {},
	receive
		{reply, Reply} -> 
		%{reply, Reply} = Message -> 
			%io:format("~p:Rx:~p~n", [self(), Message]),
			Reply
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


%%------------------------------------------------------------------------------
%%
%%
%%
%%

clear(N) ->
	receive
		Msg -> 
			io:format("~p:Rx:~p:(~p)DROP!~n", [self(), Msg, N]),
			clear(N+1)
	after(0) ->
		mailbox_clear
	end.

