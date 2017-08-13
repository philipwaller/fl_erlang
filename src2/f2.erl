%%+
%% Future Learn (https://www.futurelearn.com)
%% Concurrent Programming in Erlang, University of Kent
%%
%% Week01. Assignment: Frequency Server
%% Author: Phil Waller
%%-
%-------------------------------------------------------------------------------
% 	. NOTES .
%
% * Added functionality to trace program execution.
% * Frequency Server will only allocate available frequencies, but no check on 
%   deallocated values is permormed allowing additional frequencies and duplicates 
%   to be added to the server.
% * Frequency Server allows multiple requests from the same 'client'.
% * Server delays added to the 'allocate' and 'deallocate' request processing.
% * New 'frequencies' request type added to server which allows querying available/used 
%   frequencies.
% * New client interface calls 'clear/0' and 'fqs/0' added.
% * Timeouts added to client funcitonal interface message processing blocks.
% * The client functional interface follows the pattern of 'send request'/'wait reply'. 
%   In the situation where the server is overloaded and the client 'timesout', the 
%   client may receive a message linked to the wrong request. To prevent this 'clear/0' 
%   is called prior to server request so that the message queue is empty. A better 
%   system would be to set up a 'key' exchange that allows duplex communication.
% * The 'clear/0' functional interface call can be called directly from the shell. 
%   A more robust client would include a processing 'loop' that would call 'clear/0' 
%   intermittenly using timeout.
% * The 'allocate/2' and 'deallocate/2' helper functions where taken as supplied.
%
%-------------------------------------------------------------------------------

-module(f2).
-export([
	 	start/0,
		allocate/0,deallocate/1,stop/0,
		fqs/0,clear/0
	]).
-export([init/0]).

%
% Spawn and Register the Server using the 'Module Name'.
%
start() ->
	register(?MODULE, spawn(?MODULE, init, [])).


%
% Initialize server loop with list of frequencies.
%
init() ->
	io:format("Call: ~p:~p().~n", [self(), ?FUNCTION_NAME]),
	Frequencies = {get_frequencies(), []},
	loop(Frequencies).

%
% Hard coded 'initial' list of frequencies.
%
get_frequencies() -> [10,11,12,13,14,15].

%
% The Main Server Loop handles Message passing.
%
loop(Frequencies) ->
	%io:format("Call: ~p:~p(~p).~n", [self(), ?FUNCTION_NAME,Frequencies]),
	receive
		{request, Pid, allocate} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			timer:sleep(1000),
			{NewFrequencies, Reply} = allocate(Frequencies, Pid),
			Pid ! {reply, Reply},
			loop(NewFrequencies);

		{request, Pid , {deallocate, Freq}} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			timer:sleep(1000),
			NewFrequencies = deallocate(Frequencies, Freq),
			Pid ! {reply, ok},
			loop(NewFrequencies);

		{request, Pid, frequencies} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			Pid ! {reply, Frequencies},
			loop(Frequencies);

		{request, Pid, stop} = Message ->
			io:format("~p:Rx:~p~n", [self(), Message]),
			Pid ! {reply, stopped}
	end.

%%------------------------------------------------------------------------------
%%
%% Functional interface
%%

allocate() -> 
	io:format("Call: ~p:~p().~n", [self(), ?FUNCTION_NAME]),
	clear(),
	?MODULE ! {request, self(), allocate},
	receive 
		{reply, Reply} = Message -> 
			io:format("~p:Rx:~p~n", [self(), Message]),
			Reply

	after 300 -> {error, allocation_timeout}

	end.

deallocate(Freq) -> 
	io:format("Call: ~p:~p(~p).~n", [self(), ?FUNCTION_NAME, Freq]),
	clear(),
	?MODULE ! {request, self(), {deallocate, Freq}},
	receive 
		{reply, Reply} = Message -> 
			io:format("~p:Rx:~p~n", [self(), Message]),
			Reply

	after 300 -> {error, deallocation_timeout}

	end.

fqs() -> 
	io:format("Call: ~p:~p().~n", [self(), ?FUNCTION_NAME]),
	clear(),
	?MODULE ! {request, self(), frequencies},
	receive 
		{reply, Reply} = Message -> 
			io:format("~p:Rx:~p~n", [self(), Message]),
			Reply

	after 300 -> 
		{error, frequencies_timeout}

	end.

stop() -> 
	io:format("Call: ~p:~p().~n", [self(), ?FUNCTION_NAME]),
	clear(),
	?MODULE ! {request, self(), stop},
	receive 
		{reply, Reply} = Message -> 
			io:format("~p:Rx:~p~n", [self(), Message]),
			Reply

	after 300 -> {error, stop_timeout}

	end.

clear() ->
	receive
		Msg -> 
			io:format("~p:Rx:~p:DROP!~n", [self(), Msg]),
			clear()

	after 0 -> mailbox_clear

	end.


%%------------------------------------------------------------------------------
%% 
%% Allocate/Deallocate Frequency Helper functions
%%

allocate({[], Allocated}, _Pid) ->
	{{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
	{{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	NewAllocated=lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free],  NewAllocated}.

