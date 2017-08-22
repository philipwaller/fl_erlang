-module(router).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0,loop/1,worker/2]).

%%------------------------------------------------------------------------------
%%
%%

start() -> 
	register(?MODULE,spawn(?MODULE,init,[])).


init() ->
	process_flag(trap_exit, true),
	?MODULE:loop(workers(frequency_config())).

frequency_config() -> [
		{freqA, [10,11,12,13,14,15]},
		{freqB, [20,21,22,23,24,25]}
	].

workers(Cfg) -> 
	[?MODULE:worker(Name,Fs) || {Name,Fs} <- Cfg].

worker(Name,Fs) ->
	Wrk = spawn_link(frequency,init,[Name,Fs]),
	register(Name,Wrk),
	Wrk.

loop(Wrks) ->
	io:format("loop(~w)~n",[Wrks]),
	receive
		{request,Pid,allocate} ->
			Pid ! {reply,allocate},
			?MODULE:loop(Wrks);

		{request,Pid,{deallocate,F}} ->
			Pid ! {reply,{deallocate,F}},
			?MODULE:loop(Wrks);

		{request,Pid,stop} ->
			[exit(Wrk,kill) || Wrk <- Wrks],
			Pid ! {reply,stopped};

		{'EXIT',Pid,Reason} ->
			io:format("EXIT:~w:~w~n",[Pid,Reason]),
			?MODULE:loop(Wrks);

		_ ->
			?MODULE:loop(Wrks)
	end.



%%------------------------------------------------------------------------------
%% 	. Functional Interface .
%%

allocate() 		-> interface_helper({request, self(), allocate}).
deallocate(Freq) 	-> interface_helper({request, self(), {deallocate, Freq}}).
stop() 			-> interface_helper({request, self(), stop}).

interface_helper(Request) ->
	?MODULE ! Request,
	receive
		{reply,Reply} -> Reply
	end.

