-module(frequency).
-export([
	 	start/0, init/0	
	]).

start() ->
	register(?MODULE, spawn(?MODULE,init,[])).

init() ->
	Fs = {get_frequencies(), []},
	loop(Fs).

get_frequencies() -> lists:seq(10,15).

loop(Fs) ->
	receive
		{request,Pid,allocate} ->
			{NewFrequencies, Reply} = allocate(Fs,Pid),
			Pid ! {reply, Reply},
			loop(NewFrequencies);

		{request,Pid,deallocate} ->
			{NewFrequencies, Reply} = deallocate(Fs, Pid),
			Pid ! {reply,Reply},
			loop(NewFrequencies);

		{request,Pid,stop} ->
			Pid ! {reply,stopped}
	end.

allocate({[], Allocated}, _Pid) ->
	{ {[], Allocated}, {error, no_frequency} };

allocate({[F|Free], Allocated} = Frequencies, Pid) ->
	case (lists:keyfind(Pid,2,Allocated)) of
		false -> { {Free, [{F,Pid}|Allocated]}, {ok,F} };
		_     -> { Frequencies, {error, already_allocated} }
	end.

deallocate({Free, Allocated} = Frequencies, Pid) ->
	case (lists:keyfind(Pid,2,Allocated)) of
		false -> { Frequencies, {error, not_allocated} };
		{F, Pid} -> { { [F|Free], lists:keydelete(F,1,Allocated)}, ok }
	end.

