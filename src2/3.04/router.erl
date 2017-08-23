-module(router).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0,loop/1,worker/2]).

%%------------------------------------------------------------------------------
%%
%%

start() -> 
	register(?MODULE,spawn(?MODULE,init,[])).


%%------------------------------------------------------------------------------
%%
%%

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
	{Wrk,Fs}.

%%------------------------------------------------------------------------------
%%
%%

loop(Wrks) ->
	io:format("loop(~w)~n",[Wrks]),
	receive
		{request,_Pid,allocate} = Request ->
                        {Wrk,_Fs} = load_bal(Wrks),
                        Wrk ! Request,
			?MODULE:loop(Wrks);

		{request,_Pid,{deallocate,_F}} = Request -> 
                        process_deallocation(Wrks,Request),
                        ?MODULE:loop(Wrks);

		{request,Pid,stop} ->
			[exit(Wrk,kill) || {Wrk,_Fs} <- Wrks],
			Pid ! {reply,stopped};

		{'EXIT',_Pid,_Reason} = Request ->
			process_exit(Wrks,Request),
			?MODULE:loop(Wrks);

		_ ->
			?MODULE:loop(Wrks)
	end.

process_deallocation(Wrks,{request,Pid,{deallocate,F}}=Request) ->
        try frequency_worker(F,Wrks) of
                {Wrk,_Fs} -> frequency_worker(F,Wrks),
                Wrk ! Request
        catch
                _:_ -> Pid ! {reply,{illegal_deallocation_request,F}}
        end.

process_exit(Wrks,{'EXIT',Pid,Reason}) ->
        W = hd([ {Wrk,Fs} || {Wrk,Fs} <- Wrks, Wrk=:=Pid ]),
	%respawn name
	io:format("~w~n",[W]).





% Random load balancing
load_bal(Wrks) ->
        lists:nth(rand:uniform(length(Wrks)),Wrks).

% Frequency Worker
frequency_worker(F,Wrks) ->
        hd([{Wrk,Fs} || {Wrk,Fs} <- Wrks, lists:member(F,Fs)]).

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

