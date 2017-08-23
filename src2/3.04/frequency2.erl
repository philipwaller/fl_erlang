%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/2]).


init(Id,Frequencies) -> loop(Id,{Frequencies,[]}).

loop(Id,Frequencies) ->
	receive
		{request, Pid, allocate} ->
			{NewFrequencies, Reply} = allocate(Frequencies, Pid),
			Pid ! {reply, Reply},
			loop(Id,NewFrequencies);

		{request, Pid , {deallocate, Freq}} ->
			NewFrequencies = deallocate(Frequencies, Freq),
			Pid ! {reply, ok},
			loop(Id,NewFrequencies);

		{request, Pid, stop} ->
			Pid ! {reply, stopped}
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
