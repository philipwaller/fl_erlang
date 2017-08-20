%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(fhe).
-export([
         start/0,
         allocate/0,deallocate/1,stop/0,
         invalid/0
        ]).
-export([init/0]).

%%------------------------------------------------------------------------------
%%      . Server .
%%
%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%
% The Main Server Loop
%
% @note: extended to handle (ignore) 'unsupported' request types.
%
loop(Frequencies) ->
    io:format("fh3:loop(~w)~n", [Frequencies]),
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);

        {request, Pid , {deallocate, Freq}} ->
            {NewFrequencies,Reply} = deallocate(Frequencies, Freq),
            Pid ! {reply, Reply},
            loop(NewFrequencies);

        {request, Pid, stop} ->
            Pid ! {reply, stopped};

        {'EXIT', Pid, _Reason} ->
            NewFrequencies = exited(Frequencies, Pid), 
            loop(NewFrequencies);

        {request, Pid, _} ->
            Pid ! {reply, unsupported_operation},
            loop(Frequencies);

        _ ->
            loop(Frequencies)
    end.

%%------------------------------------------------------------------------------
%%      . Functional interface .
%%
%% @note : refactored to abstract functionality into 'forward/1'.
%%

allocate() -> forward(allocate).
deallocate(Freq) -> forward({deallocate,Freq}).
stop() -> forward(stop).

invalid() -> forward(invalid).

forward(Cmd) ->
    ?MODULE ! {request, self(), Cmd},
    receive 
        {reply, Reply} -> Reply;
        _              -> ok
    end.

%%------------------------------------------------------------------------------
%%      . The Internal Help Functions .
%%

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

%
% Deallocate frequency.
%
% @param Freqs : tuple of 'free' and 'allocated' freqencies.
% @param Freq  : frequency in 'allocated' to be freed.
% @return : New tuple of frequencies plus a 'reply' tuple.
%
% @note : extended to check for valid frequency to be freed, returning a status code.
% @note : unlink process only if no-more frequencies are allocated.
%
deallocate({Free, Allocated}=Freqs, Freq) ->
    case lists:keyfind(Freq,1,Allocated) of
        false -> 
            {Freqs, {error,unallocated_frequency}};
        {Freq,Pid} ->
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            case lists:keyfind(Pid,2,Allocated) of 
                false -> unlink(Pid);
                _     -> ok
            end,
            {{[Freq|Free], NewAllocated}, {ok,Freq}}
    end.

%
% Free frequencies help by a terminated client process.
%
% @param {Free,Allocated} : lists of frequencies
% @param Pid : process identifier for terminated client
% @return : updated frequencies with previously allocated returned to the pool.
%
% @note : fixed (with recursive call) to free up ALL allocated frequencies for Pid.
%
exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid,2,Allocated) of
    {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        exited({[Freq|Free],NewAllocated}, Pid); 
    false ->
        {Free,Allocated} 
    end.


