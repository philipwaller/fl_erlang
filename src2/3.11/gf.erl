%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(gf).
-behaviour(gen_server).

% an implementation of this is included.
-export([start_link/0]).

% you need to implement these functions.
-export([init/1, handle_call/3, handle_cast/2]).

% these are implemented for you.
-export([handle_info/2, terminate/2, code_change/3]).

% you will need to implement these.
-export([allocate/0,report/0,inject/1,deallocate/1,stop/0]).

%% These are the start functions used to create and
%% initialize the server.

start_link() ->
    gen_server:start_link(
		{local, ?MODULE}, 
		?MODULE, [], []).

init([]) ->
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].


%%
%% Functional interface
%%

allocate() -> 
    gen_server:call(?MODULE,allocate).

report() ->
    gen_server:call(?MODULE,report).

inject(Fs) ->
    gen_server:call(?MODULE,{inject,Fs}).

deallocate(Freq) -> 
    gen_server:call(?MODULE,{deallocate,Freq}).
    %gen_server:cast(?MODULE,{deallocate,Freq}).

stop() ->  
    gen_server:stop(?MODULE).
    %gen_server:call(?MODULE,stop).
    %gen_server:cast(?MODULE,stop).


%
% 'call' is required when:
%   - the 'From' pid is required
%   - feedback is necessary
%

handle_call(allocate, From, State) ->
    {NewFrequencies, Reply} = allocate(State, From),
    {reply,Reply,NewFrequencies};

handle_call(report, _From, {Free, _}=State) ->
    {reply,{length(Free),Free},State};

handle_call({inject,NFs}, _From, {Free,Alloc}=State) ->
    Allocated = [F||{F,_} <- Alloc],
    OFs = lists:usort(lists:append(Free,Allocated)),
    NewOFs = lists:usort(NFs),
    case ordsets:is_disjoint(OFs,NewOFs) of
        true -> 
            Fs = ordsets:union(OFs,NewOFs),
            {reply,{ok,length(Fs),Fs},{lists:append(Free,NewOFs),Alloc}};
        _    ->
            {reply,{dups,ordsets:intersection(OFs,NewOFs)},State}
    end;

% 'cast' is a better option.
handle_call({deallocate,Freq}, _From, State) ->
    NewFrequencies = deallocate(State, Freq),
    {reply,ok,NewFrequencies};

% 'gen_server:stop' is a better option.
handle_call(stop,_From,State) ->
    {stop,stopped,ok,State}.

%
%
%
%

handle_cast({deallocate,Freq}, State) ->
    NewFrequencies = deallocate(State, Freq),
    {noreply,NewFrequencies};

% 'gen_server:stop' is a better option.
handle_cast(stop, State) ->
    {stop,stopped,State}.
  

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.


% default implementations

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
