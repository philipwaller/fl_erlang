-module(cl1).
-export([
        setup/0,
        client/2,                                       %%% Only required to quieten WARNINGs
        require/0,release/0,stop/0
    ]).

setup() -> 
        register(?MODULE, spawn(?MODULE,client,[client_name(),[]])).

client_name() -> sam_i_am.

client(Id,Freqs) ->
    io:format("Client ~w has frequencies ~w.~n", [Id,Freqs]),
    timer:sleep(1000),

    receive

        {request,Pid,require} ->
            case (Allocation = fh1:allocate()) of
            {ok,Freq} -> 
                io:format("Frequency ~w allocated to client ~w.~n", [Freq,Id]),
                Pid ! {reply, Allocation},
                client(Id,[Freq|Freqs]);
            {error,no_frequency} ->
                io:format("No frequency available for  allocation to client ~w.~n", [Id]),
                Pid ! {reply, Allocation},
                client(Id,Freqs)
            end;

        {request,Pid,release} ->
            case (Freq = random_elem(Freqs)) of 
                empty -> 
                    io:format("No frequencies to deallocate by client ~w.~n", [Id]),
                    Pid ! {reply, Freq},
                    client(Id,Freqs);  
                Freq -> 
                    fh1:deallocate(Freq), 
                    io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
                    Pid ! {reply, Freq},
                    client(Id,lists:delete(Freq,Freqs))
            end;

        {request,Pid,stop} ->
            release(Freqs),
            Pid ! {reply,client_stopped}

    end.

release([]) -> ok;
release([F|Fs]) ->
    fh1:deallocate(F),
    release(Fs).


random_elem([]) ->
    empty;
random_elem(Xs) ->
    Len = length(Xs),
    lists:nth(rand:uniform(Len),Xs).  


%-------------------------------------------------------------------------------
%   . API .
%

require() -> 
    ?MODULE ! {request, self(), require},
    receive 
      {reply, Reply} -> Reply;
      _              -> ok
    end.

release() -> 
    ?MODULE ! {request, self(), release},
    receive 
      {reply, Reply} -> Reply;
      _              -> ok
    end.

stop() -> 
    ?MODULE ! {request, self(), stop},
    receive 
      {reply, Reply} -> Reply;
      _              -> ok
    end.

