-module(scenario).
-export([setup/0,client/2,random_elem/1]).

% Use this module to exercise the behaviour of the 
% hardened frequency server.

% Calling setup will launch the server and two clients: alice and bob.

setup() -> [
        fh1:start(),
        spawn(?MODULE,init,[alice]),
        spawn(?MODULE,init,[bob])
    ].

init(Id) ->
    client(Id, []).

shutdown() ->
    %deallocate_all(),
    %stop_client(),
    %stop_server(),
    ok.

deallocate([]) -> ok;
deallocate([F|Fs]) ->
    fh1:deallocate(F),
    deallocate(Fs).


% A client, parametrised by its name (optional, but useful instrumentation),
% and the list of frequencies currently allocated to that process. Needed
% to produce calls to deallocate/1 that don't fail.

% Could also 
%   - parameterise on the ratio of allocates to deallocates
%   - deal with case when no frequencies available: here a client fails
%   - add stop commands.

client(Id,Freqs) ->
    case rand:uniform(2) of
        1 -> 
            case fh1:allocate() of
            {ok,Freq} -> 
                io:format("Frequency ~w allocated to client ~w.~n", [Freq,Id]),
                timer:sleep(10000),
                loop(Id,[Freq|Freqs]);
            {error,no_frequency} ->
                io:format("No frequency available for  allocation to client ~w.~n", [Id]),
                timer:sleep(sleep_time()),
                loop(Id,Freqs)
            end;
        2 ->
            Len = length(Freqs),
            case Len of 
                0 -> 
                    io:format("No frequencies to deallocate by client ~w.~n", [Id]),
                    timer:sleep(sleep_time()),
                    loop(Id,Freqs);  
                _ -> 
                    Freq = lists:nth(rand:uniform(Len),Freqs),
                    fh1:deallocate(Freq), 
                    io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
                    timer:sleep(sleep_time()),
                    loop(Id,lists:delete(Freq,Freqs))
            end
    end.

shutdown(Id,[]) ->
    ok;
shutdown(Id,[F|Fs]) ->
    fh1:deallocate(F),
    shutdown(Id,Fs).


loop(Id,Freqs) ->
    receive
    stop ->
            shutdown(Id,Freqs)

    after 0 -> 
            client(Id,Freqs)

    end.

% for debugging purposes: chooses a random element of a non-empty list.

random_elem([]) ->
    empty;
random_elem(Xs) ->
    Len = length(Xs),
    lists:nth(rand:uniform(Len),Xs).  


%

sleep_time() -> 1000.


