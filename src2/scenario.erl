%% +
%%
%% FutureLearn (www.futurelearn.com)
%% Concurrent Programming in Erlang, The University of Kent
%%
%% Week 2, Step 07: Hardening the frequency server.
%% 
%% -
-module(scenario).
-export([
        start/0,loop/2,stop/0
    ]).


%%------------------------------------------------------------------------------
%%  . API .
%%

%
% Start a 'scenario'. 
% - start the server
% - spawn two clients [alice, bob]. The clients are registered so that they
%   can be 'stopped' later.
%
start() -> [
        fh1:start(),
        register(alice,spawn(?MODULE,loop,[alice,[]])),
        register(bob,spawn(?MODULE,loop,[bob,[]]))
    ].

%
% Stop the 'scenario' cleanly.
%
stop() -> 
    stop(alice),
    stop(bob),
    fh1:stop().


%%------------------------------------------------------------------------------
%%  . Main Client Processing Loop .
%%

%
% Main loop.
% - Id : client identifier, used to label client output.
% - Freqs : list of frequencies held by client.
%
% @notes:
%   try...catch...end block is used to wrap calls to client/2. If the server
%   process terminates while the client is processing, a 'badarg' error is 
%   thrown due to the absence of an process refered to by name. If the 'badarg' 
%   is allowed to propogate the client process crashes before receiving the
%   'EXIT' message.
%
loop(Id,Freqs) ->
    receive
        {request,Pid,stop} ->
            timer:sleep(2000),              %% Allow client requests to finish.
            deallocate_all(Id,Freqs),       %% Cleanly return frequencies to pool.
            Pid ! {reply, stopped};

        {'EXIT', _Pid, Reason} ->
            io:format("~w:Server EXIT detected: ~w!~n",[Id,Reason]),
            shutdown(Id,Freqs)              %% shutdown gracefully
      
    after 0 -> 
        try client(Id,Freqs) of
            NewFreqs -> loop(Id,NewFreqs)
        catch
            error:badarg -> 
                io:format("~w:~w: SERVER down!.~n", [Id, Freqs]),
                shutdown(Id,Freqs)          %% shutdown gracefully
        end

    end.

%
% Client processing.
%   If no frequencies are allocated, the client process is allowed to terminate
%   by the linked closing server process. If the client holds frequencies then
%   the server termination message is trapped and the client shutsdown gracefully.
%
% @param: Id - identifier useful for instrumentation.
% @param: Freqs - currently allocated frequencies. Valid frequency must be 
%         returned to the server process
% @returns: the new list of client frequencies.
% @throws: error:badarg in cases where server process has been terminated.
%
% @todo: could parameterise on the ratio of allocates to deallocates
%
client(Id,Freqs) ->
    case rand:uniform(2) of
        1 ->                            %%% Allocation 
            case fh1:allocate() of
            {ok,Freq} -> 
                io:format("~w:~w: +~w allocated to client.~n", [Id, Freqs,Freq]),
                set_trap_exit(true),
                timer:sleep(sleep_time()),
                [Freq|Freqs];
            {error,no_frequency} ->
                io:format("~w: No frequency available for allocation to client.~n", [Id]),
                timer:sleep(sleep_time()),
                Freqs
            end;
        2 ->                            %%% Deallocation
            Len = length(Freqs),
            case Len of 
            0 -> 
                io:format("~w: No frequencies to deallocate for client.~n", [Id]),
                timer:sleep(sleep_time()),
                Freqs;  
            _ -> 
                Freq = lists:nth(rand:uniform(Len),Freqs),
                fh1:deallocate(Freq),
                io:format("~w:~w: -~w deallocated by client.~n", [Id,Freqs,Freq]),
                NewFreqs = lists:delete(Freq,Freqs),
                set_trap_exit(NewFreqs),
                timer:sleep(sleep_time()),
                NewFreqs
            end
    end.

%
% Deallocate all current client frequencies.
%   In cases where the client is stopped but the server is still active, the 
%   client returns its allocated frequencies to the pool.
%   
deallocate_all(_Id,[]) -> ok;
deallocate_all(Id,[F|Fs]) ->
    fh1:deallocate(F),
    io:format("~w:~w deallocated.~n",[Id,F]),
    deallocate_all(Id,Fs).


%
% Shutdown the client 'cleanly'.
%
shutdown(Id,Freqs) ->
    %% @todo: finish processing and close cleanly; modelled with sleep.
    timer:sleep(10000),
    io:format("~w:~w:Client STOPPED!~n",[Id,Freqs]),
    ok.
    

%%------------------------------------------------------------------------------
%%  . Helpers.
%%  

%
% Set or clear the 'trap_exit' process flag.
%   - cleared on empty list, otherwise set.
%
set_trap_exit([]) -> process_flag(trap_exit, false);
set_trap_exit(_) -> process_flag(trap_exit, true).

%
% Utility to allow adjusting default delay times (debugging).
%
sleep_time() -> 1000.

%
% Stop a client process.
%
stop(Client) ->
    Client ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply
    end.

