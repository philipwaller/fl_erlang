%% +
%%
%%
%% -
-module(super).
-export([super/0]).

%%------------------------------------------------------------------------------
%% 
%% # Using the System
%%
%% 1> register(echo,spawn(echo,listener,[])).
%% 2> Pid=spawn(talk,worker,[]).
%%
%% 3> exit(Pid,kill).
%%      . kills the worker but server still up.
%%      . Possible to create multiple new workers with calls to 
%%        `spawn(talk,worker,[])`
%%
%% 3> exit(whereis(echo),kill).
%%      . kills the echo server
%%      . all current workers fail with `error:badarg`.
%%
%%
%% # Adding a supervisor
%%
%% * Kill 'echo' server:
%%      . new 'echo' server spawned immediately.
%%      . no break in 'count' of 'talk' client.
%%
%% * Kill 'talk' client:
%%      . new 'talk' client spawned immediately.
%%      . 'count' reset to zero.
%%
%% # Add delay to Respawn of 'echo' server:
%%
%% * Kill 'echo' server:
%%      . current 'talk' client dies with `error:badarg` as it tries to 
%%        communicate with killed 'echo' server
%%      . new 'echo' server spawned.
%%      . new 'talk' server spawned.
%%      . 'count' reset to zero.
%%
%%  * Kill 'talk' client:
%%      . As above
%%      . new 'talk' with 'count' reset.
%%
%%
%%------------------------------------------------------------------------------
%%      . supervisor . 
%%

super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo,listener,[]),
    register(echo,E),
    io:format("echo spawned.~n"),
    T = spawn_link(talk,worker,[]),
    register(talk,T),
    io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
    loop(E,T).

loop(E,T) ->
     receive
        {'EXIT', T, _} -> 
            NewT = spawn_link(talk,worker,[]),
            register(talk,NewT),
            io:format("worked re-spawned as Pid ~w.~n",[whereis(talk)]),
            loop(E,NewT);
         {'EXIT', E, _} -> 
            timer:sleep(1000), 
            NewE = spawn_link(echo,listener,[]),
            register(echo,NewE),
            io:format("echo re-spawned.~n"),
            loop(NewE,T)
    end.

