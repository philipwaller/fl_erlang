-module(rps).
-export([
        play/1,play_two/3,const/1,enum/1,val/1,tournament/2,
        echo/1,rock/1,no_repeat/1,cycle/1,rand/1,
        oStrategy/1,rStrategy/1
]).

-include_lib("eunit/include/eunit.hrl").

%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

play_two(_, _, PlaysL, PlaysR, 0) ->
        io:format("[~w]: G a m e   O v e r !~n", [tournament(PlaysL, PlaysR)]);

play_two(StrategyL, StrategyR, PlaysL, PlaysR, N) ->
        MoveL = StrategyL(PlaysR),
        MoveR = StrategyR(PlaysL),  
        Result = result(MoveL, MoveR),
        io:format("~4w : ~8w vs. ~-8w~n",[Result,MoveL,MoveR]),
        play_two(StrategyL, StrategyR, [MoveL|PlaysL], [MoveR|PlaysR], N-1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[],[]).

% tail recursive loop for play/1

play(Strategy,Moves,SMoves) ->
        {ok,P} = io:read("Play: "),
        Play = expand(P),
        case Play of
                stop ->
                        io:format("Stopped~n"),
                        io:format("Final Score: ~w~n", [tournament(Moves,SMoves)]);
                _    ->
                        SPlay=Strategy(Moves),
                        Result = result(Play, SPlay),
                        io:format("Result: ~4w ( ~8w vs ~-8w )~n",[Result, Play, SPlay]),
                        play(Strategy,[Play|Moves],[SPlay|SMoves])
        end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock)     -> 0;
val(paper)    -> 1;
val(scissors) -> 2.

% give the play which the argument beats.

beats(rock)     -> scissors;
beats(paper)    -> rock;
beats(scissors) -> paper.

%%------------------------------------------------------------------------------
%%      . Strategies .
%%

%
% Basic: supplied
%
echo([])       -> paper;
echo([Last|_]) -> Last.

rock(_) -> rock.


% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

%       . no_repeat/1 .
%
% "oponent never repeats herself"
% - not sure about this one:
% - at superficial level: never plays same individual move consecutively;
% - or more complex: never plays same sequence of  moves consecutively.
%
% Assuming no single consecutive moves: how does this guarantee knowing next 
% move? If I play 'rock', I can play either 'paper' or 'scissors' next.
no_repeat([X|_]) -> beats(X);
no_repeat([]) -> rand(dummy).

const(Play) -> fun(_) -> Play end.

% Cycle last 3 moves.
% First 3 moves random order.
cycle([_,_,Z | _])      -> Z;
cycle([X,Y | _])        -> leastf( [X,Y] );     % 2 current moves
cycle([X | _])          -> no_repeat( [X] );    % 1 current move
cycle([])               -> rand(dummy).         % 0 current moves

% Randomly play move.
rand(_) -> enum( rand:uniform(3) - 1 ).

% Least frequent.
leastf(Xs) -> leastPlayed( analyse(Xs, {0,0,0}) ).

% Least played from counts of previous moves.
% - favours rock over paper over scissors when equal moves of each type. 
leastPlayed({Rn, Pn, Sn}) ->
        if
                Rn=<Pn andalso Rn=<Sn   -> rock;
                Pn=<Sn                  -> paper;
                true                    -> scissors
        end.

% Most frequent.
mostf(Xs) -> mostPlayed( analyse(Xs, {0,0,0}) ).

% Most played from counts of previous moves.
% - favours rock over paper over scissors when equal moves of each type. 
mostPlayed({Rn, Pn, Sn}) ->
        if
                Rn>=Pn andalso Rn>=Sn   -> rock;
                Pn>=Sn                  -> paper;
                true                    -> scissors
        end.

% Analyse previous moves by counting occurances.
analyse([], A) -> A;
analyse([X|Xs], {Rn, Pn, Sn}) ->
        case X of
                rock            -> analyse( Xs, { Rn+1, Pn, Sn } );
                paper           -> analyse( Xs, { Rn, Pn+1, Sn } );
                scissors        -> analyse( Xs, { Rn, Pn, Sn+1 } )
        end.
        
%
%       . rStrategy/1 .
%       . rStrategy/2 .
%
% Return random strategy (function)  from list of strategies.
%
rStrategy(Ss) -> 
        fun (Ms) -> 
                (rStrategy( rand:uniform( length(Ss) ) - 1, Ss ))(Ms) end.

rStrategy( 0, [X|_] ) -> X;
rStrategy( N, [_|Xs] ) -> rStrategy( N-1, Xs ).

%
%       . oStrategy/1 .
%
% Optimal strategy for currently played sequence (adaptive).
%

oStrategy(Ss) -> 
        fun (Ms) -> (optimal_strategy(Ms, Ss))(Ms) end.

% Get most successful strategy against specific move sequence.
optimal_strategy(Ms, Ss) -> 
        {_, Strategy} = lists:max( score_strategies(Ms, Ss) ),
        Strategy.

% Generate list of {Score, Strategy} results for different strategies against 
% specific move sequence.
score_strategies(_, []) -> [];
score_strategies(Ms, [S|Ss]) ->
        [_Next|Prevs] = play_strategy(S, Ms),
        [ {tournament(Prevs, Ms), S} | score_strategies(Ms, Ss) ].
        

% Generate list of moves generated by playing a strategy.
% Return: [NextPlay | PreviousPlays].
play_strategy(S, []) -> [S([])];
play_strategy(S, [M|Ms]) -> [S([M]) | play_strategy(S, Ms)].


%%------------------------------------------------------------------------------
%%      . Tests .
%%

analyse_test() -> [
        ?assertEqual( {0,0,0}, analyse([], {0,0,0}) ),
        ?assertEqual( {1,0,0}, analyse([rock], {0,0,0}) ),
        ?assertEqual( {0,1,0}, analyse([paper], {0,0,0}) ),
        ?assertEqual( {0,0,1}, analyse([scissors], {0,0,0}) ),
        ?assertEqual( {2,3,1}, analyse([rock, scissors, paper, paper, rock, paper], {0,0,0}) ),

        ?assertEqual( paper, mostf([rock, scissors, paper, paper, rock, paper]) ),
        ?assertEqual( scissors, leastf([rock, scissors, paper, paper, rock, paper]) ),
        ok
].

rStrategy_test() ->[
        ?assertEqual( rock, (rStrategy([fun echo/1]))([rock]) ),
        ok
].
        
play_stategy_test() -> [
        ?assertEqual( 
                [rock,rock,rock,rock], 
                play_strategy(fun rock/1, [rock,paper,scissors]) ),
        ?assertEqual( [paper], play_strategy(fun echo/1, []) ),
        ?assertEqual( [rock,paper], play_strategy(fun echo/1, [rock]) ),
        ?assertEqual( 
                [rock,paper,scissors,paper], 
                play_strategy(fun echo/1, [rock,paper,scissors]) ),
        ok
].


