-module(s308).
-export([
                beat/1, result/2,
                tournament/2
	]).

-include_lib("eunit/include/eunit.hrl").

-define(WIN,  1).
-define(DRAW,  0).
-define(LOSE, -1).

%-------------------------------------------------------------------------------
%       . beat/1 .
%
% Move that will beat the current play.
%
%  IN: Play one of [ rock, paper, scissors ].
% OUT: Move to beat Play
% EXP: Illegal Play 
%    - consider changing error message (shout 'CHEAT').
%    - illegal moves lose by default? (both cheat?).
% 
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

%-------------------------------------------------------------------------------
%       . lose/1 .
%
% Move that will lose to current play.
%
lose(Play) -> beat(beat(Play)).

%-------------------------------------------------------------------------------
%       . result/2 .
%
% Result for play P0 against play P1.
%
%  IN: two plays
% OUT: round result [ win, lose, draw ]
%    - Currently represent as atoms
%    - Future enhancement return HOF? eg cheer(), groan(), sigh().
%    - Changed to integers to enable scoring.
%
result(P, P) -> ?DRAW;
result(P0, P1) ->
        case P0 =:= beat(P1) of 
                true -> ?WIN;
                _    -> ?LOSE
        end.

%-------------------------------------------------------------------------------
%       . tournament/2 .
%
tournament(Ls, Rs) -> 
        lists:foldr( fun(X,Y) -> X+Y end, 0, lists:zipwith(fun result/2, Ls, Rs) ).

%-------------------------------------------------------------------------------
%       . TESTS .
%

beat_test() -> [
        ?assertEqual( paper, beat(rock) ),
        ?assertEqual( scissors, beat(paper) ),
        ?assertEqual( rock, beat(scissors) )
        % Illegal Arg test
].

lose_test() -> [
        ?assertEqual( scissors, lose(rock) ),
        ?assertEqual( rock, lose(paper) ),
        ?assertEqual( paper, lose(scissors) )
        % Illegal Arg test
].

result_test() -> [
        ?assertEqual( ?LOSE, result(rock, paper) ),
        ?assertEqual( ?LOSE, result(paper, scissors) ),
        ?assertEqual( ?LOSE, result(scissors, rock) ),
        ?assertEqual( ?WIN, result(rock, scissors) ),
        ?assertEqual( ?WIN, result(paper, rock) ),
        ?assertEqual( ?WIN, result(scissors, paper) ),
        ?assertEqual( ?DRAW, result(rock, rock) ),
        ?assertEqual( ?DRAW, result(paper, paper) ),
        ?assertEqual( ?DRAW, result(scissors, scissors) )
].

tournament_test() -> [
        % ?assertEqual( 0, tournament([], []) ),
        ?assertEqual( 0, tournament([rock], [rock]) ),
        ?assertEqual( 1, tournament([rock], [scissors]) ),
        ?assertEqual( -1, tournament([rock], [paper]) ),
        ?assertEqual( -1, tournament([rock,rock,paper,paper], [rock,paper,scissors,rock]) )
].

