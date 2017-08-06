-module(s302).
-export([
		sum/1 
	]).

-include_lib("eunit/include/eunit.hrl").

sum([]) -> 0;
sum([X]) -> X;
sum([X0, X1| Xs]) -> sum([X0+X1|Xs]).

map(F, []) -> [];
map(F, [X|Xs]) -> [ F(X) | map(F , Xs) ].

all_areas(Shapes) -> map(fun area/1, Shapes).

filter(P, []) -> [];
filter(P, [X|Xs]) -> 
	case P(X) of
		true -> [X|filter(P,Xs)];
		_    ->    filter(P,Xs)
	end.

circles(Shapes) -> filter(fun is_circle/1, Shapes).

reduce(Combine, Start, []) -> Start;
reduce(Combine, Start, [X|Xs]) -> 
	Combine(X, reduce(Combine, Start, Xs)).

sum(Xs) -> reduce(fun plus/2, 0, Xs).
plus(X,Y) -> X+Y.

sumr(Xs) -> reduce(fun (X,Y) -> X+Y end, 0, Xs).

			
