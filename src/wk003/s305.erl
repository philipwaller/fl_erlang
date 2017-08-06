-module(s305).
-export([
		doubleAll/1, evens/1, product/1
	]).

-include_lib("eunit/include/eunit.hrl").


doubleAll(Xs) -> lists:map(fun(X) -> 2*X end, Xs).
evens(Xs) -> lists:filter(fun(X) -> 0 =:= X rem 2 end, Xs).
product(Xs) -> lists:foldr(fun(X,Y) -> X*Y end, 1, Xs).

hof_test() -> [
	?assertEqual( lists:seq(0,200,2), doubleAll(lists:seq(0,100)) ),
	?assertEqual( lists:seq(0,100,2), evens(lists:seq(0,100)) ),
	?assertEqual( 3628800, product(lists:seq(1,10)) )
].


zip([X|Xs],[Y|Ys]) -> [ {X,Y} | zip(Xs,Ys) ];
zip(_, _) -> [].

zip_with(Fn, [X|Xs],[Y|Ys]) -> [ Fn(X,Y) | zip_with(Fn, Xs,Ys) ];
zip_with(_, _, _) -> [].

zip_with_1(Fn, Xs, Ys) -> lists:map(fun({X,Y}) -> Fn(X,Y)  end, zip(Xs,Ys)).

zip_1(Xs, Ys) -> zip_with_1( fun(X,Y) -> {X,Y} end, Xs, Ys).

zip_test() -> [
	?assertEqual( [{0,0},{1,10},{2,20},{3,30}], zip([0,1,2,3],[0,10,20,30]) ),
	?assertEqual( [{0,0},{1,10},{2,20},{3,30}], zip([0,1,2,3,4],[0,10,20,30]) ),
	?assertEqual( [{0,0},{1,10},{2,20},{3,30}], zip([0,1,2,3],[0,10,20,30,40]) ),

	?assertEqual( [{0,0},{1,10},{2,20},{3,30}], zip_1([0,1,2,3],[0,10,20,30]) ),
	?assertEqual( [{0,0},{1,10},{2,20},{3,30}], zip_1([0,1,2,3,4],[0,10,20,30]) ),
	?assertEqual( [{0,0},{1,10},{2,20},{3,30}], zip_1([0,1,2,3],[0,10,20,30,40]) )
].
	
zip3_test() -> [
	?assertEqual( [3,7], zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) ),
	?assertEqual( [3,7], zip_with_1(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) )
].

%
% Solution andreburgaud
%

zip_withHH(F,Xs,Ys) -> lists:map(F, zip(Xs,Ys)).
zipHH(Xs,Ys) -> zip_withHH(fun(X) -> X end, Xs, Ys).

ab_test() -> [
	?assertEqual( [3,7], zip_withHH(fun({X,Y}) -> X+Y end, [1,3,5,7], [2,4]) ),
	?assertEqual([{1,2},{3,4}], zipHH([1,3,5,7],[2,4]))
].

%
% Solution: Simon
%

szip_with(_F, _, [] ) -> [];
szip_with(_F, [], _ ) -> [];
szip_with(F, [X|Xs], [Y|Ys] ) -> [ F(X,Y) | szip_with(F, Xs, Ys) ].


st_test() -> [
	?assertEqual( [1,5,4,4], szip_with(fun(X,Y) -> max(X,Y) end, [1,2,3,4], [0,5,4,3]) ),
	?assertEqual( [1,5,4,4], szip_with(fun erlang:max/2, [1,2,3,4], [0,5,4,3]) )
].

