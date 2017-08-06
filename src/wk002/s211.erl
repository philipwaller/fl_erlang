-module(s211).
-export([take/2, takeT/2]).

-include_lib("eunit/include/eunit.hrl").

-spec take(integer(), [T]) -> [T].
take(N, [X|Xs]) when N>0 -> [X|take(N-1, Xs)];
take(_N, _Xs) -> [].

-spec takeT(integer(), [T]) -> [T].
takeT(N, Xs) -> takeT(N, Xs, []).

-spec takeT(integer(), [T], [T]) -> [T].
takeT(N, [X|Xs], A) when 0<N -> takeT(N-1, Xs, A++[X]);
takeT(_N, _Xs, A) -> A.


take_test() -> [
        ?assertEqual([], take(0, "hello")),
        ?assertEqual("h", take(1, "hello")),
        ?assertEqual("he", take(2, "hello")),
        ?assertEqual("hell", take(4, "hello")),
        ?assertEqual("hello", take(5, "hello")),
        ?assertEqual("hello", take(9, "hello"))
].

takeT_test() -> [
        ?assertEqual([], takeT(0, "hello")),
        ?assertEqual("hell", takeT(4, "hello")),
        ?assertEqual("hello", takeT(5, "hello")),
        ?assertEqual("hello", takeT(9, "hello"))
].


