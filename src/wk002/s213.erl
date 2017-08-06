-module(s213).
-export([nub/1]).

-include_lib("eunit/include/eunit.hrl").

-spec nub([T]) -> [T].
nub(L) -> nub(L, []).

-spec nub([T], [T]) -> [T].
nub([H|T], A) -> 
        case lists:member(H, T) of
                true -> nub(T, A);
                _    -> nub(T, [H|A])
        end;
nub(_, A) -> lists:reverse(A, []).

-spec nubF([T]) -> [T].
nubF(L) -> nubF(L, []).

-spec nubF([T], [T]) -> [T].
nubF([H|T], A) ->
        case lists:member(H, A) of 
                true -> nubF(T, A);
                _    -> nubF(T, [H|A])
        end;
nubF(_, A) -> lists:reverse(A, []).

-spec nubD([T]) -> [T].
nubD([H|T]) ->
        case lists:member(H, T) of
                true -> nubD(T);
                _    -> [H | nubD(T)]
        end;
nubD(_) -> [].

-spec nubFD([T]) -> [T].
nubFD(L) -> lists:reverse(nubD(lists:reverse(L, [])), []).

nub_test() -> [
        ?assertEqual([], nub([])),

        ?assertEqual([2,4,1,3], nubF([2,4,1,3,3,1])),
        ?assertEqual([2,4,1,3], nubFD([2,4,1,3,3,1])),

        ?assertEqual([2,4,3,1], nub([2,4,1,3,3,1])),
        ?assertEqual([2,4,3,1], nubD([2,4,1,3,3,1]))
].

