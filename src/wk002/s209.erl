-module(s209).
-export([
                double/1, evens/1,
                median/1, modes/1, nub/1,
                merge_sort/1, reverse/1, parity/1, element_at/2
        ]).

%%------------------------------------------------------------------------------
%%      . double/1 .
%%
%% Naive `map` implementation showing use of recursion (direct).
%%
double([]) -> [];
double([X|Xs]) -> [2*X|double(Xs)].


%%------------------------------------------------------------------------------
%%      . evens/1 .
%%
%% Naive `filter` implementation showing use of recursion (direct) and `case`.
%%
evens([]) -> [];
evens([X|Xs]) ->
        case parity(X) of
                even -> [X | evens(Xs)];
                _    -> evens(Xs)
        end.

%%------------------------------------------------------------------------------
%%      . nub/1 .
%%
%% Returns a sorted-list with no-duplicates.
%%
nub(Xs) -> nub([], modes_sorted([], merge_sort(Xs))).

%%------------------------------------------------------------------------------
%%      . nub/2 .
%%
%% Worker function returns a list with no-duplicates from a list of {number,count}
%% tuples.
%%
nub(L, []) -> L;
nub(L, [{N,_}|Xs]) -> nub([N]++L, Xs).

%%------------------------------------------------------------------------------
%%      . median/1 .
%%
%% Sorts the input list and passes it to `median_sorted`.
%%
median(Xs) -> median_sorted(merge_sort(Xs)).

%%------------------------------------------------------------------------------
%%      . median_sorted/1 .
%%
%% Worker function calculates the `median` value of a sorted-list of numbers.
%%
median_sorted(Xs) ->
        L = length(Xs),                 % Length/Size of list
        P = L div 2,                    % Pivot point, (higher) mid-index
        M = element_at(P, Xs),          % Member at index P
        case parity(L) of
                odd -> M;                               % return Mp
                _   -> (element_at(P-1, Xs) + M) / 2    % Mean M(p) and M(p-1)
        end.

%%------------------------------------------------------------------------------
%%      . parity/1 .
%%
%% Utility function returns the `parity` of an integer (either `odd` or `even`). 
%%
parity(X) ->
        case (abs(X) rem 2) of
                1 -> odd;
                _ -> even
        end.

%%------------------------------------------------------------------------------
%%      . element_at/2 .
%%
%% Utility function returns the list (Es) element at index (N). List is `0` 
%% indexed with index (N) being less than list size.
%%
element_at(0, [X|_Xs]) -> X;
element_at(N, [_X|Xs]=Es) when N < length(Es) -> element_at(N-1, Xs).


%%------------------------------------------------------------------------------
%%      . modes/1 .
%%
%% Return the list of `modes` from a list of numbers. The work is done by worker
%% function which operates on a sorted-list. The sort-comparator function is 
%% passed to merge_sort.
%%
modes(Xs) -> 
	modes(
                0, [], 
                merge_sort(
                        modes_sorted([], reverse(merge_sort(Xs))), 
                        fun({Xn,Xc}, {Yn,Yc}) -> Xc>Yc orelse (Xc==Yc andalso Xn=<Yn) end
                )
        ).

%%------------------------------------------------------------------------------
%%      . modes/3 .
%%
%% Worker function returns the list of `modes` from a list of number,occurances
%% tuples. The tuple list is reverse sorted by occurances - ie highest first.
%%
modes(_Cm, Ms, []) -> Ms;
modes(_Cm, [], [{N,C}|Xs]) -> modes(C, [N], Xs);
modes(Cm, Ms, [{N,C}|Xs]) ->
        case (C == Cm) of 
                true -> modes(Cm, Ms++[N], Xs);
                _    -> Ms
        end.

%%------------------------------------------------------------------------------
%%      . modes_sorted/2 .
%%
%% Worker function returns the list of `modes` from a list of number,occurances
%% tuples. The tuple list is reverse sorted by occurances - ie highest first.
%%
modes_sorted(Ms, []) -> Ms;
modes_sorted([], [X|Xs]) -> modes_sorted([{X,1}], Xs);
modes_sorted([{N,C}|Ms]=M, [X|Xs]) -> 
        case (N == X) of 
                true -> modes_sorted([{N,C+1}] ++ Ms, Xs);
                _    -> modes_sorted([{X,  1}] ++ M , Xs)
        end.

%%------------------------------------------------------------------------------
%%      . reverse/1 .
%%
%% Utility function reverses a list. 
%%
reverse(Xs) -> reverse([], Xs).

%%------------------------------------------------------------------------------
%%      . reverse/2 .
%%
%% Worker function reverses a list into a new list. 
%%
reverse(R, []) -> R;
reverse(R, [X|Xs]) -> reverse([X]++R, Xs).

%%------------------------------------------------------------------------------
%%      . merge_sort/1 .
%%
%% Implementation of `merge` sort algorithm using basic (default) comparator fn.
%%
merge_sort(S) -> merge_sort(S, fun(X,Y) -> X=<Y end).

%%------------------------------------------------------------------------------
%%      . merge_sort/2 .
%%
%% Implementation of `merge` sort algorithm with user-defined comparator fn.
%%
merge_sort([_X,_Y|_Xs] = S, Fn) ->
        {L,R} = split(S),
        merge([], merge_sort(L,Fn), merge_sort(R,Fn), Fn);
merge_sort(Xs, _Fn) -> Xs.

%%------------------------------------------------------------------------------
%%      . split/1 .
%%
%% Utility function splits a list into Left,Right list tuple near or at middle.
%%
split(Xs) -> split([], Xs, (1 + length(Xs)) div 2).

%%------------------------------------------------------------------------------
%%      . split/2 .
%%
%% Utility function splits a list into Left,Right list tuple at user-given index.
%%
split(L, Xs, 0) -> {L, Xs};
split(L, [X|Xs], P) -> split(L++[X], Xs, P-1).

%%------------------------------------------------------------------------------
%%      . merge/4 .
%%
%% Utility function merges two sorted-lists into a single sorted-list. The 
%% comparator function is supplied.
%%
merge(S, [], R, _Fn) -> S++R;
merge(S, L, [], _Fn) -> S++L;
merge(S, [L|Ls]=Left, [R|Rs]=Right, Fn) -> 
        case Fn(L, R) of 
                true -> merge(S++[L], Ls, Right, Fn);
                _    -> merge(S++[R], Left, Rs, Fn)
        end. 

