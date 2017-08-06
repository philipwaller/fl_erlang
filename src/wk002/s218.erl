-module(s218).
-export([
		join/2, concat/1, member/2,
		merge_sort/1, quick_sort/1, insertion_sort/1,
		perms/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%%
%%

-spec join( [T], [T] ) -> [T].
join([], Ys) -> Ys;
join([X|Xs], Ys) -> [X | join(Xs, Ys)].

-spec concat( [[T]] ) -> [T].
concat([]) -> [];
concat([X|Xs]) -> join(X, concat(Xs)).

-spec member( T, [T] ) -> boolean().
member(_, []) -> false;
member(X, [X|_]) -> true;
member(X, [_|Xs]) -> member(X, Xs).


-spec merge_sort( [T] ) -> [T].
merge_sort([]) -> [];
merge_sort([X]) -> [X];
merge_sort(Xs) ->
	{L, R} = split(Xs),
	merge( merge_sort(L), merge_sort(R) ).

split(Xs) -> split(Xs, [], length(Xs) div 2).

split(Xs, Ys, 0) -> {lists:reverse(Ys), Xs};
split([X|Xs], Ys, N) when N>0 -> split( Xs, [X|Ys], N-1 ).

merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X|Xs], [Y|Ys]) ->
	case X =< Y of
		true -> [ X | merge(Xs, [Y|Ys]) ];
		_    -> [ Y | merge([X|Xs], Ys) ]
	end.

-spec quick_sort( [T] ) -> [T].
quick_sort([]) -> [];
quick_sort([X|Xs]) -> 
        {L,R} = partition(X, Xs),
        join( quick_sort(L), [X | quick_sort(R)] ).

-spec partition( T, [T] ) -> { [T], [T] }.
partition(P, Xs) -> partition(P, Xs, [], []).

-spec partition( T, [T], [T], [T] ) -> { [T], [T] }.
partition(_P, [], L, R) -> {L, R};
partition(P, [X|Xs], L, R) -> 
        case X =< P of 
                true -> partition(P, Xs, [X|L], R);
                _    -> partition(P, Xs, L, [X|R])
        end.


-spec insertion_sort( [T] ) -> [T].
insertion_sort([]) -> [];
insertion_sort([X|Xs]) -> insert(X, insertion_sort(Xs)).

insert(X, []) -> [X];
insert(X, [S|Ss]) ->
        case X =< S of
                true -> [X,S|[]];
                _    -> [S|insert(X,Ss)]
        end.

-spec perms( [T] ) -> [ [T] ].
perms(Xs) -> perms([], Xs, Xs). 

perms(P, _, []) -> [P];         % leaf
perms(_P, [], _S) -> [];        % END level processing
perms(P, [X|Xs], S) ->
        perms(P++[X], S--[X], S--[X]) ++ perms(P, Xs, S).


        

%% -----------------------------------------------------------------------------
%%      . tests .
%%

join_test() -> [
	?assertEqual("hello", join("hell", "o"))
].

concat_test() -> [
	?assertEqual("goodbye", concat(["goo","d","","by","e"]))
].

member_test() -> [
	?assert(member(2, [2,0,0,1])),	
	?assertNot(member(20, [2,0,0,1]))
].

merge_sort_test() -> [
	?assertEqual(lists:seq(0,10), merge_sort(lists:seq(10,0,-1))),
	?assertEqual(lists:seq(-9,0,1), merge_sort(lists:seq(0,-9,-1)))
].

quick_sort_test() -> [
        ?assertEqual([], quick_sort([])),
        ?assertEqual([7], quick_sort([7])),
        ?assertEqual([5,7], quick_sort([5,7])),
        ?assertEqual([5,7], quick_sort([7,5])),
	?assertEqual(lists:seq(0,10), quick_sort(lists:seq(10,0,-1))),
	?assertEqual(lists:seq(-9,0,1), quick_sort(lists:seq(0,-9,-1)))
].

insertion_sort_test() -> [
        ?assertEqual([], insertion_sort([])),
        ?assertEqual([7], insertion_sort([7])),
        ?assertEqual([5,7], insertion_sort([5,7])),
        ?assertEqual([5,7], insertion_sort([7,5])),
	?assertEqual(lists:seq(0,10), insertion_sort(lists:seq(10,0,-1))),
	?assertEqual(lists:seq(-9,0,1), insertion_sort(lists:seq(0,-9,-1)))
].

perms_test() -> [
	?assertEqual( [[]], perms([]) ),
	% ?assertEqual( [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]], perms([1,2,3]) ) - different order!
	?assertEqual( 
                [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]], 
                perms([1,2,3]) 
        )
].

perms_3_test() -> [
        ?assertEqual(["ab", "ba"], perms("", "ab", "ab")),
        ?assertEqual(
                ["abc", "acb", "bac", "bca", "cab", "cba"], 
                perms("", "abc", "abc")
        )
].
        
