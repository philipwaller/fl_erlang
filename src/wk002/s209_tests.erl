-module(s209_tests).

-include_lib("eunit/include/eunit.hrl").

double_test() -> [
        ?assertEqual([], s209:double([])),
        ?assertEqual(lists:seq(2,20,2), s209:double(lists:seq(1,10))),
        ?assertEqual(lists:seq(0,-20,-2), s209:double(lists:seq(0,-10,-1)))
].

evens_test() -> [
	?assertEqual([], s209:evens([])),
	?assertEqual(lists:seq(0,100,2), s209:evens(lists:seq(0,100,1))),
	?assertEqual(lists:seq(0,-100,-2), s209:evens(lists:seq(0,-100,-1)))
].

nub_test() -> [
	?assertEqual([], s209:nub([])),
	?assertEqual(lists:seq(0,9), s209:nub([6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,6,7,8,9]))
].

median_test() -> [
	?assertException(error, function_clause, s209:median([])),
	?assertEqual(5, s209:median([5])),
	?assertEqual(4.5, s209:median([5,4,6,9,0,3,1,8,2,7])),
	?assertEqual(5, s209:median([7,6,5,4,3]))
].

modes_test() -> [
 	?assertEqual([], s209:modes([])),
 	?assertEqual([5], s209:modes([5,4,6,7,9,5])),
	?assertEqual([2,7], s209:modes([7,5,4,6,7,9,2,2])),
	?assertEqual([7], s209:modes([7,7,7,7,7,7,7,7]))
].

merge_sort_test() -> [
	?assertEqual([], s209:merge_sort([])),
	?assertEqual([7], s209:merge_sort([7])),
	?assertEqual([5,6,7], s209:merge_sort([7,6,5])),
	?assertEqual(lists:seq(0,99,3), s209:merge_sort(lists:seq(99,0,-3)))
].

reverse_test() -> [
	?assertEqual([], s209:reverse([])),
	?assertEqual([7], s209:reverse([7])),
	?assertEqual(lists:seq(10,1,-1), s209:reverse(lists:seq(1,10)))
].

parity_test() -> [
	?assertEqual(even, s209:parity(-2)),
	?assertEqual(odd, s209:parity(-1)),
	?assertEqual(even, s209:parity(0)),
	?assertEqual(odd, s209:parity(1)),
	?assertEqual(even, s209:parity(2))
].

element_at_test() -> [
	?assertEqual(0, s209:element_at(0, lists:seq(0,10))),
	?assertEqual(10, s209:element_at(10, lists:seq(0,10))),
	?assertException(error, function_clause, s209:element_at(11, lists:seq(0,10)))
].

