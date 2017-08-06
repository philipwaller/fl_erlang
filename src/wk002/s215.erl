-module(s215).
-export([
                palindrome/1,
                pal/1,
                reverse/1, shunt/2,
                nocap/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%%
%%

-spec palindrome(string()) -> boolean().
palindrome([]) -> true;
palindrome([_H]) -> true;
palindrome([X|T]) ->            
        Xt = not is_letter(X),
        L  = lists:last(T),
        Lt = not is_letter(L),
        if
                Xt -> palindrome(T);
                Lt -> palindrome([X|lists:droplast(T)]);
                true ->
                        case is_match(X, L) of
                                true -> palindrome(lists:droplast(T));
                                _    -> false
                        end
        end.

is_letter(C) -> is_upper(C) orelse is_lower(C).
is_upper(C) -> $A =< C andalso C =< $Z.
is_lower(C) -> $a =< C andalso C =< $z.
is_match(X, Y) -> convert_char(X) =:= convert_char(Y).

convert_char(C) ->
        case is_upper(C) of
                true -> C + 32;
                _    -> C
        end.

palindrome_test() -> [
        ?assert(palindrome([])),
        ?assert(palindrome("radar")),
        ?assert(palindrome("Madam I\'m Adam")),
        ?assertNot(palindrome("abc")),
        ?assertNot(palindrome("abcd"))
].

%% -----------------------------------------------------------------------------
%%      . Simon Solution.
%%

% pal(Xs) -> pal_lit(nocaps(nopunct(Xs))).
pal(Xs) -> pal_lit(tr(Xs)).

pal_lit(Xs) -> Xs == reverse(Xs).

% naive very!
% reverse([]) -> [];
% reverse([X|Xs]) -> reverse(Xs) ++ [X].
reverse(Xs) -> shunt(Xs,[]).

shunt([],Ys) -> Ys;
shunt([X|Xs], Ys) -> shunt(Xs, [X|Ys]).

nocaps([]) -> [];
nocaps([X|Xs]) -> [nocap(X) | nocaps(Xs)].

nocap(X) -> 
        io:format("nocap( ~w )~n", [X]),
        case $A =< X andalso X =< $Z of
                true -> X+32;
                false -> X
        end.

nopunct([]) -> [];
nopunct([X|Xs]) ->
        case lists:member(X, ".,\' ;:\t\n") of
                true -> nopunct(Xs);
                _    -> [X|nopunct(Xs)]
        end.


tr([]) -> [];
tr([X|Xs]) ->
        case lists:member(X, ".,\"\' ;:\t\n") of
                true -> tr(Xs);
                _    -> [nocap(X) | tr(Xs)]
        end.

