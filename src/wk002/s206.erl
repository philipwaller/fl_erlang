-module(s206).
-export([
                product/1,productt/1,prodT/1,
                maximum/1,maximumt/1
        ]).

%       . product .
% Product of list elements.
% Note: I have taken the approach that the product of an empty list is `undefined`
% rather than 1. The case for `1` is that if you use an empty list to terminate
% your recursion then you do NOT want to multiply the result by `0`. I have 
% instead terminated the recursion on a list with one element.

product([X]) -> X;
product([X|Xs]) -> X * product(Xs).

productt([X|Xs]) -> productt(Xs, X).
productt([], A) -> A;
productt([X|Xs], A) -> productt(Xs, X*A).

% inspired by Andre Burgaud `maximumT` solution (see below).
prodT([X]) -> X;
prodT([Xc,X|Xs]) -> prodT([Xc*X|Xs]).

%       . maximum .

maximum([X]) -> X;
maximum([X|Xs]) -> max(X,maximum(Xs)).

maximumt([X|Xs]) -> maximumt(Xs,X).
maximumt([], A) -> A;
maximumt([X|Xs], A) -> maximumt(Xs,max(X,A)).

% Andre Burgaud solution for `maximumT`.
maxT([X]) -> X;
maxT([Xc,X|Xs]) -> maxT([max(Xc,X)|Xs]).

