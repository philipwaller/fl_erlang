-module(s118).
-export([fac/1,fib/1,pieces/1,pieces3d/1,piecesNd/2]).

fac(0) -> 1;
fac(N) when N>0 -> fac(N-1)*N.

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N>1 -> fib(N-1) + fib(N-2).

% fib(4)
%         = fib(3)                   + fib(2)
%         = fib(2)          + fib(1) + fib(1) + fib(0)
%         = fib(1) + fib(0) + 1      + 1      + 0
%         = 1      + 0      + 1      + 1      + 0
%         = 3

% Lazy Caterer's Sequence.
pieces(0) -> 1;
pieces(N) when N>0 -> N + pieces(N-1).

% pieces(4)
%         = 4 + pieces(3)
%         = 4 + 3 + pieces(2)
%         = 4 + 3 + 2 + pieces(1)
%         = 4 + 3 + 2 + 2
%         = 11

% The Cake Number.
pieces3d(0) -> 1;
pieces3d(N) when N>0 -> pieces3d(N-1) + pieces(N-1).

% General pieces problem in N dimensions.
%
% WARNING: assumes that pieces(Dn, N) = pieces(Dn, N-1) + pieces (Dn-1, N-1).
% This works for 3-dimensional space but I'm only guessing that this approach works for N-dimensional space.
% Need to do the math!
piecesNd(_, 0) -> 1; 
piecesNd(2, N) when N>0 -> pieces(N);
piecesNd(D, N) when N>0, D>2 -> piecesNd(D, N-1) + piecesNd(D-1, N-1).
 
