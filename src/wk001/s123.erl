-module(s123).
-export([fib/1]).

fibP(0) -> {0,1};
fibP(N) ->
        {P,C} = fibP(N-1),
        {C,P+C}

fib(N) ->
        {P,_}  = fibP(N),
        P.

% {circle, {X,Y}, R}
% {rectangle, {X,Y}, H, W}

area({cricle, {X,Y}, R}) -> math:pi()*R*R;
area({rectangle, {X,Y}, H, W}) -> H*W.


