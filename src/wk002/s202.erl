-module(s202).
-export([head/1,tail/1]).

head([X|_Xs])->X.
tail([_X|Xs])->Xs.

