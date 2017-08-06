-module(s204).
-export([sum/1,sumt/1]).

sum([]) -> 0;
sum([X|Xs]) -> X + sum(Xs).

% sum([2,3,4,5])
% 2 + sum([3,4,5])
% 2 + (3 + sum([4,5]))
% 2 + (3 + (4 + sum([5])))
% 2 + (3 + (4 + (5 + sum([]))))
% 2 + (3 + (4 + (5 + (0))))

sumt(L) -> sumt(L,0).

sumt([],A) -> A;
sumt([X|Xs],A) -> sumt(Xs, A+X).

% sumt([3,1,16])
% = sumt([3,1,16], 0)
% = sumt([1,16], 0+3)
% = sumt([16], 3+1)
% = sumt([], 4+16)
% = 20

product().
maximum().
