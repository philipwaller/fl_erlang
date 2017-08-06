-module(s121).
-export([fib/1,perfect/1]).

%
% Efficient Fibonacci using tail-recursion.
%
fib(0) -> 0;                            % deal with 0-case
fib(N) when N>0  -> fib(N, 1, 0).       % use tail recursion function

fib(1, P1,  _) -> P1;
fib(N, P1, P0) -> fib(N-1, P1+P0, P1).

%
% step-by-step 4th Fibonacci term.
%
% fib(4)        N P1 P0
%         = fib(4, 1, 0)
%         = fib(3, 1, 1)
%         = fib(2, 2, 1)
%         = fib(1, 3, 2)
%         = 3

%
% Is N a Perfect Number?
%
perfect(0) -> true;
perfect(1) -> false;                                    % Not required but highlights case.
perfect(N) when N>0 -> N == sum_divs(N, N-1, 0).        % Every number is divisible by self (discount).

% Sum Divisors
% param: N number to test
% param: D current divisor (loop counter)
% param: A accumulator
sum_divs(_, 0, A) -> 
        A;
sum_divs(N, D, A) when (0 == (N rem D)) ->
        sum_divs(N, D-1, A+D);
sum_divs(N, D, A) -> 
        sum_divs(N, D-1, A).

%
% Step-by-Step: N=6
%
% perfect(6)          N  D  A
%         = sum_divs( 6, 5, 0)
%         = sum_divs( 6, 4, 0)
%         = sum_divs( 6, 3, 0)
%         = sum_divs( 6, 2, 3)
%         = sum_divs( 6, 1, 5)
%         = sum_divs( 6, 0, 6)
%         = true
