%%
%% Future Learn
%% Functional Programming in Erlang
%% The University of Kent
%%
%% Week 1, Assignment
%% Philip Waller (philip.waller@gmail.com)
%%

-module(ass01).
-export([
	 perimeter/1,area/1,enclose/1,
	 bits/1,bits_tail/1,
         test_shapes/0, test_bits/0
	]).

%
% 	. Shapes .
%
% Functions that are defined for the following shapes:
%
% circle - is defined by a center point, and radius.
% 	{circle, {X,Y}, R}
% rectangle - is defined by a center point, witdth, and height.
% 	{rectangle, {X,Y}, W, H}
% 	Rectangles are parallel to the x-axis and y-axis.
% triangle - is defined by 3 verticies.
% 	{triangle, {X1,Y1}, {X2,Y2}, {X3, Y3}}
%

% Calculate the area of a shape.
area({circle, _, R}) ->
        math:pi() * R * R;

area({rectangle, _, H, W}) ->
        H * W;

area({triangle, {XA,YA}, {XB,YB}, {XC,YC}}) ->
        ( (XB-XA)*(YC-YA) - (XC-XA)*(YB-YA) ) / 2.  % simple and efficient algorithm


% Calculate the perimeter of a shape.
perimeter({circle, _, R}) ->
        2 * math:pi() * R;

perimeter({rectangle, _, H, W}) ->
        2 * (H + W);

perimeter({triangle, PA, PB, PC}) ->
        hypot(PA,PB) + hypot(PB,PC) + hypot(PC,PA).


% Calculate bounding-box of a shape.
enclose({circle, P, R}) ->
        {rectangle, P, 2*R, 2*R};

enclose({rectangle, P, W, H}) ->
        {rectangle, P, W, H};

enclose({triangle, {XA,YA}, {XB,YB}, {XC,YC}}) ->
	Xmin = min(XA, min(XB,XC)),
	Ymin = min(YA, min(YB,YC)),
	Xmax = max(XA, max(XB,XC)),
	Ymax = max(YA, max(YB,YC)),
	W = Xmax - Xmin,
	H = Ymax - Ymin,
	{rectangle, {Xmin + W/2, Ymin + H/2}, W, H}.


%
% 	. Summing the bits .
%
% Both methods continuously divide the number N by 2 and summing any
% remainders.
% The 'direct' recursion method is simpler to understand and code.
% The 'tail' recursion method is more memory efficient.
% The 'direct' method is probably preferable in this case due to its
% elegance, since for large N tested performance didn't appear to be an
% issue.
%

% Direct Recursion Algorithm.
bits(0) -> 0;
bits(N) when N>0 ->
	{Q,R} = int_div_2(N),
	R + bits(Q).


% Tail Recursion Algorithm.
bits_tail(0) -> 0;
bits_tail(N) when N>0 -> bits_tail(N, 0).

bits_tail(0, A) -> A;
bits_tail(N, A) ->
	{Q,R} = int_div_2(N),
	bits_tail(Q, A+R).


%
% 	. Utility functions .
%

% Calc the distance between 2 points in the X-Y plane.
hypot({XA,YA}, {XB,YB}) ->
        math:sqrt(math:pow(XA-XB,2) + math:pow(YA-YB,2)).

% Calc the quotient and remainder when divinding N by D (general case).
int_div(N,D) when D=/=0 ->
	{N div D, N rem D}.

% Calc the quotient and remainder when dividing N by 2 (special case).
int_div_2(N) -> int_div(N,2).


%
% 	. Test Shapes .
%
% Testing not exhaustive; for example no edge cases tested for.
%

test_shapes() ->
	C = {circle, {2,2}, 2},
	R = {rectangle, {-3,-3}, 4, 3},
	T = {triangle, {2,2}, {-3,-3}, {3,0}},

        PI4 = 4 * math:pi(),
        Tp = 16.015339721864635,        % triangle perimeter

        {PI4, PI4, {rectangle, {2,2}, 4, 4}} = {area(C), perimeter(C), enclose(C)},
        {12, 14, R} = {area(R), perimeter(R), enclose(R)},
        {7.5, Tp, {rectangle, {0.0, -0.5}, 6, 5}} = {area(T), perimeter(T), enclose(T)},

        success.

%
%       . Test Bit Sums .
%

test_bits() ->
        {1,10,1,0} = {bits(1024), bits(1023), bits(1), bits(0)},
        helper_test_bits(1024). 
        
% compare different methods
helper_test_bits(0) -> success;
helper_test_bits(N) ->
        Bd = bits(N),
        Bt = bits_tail(N),

        Bd = Bt,

        helper_test_bits(N-1).

