%%
%% Functional Programming in Erlang
%% The University of Kent
%%
%% Week 1, Step 9 [1.9]: My First Erlang Program
%%
%% Note: module might better be called 'right_angle_triangle'.
%%
-module(second).
-export([hypot/2,perim/2,area/2]).

% Calculate hypotenuse of a right-angled triangle.
% Inputs: A,B - lengths of two short sides.
hypot(A,B) ->
        math:sqrt(first:square(A) + first:square(B)).

% Calculate the perimeter of a righ-angled triangle.
% Inputs: A,B - lengths of two short sides.
perim(A,B) ->
        A + B + hypot(A,B).

% Calculate the area of a righ-angled triangle.
% Inputs: A,B - lengths of two short sides.
area(A,B) ->
        first:area(A,B, hypot(A,B)).

