%%
%%
%%

-module(ass01).
-export([perimeter/1,area/1,enclose/1]).

%
%
%
area({circle, _P, R}) ->
        math:pi() * R * R;
area({rectangle, _P, H, W}) ->
        H * W;
area({triangle, {XA,YA}, {XB,YB}, {XC,YC}}) ->
        ( (XB-XA)*(YC-YA) - (XC-XA)*(YB-YA) ) / 2.

perimeter({circle, _P, R}) ->
        2 * math:pi() * R;
perimeter({rectangle, _P, H, W}) ->
        2 * (H + W);
perimeter({triangle, PA, PB, PC}) ->
        hypot(PA,PB) + hypot(PB,PC) + hypot(PC,PA).

enclose({circle, P, R}) ->
        {rectangle, P, 2*R, 2*R};
enclose({rectangle, P, W, H}) ->
        {rectangle, P, W, H};
enclose({triangle, {XA,YA}, {XB,YB}, {XC,YC}}) ->
        error.

hypot({XA,YA}, {XB,YB}) ->
        math:sqrt(math:pow(XA-XB,2) + math:pow(YA-YB,2)).



