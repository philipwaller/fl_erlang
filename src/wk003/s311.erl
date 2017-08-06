-module(s311).
-export([
        add/1,times/1,compose/2,id/1,iterate/1
]).

-include_lib("eunit/include/eunit.hrl").

add(X) -> fun(Y) -> X+Y end.
times(X) -> fun(Y) -> X*Y end.

%%------------------------------------------------------------------------------
%% . compose/2 .
%% . compose/1 .
%%
%% TRUE: compose(F,G) equivalent compose( [F,G] ).

compose(F,G) -> fun(X) -> G(F(X)) end.

compose(Fns) -> lists:foldr(fun compose/2, fun id/1, Fns ).

id(X) -> X.

%%------------------------------------------------------------------------------
%% . twice/1 .
%%

twice(F) -> compose(F,F).

twice_test() -> 
        Times3 = times(3),
        TwiceTimes3 = twice(Times3),

        % Named for a comment at:
        % https://english.stackexchange.com/questions/6798/
        % is-there-a-word-for-four-times-as-much-analogous-to-once-twice-and-thrice
        Frice = twice(fun twice/1),

        [
                %       . Test: Readability .
                ?assertEqual( 18, (twice(times(3)))(2) ),
                ?assertEqual( (twice(Times3))(2), TwiceTimes3(2) ),

                %
                ?assertEqual( 3*3*3*3*(2), (Frice(Times3))(2) )
        ].
        

%%------------------------------------------------------------------------------
%% . iterate/1 .
%%

% Simon Solution:
siterate(0) -> fun(_F) -> fun id/1 end.
siterate(N) -> fun(F) -> compose(F, (iterate(N-1))(F)) end.

% todo: use map,  foldr, compose solution?



iterate(0) -> fun id/1;
iterate(N) ->
        fun(Fn) -> compose( lists:duplicate(N, Fn) ) end.

iterate2(N) -> fun (Fn) -> iterate2(N, Fn) end.
iterate2(0, Fn) -> Fn;
iterate2(N, Fn) -> compose(Fn, iterate2(N-1, Fn) ).


iterate_test() ->
        Times2 = times(2),
        Iterate10 = iterate(10),
        Times2_Iterate10 = Iterate10(Times2),
        [
                ?assertEqual( 1024, Times2_Iterate10(1) ),
                ?assertEqual( 2048, ((iterate(11))(times(2)))(1) ),

                ?assertEqual( 2, ((iterate(0))(times(2)))(1) ),

                % Different definition of N:
                % - 1st: N times
                % - 2nd: range 0..N inclusive (ie extra term).
                ?assertEqual( Times2_Iterate10(1024), ((iterate2(9))(Times2))(1024) )
        ].
      

%%------------------------------------------------------------------------------
%% . Tests .
%%
     
fun_test() ->
        Add = fun (X,Y) -> X+Y end,
        Sum = fun (Xs) -> lists:foldr(Add, 0, Xs) end,
        EmptyTest = fun ([]) -> true; ([_|_]) -> false end,

        Foo = fun Product([]) -> 1; Product([X|Xs]) -> X*Product(Xs) end,

        [
                ?assertEqual( 3, Add(1,2) ),
                ?assertEqual( 74, Sum([3,4,67]) ),
                ?assertNot( EmptyTest([3,4,67]) ),
                ?assert( EmptyTest([]) ),

                ?assertEqual( 0, Foo([0,1,2]) ),
                ?assertEqual( 20, Foo([10,1,2]) )
        ].

compose_test() ->
        Add3 = add(3), 
        Add5 = add(5),
        Add7 = add(7),
        
        Times0 = times(0),
        Times7 = times(7),

        Add3_and_Times0 = compose(Times0, Add3),
        Times0_and_Add3 = compose(Add3, Times0),

        Add_3_5_7 = compose([Add3,Add5,Add7]),
        Add_3_7_5 = compose([Add3,Add7,Add5]),
        Add_5_3_7 = compose([Add5,Add3,Add7]),
        Add_5_7_3 = compose([Add5,Add7,Add3]),
        Add_7_3_5 = compose([Add7,Add3,Add5]),
        Add_7_5_3 = compose([Add7,Add5,Add3]),

        % Not necessary to assign a variable.
        Fns = compose([times(5), times(4), times(3), times(2), times(1)]),

        % Recursive fun - for 'fun' and testing.
        Bar = fun Fact(1) -> 1; Fact(N) -> N * Fact(N-1) end,

        [
                %       Test: non-commutative
                ?assertNotEqual( Add3_and_Times0(17), Times0_and_Add3(17) ),
                ?assertEqual( 3, Add3_and_Times0(17) ),
                ?assertEqual( 0, Times0_and_Add3(17) ),

                %       . Test: fun list compose .
                ?assertEqual( 15, Add_3_5_7(0) ), 

                %       . Test: commutative .
                ?assertEqual( Add_3_5_7(2), Add_3_7_5(2) ), 
                ?assertEqual( Add_5_3_7(4), Add_5_7_3(4) ), 
                ?assertEqual( Add_7_3_5(6), Add_7_5_3(6) ),

                %       . Test: differnt factorial(5) .
                ?assertEqual( Bar(5), Fns(1) ),

                %       . Show: create and call in same stateemnt.
                ?assertEqual( Times7(3), (times(7))(3) )
        ].


