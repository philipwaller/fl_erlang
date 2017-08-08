-module(s108).
-export([
		 receiver/0, receiver2/0, receiver3/0
	]).


receiver() ->
	timer:sleep(500),
	receive
		stop -> 
			io:format("~p:Xx:~p!~n", [self(), stop]);
		Message -> 
			io:format("~p:Rx:~p!~n", [self(), Message]),
			receiver()
	end.

receiver2() ->
	timer:sleep(500),
	receive
		Message -> 
			case (Message) of
			stop -> 
				io:format("~p:Xx:~p!~n", [self(), stop]);
			_ ->
				io:format("~p:Rx:~p!~n", [self(), Message]),
				receiver()
			end
	end.

%%
%%  Process messages in order {first,second}.
%%
%	34> Rx = spawn(s108, receiver3, []).          
%	<0.131.0>
%	35> [Rx!{second, goodbye}, Rx!{first, hello}].
%	[{second,goodbye},{first,hello}]
%	<0.131.0>:Rx:hello!
%	<0.131.0>:Rx:goodbye!
%	36> [Rx!{first, hello}, Rx!{second, goodbye}].
%	[{first,hello},{second,goodbye}]
%	<0.131.0>:Rx:hello!
%	<0.131.0>:Rx:goodbye!
%
receiver3() ->
	timer:sleep(1000),
	receive
		{first, String1} -> 
			io:format("~p:Rx:~s!~n", [self(), String1])
	end,
	receive
		{second, String2} ->
			io:format("~p:Rx:~s!~n", [self(), String2])
	end,
	receiver3().

