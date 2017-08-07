-module(s105).
-export([
	 server/1
	]).


to_lower(String) -> lists:map(
	fun(C) ->
		case($A =< C andalso C=<$Z) of 
			true -> C+32;
			_    -> C
		end
	end,
	String).

clean_punct(String) -> lists:filter(
	fun(C) -> $a=<C andalso C=<$z end, 
	String	
).

is_pal(String) ->
	Norm = clean_punct(to_lower(String)),
	lists:reverse(Norm) == Norm.

server(Pid) ->
	receive
		{check, String} ->
			IsPal = case (is_pal(String)) of
				true -> " ";
				_    -> " NOT "
			end,
			Pid ! {
			  	result, 
				io_lib:format("\"~s\" is~sa palindrome.", [String,IsPal])
			 },
			server(Pid);
		Msg -> io:format("ServerStopped! [~s]!~n", [Msg])
	end.


