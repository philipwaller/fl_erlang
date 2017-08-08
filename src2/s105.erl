-module(s105).
-export([
                server/1, 
                client/1, server/0,
                manager/0
	]).


%%------------------------------------------------------------------------------
%%
%%      . Palindrome Test .
%%
%%------------------------------------------------------------------------------

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

%%------------------------------------------------------------------------------
%%
%%      . Simple Server Model .
%%
%%------------------------------------------------------------------------------

server(Pid) ->
	receive
		{check, String} ->
			IsPal = case (is_pal(String)) of
				true -> " ";
				_    -> " NOT "
			end,
			Pid ! {
			  	result, 
				"\"" ++ String ++ "\" is" ++ IsPal ++ "a palindrome."
			 },
			server(Pid);
		Msg -> io:format("ServerStopped! [~p]!~n", [Msg])
	end.

%%------------------------------------------------------------------------------
%%
%%      . Client Server Model .
%%
%% Interpreter Calls launch 1 server + multiple (5) clients:
%%
%%      > c(s105).                                                                                                 
%%      > S = spawn(s105,server,[]).                                    
%%      > lists:map(fun(_)->spawn(s105,client,[S]) end, lists:seq(1,5)).
%%
%%------------------------------------------------------------------------------

server() ->
        receive
                {Pid, check, String} = Request ->
                        io:format("~p:Ps:server: [~p]!~n", [self(), Request]),
                        case(is_pal(String)) of 
                                true -> Pid ! { is_palindrome, String };
                                _    -> Pid ! { not_palindrome, String }
                        end,
                        server();
		Msg -> 
                        io:format("~p:XX:server: [~p]!~n", [self(),Msg]),
                        {ok}
        end.

%
% . Client .
%
% - Generates 10 strings to test.
% - Strings are random sublists of "ababcbaba".
%
client(Pid) -> client(Pid, 10).

client( _, 0) -> 
        %io:format("~p:XX:client!~n", [self()]),
        {ok};
client(Pid, N) when N > 0 ->
        timer:sleep(rand:uniform(5)*500),
%        io:format( "~p:Tx:client: [~p]!~n", [
%                self(), 
%                Pid ! {self(), check, lists:sublist("ababcbaba", rand:uniform(9))}
%        ]),
                Pid ! {self(), check, lists:sublist("ababcbaba", rand:uniform(9))},
        receive
                _Msg -> 
                        %io:format("~p:Rx:client: [~p]!~n", [self(), Msg]),
                        ok
        end,
        client(Pid, N-1). 


%%------------------------------------------------------------------------------
%%
%%      . Server Manager .
%%
%%------------------------------------------------------------------------------

manager() -> manager(3).

manager(N) when N>0 ->
        manager(
                lists:map( fun(_) -> spawn(s105, server, []) end, lists:seq(1,N) ),
                0
        ).

manager(Xs, N) when is_list(Xs), N>=0, N<length(Xs) -> 
        receive
                stop ->
                        lists:map( fun(Pid) -> Pid ! stop end, Xs );

                Request = {_, check, _} -> 
                        % io:format("~p:Ps:contrl: [~p]!~n", [self(), Request]),
                        lists:nth(N+1, Xs) ! Request,
                        manager(Xs, (N+1) rem length(Xs));
                _Msg ->
                        % io:format("~p:No:contrl: [~p]!~n", [self(), Msg]),
                        manager(Xs, N)
        end.


