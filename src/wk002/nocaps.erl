-module(nocaps).

-export( [nocaps/1] ).

nocaps([]) -> [];
nocaps([X|Xs]) -> [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
	case $A=<X andalso X=<$Z of
		true -> X+32;
		_    -> X
	end.

