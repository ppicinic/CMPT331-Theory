% Caesar Cypher - Phil Picinic
-module(caeser).
-export([encrypt/2,decrypt/2,solve/2]).

encrypt(X, Y) ->
	encrypthelper(string:to_upper(X), Y).

encrypthelper([], _) ->
	[];
encrypthelper([HEAD|REST], Y) ->
	if
		(HEAD > 64) and (HEAD < 91) -> 
			[encryptOnce(HEAD, Y)] ++ encrypthelper(REST, Y);
		true -> [HEAD] ++ encrypthelper(REST, Y)
	end.

encryptOnce(X, 0) ->
	X;
encryptOnce(X, Y) ->
	encryptOnce(fixEncrypt((X + 1)), (Y - 1)).

fixEncrypt(91) ->
	65;
fixEncrypt(X)->
	X.

decrypt(X, Y) ->
	decrypthelper(string:to_upper(X), Y).

decrypthelper([], _) ->
	[];
decrypthelper([HEAD|REST], Y) ->
	if
		(HEAD > 64) and (HEAD < 91) -> 
			[decryptOnce(HEAD, Y)] ++ decrypthelper(REST, Y);
		true -> [HEAD] ++ decrypthelper(REST, Y)
	end.

decryptOnce(X, 0) ->
	X;
decryptOnce(X, Y) ->
	decryptOnce(fixDecrypt((X - 1)), (Y - 1)).

fixDecrypt(64) ->
	90;
fixDecrypt(X)->
	X.

solve(X, 0) ->
	"CAESER 0: " ++ decrypt(X, 0);
solve(X, Y) ->
	"CAESER " ++ integer_to_list(Y) ++ ": " ++ decrypt(X, Y) ++ "\n" ++ solve(X, (Y-1)).