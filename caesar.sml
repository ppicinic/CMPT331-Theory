(* Caesar Cypher - Phil Picinic *)
fun mymap(f, []) = []
	| mymap(f, x::xs) = (f x) :: mymap(f, xs);

fun myupper(str) =
	implode(mymap(Char.toUpper, explode(str)));

fun encrypt(str, shiftAmount) = 
	implode( encryptChar(explode(myupper(str)), shiftAmount ) )
and
encryptChar([], shiftAmount) = []
	| encryptChar(c::xs, shiftAmount) =
		(if c >= #"A" then if c <= #"Z" then fixEncrypt(chr(ord(c) + shiftAmount)) else c else c) :: encryptChar(xs, shiftAmount)
and
fixEncrypt(c) = 
	if c > #"Z" then chr( ord(c) - 91 + 65 ) else c;

fun decrypt(str, shiftAmount) = 
	implode( decryptChar(explode(myupper(str)), shiftAmount ) )
and
decryptChar([], shiftAmount) = []
	| decryptChar(c::xs, shiftAmount) =
		(if c >= #"A" then if c <= #"Z" then fixDecrypt(chr(ord(c) - shiftAmount)) else c else c) :: decryptChar(xs, shiftAmount)
and
fixDecrypt(c) = 
	if c < #"A" then chr( 91 - (65 - ord(c)) ) else c;

fun solve(str, 0) = concat("CAESER 0: " :: decrypt(str, 0) :: [])
	| solve(str, maxShiftValue) = concat("CAESER " :: Int.toString(maxShiftValue) :: ": " :: decrypt(str, maxShiftValue) :: "\n" :: solve(str, (maxShiftValue - 1)) :: []) 