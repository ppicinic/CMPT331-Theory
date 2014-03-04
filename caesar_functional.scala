def fixEncrypt(c: Char): Char =
	if(c > 'Z') 'A' else c

def encryptChar(c: Char): Char = 
	if (c >= 'A' && c <= 'Z') fixEncrypt((c.toInt + 1).toChar) else c

def encryptOnce(str: String): String = 
	str.map(x => encryptChar(x))

def encryptHelp(str: String, shiftAmount: Int): String = 
	if (shiftAmount == 0) str else encryptHelp( encryptOnce(str), (shiftAmount - 1))

def encrypt(str:String, shiftAmount: Int): String =
	encryptHelp(str.toUpperCase(), shiftAmount)

def fixDecrypt(c: Char): Char =
	if(c < 'A') 'Z' else c

def decryptChar(c: Char): Char = 
	if (c >= 'A' && c <= 'Z') fixDecrypt((c.toInt - 1).toChar) else c

def decryptOnce(str: String): String = 
	str.map(x => decryptChar(x))

def decryptHelp(str: String, shiftAmount: Int): String = 
	if (shiftAmount == 0) str else decryptHelp( decryptOnce(str), (shiftAmount - 1))

def decrypt(str:String, shiftAmount: Int): String =
	decryptHelp(str.toUpperCase(), shiftAmount)

def solve(str: String, maxShiftValue:Int): String = 
	if(maxShiftValue == 0) "CEASER " + 0 + ": " + str else "CEASER " + maxShiftValue + ": " + decrypt(str, maxShiftValue) + "\n" + solve(str, maxShiftValue - 1)

/*println(encrypt("xyzabcde", 28));
println(decrypt("xyzabcde", 2));
println(solve("HAL", 26));*/