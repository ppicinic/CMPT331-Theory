/** 
*	Caesar Cypher - Scala (Procedural)
*	@author Phil Picinic
*/

/**
*	Encrypt caesar cypher function
*	@param str the string to encrypt
*	@param shiftAmount the amount to shift
*	@return the encrypted string
*/
def encrypt(str: String, shiftAmount: Int): String = {
	var shift = shiftAmount % 26;
	var temp = str.toUpperCase();
	var result = "";
	for(c <- temp){
		var tempc = c;
		if(c >= 'A' && c <= 'Z'){
			tempc = (tempc.toInt + shift).toChar;
			if(tempc > 'Z'){
				var diff = tempc.toInt - 91;
				tempc = (65 + diff).toChar
			}
		}
		result += tempc;
	}
	
	return result;
}

/**
*	Decrypt caesar cypher function
*	@param str the string to decrypt
*	@param shiftAmount the amount to shift
*	@return the decrypted string
*/
def decrypt(str: String, shiftAmount: Int): String = {
	var shift = shiftAmount % 26;
	var temp = str.toUpperCase();
	var result = "";
	for(c <- temp){
		var tempc = c;
		if(c >= 'A' && c <= 'Z'){
			tempc = (tempc.toInt - shift).toChar;
			if(tempc < 'A'){
				var diff = 65 - tempc.toInt;
				tempc = (91 - diff).toChar
			}
		}
		result += tempc;
	}
	
	return result;
}

/**
*	Solve caesar cypher function
*	@param str the string to solve a caesar cypher of
*	@param maxShiftValue the amount of decrypt values to attempt
*/
def solve(str: String, maxShiftValue:Int): Unit = {
	for(i <- maxShiftValue to 0 by -1){
		print("CEASER " + i + ": ");
		println(decrypt(str, i));
	}
}