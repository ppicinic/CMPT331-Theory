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

def solve(str: String, maxShiftValue:Int): Unit = {
	for(i <- maxShiftValue to 0 by -1){
		print("CEASER " + i + ": ");
		println(decrypt(str, i));
	}
}

println(encrypt("xyzabcde", 28));
println(decrypt("xyzabcde", 2));
solve("HAL", 26);