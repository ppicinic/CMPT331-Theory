program Hello;

Uses sysutils;

var
    str, res: String;
	
	function encrypt (str : String; shiftAmount : integer) : String;
	var
		temp : String;
		index, ascii, shift : integer;
		c : char;

	begin
		temp := str;
		shift := shiftAmount;
		temp := UpperCase(temp);
		shift := shift mod 26;
		for index := 1 to length(str) do
		begin
			c := temp[index];
			if (c >= 'A') and (c <= 'Z') then
			begin
				c := chr(ord(c) + shift);
				if c > 'Z' then
				begin
					ascii := ord(c);
					ascii := ascii - 91;
					c := chr(ascii + 65);
				end;
			end;
			temp[index] := c;
		end;
		encrypt := temp;
	end;

	function decrypt (str : String; shiftAmount : integer) : String;
	var
		temp : String;
		index, ascii, shift : integer;
		c : char;

	begin
		temp := str;
		shift := shiftAmount;
		temp := UpperCase(temp);
		shift := shift mod 26;
		for index := 1 to length(str) do
		begin
			c := temp[index];
			if (c >= 'A') and (c <= 'Z') then
			begin
				c := chr(ord(c) - shift);
				if c < 'A' then
				begin
					ascii := ord(c);
					ascii := 65 - ascii; 
					c := chr(91 - ascii);
				end;
			end;
			temp[index] := c;
		end;
		decrypt := temp;
	end;

	procedure solve(str : String; maxShiftValue: integer);
	var
		temp, output : String;
		index : integer;
	begin
		for index := maxShiftValue downto 0 do
		begin
			temp := decrypt(str, index);
			output := 'CEASER ' + IntToStr(index) + ': ' + temp;
			writeln(output);
		end;
	end;

begin (* Main *)
	str := 'This is a test from alan testing @!';
	res := encrypt(str, 5);
	writeln(res);
	str := 'Check this code# !out';
	res := encrypt(str, 16);
	writeln(res);
	str := 'Estd tD L EpDE QCzX LWlY EPDEtyr @!';
	res := decrypt(str, 11);
	writeln(res);
	str := 'YdayG pdEO YkzA# !KqP';
	res := decrypt(str, 22);
	writeln(res);
	str := 'MABL bL T MxLM YkHF TETG MxLMBgZ @!';
	solve(str, 26);
end.

