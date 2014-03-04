IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 mystring PIC X(100) VALUE "test! test".
01 shift    PIC 9(8)   VALUE 2.
01 maxshift PIC 9(8)   VALUE 26.
01 maxc     PIC 9(8)   VALUE 26.
01 temp     PIC X(100) VALUE " ".
01 tempc    PIC X(1)   VALUE " ".
01 result   PIC X(100) VALUE " ".
01 asciiv   PIC 9(8)   VALUE 0.
01 wrap     PIC 9(8)   VALUE 0.
01 stre     PIC 9(3)   VALUE 100.
01 il       PIC 9(3)   VALUE 1.
PROCEDURE DIVISION.
Toplevel.
    DISPLAY mystring.
    PERFORM Encrypt
    MOVE "This is a new string " to mystring
    PERFORM Encrypt
    MOVE "wxyzabc" to mystring
    MOVE 25 to shift
    PERFORM Encrypt
    MOVE "wxyzabc" to mystring
    MOVE 25 to shift
    PERFORM Decrypt
    PERFORM Solve
    STOP RUN.

Encrypt.
    MOVE mystring to temp
    MOVE FUNCTION upper-case(temp) to temp
    MOVE 1 to il
    MOVE " " to result
    PERFORM stre TIMES
        MOVE temp(il:1) to tempc
        MOVE function ord(tempc) to asciiv
        if asciiv > 65 and asciiv < 92 then
            ADD shift to asciiv
            if asciiv > 91 then
                SUBTRACT 91 from asciiv 
                ADD 65 to asciiv
            end-if
            MOVE function char(asciiv) to tempc
        end-if
        STRING tempc DELIMITED BY SIZE into result with pointer il
    END-PERFORM
    DISPLAY result.
    
Decrypt.
    MOVE mystring to temp
    MOVE FUNCTION upper-case(temp) to temp
    MOVE 1 to il
    MOVE " " to result
    PERFORM stre TIMES
        MOVE temp(il:1) to tempc
        MOVE function ord(tempc) to asciiv
        if asciiv > 65 and asciiv < 92 then
            SUBTRACT shift FROM asciiv
            if asciiv < 66 then
                SUBTRACT asciiv from 66 GIVING wrap 
                SUBTRACT wrap from 92 GIVING asciiv
            end-if
            MOVE function char(asciiv) to tempc
        end-if
        STRING tempc DELIMITED BY SIZE into result with pointer il
    END-PERFORM
    DISPLAY result.
    
Solve.
    MOVE maxshift to shift
    ADD 1 to maxshift
    PERFORM maxshift TIMES
        DISPLAY "CEASER " shift ": " WITH NO ADVANCING
        PERFORM Decrypt
        SUBTRACT 1 from shift
    END-PERFORM.