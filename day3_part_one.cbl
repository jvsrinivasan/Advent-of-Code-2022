IDENTIFICATION DIVISION.
PROGRAM-ID. RUCKSACK.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT INFILE ASSIGN TO '/uploads/Dec3Input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
    FD INFILE.
    01 INPUT-REC.
        05 RUCKSACK PIC X(100).
        
    WORKING-STORAGE SECTION.
    01 WS-INPUT.
        05 WS-RUCKSACK PIC X(100). 
    01 WS-WORK.
        05 WS-EOF              PIC A(1). 
        05 WS-NOT-FOUND           PIC A(1). 
        05 WS-FOUND            PIC A(1).
        05 WS-SUM              PIC 9(6) VALUE 0.
        05 WS-COUNT           PIC 9(2) VALUE 0.
        05 WS-ACTUAL-LENGTH   PIC 9(2) VALUE 0.
        05 WS-HALF            PIC 9(2) VALUE 0.
        05 WS-PART1           PIC X(50).
        05 WS-PRIORITY        PIC X(1).
        05 WS-I               PIC 9(2) VALUE 0.
        05 WS-TABLE.
            10 WS-TABLE-ENTRIES OCCURS 50 TIMES INDEXED BY IX.
                15 WS-LETTER PIC X(1).
PROCEDURE DIVISION.
    OPEN INPUT INFILE.
        PERFORM UNTIL WS-EOF = 'Y'
            READ INFILE INTO WS-INPUT
            AT END MOVE 'Y' TO WS-EOF
            NOT AT END 
            PERFORM 100-CALC-RTN
            END-READ
        END-PERFORM
        CLOSE INFILE
        DISPLAY 'TOTAL SCORE: ' WS-SUM
        STOP RUN. 
    100-CALC-RTN.
        INITIALIZE  WS-COUNT
                    WS-ACTUAL-LENGTH
                    WS-PART1
                    WS-PRIORITY
                    WS-TABLE
                    WS-I
                    WS-FOUND
        INSPECT FUNCTION REVERSE(WS-RUCKSACK) TALLYING WS-COUNT FOR LEADING SPACE  
        SUBTRACT WS-COUNT FROM FUNCTION LENGTH(WS-RUCKSACK) GIVING WS-ACTUAL-LENGTH
        COMPUTE WS-HALF = WS-ACTUAL-LENGTH / 2
        MOVE WS-RUCKSACK(1:WS-HALF) TO WS-PART1
        MOVE WS-RUCKSACK(WS-HALF + 1:) TO WS-TABLE
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I = 50
            IF WS-PART1(WS-I:1) NOT EQUAL SPACES AND WS-FOUND NOT EQUAL 'Y'
                SET IX TO 1
                SEARCH WS-TABLE-ENTRIES
                AT END
                MOVE 'Y' TO WS-NOT-FOUND
                WHEN WS-LETTER(IX) = WS-PART1(WS-I:1)
                MOVE WS-PART1(WS-I:1) TO WS-PRIORITY
                PERFORM 200-GET-PTY
                MOVE 'Y' TO WS-FOUND
                END-SEARCH
            END-IF    
        END-PERFORM.
    200-GET-PTY.        
        EVALUATE WS-PRIORITY
        WHEN 'a'
            COMPUTE WS-SUM = WS-SUM + 1
        WHEN 'b'
            COMPUTE WS-SUM = WS-SUM + 2
        WHEN 'c'
            COMPUTE WS-SUM = WS-SUM + 3
        WHEN 'd'
            COMPUTE WS-SUM = WS-SUM + 4
        WHEN 'e'
            COMPUTE WS-SUM = WS-SUM + 5
        WHEN 'f'
            COMPUTE WS-SUM = WS-SUM + 6
        WHEN 'g'
            COMPUTE WS-SUM = WS-SUM + 7
        WHEN 'h'
            COMPUTE WS-SUM = WS-SUM + 8
        WHEN 'i'
            COMPUTE WS-SUM = WS-SUM + 9
        WHEN 'j'
            COMPUTE WS-SUM = WS-SUM + 10
        WHEN 'k'
            COMPUTE WS-SUM = WS-SUM + 11
        WHEN 'l'
            COMPUTE WS-SUM = WS-SUM + 12          
        WHEN 'm'
            COMPUTE WS-SUM = WS-SUM + 13
        WHEN 'n'
            COMPUTE WS-SUM = WS-SUM + 14
        WHEN 'o'
            COMPUTE WS-SUM = WS-SUM + 15
        WHEN 'p'
            COMPUTE WS-SUM = WS-SUM + 16
        WHEN 'q'
            COMPUTE WS-SUM = WS-SUM + 17
        WHEN 'r'
            COMPUTE WS-SUM = WS-SUM + 18
        WHEN 's'
            COMPUTE WS-SUM = WS-SUM + 19
        WHEN 't'
            COMPUTE WS-SUM = WS-SUM + 20
        WHEN 'u'
            COMPUTE WS-SUM = WS-SUM + 21
        WHEN 'v'
            COMPUTE WS-SUM = WS-SUM + 22
        WHEN 'w'
            COMPUTE WS-SUM = WS-SUM + 23
        WHEN 'x'
            COMPUTE WS-SUM = WS-SUM + 24
        WHEN 'y'
            COMPUTE WS-SUM = WS-SUM + 25
        WHEN 'z'
            COMPUTE WS-SUM = WS-SUM + 26
        WHEN 'A'
            COMPUTE WS-SUM = WS-SUM + 27
        WHEN 'B'
            COMPUTE WS-SUM = WS-SUM + 28
        WHEN 'C'
            COMPUTE WS-SUM = WS-SUM + 29
        WHEN 'D'
            COMPUTE WS-SUM = WS-SUM + 30
        WHEN 'E'
            COMPUTE WS-SUM = WS-SUM + 31
        WHEN 'F'
            COMPUTE WS-SUM = WS-SUM + 32
        WHEN 'G'
            COMPUTE WS-SUM = WS-SUM + 33
        WHEN 'H'
            COMPUTE WS-SUM = WS-SUM + 34
        WHEN 'I'
            COMPUTE WS-SUM = WS-SUM + 35
        WHEN 'J'
            COMPUTE WS-SUM = WS-SUM + 36
        WHEN 'K'
            COMPUTE WS-SUM = WS-SUM + 37
        WHEN 'L'
            COMPUTE WS-SUM = WS-SUM + 38          
        WHEN 'M'
            COMPUTE WS-SUM = WS-SUM + 39
        WHEN 'N'
            COMPUTE WS-SUM = WS-SUM + 40
        WHEN 'O'
            COMPUTE WS-SUM = WS-SUM + 41
        WHEN 'P'
            COMPUTE WS-SUM = WS-SUM + 42
        WHEN 'Q'
            COMPUTE WS-SUM = WS-SUM + 43
        WHEN 'R'
            COMPUTE WS-SUM = WS-SUM + 44
        WHEN 'S'
            COMPUTE WS-SUM = WS-SUM + 45
        WHEN 'T'
            COMPUTE WS-SUM = WS-SUM + 46
        WHEN 'U'
            COMPUTE WS-SUM = WS-SUM + 47
        WHEN 'V'
            COMPUTE WS-SUM = WS-SUM + 48
        WHEN 'W'
            COMPUTE WS-SUM = WS-SUM + 49
        WHEN 'X'
            COMPUTE WS-SUM = WS-SUM + 50
        WHEN 'Y'
            COMPUTE WS-SUM = WS-SUM + 51
        WHEN 'Z'
            COMPUTE WS-SUM = WS-SUM + 52
        WHEN OTHER
            DISPLAY 'Invalid Letter' WS-PRIORITY
        END-EVALUATE.
