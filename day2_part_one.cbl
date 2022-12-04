IDENTIFICATION DIVISION.
PROGRAM-ID. ROCK-PAPER-SCISSORS.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT INFILE ASSIGN TO '/uploads/Dec2Input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
    FD INFILE.
    01 INPUT-REC.
        05 ROUND PIC X(3).
        
    WORKING-STORAGE SECTION.
    01 WS-INPUT.
        05 WS-ROUND PIC X(3). 
    01 WS-WORK.
        05 WS-EOF              PIC A(1). 
        05 WS-SUM              PIC 9(6) VALUE 0.
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
        EVALUATE WS-ROUND
        WHEN 'A X'
            COMPUTE WS-SUM = WS-SUM + 4
        WHEN 'A Y'
            COMPUTE WS-SUM = WS-SUM + 8
        WHEN 'A Z'
            COMPUTE WS-SUM = WS-SUM + 3
        WHEN 'B X'
            COMPUTE WS-SUM = WS-SUM + 1
        WHEN 'B Y'
            COMPUTE WS-SUM = WS-SUM + 5
        WHEN 'B Z'
            COMPUTE WS-SUM = WS-SUM + 9
        WHEN 'C X'
            COMPUTE WS-SUM = WS-SUM + 7
        WHEN 'C Y'
            COMPUTE WS-SUM = WS-SUM + 2
        WHEN 'C Z'
            COMPUTE WS-SUM = WS-SUM + 6
        WHEN OTHER
            DISPLAY 'Invalid Record - ' WS-ROUND
        END-EVALUATE.
        
