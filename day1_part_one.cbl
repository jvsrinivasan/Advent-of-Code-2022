IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT INFILE ASSIGN TO '/uploads/Dec1Input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
    FD INFILE.
    01 INPUT-REC.
        05 CALORIES PIC X(6).
        
    WORKING-STORAGE SECTION.
    01 WS-INPUT.
        05 WS-CALORIES PIC X(6). 
    01 WS-WORK.
        05 WS-EOF              PIC A(1). 
        05 WS-SUM              PIC 9(6) VALUE 0.
        05 WS-HIGHEST          PIC 9(6) VALUE 0.
        05 WS-CALORIES-N       PIC 9(6). 
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
        DISPLAY 'MOST CALORIES: ' WS-HIGHEST
        STOP RUN. 
    100-CALC-RTN.
        IF WS-CALORIES NOT EQUAL SPACES
            INITIALIZE WS-CALORIES-N
            MOVE WS-CALORIES TO WS-CALORIES-N
            COMPUTE WS-SUM = WS-SUM + WS-CALORIES-N
        ELSE
            IF WS-SUM > WS-HIGHEST
                MOVE WS-SUM TO WS-HIGHEST
            END-IF
            INITIALIZE WS-SUM
        END-IF. 
