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
        05 WS-HIGHEST1         PIC 9(6) VALUE 0.
        05 WS-HIGHEST2         PIC 9(6) VALUE 0.
        05 WS-HIGHEST3         PIC 9(6) VALUE 0.
        05 WS-TOTAL            PIC 9(6) VALUE 0.
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
        COMPUTE WS-TOTAL = WS-HIGHEST1 + WS-HIGHEST2 + WS-HIGHEST3
        DISPLAY 'MOST CALORIES: ' WS-TOTAL
        STOP RUN. 
    100-CALC-RTN.
        IF WS-CALORIES NOT EQUAL SPACES
            INITIALIZE WS-CALORIES-N
            MOVE WS-CALORIES TO WS-CALORIES-N
            COMPUTE WS-SUM = WS-SUM + WS-CALORIES-N
        ELSE
            IF WS-SUM > WS-HIGHEST1
                MOVE WS-SUM TO WS-HIGHEST1
            ELSE 
                IF WS-SUM > WS-HIGHEST2
                    MOVE WS-SUM TO WS-HIGHEST2
                ELSE
                    IF WS-SUM > WS-HIGHEST3
                        MOVE WS-SUM TO WS-HIGHEST3
                    END-IF
                END-IF
            END-IF
            INITIALIZE WS-SUM
        END-IF. 
