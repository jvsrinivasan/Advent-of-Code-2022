IDENTIFICATION DIVISION.
PROGRAM-ID. TREE-HOUSE.
ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT INFILE ASSIGN TO '/uploads/Dec8Input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
    FD INFILE.
    01 INPUT-REC.
        05 GRID PIC X(99).
WORKING-STORAGE SECTION.
    01 WS-INPUT.
        05 WS-GRID      PIC X(99).
    01 WS-TABLE.
        05 WS-LINE      PIC X(99) OCCURS 99 TIMES. 
    01 WS-OUTPUT-TABLE.
        05 WS-OUTPUT    PIC X(99) OCCURS 99 TIMES.
    01 WS-VISIBLE       PIC 9(4) VALUE 0.
    01 WS-SUBSCRIPT     PIC 9(3) VALUE 0. 
    01 WS-EOF           PIC A(1) VALUE ' '.
    01 WS-UP-FLAG       PIC A(1) VALUE ' '.
    01 WS-DOWN-FLAG     PIC A(1) VALUE ' '.
    01 WS-LEFT-FLAG     PIC A(1) VALUE ' '.
    01 WS-RIGHT-FLAG    PIC A(1) VALUE ' '.
    01 WS-UP            PIC 9(2) VALUE 0.
    01 WS-DOWN          PIC 9(3) VALUE 0.
    01 WS-LEFT          PIC 9(3) VALUE 0.
    01 WS-RIGHT         PIC 9(3) VALUE 0.
    01 WS-I             PIC 9(3) VALUE 0.
    01 WS-J             PIC 9(3) VALUE 0.
    01 WS-K             PIC 9(3) VALUE 0.
    01 WS-A             PIC S9(3) VALUE 0.
    01 WS-B             PIC 9(3) VALUE 0.
    01 WS-C             PIC 9(3) VALUE 0.
    01 WS-D             PIC S9(3) VALUE 0.
    
PROCEDURE DIVISION.
    OPEN INPUT INFILE.
    PERFORM UNTIL WS-EOF = 'Y'
        READ INFILE INTO WS-INPUT
        AT END MOVE 'Y' TO WS-EOF
        NOT AT END 
        PERFORM 100-MOVE-RTN
        END-READ
    END-PERFORM
    CLOSE INFILE
    PERFORM 200-PROC-RTN
    COMPUTE WS-VISIBLE = WS-VISIBLE + (2 * 99) + (2 * 97)
    DISPLAY 'TOTAL VISIBLE TREES: ' WS-VISIBLE ' (Y=VISIBLE / N=NOT VISIBLE'
    MOVE ALL 'Y' TO WS-OUTPUT(1)
    MOVE ALL 'Y' TO WS-OUTPUT(99)
    PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 99
        MOVE 'Y' TO WS-OUTPUT(WS-K)(1:1)
        MOVE 'Y' TO WS-OUTPUT(WS-K)(99:1)
        DISPLAY WS-OUTPUT(WS-K)
    END-PERFORM
    STOP RUN. 

    100-MOVE-RTN.
        ADD 1 TO WS-SUBSCRIPT
        MOVE WS-GRID TO WS-LINE(WS-SUBSCRIPT).

    200-PROC-RTN.
        PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > 98
            COMPUTE WS-UP = WS-I - 1
            COMPUTE WS-DOWN = WS-I + 1
            PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J > 98
                COMPUTE WS-LEFT = WS-J - 1
                COMPUTE WS-RIGHT = WS-J + 1
                PERFORM VARYING WS-A FROM WS-UP BY -1 UNTIL (WS-A < 1 OR WS-UP-FLAG EQUAL 'Y')
                    IF WS-LINE(WS-I)(WS-J:1) <= WS-LINE(WS-A)(WS-J:1)
                        MOVE 'Y' TO WS-UP-FLAG
                    END-IF    
                END-PERFORM
                PERFORM VARYING WS-B FROM WS-DOWN BY 1 UNTIL (WS-B > 99 OR WS-DOWN-FLAG EQUAL 'Y')
                    IF WS-LINE(WS-I)(WS-J:1) <= WS-LINE(WS-B)(WS-J:1)
                        MOVE 'Y' TO WS-DOWN-FLAG
                    END-IF    
                END-PERFORM
                PERFORM VARYING WS-C FROM WS-RIGHT BY 1 UNTIL (WS-C > 99 OR WS-RIGHT-FLAG EQUAL 'Y')
                    IF WS-LINE(WS-I)(WS-J:1) <= WS-LINE(WS-I)(WS-C:1)
                        MOVE 'Y' TO WS-RIGHT-FLAG
                    END-IF
                END-PERFORM
                PERFORM VARYING WS-D FROM WS-LEFT BY -1 UNTIL (WS-D < 1 OR WS-LEFT-FLAG EQUAL 'Y')
                    IF WS-LINE(WS-I)(WS-J:1) <= WS-LINE(WS-I)(WS-D:1)
                        MOVE 'Y' TO WS-LEFT-FLAG
                    END-IF
                END-PERFORM
                
                IF      WS-UP-FLAG = 'Y'
                    AND WS-DOWN-FLAG = 'Y'
                    AND WS-LEFT-FLAG = 'Y'
                    AND WS-RIGHT-FLAG = 'Y'
                    MOVE 'N' TO WS-OUTPUT(WS-I)(WS-J:1)
                ELSE
                    ADD 1 TO WS-VISIBLE
                    MOVE 'Y' TO WS-OUTPUT(WS-I)(WS-J:1)
                END-IF
                MOVE SPACE TO WS-UP-FLAG
                MOVE SPACE TO WS-DOWN-FLAG
                MOVE SPACE TO WS-LEFT-FLAG
                MOVE SPACE TO WS-RIGHT-FLAG
            END-PERFORM
        END-PERFORM.
