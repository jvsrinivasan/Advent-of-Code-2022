/* REXX */
STACK1 = "SLFZDBRH"
STACK2 = "RZMBT"
STACK3 = "SNHCLZ"
STACK4 = "JFCS"
STACK5 = "BZRWHGP"
STACK6 = "TMNDGZJV"
STACK7 = "QPSFWNLG"
STACK8 = "RZM"
STACK9 = "TRVGLCM"
ADDRESS TSO
"ALLOC DA('Z02923.AOC.DAY5.INPUT') DDN(INDD) SHR"
"EXECIO * DISKR INDD(STEM IN. FINIS)"
DO I = 1 TO IN.0
    PARSE VAR IN.I "move" CRATE "from" FROM_STACK "to" TO_STACK
    SELECT
        WHEN STRIP(FROM_STACK) = 1 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK1),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK1),1,STRIP(CRATE))
            STACK1 = SUBSTR(STRIP(STACK1),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 2 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK2),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK2),1,STRIP(CRATE))
            STACK2 = SUBSTR(STRIP(STACK2),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 3 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK3),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK3),1,STRIP(CRATE))
            STACK3 = SUBSTR(STRIP(STACK3),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 4 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK4),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK4),1,STRIP(CRATE))
            STACK4 = SUBSTR(STRIP(STACK4),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 5 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK5),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK5),1,STRIP(CRATE))
            STACK5 = SUBSTR(STRIP(STACK5),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 6 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK6),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK6),1,STRIP(CRATE))
            STACK6 = SUBSTR(STRIP(STACK6),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 7 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK7),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK7),1,STRIP(CRATE))
            STACK7 = SUBSTR(STRIP(STACK7),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 8 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK8),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK8),1,STRIP(CRATE))
            STACK8 = SUBSTR(STRIP(STACK8),CRATE+1,)
        END
        WHEN STRIP(FROM_STACK) = 9 THEN
        DO
            IF CRATE > 1 THEN
            DO
            STACK_TEMP = REVERSE(STRIP(SUBSTR(STRIP(STACK9),1,  ,
                         STRIP(CRATE))))
            END
            ELSE
            STACK_TEMP = SUBSTR(STRIP(STACK9),1,STRIP(CRATE))
            STACK9 = SUBSTR(STRIP(STACK9),CRATE+1,)
        END
        OTHERWISE
        SAY 'Invalid From Stack'
    END
    SELECT
        WHEN STRIP(TO_STACK) = 1
        THEN STACK1 = STRIP(STACK_TEMP) || STRIP(STACK1)
        WHEN STRIP(TO_STACK) = 2
        THEN STACK2 = STRIP(STACK_TEMP) || STRIP(STACK2)
        WHEN STRIP(TO_STACK) = 3
        THEN STACK3 = STRIP(STACK_TEMP) || STRIP(STACK3)
        WHEN STRIP(TO_STACK) = 4
        THEN STACK4 = STRIP(STACK_TEMP) || STRIP(STACK4)
        WHEN STRIP(TO_STACK) = 5
        THEN STACK5 = STRIP(STACK_TEMP) || STRIP(STACK5)
        WHEN STRIP(TO_STACK) = 6
        THEN STACK6 = STRIP(STACK_TEMP) || STRIP(STACK6)
        WHEN STRIP(TO_STACK) = 7
        THEN STACK7 = STRIP(STACK_TEMP) || STRIP(STACK7)
        WHEN STRIP(TO_STACK) = 8
        THEN STACK8 = STRIP(STACK_TEMP) || STRIP(STACK8)
        WHEN STRIP(TO_STACK) = 9
        THEN STACK9 = STRIP(STACK_TEMP) || STRIP(STACK9)
        OTHERWISE
        SAY 'Invalid To Stack'
    END
END

SAY "Crates at the top of each stack: " SUBSTR(STACK1,1,1) || ,
                                        SUBSTR(STACK1,1,1) || ,
                                        SUBSTR(STACK2,1,1) || ,
                                        SUBSTR(STACK3,1,1) || ,
                                        SUBSTR(STACK4,1,1) || ,
                                        SUBSTR(STACK5,1,1) || ,
                                        SUBSTR(STACK6,1,1) || ,
                                        SUBSTR(STACK7,1,1) || ,
                                        SUBSTR(STACK8,1,1) || ,
                                        SUBSTR(STACK9,1,1)
ADDRESS TSO
"FREE FI(INDD"
EXIT
