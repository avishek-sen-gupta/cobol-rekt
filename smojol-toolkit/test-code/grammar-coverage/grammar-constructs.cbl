       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAMMAR-COV.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 WS-COUNTER     PIC 9(4) VALUE 0.
               01 WS-LIMIT       PIC 9(4) VALUE 10.
               01 WS-RESULT      PIC 9(8) VALUE 0.
               01 WS-NAME        PIC X(20) VALUE SPACES.
               01 WS-LEN         PIC 9(4) VALUE 0.
               01 WS-MATRIX.
                   05 WS-ROW OCCURS 3.
                       10 WS-COL PIC 9(4) OCCURS 4.
               01 WS-IDX-I       PIC 9 VALUE 1.
               01 WS-IDX-J       PIC 9 VALUE 1.
       PROCEDURE DIVISION.
       100 SECTION.
        INIT-PARA.
            MOVE 'HELLO WORLD' TO WS-NAME.
            COMPUTE WS-LEN = FUNCTION LENGTH(WS-NAME).
            MOVE 1 TO WS-IDX-I.
            MOVE 2 TO WS-IDX-J.
            MOVE 42 TO WS-COL(WS-IDX-I, WS-IDX-J).
       PROCESS-SECTION SECTION.
        LOOP-PARA.
            PERFORM UNTIL WS-COUNTER >= WS-LIMIT
                ADD 1 TO WS-COUNTER
                COMPUTE WS-RESULT =
                    WS-RESULT + WS-COUNTER
            END-PERFORM.
       FINAL-SECTION SECTION.
        DONE-PARA.
            DISPLAY 'RESULT: ' WS-RESULT.
            STOP RUN.
