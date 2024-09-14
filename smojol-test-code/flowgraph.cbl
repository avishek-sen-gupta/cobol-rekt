       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FLOWGRAPH.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CONDI                PIC X VALUE "E".
            88 FAILURE          VALUE "E".
            88 DB-STATUS-OK     VALUE "F".
            88 RECORD-NOT-FOUND VALUE "F".
            88 DB-READ-END    VALUE "F".
         01 S-ERROR             PIC XXXX.
         01 STATE-1             PIC 9999.

       PROCEDURE DIVISION.
       S SECTION.
       SA1.
           PERFORM A1.
           IF  NOT FAILURE
               GO TO SZ1.
       SE1.
           PERFORM E1.
       SZ1.
           PERFORM Z1.
       STATUS-CHECK SECTION.
       STATUS-CHECK-A.
           IF  DB-STATUS-OK
           OR  RECORD-NOT-FOUND
           OR  DB-READ-END
               NEXT SENTENCE
           ELSE
               PERFORM YES-ABORT
               PERFORM Z1.
       STATUS-Z.
           EXIT.
       YES-ABORT SECTION.
       YES-ABORT-A.
           DISPLAY 'ABORTA'
       ABORT-Z.
           EXIT.
       A1 SECTION.
       A1A.
           BIND RUN-UNIT.
           BIND DB-FIELD-1
              ON ANY-ERROR-STATUS
                DISPLAY "TEST1"
                 GO TO A1Z.
           BIND DB-FIELD-2
              ON ANY-ERROR-STATUS
                DISPLAY "TEST2"
                 GO TO A1Z.
           BIND DB-FIELD-3
              ON ANY-ERROR-STATUS
                DISPLAY "TEST3"
                 GO TO A1Z.
       A1Z.
           EXIT.
       E1 SECTION.
       E1A.
           DISPLAY "E1A"
           OBTAIN CALC DB-FIELD-1
              ON RECORD-NOT-FOUND
                DISPLAY "TEST4"
                 GO TO E1Z.
           IF STATE-1 = ZERO
              GO TO E1Z.
           OBTAIN FIRST DB-FIELD-2 WITHIN DB-SET-2
             ON DB-READ-END
                 GO TO E1Z.
       E1B.
           OBTAIN NEXT DB-FIELD-3 WITHIN DB-SET-3
              ON DB-READ-END
                 GO TO E1Z.
           IF   DB-STATE = STATE-1
                DISPLAY "TEST5"
           ELSE
                GO TO E1B.
       E1Z.
           EXIT.
       Z1 SECTION.
       Z1A.
           FINISH
             ON ANY-STATUS
                 NEXT SENTENCE.
           GOBACK.
       Z1Z.
           EXIT.
