       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SIMPLE-GOTO.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            01 BILLMSTR-CODE-STAT-DSTR PIC 9(9) VALUE 5.
       PROCEDURE DIVISION.
       S SECTION.
       SA1.
           DISPLAY "ABCD".
           EVALUATE BILLMSTR-CODE-STAT-DSTR
                WHEN (' ')
                   CONTINUE

                WHEN ('P')
                   MOVE 'Y' TO FG-ERROR-FLAG
                   MOVE 'ORDER HAS BEEN POSTED - VIEW THRU DSPR '
                                        TO errorMessage
                   MOVE '9101' TO errorCode

                WHEN OTHER
                   MOVE 'Y' TO FG-ERROR-FLAG
                   MOVE 'ORDER HAS BEEN RELEASED TO BILL '
                                        TO errorMessage
                   MOVE '9102' TO errorCode
         END-EVALUATE.
        SZ1.
           DISPLAY "ENDING...".
