       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SIMPLE-GOTO.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            01 WS-NUM1 PIC 9(9) VALUE 5.
       PROCEDURE DIVISION.
       S SECTION.
       SA1.
           DISPLAY "ABCD".
           ADD 1 TO WS-NUM1.
           IF WS-NUM1 <= 10
            THEN
                GO TO SA1
            ELSE
                DISPLAY "EXITING LOOP".
       SZ1.
           DISPLAY "ENDING...".
