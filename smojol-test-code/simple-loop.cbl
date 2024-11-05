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
           IF WS-NUM1 > 10
            THEN
                GO TO SA1
            ELSE
                DISPLAY "<= 10".
           DISPLAY "SA1-1"
           DISPLAY "SA1-2".
       SZ1.
           DISPLAY "ENDING...".
