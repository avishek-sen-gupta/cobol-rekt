       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STOPRUN.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            01 WS-NUM1 PIC 9(9) VALUE 5.
       PROCEDURE DIVISION.
       S SECTION.
       SA0.
            DISPLAY "IN LOOP".
       SA1.
           IF WS-NUM1 > 10
            THEN
                DISPLAY "> 10"
                GO TO SA0
            ELSE
                DISPLAY "DONE".
           DISPLAY "SA1".
       SZ1.
           DISPLAY "ENDING...".
