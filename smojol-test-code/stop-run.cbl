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
       SA1.
           DISPLAY "SA1".
           PERFORM SZ1.
       SE1.
           DISPLAY "SE1".
           IF WS-NUM1 = 5
                GO TO SA1
           ELSE
                GO TO SZ2.
           STOP RUN.
       SZ1.
           DISPLAY "SZ1".
       SZ2.
           EXIT.
       SZ3.
           DISPLAY "ENDING...".
