       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IMPROPER-GOTO.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            01 WS-NUM1 PIC 9(9) VALUE 5.
       PROCEDURE DIVISION.
       R SECTION.
       RA1.
           DISPLAY "RA1".
           PERFORM S.
       S SECTION.
       SA1.
           DISPLAY "SA1".
           IF WS-NUM1 = 5
                GO TO SA1.
           PERFORM SZ1.
       SE1.
           DISPLAY "SE1".
           STOP RUN.
       SZ1.
           DISPLAY "SZ1".
           GO TO SZ2.
       SZ2.
           DISPLAY "SZ2".
       SZ3.
           EXIT.
