       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STOPRUN.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CONDI                PIC X VALUE "E".
            88 FAILURE          VALUE "E".

       PROCEDURE DIVISION.
       S SECTION.
       SA1.
           DISPLAY "SA1".
           IF  NOT FAILURE
               GO TO SZ1.
       SE1.
           DISPLAY "SE1".
           STOP RUN.
       SZ1.
           DISPLAY "SZ1".
       SZ1.
           EXIT.
