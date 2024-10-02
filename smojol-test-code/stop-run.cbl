       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STOPRUN.
       AUTHOR.        MOJO
       DATE-WRITTEN.  SEP 2024.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       S SECTION.
       SA1.
           DISPLAY "SA1".
           PERFORM SZ1.
       SE1.
           DISPLAY "SE1".
           STOP RUN.
       SZ1.
           DISPLAY "SZ1".
       SZ2.
           EXIT.
