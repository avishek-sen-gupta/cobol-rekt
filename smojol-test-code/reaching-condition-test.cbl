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
           IF WS-NUM1 > 10
            THEN
                GO TO SA2.
           STOP RUN.
       SA2.
           IF WS-NUM1 = 201
            THEN
                DISPLAY "IT IS DONE".
        STOP RUN.
