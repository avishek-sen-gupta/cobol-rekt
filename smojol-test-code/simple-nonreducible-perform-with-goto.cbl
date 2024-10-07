       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRREDUCIBLE-TEST.
       AUTHOR.        MOJO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TESTING    PIC 9(8) VALUE 100.
       PROCEDURE DIVISION.
            ROOT SECTION.
                PERFORM MAIN-SECTION-01.
                STOP RUN.
            MAIN-SECTION-01 SECTION.
                MAIN-SECTION-01-A.
                    IF  TESTING = 5
                        GO  TO  MAIN-SECTION-01-Z.
                    PERFORM  SUBROUTINE-1.
                MAIN-SECTION-01-Z.
                    EXIT.
            SUBROUTINE-1 SECTION.
                SUBROUTINE-1-A.
                    EXIT.
