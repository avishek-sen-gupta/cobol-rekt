
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRREDUCIBLE-TEST.
       AUTHOR.        MOJO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TESTING    PIC 9(8) VALUE 100.
       PROCEDURE DIVISION.
            MAIN-SECTION SECTION.
                PERFORM MAIN-SECTION-01.
            MAIN-SECTION-01 SECTION.
                MAIN-SECTION-01-A.
                    IF  TESTING = 5
                        DISPLAY "SOMETHING"
                        IF  TESTING = 6
                            GO  TO  MAIN-SECTION-01-Z
                    ELSE
                        DISPLAY "SOMETHING".
                        PERFORM  SUBROUTINE-1.
                    DISPLAY "NEXT SENTENCE".
                MAIN-SECTION-01-FORW.
                    PERFORM  SUBROUTINE-1.
                MAIN-SECTION-01-Z.
                    EXIT.
            SUBROUTINE-1 SECTION.
                SUBROUTINE-1-A.
                    EXIT.
