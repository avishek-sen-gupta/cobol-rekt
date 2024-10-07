
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRREDUCIBLE-TEST.
       AUTHOR.        MOJO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TESTING    PIC 9(8) VALUE 100.
       PROCEDURE DIVISION.
            MAIN-SECTION SECTION.
                PERFORM U320-DBTOG.
                STOP RUN.
            U320-DBTOG SECTION.
                U320-MAIN-PART.
                    IF  TESTING = 5
                        DISPLAY "SOMETHING"
                        IF  TESTING = 6
                            GO  TO  U320Z
                    ELSE
                        DISPLAY "SOMETHING".
                        PERFORM  U560-FILL-DBTOG-TAB.
                    DISPLAY "NEXT SENTENCE".
                U320-DBTOG-FORW.
                    PERFORM  U560-FILL-DBTOG-TAB.
                U320Z.
                    EXIT.
            U560-FILL-DBTOG-TAB SECTION.
                U560A.
                    EXIT.
