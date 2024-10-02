       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01  CONDI         PIC X VALUE "E".
                    88 V1      VALUE "E".
                    88 V2      VALUE "F".
       PROCEDURE DIVISION.
       SECTION-0 SECTION.
        P1.
            DISPLAY "Node 1".
        P2.
            IF V1
                DISPLAY "V1 IS TRUE, GOING TO Node 3"
                GO TO P3
            ELSE
                DISPLAY "V1 IS TRUE, GOING TO Node 4"
                GO TO P4.
        P3.
            IF V1
                DISPLAY "V1 IS TRUE, GOING TO Node 2"
                GO TO P2
            ELSE
                DISPLAY "V1 IS TRUE, GOING TO Node 4"
                GO TO P4.
        P4.
            IF V1
                DISPLAY "V1 IS TRUE, GOING TO Node 2"
                GO TO P2
            ELSE
                DISPLAY "V1 IS TRUE, GOING TO Node 3"
                GO TO P3.
        P5.
           DISPLAY "EXITING..."
           STOP RUN.
