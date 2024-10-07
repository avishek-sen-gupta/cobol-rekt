       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       SECTION-0 SECTION.
        A.
            DISPLAY "Node 1".
            PERFORM P1.
            PERFORM P2.
            STOP RUN.
        P1.
            DISPLAY "P1".
        P2.
            DISPLAY "P2".
