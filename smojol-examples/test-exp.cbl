       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 SOME-GROUP.
                    10 LEVEL-10-A  PIC XXXX OCCURS 3.
                    10 AA REDEFINES LEVEL-10-A.
                        20 AA1     PIC 9.
                        20 AA2     PIC 99.
                    10 LEVEL-10-B  PIC XX.
               01  SOMETHING      PIC XXXX.
               01  SOMEFRACTION   PIC 9999 OCCURS 2.
               01  SOME-UNION-1 REDEFINES SOMEFRACTION.
                    10 UNION-CHILD-1 PIC 99.
                    10 UNION-CHILD-2 REDEFINES UNION-CHILD-1 PIC XX.
               01  SOMETEXT      PIC S99V9.
               01  REDEF-SOMETEXT REDEFINES SOMETEXT PIC XXX.
               01  NUMERIC-SOMETEXT REDEFINES SOMETEXT PIC 99.
               01  CENTURY      PIC 999.
               01  REDEF REDEFINES CENTURY PIC 9999.
               01  SCALED       PIC 99PP.
               01  RESULT       PIC 99999.
               01  CONDI         PIC X VALUE "E".
                    88 V1      VALUE "E".
                    88 V2      VALUE "F".
       PROCEDURE DIVISION.
       SECTION-A SECTION.
       MOVE "-6" TO SOMETEXT.
       MOVE "ABCD" TO LEVEL-10-A(0).
       MOVE "EFGH" TO LEVEL-10-A(1).
       MOVE "IJKL" TO LEVEL-10-A(2).
       ADD 1 TO SOMETEXT.
       SUBTRACT 1 FROM SOMETEXT.
       ADD SOMETEXT TO SOMETEXT.
       COMPUTE SOMETEXT = 2 * SOMETEXT + 1.
       DISPLAY "SOMETEXT = " SOMETEXT.
       IF (SOMETEXT) = "12" OR "13"
        DISPLAY "THAT WORKED"
       ELSE
        DISPLAY "THAT DIDN'T WORK".
       DISPLAY "WEIRD STUFF".
       MOVE 1234 TO SOMEFRACTION(0).
       ADD 0 TO SOMEFRACTION(1).
       SUBTRACT 0 FROM SOMEFRACTION(0).
       DISPLAY "SOME-UNION-1 = " SOME-UNION-1.
       MOVE 50 TO SOMEFRACTION(1).
       MOVE "ABCD" TO SOMETHING.
       MOVE "E" TO CONDI.
       IF (SOMEFRACTION(1) = 10) OR >20 AND V1
           DISPLAY "AMAZE".
       IF SOMEFRACTION(0) = 100 OR 30 OR 50
           DISPLAY "AMAZE3".
       STOP RUN.

