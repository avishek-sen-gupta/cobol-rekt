       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 SOME-GROUP.
                    10 LEVEL-10-A  PIC XXXX.
                    10 AA REDEFINES LEVEL-10-A.
                        20 AA1     PIC 9.
                        20 AA2     PIC 99.
                    10 LEVEL-10-B  PIC XX.
               01  SOMETHING      PIC XXXX.
               01  SOMEFRACTION   PIC 9999.
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
       CALL 'ABCD'.
       MOVE "RESOLVED_PATH" TO SOMETHING.
       CALL SOMETHING.
       TRANSFER CONTROL "EFGH".
       MOVE 50 TO SOMEFRACTION.
       MOVE "ABCD" TO SOMETHING.
       MOVE "E" TO CONDI.
       IF (SOMEFRACTION = 10) OR >20 AND V1
	       DISPLAY "AMAZE".
       IF SOMEFRACTION = 100 OR 30 OR 50
	       DISPLAY "AMAZE3".
       STOP RUN.

