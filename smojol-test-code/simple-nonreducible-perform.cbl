       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 EXCHANGE-PART-01 PIC XXXX.
               01 SOME-ARRAY PIC XXXX OCCURS 10.
               01 SOME-PART PIC XXXX.
                    10 SOME-PART-1 PIC XXXX.
                    10 SOME-PART-2 PIC XXXX.
                    10 INVOICE-AMOUNT PIC 9999.
                    10 VENDOR-CORRECTION PIC 9999.

               01 SOME-GROUP.
                    10 LEVEL-10-A  PIC XXXX OCCURS 3.
                    10 LEVEL-10-B  OCCURS 2.
                        20 LEVEL-20-B PIC XXXX OCCURS 2.
                    10 AA REDEFINES LEVEL-10-A.
                        20 AA1     PIC 9.
                        20 AA2     PIC 99.
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
           LINKAGE SECTION.
               01  SOMETHING-LINKAGE      PIC XXXX.
               01  SOMEFRACTION-LINKAGE   PIC 9999 OCCURS 2.
       PROCEDURE DIVISION.
       SECTION-0 SECTION.
        P1.
            IF V1
                DISPLAY "V1 IS TRUE"
            ELSE
                DISPLAY "V1 IS FALSE".
            DISPLAY "IN P1, ABOUT TO PERFORM P3".
            PERFORM P3.
       SECTION-A SECTION.
        P2.
            DISPLAY "IN P2, ABOUT TO PERFORM P3...".
            PERFORM P4.
            DISPLAY "IN P2, ABOUT TO JUMP TO P5...".
            IF V1
                GO TO P5.
       SECTION-B SECTION.
        P3.
           DISPLAY "EXECUTING P3...".
        P4.
           DISPLAY "EXECUTING P4...".
        P5.
           DISPLAY "EXITING..."
           STOP RUN.

