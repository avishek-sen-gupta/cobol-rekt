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
        EVALUATE TRUE ALSO TRUE
              WHEN SCALED + RESULT < 10 ALSO INVOICE-AMOUNT = 10
                MOVE "CASE 1" TO SOMETHING
              WHEN SCALED + RESULT > 50 ALSO
                INVOICE-AMOUNT = ( SOMETEXT + RESULT ) / SCALED
                MOVE "CASE 2" TO SOMETHING
              WHEN OTHER
                MOVE "CASE OTHER" TO SOMETHING
            END-EVALUATE

           SEARCH SOME-ARRAY
           WHEN SOME-PART-1 > 10
            DISPLAY "CONDITION 1"
           WHEN SOME-PART-2 <= 10
            DISPLAY "CONDITION 2".
           PERFORM TEST BEFORE VARYING SOME-PART-1 FROM 1 BY 1
           UNTIL SOME-PART-1 > 10
           AFTER SOME-PART-2 FROM 1 BY 1 UNTIL SOME-PART-2 > 10
                DISPLAY "GOING " SOME-PART-1 " AND " SOME-PART-2
           END-PERFORM.
           GO TO SECTION-A, SECTION-B, SECTION-B1 DEPENDING ON RESULT.

       SECTION-A SECTION.
        P2.
           ADD SOMETEXT, SCALED, 30 TO SOMETHING, RESULT.
           MOVE SOMETEXT2 TO SOMETEXT2.
           MOVE 0 TO SOMETEXT2.
           MOVE "ABCD" TO LEVEL-10-A(1).
           MOVE "EFGH" TO LEVEL-10-A(2).
           MOVE "IJKL" TO LEVEL-10-A(3).
        P3.
           MOVE "0000" TO LEVEL-20-B(1 1).
           MOVE "0011" TO LEVEL-20-B(1 2).
           MOVE "1100" TO LEVEL-20-B(2 1).
           MOVE "1111" TO LEVEL-20-B(2 2).
           DISPLAY "LEVEL-20-A = " LEVEL-20-A.
           ADD 1 TO 1 GIVING SOMETEXT.
           DIVIDE 10 INTO SOMETEXT.
           DISPLAY "SOMETEXT XX= " SOMETEXT.
           ADD 1 TO 1 GIVING SOMETEXT.
           SUBTRACT 5 FROM 30 GIVING SOMETEXT.
           DISPLAY "SOMETEXT YY= " SOMETEXT.
           MULTIPLY 2 BY 2 GIVING INVOICE-AMOUNT.
        P4.
           ADD SOMETEXT TO SOMETEXT.
           MOVE 10 TO SOMEFRACTION(1).
           COMPUTE SOMETEXT = 2 * SOMETEXT + 1.
           COMPUTE SOMETEXT = SOMETEXT / SOMEFRACTION(1).
       SECTION-B SECTION.
        P5.
           DISPLAY "SOMETEXT = " SOMETEXT.
           IF (SOMETEXT) = "12" OR "13"
            DISPLAY "THAT WORKED"
           ELSE
            DISPLAY "THAT DIDN'T WORK".
           DISPLAY "WEIRD STUFF".
        P6.
           MOVE 1234 TO SOMEFRACTION(1).
           ADD 0 TO SOMEFRACTION(1).
           SUBTRACT 0 FROM SOMEFRACTION(1).
       SECTION-B1 SECTION.
        P7.
           DISPLAY "SOMETEXT = " SOMETEXT.
           IF (SOMETEXT) = "12" OR "13"
            DISPLAY "THAT WORKED"
           ELSE
            DISPLAY "THAT DIDN'T WORK".
        P8.
           MOVE 1234 TO SOMEFRACTION(1).
           ADD 0 TO SOMEFRACTION(1).
           SUBTRACT 0 FROM SOMEFRACTION(1).
       SECTION-C SECTION.
        P9.
           DISPLAY "SOME-UNION-1 = " SOME-UNION-1.
           MOVE 50 TO SOMEFRACTION(1).
           MOVE "ABCD" TO SOMETHING.
           MOVE "E" TO CONDI.
        P10.
           IF (SOMEFRACTION(1) = 10) OR >20 AND V1
               DISPLAY "AMAZE".
           IF SOMEFRACTION(1) = 100 OR 30 OR 50
               DISPLAY "AMAZE3".
           STOP RUN.

