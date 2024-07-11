       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 SOME-GROUP-A.
                    10 LEVEL-10-A  OCCURS 2.
                        20 LEVEL-20-A PIC XX.
               01 SOME-GROUP-B.
                    10 LEVEL-10-B  OCCURS 2.
                        20 LEVEL-20-B PIC XXXX OCCURS 2.
               01 SOME-GROUP-C.
                    10 LEVEL-10-C PIC XXXX OCCURS 2.
       PROCEDURE DIVISION.
       SECTION-A SECTION.
           MOVE "0000" TO LEVEL-20-B(1 1).
           MOVE "0011" TO LEVEL-20-B(1 2).
           MOVE "1100" TO LEVEL-20-B(2 1).
           MOVE "1111" TO LEVEL-20-B(2 2).
           MOVE "CD" TO LEVEL-20-A(1)
           MOVE "AB" TO LEVEL-20-A(2)
           MOVE "EF" TO LEVEL-10-C(1)
           MOVE "GH" TO LEVEL-10-C(2)

           DISPLAY "LEVEL-20-A = " SOME-GROUP-B

           STOP RUN.

