       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDMS-MULTILINE.
       ENVIRONMENT DIVISION.
       IDMS-CONTROL SECTION.
       PROTOCOL. MODE IS BATCH DEBUG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STATUS           PIC X(4) VALUE SPACES.
       01 IX-EMP              PIC X.
       01 MT-FLAG             PIC X.
       PROCEDURE DIVISION.
       MAIN-PARA.
           BIND RUN-UNIT.
           READY.
           FINISH TASK
           ON ANY-STATUS
              MOVE 'DONE' TO WS-STATUS
           END-IF.
           IF IX-EMP EMPTY
              MOVE 'X' TO MT-FLAG.
           STOP RUN.
