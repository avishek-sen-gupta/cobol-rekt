       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRREDUCIBLE-TEST.
       AUTHOR.        MOJO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TESTING    PIC 9(8) VALUE 100.
       PROCEDURE DIVISION.
       INITSTART.
      *****************************************************************
      *****************************************************************
       WRITEQTDQ.
      *****************************************************************

           EXEC CICS
                WRITEQ TD
                QUEUE   ('TERR')
                FROM    (MSGE-RCRD)
                LENGTH  (LENGTH OF MSGE-RCRD)
                NOHANDLE
           END-EXEC.

           IF EIBRESP >  0
               PERFORM WRITECONSOLE THRU WC-EXIT
           END-IF.

       TDQ-EXIT. EXIT.
      *****************************************************************
       WRITECONSOLE.
      *****************************************************************

           MOVE EIBRESP    TO WSC-EIBRESP.
           MOVE MSGNO      TO WSC-MSGNO.

           EXEC CICS
                WRITE OPERATOR
                TEXT  (WSC-MSG)
                TEXTLENGTH (62)
                EVENTUAL
           END-EXEC.

       WC-EXIT. EXIT.

      *****************************************************************
       TERMIN.
      *****************************************************************

           EXEC CICS RETURN END-EXEC.

       TERMIN-EXIT. EXIT.
