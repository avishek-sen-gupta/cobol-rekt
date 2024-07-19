      *****************************************************************
      * Program name:    HELLO
      * Original author: Dave Nicolette
      *
      * Demonstrates:
      *
      * How to handle variable-length text values using the INSPECT
      * and STRING verbs and the REVERSE intrinsic function.
      *
      * This program prompts the user for a name via stdin (SYSIN) and
      * then writes a greeting of the form, "Hello, Your Name!" to
      * stdout (SYSOUT).
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGES.
           05  WS-PROMPT             PIC X(20)
                                     VALUE 'Please enter a name:'.
           05  WS-FRIEND             PIC X(80) VALUE SPACES.
           05  WS-GREETING           PIC X(07) VALUE 'Hello, '.
           05  WS-EXCLAMATION-POINT  PIC X VALUE '!'.
           05  WS-MESSAGE            PIC X(120) VALUE SPACES.
           05  WS-TRAILING-SPACES    PIC S9(05) COMP-3 VALUE ZERO.
       PROCEDURE DIVISION.
           DISPLAY WS-PROMPT
           ACCEPT WS-FRIEND

           INSPECT FUNCTION REVERSE(WS-FRIEND)
               TALLYING WS-TRAILING-SPACES FOR LEADING SPACES

           STRING
               WS-GREETING
                   DELIMITED BY SIZE
               WS-FRIEND(1:LENGTH OF WS-FRIEND - WS-TRAILING-SPACES)
                   DELIMITED BY SIZE
               WS-EXCLAMATION-POINT
                   DELIMITED BY SIZE
               INTO WS-MESSAGE

           DISPLAY WS-MESSAGE
           GOBACK
           .
