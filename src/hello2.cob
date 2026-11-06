       IDENTIFICATION DIVISION.

       PROGRAM-ID.    HELLO2.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY 41786AL.
       DATE-WRITTEN.  OCTOBER 5, 2026.
       DATE-COMPILED. OCTOBER 5, 2026.
       SECURITY.      UNCLAS.

      ***************************************************************** 
      *                                                               * 
      * THIS IS THE CLASSIC HELLO WORLD APPLICATION, WITH UPDATES.    * 
      *                                                               * 
      ***************************************************************** 

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01  WS-FIRST-NAME      PIC X(10) VALUE 'ALEX'.
           01  WS-LAST-NAME       PIC X(10) VALUE 'MARTIN'.
           01  WS-FULL-NAME       PIC X(25).
           01  WS-GREETING        PIC X(60).
           01  WS-TEMP            PIC X(60).

       PROCEDURE DIVISION.
           STRING WS-FIRST-NAME DELIMITED BY SPACE
           SPACE   
           WS-LAST-NAME  DELIMITED BY SPACE
           INTO WS-FULL-NAME
           END-STRING
           MOVE 'Hello, ' TO WS-GREETING(1:7)
           STRING WS-FULL-NAME DELIMITED BY SIZE
           '!'          DELIMITED BY SIZE
           INTO WS-GREETING(1:60)
           END-STRING
           MOVE WS-GREETING TO WS-TEMP
           INSPECT WS-TEMP CONVERTING 'abcdefghijklmnopqrstuvwxyz'
           TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           DISPLAY WS-TEMP
           DISPLAY 'First name only: ' WS-FULL-NAME(1:10)
           GOBACK.
