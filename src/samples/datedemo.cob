       IDENTIFICATION DIVISION.

       PROGRAM-ID.    DATE-DEMO.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  OCTOBER 5, 2026.
       DATE-COMPILED. OCTOBER 5, 2026.
       SECURITY.      UNCLAS.

      ******************************************************************
      *                                                                *
      * DATE-DEMO - A DEMONSTRATION OF FUNDAMENTAL DATE/TIME HANDLING  *
      *                                                                *
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01  WS-NOW              PIC X(21).
           01  WS-YEAR             PIC 9(04).
           01  WS-MONTH            PIC 9(02).
           01  WS-DAY              PIC 9(02).
           01  WS-DUE-DAY          PIC 9(02).
           01  WS-FORMATTED        PIC X(22).


       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           MOVE WS-NOW(1:4)   TO WS-YEAR
           MOVE WS-NOW(5:2)   TO WS-MONTH
           MOVE WS-NOW(7:2)   TO WS-DAY
           MOVE 'YYYY-MM-DD: ' TO WS-FORMATTED(1:12)
           MOVE WS-YEAR         TO WS-FORMATTED(13:4)
           MOVE '-'             TO WS-FORMATTED(17:1)
           MOVE WS-MONTH        TO WS-FORMATTED(18:2)
           MOVE '-'             TO WS-FORMATTED(20:1)
           MOVE WS-DAY          TO WS-FORMATTED(21:2)
           DISPLAY WS-FORMATTED
           ADD 7 TO WS-DAY GIVING WS-DUE-DAY
           DISPLAY 'Due day (simple +7): ' WS-DUE-DAY
           GOBACK.
