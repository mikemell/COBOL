       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SEQ-FILE.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  05 OCT 2026.
       DATE-COMPILED. 05 OCT 2026.
       SECURITY.      UNCLAS.

      ******************************************************************
      *                                                                *
      * SEQ-FILE - EXAMPLE SEQUENTIAL FILE I/O OPERATIONS              *
      *                                                                *
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CUST-FILE
             ASSIGN TO 'customers.dat'
             ORGANIZATION IS LINE SEQUENTIAL

       DATA DIVISION.

       FILE SECTION.

           FD  CUST-FILE
           01  CUST-REC.
           05  C-ID          PIC 9(05).
           05  C-NAME        PIC X(20).

       WORKING-STORAGE SECTION.
           01  WS-I              PIC 9(02) VALUE 1.
           01  WS-EOF            PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
           OPEN OUTPUT CUST-FILE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
           MOVE WS-I           TO C-ID
           MOVE 'NAME-'        TO C-NAME(1:5)
           MOVE WS-I           TO C-NAME(6:2)
           WRITE CUST-REC
           END-PERFORM
           CLOSE CUST-FILE
           OPEN INPUT CUST-FILE
           PERFORM UNTIL WS-EOF = 'Y'
           READ CUST-FILE
           AT END MOVE 'Y' TO WS-EOF
           NOT AT END
           DISPLAY 'ID=' C-ID ' NAME=' C-NAME
           END-READ
           END-PERFORM
           CLOSE CUST-FILE
           GOBACK.
