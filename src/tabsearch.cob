       IDENTIFICATION DIVISION.

       PROGRAM-ID.    TABLE-SEARCH.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  OCTOBER 5, 2026.
       DATE-COMPILED. OCTOBER 5, 2026.
       SECURITY.      UNCLAS.

      ******************************************************************
      *                                                                *
      * TABLE SEARCH - CODE DEMONSTRATING BASIC TABLE SEARCH           *
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
           01  WS-KEY             PIC X(05) VALUE 'P003'.
           01  PRODUCT-TABLE.
           05  PROD-ENTRY OCCURS 5 TIMES INDEXED BY IDX.
           10  PROD-CODE   PIC X(05).
           10  PROD-DESC   PIC X(20).
           01  WS-FOUND           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           MOVE 'P001' TO PROD-CODE(1)
           MOVE 'P002' TO PROD-CODE(2)
           MOVE 'P003' TO PROD-CODE(3)
           MOVE 'P004' TO PROD-CODE(4)
           MOVE 'P005' TO PROD-CODE(5)
           MOVE 'Widget A' TO PROD-DESC(1)
           MOVE 'Widget B' TO PROD-DESC(2)
           MOVE 'Widget C' TO PROD-DESC(3)
           MOVE 'Widget D' TO PROD-DESC(4)
           MOVE 'Widget E' TO PROD-DESC(5)
           SET IDX TO 1
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX > 5 OR WS-FOUND = 'Y'
           IF PROD-CODE(IDX) = WS-KEY
           MOVE 'Y' TO WS-FOUND
           DISPLAY 'Found: ' PROD-CODE(IDX) ' - ' PROD-DESC(IDX)
           END-IF
           END-PERFORM
           IF WS-FOUND NOT = 'Y'
           DISPLAY 'Code not found.'
           END-IF
           GOBACK.
