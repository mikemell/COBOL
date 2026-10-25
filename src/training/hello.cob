       IDENTIFICATION DIVISION.

       PROGRAM-ID.    HELLO.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41785AL.
       DATE-WRITTEN.  OCTOBER 5, 2026.
       DATE-COMPILED. OCTOBER 5, 2026.
       SECURITY.      UNCLAS.

      *****************************************************************
      *                                                               * 
      * THIS IS THE CLASSIC HELLO WORLD APPLICATION.                  * 
      *                                                               * 
      ***************************************************************** 

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
           DISPLAY 'Hello, world, from a compiled COBOL executable'.
           STOP RUN.

