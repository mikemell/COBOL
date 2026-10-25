#line 1 "/home/mmell/COBOL/src/training/hello.cob"
 IDENTIFICATION DIVISION.

 PROGRAM-ID. HELLO.






 *****************************************************************
 * * 
 * THIS IS THE CLASSIC HELLO WORLD APPLICATION. * 
 * * 
 ***************************************************************** 

 ENVIRONMENT DIVISION.

 CONFIGURATION SECTION.

 SOURCE-COMPUTER. X86_64 Linux.
 OBJECT-COMPUTER. X86_64 Linux.

 DATA DIVISION.

 FILE SECTION.

 WORKING-STORAGE SECTION.

 PROCEDURE DIVISION.
 DISPLAY 'HELLO WORLD!'.
 STOP RUN.

