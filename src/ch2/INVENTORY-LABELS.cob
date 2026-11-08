       IDENTIFICATION DIVISION.

       PROGRAM-ID.    INVENTORY-LABELS.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 8, 2026.
       DATE-COMPILED. NOVEMBER 8. 2026.
       SECURITY.      FOR OFFICIAL USE ONLY.
           
      ******************************************************************
      *                                                                *
      * INVENTORY-LABELS, A PROGRAM TO PRINT INVENTORY LABELS FROM     *
      * IN "./lib/DATASET3.dat"                                        *
      *                                                                *
      ******************************************************************
      /
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. homer.
       OBJECT-COMPUTER. odysseus.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE
             ASSIGN TO '/home/mmell/lib/DATASET3.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'INVENTORY-LABELS.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 64 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                        PICTURE X(6).
           05  FOOD-NUMBER-INPUT             PICTURE X(4).
           05  FILLER                        PICTURE X(6).
           05  FOOD-DESCRIPTION-INPUT        PICTURE X(20).
           05  FILLER                        PICTURE X(28).

       FD  REPORT-FILE
             RECORD CONTAINS 80 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  FILLER                        PICTURE X(1).
           05  FOOD-NUMBER-A-REPORT          PICTURE X(4).
           05  FILLER                        PICTURE X(5).
           05  FOOD-DESCRIPTION-A-REPORT     PICTURE X(20).
           05  FILLER                        PICTURE X(9).
           05  FOOD-NUMBER-B-REPORT          PICTURE X(4).
           05  FILLER                        PICTURE X(6).
           05  FOOD-DESCRIPTION-B-REPORT     PICTURE X(20).
           05  FILLER                        PICTURE X(15).
      /
       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS        PICTURE X(3) VALUE 'YES'.

       PROCEDURE DIVISION.

       A000-CREATE-POLICY-REPORT.
           OPEN INPUT  INPUT-FILE
                OUTPUT REPORT-FILE.
           READ INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
           PERFORM A001-FORMAT-PRINT-LINE
                 UNTIL ARE-THERE-MORE-RECORDS = 'NO '.
           CLOSE INPUT-FILE
                 REPORT-FILE.
           STOP RUN.

       A001-FORMAT-PRINT-LINE.
           MOVE SPACES TO REPORT-LINE
           MOVE FOOD-NUMBER-INPUT TO FOOD-NUMBER-A-REPORT
           MOVE FOOD-NUMBER-INPUT TO FOOD-NUMBER-B-REPORT
           MOVE FOOD-DESCRIPTION-INPUT TO FOOD-DESCRIPTION-A-REPORT
           MOVE FOOD-DESCRIPTION-INPUT TO FOOD-DESCRIPTION-B-REPORT
           WRITE REPORT-LINE
               AFTER ADVANCING 1 LINES
           READ INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
