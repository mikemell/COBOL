       IDENTIFICATION DIVISION.

       PROGRAM-ID.    INV-RPT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 6, 2027.
       DATE-COMPILED. .
       SECURITY.      FOUO.
           
      ******************************************************************
      *                                                                *
      * INV-RPT, A PROGRAM TO PRINT AN INVENTORY REPORT BASED ON THE   *
      * DATA IN "./lib/DATASET1.dat"                             *
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
             ASSIGN TO 'lib/DATASET1.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'var/INV_RPT.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 61 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER             PICTURE X(5).
           05  PART-NUMBER-INPUT       PICTURE X(4).
           05  FILLER             PICTURE X(20).
           05  PART-DESCRIPTION-INPUT  PICTURE X(20).
           05  FILLER             PICTURE X(6).
           05  QUANTITY-ON-HAND-INPUT  PICTURE X(3).
           05  QUANTITY-SOLD-INPUT     PICTURE X(3).

       FD  REPORT-FILE
             RECORD CONTAINS 133 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  CARRIAGE-CONTROL        PICTURE X.
           05  PART-NUMBER-REPORT      PICTURE X(4).
           05  FILLER             PICTURE X(8).
           05  QUANTITY-SOLD-REPORT    PICTURE X(3).
           05  FILLER             PICTURE X(7).
           05  PART-DESCRIPTION-REPORT PICTURE X(20).
           05  FILLER             PICTURE X(7).
           05  QUANTITY-ON-HAND-REPORT PICTURE X(3).
           05  FILLER             PICTURE X(80).
      /
       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS  PICTURE X(3) VALUE 'YES'.

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
           MOVE SPACES TO REPORT-LINE.
           MOVE PART-NUMBER-INPUT TO PART-NUMBER-REPORT.
           MOVE QUANTITY-SOLD-INPUT TO QUANTITY-SOLD-REPORT.
           MOVE PART-DESCRIPTION-INPUT TO PART-DESCRIPTION-REPORT.
           MOVE QUANTITY-ON-HAND-INPUT TO QUANTITY-ON-HAND-REPORT.
           WRITE REPORT-LINE
                 AFTER ADVANCING 1 LINES.
           READ INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
