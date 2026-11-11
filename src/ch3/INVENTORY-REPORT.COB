       IDENTIFICATION DIVISION.

       PROGRAM-ID.    INVENTORY-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 9, 2026.
       DATE-COMPILED. NOVENBER 9, 2026.
       SECURITY.      FOR OFFICIAL USE ONLY.
           
      ******************************************************************
      *                                                                *
      * INV-RPT, A PROGRAM TO PRINT AN INVENTORY REPORT BASED ON THE   *
      * DATA IN "./lib/DATASET1.dat"                                   *
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
             ASSIGN TO '/home/mmell/lib/DATASET1.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'INVENTORY-REPORT.txt'
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
             RECORD CONTAINS 80 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  CARRIAGE-CONTROL        PICTURE X.
           05  PART-NUMBER-REPORT      PICTURE X(4).
           05  FILLER                  PICTURE X(8).
           05  QUANTITY-SOLD-REPORT    PICTURE X(3).
           05  FILLER                  PICTURE X(7).
           05  PART-DESCRIPTION-REPORT PICTURE X(20).
           05  FILLER                  PICTURE X(7).
           05  QUANTITY-ON-HAND-REPORT PICTURE X(3).
           05  FILLER                  PICTURE X(27).
      /
       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  PAGE-LENGTH            PICTURE 9(2) VALUE 66.
           05  LINE-NUMBER            PICTURE 9(2) VALUE 1.
           05  LINES-TO-GO            PICTURE 99.
           05  PAGE-NUMBER            PICTURE 999 VALUE 1.
           05  ARE-THERE-MORE-RECORDS PICTURE X(3) VALUE 'YES'.
       01  REPORT-TITLE.
           05  FILLER                 PICTURE X(32) VALUE SPACES.
           05  FILLER            PICTURE X(16) VALUE 'INVENTORY REPORT'.
           05  FILLER                 PICTURE X(32) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER                 PICTURE X VALUE IS SPACE.
           05  FILLER                 PICTURE X(4) VALUE 'PART'.
           05  FILLER                 PICTURE X(6) VALUE SPACES.
           05  FILLER                 PICTURE X(8) VALUE 'QUANTITY'.
           05  FILLER                 PICTURE X(28) VALUE SPACES.
           05  FILLER                 PICTURE X(8) VALUE 'QUANTITY'.
           05  FILLER                 PICTURE X(25) VALUE SPACES.
       01  COLUMN-HEADER-LINE-TWO.
           05  FILLER                 PICTURE X(6) VALUE 'NUMBER'.
           05  FILLER                 PICTURE X(6) VALUE SPACES.
           05  FILLER                 PICTURE X(4) VALUE 'SOLD'.
           05  FILLER                 PICTURE X(11) VALUE SPACES.
           05  FILLER                 PICTURE X(11) VALUE 'DESCRIPTION'.
           05  FILLER                 PICTURE X(9) VALUE SPACES.
           05  FILLER                 PICTURE X(7) VALUE 'ON HAND'.
           05  FILLER                 PICTURE X(26) VALUE SPACES.
       01  REPORT-FOOTER.
           05  FILLER              PICTURE X(36) VALUE SPACES.
           05  FILLER              PICTURE X(5) VALUE 'PAGE '.
           05  PAGE-NUMBER-REPORT  PICTURE ZZ9.
           05  FILLER              PICTURE X(39) VALUE SPACES.

       PROCEDURE DIVISION.

       A000-CREATE-INVENTORY-REPORT.
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
           IF LINE-NUMBER = 1
               MOVE REPORT-TITLE to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-ONE to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-TWO to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER.
           MOVE SPACES TO REPORT-LINE
           MOVE PART-NUMBER-INPUT TO PART-NUMBER-REPORT
           MOVE QUANTITY-SOLD-INPUT TO QUANTITY-SOLD-REPORT
           MOVE PART-DESCRIPTION-INPUT TO PART-DESCRIPTION-REPORT
           MOVE QUANTITY-ON-HAND-INPUT TO QUANTITY-ON-HAND-REPORT.
           WRITE REPORT-LINE
               BEFORE ADVANCING 1 LINES
           ADD 1 TO LINE-NUMBER.
           IF LINE-NUMBER >= PAGE-LENGTH - 1
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINES
               MOVE PAGE-NUMBER TO PAGE-NUMBER-REPORT
               MOVE REPORT-FOOTER TO REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINE
               ADD 1 TO PAGE-NUMBER GIVING PAGE-NUMBER
               MOVE 1 TO LINE-NUMBER.
           READ INPUT-FILE
                 AT END
                   MOVE SPACES TO REPORT-LINE
                   MOVE PAGE-NUMBER TO PAGE-NUMBER-REPORT
                   SUBTRACT LINE-NUMBER FROM PAGE-LENGTH
                       GIVING LINES-TO-GO
                   WRITE REPORT-LINE
                   BEFORE ADVANCING LINES-TO-GO LINES
                   MOVE REPORT-FOOTER TO REPORT-LINE
                   WRITE REPORT-LINE
                       BEFORE ADVANCING 1 LINE
                   MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.

