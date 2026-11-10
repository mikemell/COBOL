       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SALES-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 9, 2026.
       DATE-COMPILED. NOVEMBER 9, 2026.
       SECURITY.      FOR OFFICIAL USE ONLY.
           
      ******************************************************************
      *                                                                *
      * SALES-RPT, A PROGRAM TO PRINT A SALES REPORT BASED ON THE DATA *
      * IN "./lib/DATASET2.dat"                                        *
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
             ASSIGN TO '/home/mmell/lib/DATASET2.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'SALES-REPORT.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 67 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                        PICTURE X(6).
           05  STORE-NUMBER-INPUT            PICTURE X(2).
           05  DEPARTMENT-NUMBER-INPUT       PICTURE X(2).
           05  SALESPERSON-NUMBER-INPUT      PICTURE X(2).
           05  SALESPERSON-NAME-INPUT        PICTURE X(20).
           05  FILLER                        PICTURE X(21).
           05  SALES-AMOUNT-INPUT            PICTURE X(6).
           05  FILLER                        PICTURE X(8).

       FD  REPORT-FILE
             RECORD CONTAINS 80 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  FILLER                     PICTURE X(5).
           05  SALESPERSON-NUMBER-REPORT  PICTURE X(2).
           05  FILLER                     PICTURE X(7).
           05  SALESPERSON-NAME-REPORT    PICTURE X(20).
           05  FILLER                     PICTURE X(9).
           05  STORE-NUMBER-REPORT        PICTURE X(2).
           05  FILLER                     PICTURE X(9).
           05  DEPARTMENT-NUMBER-REPORT   PICTURE X(2).
           05  FILLER                     PICTURE X(9).
           05  SALES-AMOUNT-REPORT        PICTURE X(6).
      /
       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  PAGE-LENGTH              PICTURE 99 VALUE 66.
           05  LINE-NUMBER              PICTURE 99 VALUE 1.
           05  LINES-TO-GO              PICTURE 99.
           05  PAGE-NUMBER              PICTURE 999 VALUE 1.
           05  ARE-THERE-MORE-RECORDS   PICTURE X(3) VALUE 'YES'.
       01  REPORT-HEADER.
           05  FILLER   PICTURE X(34) VALUE SPACES.
           05  FILLER   PICTURE X(12) VALUE 'SALES REPORT'.
           05  FILLER   PICTURE X(34) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER   PICTURE X VALUE IS SPACE.
           05  FILLER   PICTURE X(11) VALUE 'SALESPERSON'.
           05  FILLER   PICTURE X(6) VALUE SPACES.
           05  FILLER   PICTURE X(11) VALUE 'SALESPERSON'.
           05  FILLER   PICTURE X(11) VALUE SPACES.
           05  FILLER   PICTURE X(5) VALUE 'STORE'.
           05  FILLER   PICTURE X(4) VALUE SPACES.
           05  FILLER   PICTURE X(11) VALUE 'DEPARTMENT'.
           05  FILLER   PICTURE X(5) VALUE SPACES.
           05  FILLER   PICTURE X(5) VALUE 'STORE'.
           05  FILLER   PICTURE X(10) VALUE SPACES.
       01  COLUMN-HEADER-LINE-TWO.
           05  FILLER   PICTURE X(2) VALUE IS SPACES.
           05  FILLER   PICTURE X(6) VALUE 'NUMBER'.
           05  FILLER   PICTURE X(13) VALUE SPACES.
           05  FILLER   PICTURE X(4) VALUE 'NAME'.
           05  FILLER   PICTURE X(15) VALUE SPACES.
           05  FILLER   PICTURE X(6) VALUE 'NUMBER'.
           05  FILLER   PICTURE X(5) VALUE SPACES.
           05  FILLER   PICTURE X(6) VALUE 'NUMBER'.
           05  FILLER   PICTURE X(7) VALUE SPACES.
           05  FILLER   PICTURE X(6) VALUE 'AMOUNT'.
           05  FILLER   PICTURE X(10) VALUE SPACES.
       01  REPORT-FOOTER.
           05  FILLER              PICTURE X(36) VALUE SPACES.
           05  FILLER              PICTURE X(5) VALUE 'PAGE '.
           05  PAGE-NUMBER-REPORT  PICTURE ZZ9.
           05  FILLER              PICTURE X(39) VALUE SPACES.

       PROCEDURE DIVISION.

       A000-CREATE-SALES-REPORT.
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
               MOVE REPORT-HEADER to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-ONE to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-TWO to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER.
           MOVE SPACES TO REPORT-LINE
           MOVE SALESPERSON-NUMBER-INPUT
               TO SALESPERSON-NUMBER-REPORT
           MOVE SALESPERSON-NAME-INPUT TO SALESPERSON-NAME-REPORT
           MOVE STORE-NUMBER-INPUT TO STORE-NUMBER-REPORT
           MOVE DEPARTMENT-NUMBER-INPUT TO DEPARTMENT-NUMBER-REPORT
           MOVE SALES-AMOUNT-INPUT TO SALES-AMOUNT-REPORT
           WRITE REPORT-LINE
               BEFORE ADVANCING 2 LINES
           ADD 2 TO LINE-NUMBER.
           IF LINE-NUMBER >= PAGE-LENGTH - 3
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 3 LINES
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
                   AFTER ADVANCING LINES-TO-GO LINES
                   MOVE REPORT-FOOTER TO REPORT-LINE
                   WRITE REPORT-LINE
                       BEFORE ADVANCING 1 LINE
                   MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
