       IDENTIFICATION DIVISION.

       PROGRAM-ID.    SHIPPING-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 16, 2026.
       DATE-COMPILED. NOVEMBER , 2026.
       SECURITY.      UNCLASSIFIED.
           
      ******************************************************************
      *                                                                *
      * SHIPPING-REPORT, A PROGRAM TO PRINT A SHIPPING REPORT BASED ON *
      * THE DATA IN "/usr/local/lib/DATASET1.DAT"                      *
      *                                                                *
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. Linux x86_64.
       OBJECT-COMPUTER. Linux x86_64.
       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE
             ASSIGN TO '/usr/local/lib/scpe/DATASET1.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'SHIPPING-REPORT.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 61 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                  PIC X(5).
           05  PACKAGE-NUMBER-INPUT    PIC 9(4).

       FD  REPORT-FILE
             RECORD CONTAINS 132 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  FILLER                  PIC X(2)    VALUE SPACES.
           05  PACKAGE-NUMBER-REPORT   PIC ZZZ9.

       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS   PIC X(3)   VALUE 'YES'.
               88  THERE-IS-A-RECORD               VALUE 'YES'.
               88  THERE-ARE-NO-MORE-RECORDS       VALUE 'NO'.


       01  PROGRAM-TABLES.
           05  MONTHS-TABLE.
               10  MONTH-CONSTANTS.
                   15  FILLER           PIC X(9)   VALUE 'JANUARY  '.
                   15  FILLER           PIC X(9)   VALUE 'FEBRUARY '.
                   15  FILLER           PIC X(9)   VALUE 'MARCH    '.
                   15  FILLER           PIC X(9)   VALUE 'APRIL    '.
                   15  FILLER           PIC X(9)   VALUE 'MAY      '.
                   15  FILLER           PIC X(9)   VALUE 'JUNE     '.
                   15  FILLER           PIC X(9)   VALUE 'JULY     '.
                   15  FILLER           PIC X(9)   VALUE 'AUGUST   '.
                   15  FILLER           PIC X(9)   VALUE 'SEPTEMBER'.
                   15  FILLER           PIC X(9)   VALUE 'OCTOBER  '.
                   15  FILLER           PIC X(9)   VALUE 'NOVEMBER '.
                   15  FILLER           PIC X(9)   VALUE 'DECEMBER '.
               10  MONTH-TABLE REDEFINES MONTH-CONSTANTS
                                        PIC X(9)   OCCURS 12 TIMES.
       01  PAGE-HEADER.
           05  FILLER                   PIC X(59)  VALUE SPACES.
           05  FILLER             PIC X(14)    VALUE 'SHIPPING REPORT'.
           05  FILLER                   PIC X(59)  VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(9)   VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(11)  VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(5)   VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(3)   VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(4)   VALUE SPACES.
           05  FILLER                   PIC X(7)   VALUE ''.
           05  FILLER                   PIC X(4)   VALUE SPACES.
           05  FILLER                   PIC X(7)   VALUE ''.
           05  FILLER                   PIC X(42)  VALUE SPACES.
       01  COLUMN-HEADER-LINE-TWO.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(9)   VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(11)  VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(5)   VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(3)   VALUE SPACES.
           05  FILLER                   PIC X(8)   VALUE ''.
           05  FILLER                   PIC X(4)   VALUE SPACES.
           05  FILLER                   PIC X(7)   VALUE ''.
           05  FILLER                   PIC X(4)   VALUE SPACES.
           05  FILLER                   PIC X(7)   VALUE ''.
           05  FILLER                   PIC X(42)  VALUE SPACES.
       01  REPORT-FOOTER.
           05  FILLER                   PIC X(62)  VALUE SPACES.
           05  FILLER                   PIC X(5)   VALUE 'PAGE '.
           05  PAGE-NUMBER-REPORT       PIC ZZ9.
           05  FILLER                   PIC X(62)  VALUE SPACES.

       01  PROGRAM-CONSTANTS.
           05  SHIPPING-COST-PER-UNIT-WORK     PIC V99     VALUE .07.
           05  INSURANCE-COST-PER-UNIT-WORK    PIC V999    VALUE .005.

       01  WORK-AREAS.
           05  SHIPPING-COST-WORK              PIC 9V99.
           05  INSURANCE-COST-WORK             PIC 99V99.
           05  PACKAGE-TOTAL-COST-WORK         PIC 99V99.

       01  PROGRAM-ACCUMULATORS-COUNTERS
           05  REPORT-TOTAL-SHIPMENTS-COUNT    PIC 999.
           05  SHIPPING-COST-ACCUMULATOR       PIC 999V99.
           05  INSURANCE-COST-ACCUMULATOR      PIC 9999V99.
           05  TOTAL-COST-ACCUMULATOR          PIC 9999V99.

       01  PRINTER-CONTROL
      *    05  TOP-OF-FORM                     PIC X(3) VALUE 'ESC [ H'.
           05  PAGE-LENGTH                     PIC 99     VALUE 66.
           05  LINE-LENGTH                     PIC 999    VALUE 132.
           05  LINE-NUMBER                     PIC 99     VALUE 1.
           05  LINE-SPACING                    PIC 9      VALUE 1.
           05  LINES-TO-GO                     PIC 99.
           05  PAGE-NUMBER                     PIC 999    VALUE 1.
           05  PROPER-SPACING                  PIC 9.
           05  SPACE-ONE-LINE                  PIC 9       VALUE 1.
           05  SPACE-TGW-LINES                 PIC 9       VALUE 2.
      /
       PROCEDURE DIVISION.

      ******************************************************************
      *                                                                *
      *              T A B L E   O F   C O N T E N T S                 *
      *                                                                *
      * A000-CREATE-TAX-REPORT         A100-CREATE-TAX-REPORT-PAGE     *
      * B000-PROCESS-DETAIL-RECORDS    B900-PRINT-FINAL-TOTALS         *
      *                                                                *
      ******************************************************************

       A000-CREATE-TAX-REPORT.
           MOVE SINGLE-SPACED TO LINE-SPACING.
           OPEN INPUT  INPUT-FILE
                OUTPUT REPORT-FILE.
           READ INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
           PERFORM A001-CREATE-REPORT-PAGE
                 UNTIL ARE-THERE-MORE-RECORDS = 'NO '.
           CLOSE INPUT-FILE
                 REPORT-FILE.
           STOP RUN.

       A001-CREATE-REPORT-PAGE.
      ******************************************************************
      *                                                                *
      * FIRST, PRINT THE TITLE AND COLUMN HEADERS.                     *
      *                                                                *
      ******************************************************************
           IF LINE-NUMBER = 1
               MOVE PAGE-HEADER to REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-ONE to REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-TWO to REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER.
      ******************************************************************
      *                                                                *
      * GENERATE ONE LINE OF THE ACTUAL REPORT HERE.                   *
      *                                                                *
      ******************************************************************
           MOVE SPACES TO REPORT-LINE
           MOVE CUSTOMER-NUMBER-INPUT TO CUSTOMER-NUMBER-REPORT
           MOVE CUSTOMER-NAME-INPUT TO CUSTOMER-NAME-REPORT
           MOVE PURCHASE-MONTH-INPUT TO PURCHASE-MONTH-REPORT
           MOVE PURCHASE-DAY-INPUT TO PURCHASE-DAY-REPORT
           MOVE PURCHASE-YEAR-INPUT TO PURCHASE-YEAR-REPORT
           MOVE PURCHASE-PRICE-INPUT TO PURCHASE-PRICE-REPORT
           MOVE PURCHASE-PRICE-INPUT TO PURCHASE-PRICE-WORK
           MOVE INTEREST-RATE-WORK TO INTEREST-RATE-REPORT
           MOVE '%' TO PERCENT-SIGN-LITERAL-REPORT
           DIVIDE INTEREST-RATE-WORK BY 100 GIVING
           INTEREST-RATE-COMPUTE-WORK
           COMPUTE MONTHLY-PAYMENT-WORK = (PURCHASE-PRICE-WORK +
           (PURCHASE-PRICE-WORK * INTEREST-RATE-COMPUTE-WORK)) / 12
           MOVE MONTHLY-PAYMENT-WORK TO MONTHLY-PAYMENT-REPORT
           WRITE REPORT-LINE BEFORE ADVANCING LINE-SPACING LINES
      ******************************************************************
      *                                                                *
      * REPORT SPECIFIC PROCESSING COMPLETED.                          *
      *                                                                *
      ******************************************************************
           ADD LINE-SPACING TO LINE-NUMBER.
           SUBTRACT LINE-NUMBER FROM PAGE-LENGTH GIVING LINES-TO-GO.
           IF LINES-TO-GO <= LINE-SPACING
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 1 LINES
               MOVE PAGE-NUMBER TO PAGE-NUMBER-REPORT
               MOVE REPORT-FOOTER TO REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 1 LINE
               ADD 1 TO PAGE-NUMBER 
               MOVE 1 TO LINE-NUMBER.
           READ INPUT-FILE
               AT END
                   MOVE SPACES TO REPORT-LINE
                   MOVE PAGE-NUMBER TO PAGE-NUMBER-REPORT
                   WRITE REPORT-LINE AFTER ADVANCING LINES-TO-GO LINES
                   MOVE REPORT-FOOTER TO REPORT-LINE
                   WRITE REPORT-LINE BEFORE ADVANCING 1 LINE
                   MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
