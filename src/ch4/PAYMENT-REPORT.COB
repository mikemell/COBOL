       IDENTIFICATION DIVISION.

       PROGRAM-ID.    PAYMENT-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 12, 2026.
       DATE-COMPILED. NOVEMBER , 2026.
       SECURITY.      UNCLASSIFIED.
           
      ******************************************************************
      *                                                                *
      * PAYMENT-REPORT, A PROGRAM TO PRINT A TAX REPORT BASED ON THE   *
      * DATA IN "/usr/local/lib/DATASET4.DAT"                          *
      *                                                                *
      ******************************************************************
      /
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. Linux x86_64.
       OBJECT-COMPUTER. Linux x86_64.
       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE
             ASSIGN TO '/usr/local/lib/scpe/DATASET4.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'PAYMENT-REPORT.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 73 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                      PIC X(9).
           05  CUSTOMER-NUMBER-INPUT       PIC 9(4).
           05  CUSTOMER-NAME-INPUT         PIC X(20).
           05  PURCHASE-MONTH-INPUT        PIC X(2).
           05  PURCHASE-DAY-INPUT          PIC X(2).
           05  PURCHASE-YEAR-INPUT         PIC X(2).
           05  FILLER                      PIC X(20).
           05  PURCHASE-PRICE-INPUT        PIC 9(4)V99.
           05  FILLER                      PIC X(8).

       FD  REPORT-FILE
             RECORD CONTAINS 132 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  CUSTOMER-NUMBER-REPORT     PIC ZZZ9.
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  CUSTOMER-NAME-REPORT       PIC X(20).
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  PURCHASE-MONTH-REPORT      PIC X(2).
           05  FILLER                     PIC X(1) VALUE SPACE.
           05  PURCHASE-DAY-REPORT        PIC X(2).
           05  FILLER                     PIC X(1) VALUE SPACE.
           05  PURCHASE-YEAR-REPORT       PIC X(2).
           05  FILLER                     PIC X(4) VALUE SPACE.
           05  PURCHASE-PRICE-REPORT      PIC $$,$$$.99.
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  INTEREST-RATE-REPORT       PIC 99.9.
           05  PERCENT-SIGN-LITERAL-REPORT PIC X.
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  MONTHLY-PAYMENT-REPORT     PIC $$$$.99.
           05  FILLER                     PIC X(43).

       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS       PIC X(3) VALUE 'YES'.
               88  THERE-IS-A-RECORD                 VALUE 'YES'.
               88  THERE-ARE-NO-MORE-RECORDS         VALUE 'NO'.
       01  PRINTER-CONTROL.
           05  PAGE-LENGTH                  PIC 99 VALUE 66.
           05  LINE-NUMBER                  PIC 99 VALUE 1.
           05  LINE-SPACING                 PIC 9 VALUE 1.
           05  SINGLE-SPACED                PIC 9 VALUE 1.
           05  DOUBLE-SPACED                PIC 9 VALUE 2.
           05  LINES-TO-GO                  PIC 99.
           05  PAGE-NUMBER                  PIC 999 VALUE 1.
       01  PROGRAM-TABLES.
           05  MONTHS-TABLE.
               10  MONTH-CONSTANTS.
                   15  FILLER               PIC X(9) VALUE 'JANUARY  '.
                   15  FILLER               PIC X(9) VALUE 'FEBRUARY '.
                   15  FILLER               PIC X(9) VALUE 'MARCH    '.
                   15  FILLER               PIC X(9) VALUE 'APRIL    '.
                   15  FILLER               PIC X(9) VALUE 'MAY      '.
                   15  FILLER               PIC X(9) VALUE 'JUNE     '.
                   15  FILLER               PIC X(9) VALUE 'JULY     '.
                   15  FILLER               PIC X(9) VALUE 'AUGUST   '.
                   15  FILLER               PIC X(9) VALUE 'SEPTEMBER'.
                   15  FILLER               PIC X(9) VALUE 'OCTOBER  '.
                   15  FILLER               PIC X(9) VALUE 'NOVEMBER '.
                   15  FILLER               PIC X(9) VALUE 'DECEMBER '.
               10  MONTH-TABLE REDEFINES MONTH-CONSTANTS
                                            PIC X(9) OCCURS 12 TIMES.
       01  PAGE-HEADER.
           05  FILLER                       PIC X(59) VALUE SPACES.
           05  FILLER                  PIC X(14) VALUE 'PAYMENT REPORT'.
           05  FILLER                       PIC X(59) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER                       PIC X(8) VALUE 'CUSTOMER'.
           05  FILLER                       PIC X(9) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE 'CUSTOMER'.
           05  FILLER                       PIC X(11) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE 'PURCHASE'.
           05  FILLER                       PIC X(5) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE 'PURCHASE'.
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE 'INTEREST'.
           05  FILLER                       PIC X(4) VALUE SPACES.
           05  FILLER                       PIC X(7) VALUE 'MONTHLY'.
           05  FILLER                       PIC X(53) VALUE SPACES.
       01  COLUMN-HEADER-LINE-TWO.
           05  FILLER                       PIC X VALUE SPACE.
           05  FILLER                       PIC X(6) VALUE 'NUMBER'.
           05  FILLER                       PIC X(12) VALUE SPACES.
           05  FILLER                       PIC X(4) VALUE 'NAME'.
           05  FILLER                       PIC X(15) VALUE SPACES.
           05  FILLER                       PIC X(4) VALUE 'DATE'.
           05  FILLER                       PIC X(9) VALUE SPACES.
           05  FILLER                       PIC X(5) VALUE 'PRICE'.
           05  FILLER                       PIC X(6) VALUE SPACES.
           05  FILLER                       PIC X(4) VALUE 'RATE'.
           05  FILLER                       PIC X(6) VALUE SPACES.
           05  FILLER                       PIC X(7) VALUE 'PAYMENT'.
           05  FILLER                       PIC X(53) VALUE SPACES.
       01  REPORT-FOOTER.
           05  FILLER                       PIC X(62) VALUE SPACES.
           05  FILLER                       PIC X(5) VALUE 'PAGE '.
           05  PAGE-NUMBER-REPORT           PIC ZZ9.
           05  FILLER                       PIC X(62) VALUE SPACES.
      ******************************************************************
      *                                                                *
      * REPORT SPECIFIC WORK VALUES ARE CREATED HERE.                  *
      *                                                                *
      ******************************************************************
       01  PROGRAM-VALUES.
           05  INTEREST-RATE-WORK           PIC 99V9 VALUE 12.5.
           05  INTEREST-RATE-COMPUTE-WORK   PIC V999.
           05  PURCHASE-PRICE-WORK          PIC 9999V99.
           05  MONTHLY-PAYMENT-WORK         PIC 9999V99.

       PROCEDURE DIVISION.

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
