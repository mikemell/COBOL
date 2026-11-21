001010 IDENTIFICATION DIVISION.
001020
001030 PROGRAM-ID.    SHIPPING-REPORT.
001040 AUTHOR.        MICHAEL S. MELL.
001050 INSTALLATION.  TT ODYSSEY, 41786AL.
001060 DATE-WRITTEN.  NOVEMBER 16, 2026.
001070 DATE-COMPILED. NOVEMBER , 2026.
001080 SECURITY.      UNCLASSIFIED.
001090     
001100******************************************************************
001110*                                                                *
001120* SHIPPING-REPORT, A PROGRAM TO PRINT A SHIPPING REPORT BASED ON *
001130* THE DATA IN "/usr/local/lib/DATASET1.DAT"                      *
001140*                                                                *
001150******************************************************************
001160/
002010 ENVIRONMENT DIVISION.
002020
002030 CONFIGURATION SECTION.
002040
002050 SOURCE-COMPUTER. penelope.
002060 OBJECT-COMPUTER. Linux x86_64.
002070 SPECIAL-NAMES.
002080
002090 INPUT-OUTPUT SECTION.
002100
002110 FILE-CONTROL.
002120
002130     SELECT INPUT-FILE
002140       ASSIGN TO '/usr/local/lib/scpe/DATASET1.DAT'
002150       ORGANIZATION IS LINE SEQUENTIAL.
002160     SELECT REPORT-FILE
002170       ASSIGN TO 'SHIPPING-REPORT.TXT'
002180       ORGANIZATION IS LINE SEQUENTIAL.
002190/
003010 DATA DIVISION.
003020
003030 FILE SECTION.
003040
003050 FD  INPUT-FILE
003060       RECORD CONTAINS 61 CHARACTERS
003070       LABEL RECORDS ARE STANDARD
003080       DATA RECORD IS INPUT-RECORD.
003090 01  INPUT-RECORD.
003100     05  FILLER                  PIC X(5).
003110     05  PACKAGE-NUMBER-INPUT    PIC 9(3).
003120     05  FILLER                  PIC X(21).
003130     05  PACKAGE-CONTENTS-INPUT  PIC X(20).
003140     05  FILLER                  PIC X(4).
003150     05  SHIPPING-WEIGHT-INPUT   PIC 9(2).
003160     06  SHIPPING-VALUE-INPUT    PIC 9(4)V9(2).
003170
003180 FD  REPORT-FILE
003190       RECORD CONTAINS 127 CHARACTERS
003200       LABEL RECORDS ARE OMITTED
003210       DATA RECORD IS REPORT-LINE.
003220 01  REPORT-LINE.
003230     05  FILLER                  PIC X(2)    VALUE SPACES.
003240     05  PACKAGE-NUMBER-REPORT   PIC ZZ9.
003250     05  FILLER                  PIC X(4)    VALUE SPACES.
003260     05  PACKAGE-CONTENTS-REPORT PIC X(20).
003270     05  FILLER                  PIC X(6)    VALUE SPACES.
003280     05  SHIPPING-WEIGHT-REPORT  PIC Z9.
003290     05  FILLER                  PIC X(6)    VALUE SPACES.
003300     05  SHIPPING-VALUE-REPORT   PIC Z,ZZZ.99.
003310     05  FILLER                  PIC X(6)    VALUE SPACES.
003320     05  SHIPPING-COST-REPORT    PIC Z.99.
003330     05  FILLER                  PIC X(9)    VALUE SPACES.
003340     05  INSURANCE-COST-REPORT   PIC ZZ.99.
003350     05  FILLER                  PIC X(8)    VALUE SPACES.
003360     05  TOTAL-COST-REPORT       PIC ZZ.99.
003370     05  FILLER                  PIC X(35)   VALUE SPACES.
003380
003390 WORKING-STORAGE SECTION.
003400
003410 01  PROGRAM-INDICATORS.
003420     05  ARE-THERE-MORE-RECORDS   PIC X(3)   VALUE 'YES'.
003430         88  THERE-IS-A-RECORD               VALUE 'YES'.
003440         88  THERE-ARE-NO-MORE-RECORDS       VALUE 'NO'.
003450
003460 01  PROGRAM-TABLES.
003470     05  MONTHS-TABLE.
003480         10  MONTH-CONSTANTS.
003490             15  FILLER           PIC X(9)   VALUE 'JANUARY  '.
003500             15  FILLER           PIC X(9)   VALUE 'FEBRUARY '.
003510             15  FILLER           PIC X(9)   VALUE 'MARCH    '.
003520             15  FILLER           PIC X(9)   VALUE 'APRIL    '.
003530             15  FILLER           PIC X(9)   VALUE 'MAY      '.
003540             15  FILLER           PIC X(9)   VALUE 'JUNE     '.
003550             15  FILLER           PIC X(9)   VALUE 'JULY     '.
003560             15  FILLER           PIC X(9)   VALUE 'AUGUST   '.
003570             15  FILLER           PIC X(9)   VALUE 'SEPTEMBER'.
003580             15  FILLER           PIC X(9)   VALUE 'OCTOBER  '.
003590             15  FILLER           PIC X(9)   VALUE 'NOVEMBER '.
003600             15  FILLER           PIC X(9)   VALUE 'DECEMBER '.
003610         10  MONTH-TABLE REDEFINES MONTH-CONSTANTS
003620                                  PIC X(9)   OCCURS 12 TIMES.
003630/
004010 01  PAGE-HEADER.
004020     05  FILLER                   PIC X(59)  VALUE SPACES.
004030     05  FILLER             PIC X(14)    VALUE 'SHIPPING REPORT'.
004040     05  FILLER                   PIC X(59)  VALUE SPACES.
004050 01  COLUMN-HEADER-LINE-ONE.
004060     05  FILLER                   PIC X(8)   VALUE 'PACKAGE'.
004070     05  FILLER                   PIC X(9)   VALUE SPACES.
004080     05  FILLER                   PIC X(8)   VALUE 'PACKAGE'.
004090     05  FILLER                   PIC X(9)   VALUE SPACES.
004100     05  FILLER                   PIC X(8)   VALUE 'SHIPMENT'.
004110     05  FILLER                   PIC X(3)   VALUE SPACES.
004120     05  FILLER                   PIC X(8)   VALUE 'SHIPMENT'.
004130     05  FILLER                   PIC X(6)   VALUE SPACES.
004140     05  FILLER                   PIC X(9)   VALUE 'INSURANCE'.
004150     05  FILLER                   PIC X(6)   VALUE SPACES.
004160     05  FILLER                   PIC X(5)   VALUE 'TOTAL'.
004170     05  FILLER                   PIC X(49)   VALUE SPACES.
004180 01  COLUMN-HEADER-LINE-TWO.
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

       01  PROGRAM-VARIABLES.
           05  NOW-WORK                        PIC X(21).
           05  YEAR-WORK                       PIC 9(04).
           05  MONTH-WORK                      PIC 9(02).
           05  DAY-WORK                        PIC 9(02).
           05  HOUR-WORK                       PIC 9(02).
           05  MINUTE-WORK                     PIC 9(02).
           05  SECOND-WORK                     PIC 9(02).
           05  MILLISECOND-WORK                PIC 9(02).
           05  OFFSET-FROM-GMT-WORK            PIC X(05).
           05  SHIPPING-COST-WORK              PIC 9V99.
           05  INSURANCE-COST-WORK             PIC 99V99.
           05  PACKAGE-TOTAL-COST-WORK         PIC 99V99.
      /
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
      * B000-PROCESS-DETAIL-RECORDS    B100-CREATE-PAGE-HEADER         *
      * B200-PRINT-FINAL-TOTALS        B300-CREATE-PAGE-FOOTER         *
      *                                                                *
      ******************************************************************

       A000-CREATE-TAX-REPORT.
           OPEN INPUT  INPUT-FILE
                OUTPUT REPORT-FILE.
           MOVE FUNCTION CURRENT-DATE TO NOW-WORK
           MOVE NOW-WORK(1:4)   TO YEAR-WORK
           MOVE NOW-WORK(5:2)   TO MONTH-WORK
           MOVE NOW-WORK(7:2)   TO DAY-WORK
           MOVE NOW-WORK(9:2)   TO HOUR-WORK
           MOVE NOW-WORK(11:2)  TO MINUTE-WORK
           MOVE NOW-WORK(13:2)  TO SECOND-WORK
           MOVE NOW-WORK(15:2)  TO MILLISECOND-WORK
           MOVE NOW-WORK(17:5)  TO OFFSET-FROM-GMT-WORK
           MOVE MONTH-WORK TO MONTH-REPORT
           MOVE DAY-WORK TO DAY-REPORT
           MOVE YEAR-WORK TO YEAR-REPORT
           MOVE SINGLE-SPACED TO LINE-SPACING.
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
               MOVE MONTH-WORK TO MONTH-REPORT
               MOVE DAY-WORK TO DAY-REPORT
               MOVE YEAR-WORK TO YEAR-REPORT
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
