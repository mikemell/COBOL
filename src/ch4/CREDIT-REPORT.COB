       IDENTIFICATION DIVISION.

       PROGRAM-ID.    CREDIT-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 10, 2026.
       DATE-COMPILED. NOVEMBER 10, 2026.
       SECURITY.      FOR OFFICIAL USE ONLY.
           
      ******************************************************************
      *                                                                *
      * CREDIT-REPORT, A PROGRAM TO PRINT A PAYROLL REPORT BASED ON    *
      * THE DATA IN "./lib/DATASET2.dat"                               *
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
             ASSIGN TO 'CREDIT-REPORT.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 67 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                      PIC X(6).
           05  ACCOUNT-NUMBER-INPUT        PIC 9(6).
           05  CUSTOMER-NAME-INPUT         PIC X(20).
           05  FILLER                      PIC X(21).
           05  BALANCE-INPUT               PIC 9999V99.
           05  PURCHASES-INPUT             PIC 9999V99.
           05  FILLER                      PIC X(2).

       FD  REPORT-FILE
             RECORD CONTAINS 132 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  ACCOUNT-NUMBER-REPORT      PIC Z(5)9.
           05  FILLER                     PIC X(6).
           05  CUSTOMER-NAME-REPORT       PIC X(20).
           05  FILLER                     PIC X(4).
           05  BALANCE-REPORT             PIC Z,ZZZ.99.
           05  FILLER                     PIC X(4).
           05  PURCHASES-REPORT           PIC Z,ZZZ.99.
           05  FILLER                     PIC X(4).
           05  CREDIT-LIMIT-REPORT        PIC Z,ZZZ.99.
           05  FILLER                     PIC X(4).
           05  AVAILABLE-CREDIT-REPORT    PIC $$,$$$.99.
           05  FILLER                     PIC X(52).

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
           05  FILLER                  PIC X(14) VALUE 'CREDIT REPORT'.
           05  FILLER                       PIC X(59) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER                       PIC X(6) VALUE 'NUMBER'.
           05  FILLER                       PIC X(14) VALUE SPACES.
           05  FILLER                       PIC X(4) VALUE 'NAME'.
           05  FILLER                       PIC X(13) VALUE SPACES.
           05  FILLER                       PIC X(7) VALUE 'BALANCE'.
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  FILLER                       PIC X(9) VALUE 'PURCHASES'.
           05  FILLER                       PIC X(5) VALUE SPACES.
           05  FILLER                       PIC X(5) VALUE 'LIMIT'.
           05  FILLER                       PIC X(7) VALUE SPACES.
           05  FILLER                       PIC X(6) VALUE 'CREDIT'.
           05  FILLER                       PIC X(49) VALUE SPACES.
       01  REPORT-FOOTER.
           05  FILLER                       PIC X(62) VALUE SPACES.
           05  FILLER                       PIC X(5) VALUE 'PAGE '.
           05  PAGE-NUMBER-REPORT           PIC ZZ9.
           05  FILLER                       PIC X(62) VALUE SPACES.
       01  PROGRAM-VALUES.
           05  BALANCE-WORK                 PIC 9999V99.
           05  PURCHASES-WORK               PIC 9999V99.
           05  CREDIT-LIMIT-WORK            PIC 9999V99 VALUE 1500.
           05  AVAILABLE-CREDIT-WORK        PIC 9(5)V99.
       

       PROCEDURE DIVISION.

       A000-CREATE-PAYROLL-REPORT.
           MOVE SINGLE-SPACED TO LINE-SPACING.
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
               MOVE PAGE-HEADER to REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-ONE to REPORT-LINE
               WRITE REPORT-LINE BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER.
           MOVE SPACES TO REPORT-LINE
           MOVE BALANCE-INPUT TO BALANCE-WORK
           MOVE PURCHASES-INPUT TO PURCHASES-WORK
           MOVE CREDIT-LIMIT-WORK TO AVAILABLE-CREDIT-WORK
           SUBTRACT BALANCE-WORK FROM AVAILABLE-CREDIT-WORK
           SUBTRACT PURCHASES-WORK FROM AVAILABLE-CREDIT-WORK
           MOVE ACCOUNT-NUMBER-INPUT TO ACCOUNT-NUMBER-REPORT
           MOVE CUSTOMER-NAME-INPUT TO CUSTOMER-NAME-REPORT
           MOVE BALANCE-INPUT TO BALANCE-REPORT
           MOVE PURCHASES-INPUT TO PURCHASES-REPORT
           MOVE CREDIT-LIMIT-WORK TO CREDIT-LIMIT-REPORT
           MOVE AVAILABLE-CREDIT-WORK TO AVAILABLE-CREDIT-REPORT
           WRITE REPORT-LINE BEFORE ADVANCING LINE-SPACING LINES
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
