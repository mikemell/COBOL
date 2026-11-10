       IDENTIFICATION DIVISION.

       PROGRAM-ID.    PAYROLL-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER , 2026.
       DATE-COMPILED. NOVEMBER , 2026.
       SECURITY.      FOR OFFICIAL USE ONLY.
           
      ******************************************************************
      *                                                                *
      * PAYROLL-REPORT, A PROGRAM TO PRINT A SALES REPORT BASED ON THE *
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
             ASSIGN TO 'PAYROLL-REPORT.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 61 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                        PIC X(5).
           05  SALESPERSON-NUMBER-INPUT      PIC 999.
           05  FILLER                        PIC X.
           05  SALESPERSON-NAME-INPUT        PIC X(20).
           05  FILLER                        PIC X(21).
           05  SALARY-INPUT                  PIC 999V99.
           05  FILLER                        PIC X.
           05  COMMISSION-INPUT              PIC 999V99.

       FD  REPORT-FILE
             RECORD CONTAINS 132 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  FILLER                     PIC X.
           05  SALESPERSON-NUMBER-REPORT  PIC ZZ9.
           05  FILLER                     PIC X(8).
           05  SALESPERSON-NAME-REPORT    PIC X(20).
           05  FILLER                     PIC X(6).
           05  SALARY-REPORT              PIC ZZZ.99.
           05  FILLER                     PIC X(6).
           05  COMMISSION-REPORT          PIC ZZZ.99.
           05  FILLER                     PIC X(6).
           05  TOTAL-PAY-REPORT           PIC $$,$$$.99.
           05  FILLER                     PIC X(61).

       WORKING-STORAGE SECTION.

       01  PROGRAM-VALUES.
           05  SALARY-WORK              PIC 999V99.
           05  COMMISSION-WORK          PIC 999V99.
           05  TOTAL-PAY-WORK           PIC 9999V99.
       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS   PIC X(3) VALUE 'YES'.
               88  THERE-IS-A-RECORD             VALUE 'YES'.
               88  THERE-ARE-NO-MORE-RECORES     VALUE 'NO'.
       01  PRINTER-CONTROL.
           05  PAGE-LENGTH              PIC 99 VALUE 66.
           05  LINE-NUMBER              PIC 99 VALUE 1.
           05  LINE-SPACING             PIC 9 VALUE 1.
           05  LINES-TO-GO              PIC 99.
           05  PAGE-NUMBER              PIC 999 VALUE 1.
       01  PROGRAM-TABLES.
           05  MONTHS-TABLE.
               10  MONTH-CONSTANTS.
                   15  FILLER  PIC X(9) VALUE 'JANUARY  '.
                   15  FILLER  PIC X(9) VALUE 'FEBRUARY '.
                   15  FILLER  PIC X(9) VALUE 'MARCH    '.
                   15  FILLER  PIC X(9) VALUE 'APRIL    '.
                   15  FILLER  PIC X(9) VALUE 'MAY      '.
                   15  FILLER  PIC X(9) VALUE 'JUNE     '.
                   15  FILLER  PIC X(9) VALUE 'JULY     '.
                   15  FILLER  PIC X(9) VALUE 'AUGUST   '.
                   15  FILLER  PIC X(9) VALUE 'SEPTEMBER'.
                   15  FILLER  PIC X(9) VALUE 'OCTOBER  '.
                   15  FILLER  PIC X(9) VALUE 'NOVEMBER '.
                   15  FILLER  PIC X(9) VALUE 'DECEMBER '.
               10  MONTH-TABLE REDEFINES MONTH-CONSTANTS
                               PIC X(9) OCCURS 12 TIMES.
       01  PAGE-HEADER.
           05  FILLER   PIC X(59) VALUE SPACES.
           05  FILLER   PIC X(14) VALUE 'PAYROLL REPORT'.
           05  FILLER   PIC X(59) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER   PIC X(6) VALUE 'NUMBER'.
           05  FILLER   PIC X(14) VALUE SPACES.
           05  FILLER   PIC X(4) VALUE 'NAME'.
           05  FILLER   PIC X(14) VALUE SPACES.
           05  FILLER   PIC X(6) VALUE 'SALARY'.
           05  FILLER   PIC X(4) VALUE SPACES.
           05  FILLER   PIC X(10) VALUE 'COMMISSION'.
           05  FILLER   PIC X(4) VALUE SPACES.
           05  FILLER   PIC X(9) VALUE 'TOTAL PAY'.
           05  FILLER   PIC X(61) VALUE SPACES.
       01  REPORT-FOOTER.
           05  FILLER              PIC X(62) VALUE SPACES.
           05  FILLER              PIC X(5) VALUE 'PAGE '.
           05  PAGE-NUMBER-REPORT  PIC ZZ9.
           05  FILLER              PIC X(62) VALUE SPACES.
       

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
               MOVE PAGE-HEADER to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-ONE to REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER.
           MOVE SPACES TO REPORT-LINE
           MOVE SALESPERSON-NUMBER-INPUT
               TO SALESPERSON-NUMBER-REPORT
           MOVE SALESPERSON-NAME-INPUT TO SALESPERSON-NAME-REPORT
           MOVE SALARY-INPUT TO SALARY-REPORT
           MOVE COMMISSION-INPUT TO COMMISSION-REPORT
           MOVE SALARY-INPUT TO SALARY-WORK
           MOVE COMMISSION-INPUT TO COMMISSION-WORK
           ADD SALARY-WORK TO COMMISSION-WORK
               GIVING TOTAL-PAY-WORK
           MOVE TOTAL-PAY-WORK TO TOTAL-PAY-REPORT
           WRITE REPORT-LINE
               BEFORE ADVANCING 1 LINES
           ADD 1 TO LINE-NUMBER.
           IF LINE-NUMBER >= PAGE-LENGTH - 2
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   BEFORE ADVANCING 2 LINES
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
