       IDENTIFICATION DIVISION.

       PROGRAM-ID.    TAX-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 10, 2026.
       DATE-COMPILED. NOVEMBER 10, 2026.
       SECURITY.      CLASSIFIED.
           
      ******************************************************************
      *                                                                *
      * TAX-REPORT, A PROGRAM TO PRINT A TAX REPORT BASED ON THE DATA  *
      * IN "./lib/DATASET3.DAT"                                        *
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
             ASSIGN TO '/home/mmell/lib/DATASET3.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'TAX-REPORT.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 64 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  FILLER                      PIC X(6).
           05  PROPERTY-NUMBER-INPUT       PIC 9(4).
           05  FILLER                      PIC X(26).
           05  PROPERTY-OWNER-INPUT        PIC X(20).
           05  FILLER                      PIC X(2).
           05  ASSESSED-VALUE-INPUT        PIC 9(6).

       FD  REPORT-FILE
             RECORD CONTAINS 132 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  FILLER                     PIC X.
           05  PROPERTY-NUMBER-REPORT     PIC ZZZ9.
           05  FILLER                     PIC X(4).
           05  PROPERTY-OWNER-REPORT      PIC X(20).
           05  FILLER                     PIC X(5).
           05  ASSESSED-VALUE-REPORT      PIC ZZZ,ZZ9.
           05  FILLER                     PIC X(4).
           05  OLD-TAX-RATE-REPORT            PIC 9.99.
           05  FILLER                     PIC X VALUE '%'.
           05  FILLER                     PIC X(4).
           05  OLD-TAXES-REPORT           PIC $$,$$$.99.
           05  FILLER                     PIC X(5).
           05  NEW-TAX-RATE-REPORT            PIC 9.99.
           05  FILLER                     PIC X VALUE '%'.
           05  FILLER                     PIC X(3).
           05  NEW-TAXES-REPORT           PIC $$,$$$.99.
           05  FILLER                     PIC X(47).

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
           05  FILLER                       PIC X(61) VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE 'TAX REPORT'.
           05  FILLER                       PIC X(61) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER                       PIC X(6) VALUE 'NUMBER'.
           05  FILLER                       PIC X(10) VALUE SPACES.
           05  FILLER                       PIC X(5) VALUE 'OWNER'.
           05  FILLER                       PIC X(14) VALUE SPACES.
           05  FILLER                       PIC X(5) VALUE 'VALUE'.
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE 'OLD RATE'.
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  FILLER                       PIC X(9) VALUE 'OLD TAXES'.
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE 'NEW RATE'.
           05  FILLER                       PIC X(2) VALUE SPACES.
           05  FILLER                       PIC X(9) VALUE 'NEW TAXES'.
           05  FILLER                       PIC X(57) VALUE SPACES.
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
           05  ASSESSED-VALUE-WORK          PIC 9(6).
           05  OLD-TAX-RATE-WORK            PIC 9V99 VALUE 1.25.
           05  NEW-TAX-RATE-WORK            PIC 9V99 VALUE 0.75.
           05  OLD-TAXES-WORK               PIC 9(4)V99.
           05  NEW-TAXES-WORK               PIC 9(4)V99.

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
               WRITE REPORT-LINE BEFORE ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER.
      ******************************************************************
      *                                                                *
      * GENERATE ONE LINE OF THE ACTUAL REPORT HERE.                   *
      *                                                                *
      ******************************************************************
           
           MOVE ASSESSED-VALUE-INPUT TO ASSESSED-VALUE-WORK
           COMPUTE
          OLD-TAXES-WORK = ASSESSED-VALUE-WORK * OLD-TAX-RATE-WORK / 100
           COMPUTE
          NEW-TAXES-WORK = ASSESSED-VALUE-WORK * NEW-TAX-RATE-WORK / 100
           MOVE SPACES TO REPORT-LINE
           MOVE PROPERTY-NUMBER-INPUT TO PROPERTY-NUMBER-REPORT
           MOVE PROPERTY-OWNER-INPUT TO PROPERTY-OWNER-REPORT
           MOVE ASSESSED-VALUE-INPUT TO ASSESSED-VALUE-REPORT
           MOVE OLD-TAX-RATE-WORK TO OLD-TAX-RATE-REPORT
           MOVE OLD-TAXES-WORK TO OLD-TAXES-REPORT
           MOVE NEW-TAX-RATE-WORK TO NEW-TAX-RATE-REPORT
           MOVE NEW-TAXES-WORK TO NEW-TAXES-REPORT
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
