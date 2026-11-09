       IDENTIFICATION DIVISION.

       PROGRAM-ID.    USER-REPORT.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  NOVEMBER 8, 2026.
       DATE-COMPILED. NOVEMBER 8, 2026.
       SECURITY.      FOR OFFICIAL USE ONLY.
           
      ******************************************************************
      *                                                                *
      * USER-RPT, A PROGRAM TO PRINT A SALES REPORT BASED ON THE DATA  *
      * IN "./lib/DATASET4.dat"                                        *
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
             ASSIGN TO '/home/mmell/lib/DATASET4.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE
             ASSIGN TO 'USER-REPORT.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
      /
       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 73 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  CRT-NUMBER-INPUT              PICTURE X(2).
           05  FILLER                        PICTURE X(31).
           05  CONNECT-DATE-MM-INPUT         PICTURE X(2).
           05  CONNECT-DATE-DD-INPUT         PICTURE X(2).
           05  CONNECT-DATE-YY-INPUT         PICTURE X(2).
           05  USER-NAME-A-INPUT             PICTURE X(4).
           05  USER-NAME-B-INPUT             PICTURE X(16).
           05  FILLER                        PICTURE X(14).

       FD  REPORT-FILE
             RECORD CONTAINS 80 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  UID-FIRST-FIELD-REPORT        PICTURE X(4).
           05  UID-SECOND-FIELD-A-REPORT     PICTURE X(2).
           05  UID-SECOND-FIELD-B-REPORT     PICTURE X(2).
           05  UID-THIRD-FIELD-REPORT        PICTURE X(2).
           05  FILLER                        PICTURE X(6).
           05  USER-NAME-A-REPORT            PICTURE X(4).
           05  USER-NAME-B-REPORT            PICTURE X(16).
           05  FILLER                        PICTURE X(6).
           05  CONNECT-DATE-MM-REPORT        PICTURE X(2).
           05  FILLER                        PICTURE X(1).
           05  CONNECT-DATE-DD-REPORT        PICTURE X(2).
           05  FILLER                        PICTURE X(1).
           05  CONNECT-DATE-YY-REPORT        PICTURE X(2).
           05  FILLER                        PICTURE X(6).
           05  CRT-NUMBER-REPORT             PICTURE X(2).
           05  FILLER                        PICTURE X(22).
      /
       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  PAGE-LENGTH                   PICTURE 9(2) VALUE 66.
           05  LINE-NUMBER                   PICTURE 9(2) VALUE 1.
           05  ARE-THERE-MORE-RECORDS        PICTURE X(3) VALUE 'YES'.
       01  REPORT-TITLE.
           05  FILLER                        PICTURE X(34) VALUE SPACES.
           05  FILLER                PICTURE X(12) VALUE 'SALES REPORT'.
           05  FILLER                        PICTURE X(34) VALUE SPACES.
       01  COLUMN-HEADER-LINE-ONE.
           05  FILLER                 PICTURE X VALUE IS SPACE.
           05  FILLER                 PICTURE X(7) VALUE 'USER ID'.
           05  FILLER                 PICTURE X(13) VALUE SPACES.
           05  FILLER                 PICTURE X(9) VALUE 'USER NAME'.
           05  FILLER                 PICTURE X(14) VALUE SPACES.
           05  FILLER                 PICTURE X(4) VALUE 'DATE'.
           05  FILLER                 PICTURE X(7) VALUE SPACES.
           05  FILLER                 PICTURE X(3) VALUE 'CRT'.
           05  FILLER                 PICTURE X(21) VALUE SPACES.

       PROCEDURE DIVISION.

       A000-CREATE-USER-REPORT.
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
                   AFTER ADVANCING 6 LINES
               ADD 6 TO LINE-NUMBER
               MOVE COLUMN-HEADER-LINE-ONE to REPORT-LINE
               WRITE REPORT-LINE
                   AFTER ADVANCING 2 LINES
               ADD 2 TO LINE-NUMBER
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   AFTER ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER
           ELSE
               MOVE SPACES TO REPORT-LINE
               MOVE USER-NAME-A-INPUT TO UID-FIRST-FIELD-REPORT
               MOVE CONNECT-DATE-DD-INPUT TO UID-SECOND-FIELD-A-REPORT
               MOVE CONNECT-DATE-YY-INPUT TO UID-SECOND-FIELD-B-REPORT
               MOVE CRT-NUMBER-INPUT TO UID-THIRD-FIELD-REPORT
               MOVE USER-NAME-A-INPUT TO USER-NAME-A-REPORT
               MOVE USER-NAME-B-INPUT TO USER-NAME-B-REPORT
               MOVE CONNECT-DATE-MM-INPUT TO CONNECT-DATE-MM-REPORT
               MOVE CONNECT-DATE-DD-INPUT TO CONNECT-DATE-DD-REPORT
               MOVE CONNECT-DATE-YY-INPUT TO CONNECT-DATE-YY-REPORT
               MOVE CRT-NUMBER-INPUT TO CRT-NUMBER-REPORT
               WRITE REPORT-LINE
                   AFTER ADVANCING 1 LINES
               ADD 1 TO LINE-NUMBER.
           IF LINE-NUMBER >= PAGE-LENGTH - 3
               MOVE SPACES TO REPORT-LINE
               WRITE REPORT-LINE
                   AFTER ADVANCING 4 LINES
               MOVE 1 TO LINE-NUMBER.
           READ INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
