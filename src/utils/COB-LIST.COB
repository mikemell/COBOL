001001 IDENTIFICATION DIVISION.                                         COB-LIST
001002                                                                  COB-LIST
001003 PROGRAM-ID.    COB-LIST.                                         COB-LIST
001004 AUTHOR.        MICHAEL S. MELL.                                  COB-LIST
001005 INSTALLATION.  TT ODYSSEY, 41786AL.                              COB-LIST
001006 DATE-WRITTEN.  NOVEMBER 20, 2026.                                COB-LIST
001007 DATE-COMPILED. NOVEMBER 20, 2026.                                COB-LIST
001008 SECURITY.      UNCLASSIFIED.                                     COB-LIST
001009                                                                  COB-LIST
001010******************************************************************COB-LIST
001011*                                                                *COB-LIST
001012* COB-LIST, A PROGRAM TO (RE)NUMBER A ".cob" FILE AND SAVE THE   *COB-LIST
001013* LEGACY-STYLE NUMBERED CODE IN A FILE WITH A ".COB" EXTENSION.  *COB-LIST
001014*                                                                *COB-LIST
001015******************************************************************COB-LIST
001016/                                                                 COB-LIST
002001 ENVIRONMENT DIVISION.                                            COB-LIST
002002                                                                  COB-LIST
002003 CONFIGURATION SECTION.                                           COB-LIST
002004                                                                  COB-LIST
002005 SOURCE-COMPUTER. odysseus.                                       COB-LIST
002006 OBJECT-COMPUTER. Linux x86_64.                                   COB-LIST
002007                                                                  COB-LIST
002008 INPUT-OUTPUT SECTION.                                            COB-LIST
002009                                                                  COB-LIST
002010 FILE-CONTROL.                                                    COB-LIST
002011                                                                  COB-LIST
002012     SELECT INPUT-FILE                                            COB-LIST
002013       ASSIGN TO INPUT-FILE-SPECIFICATION                         COB-LIST
002014       ORGANIZATION IS LINE SEQUENTIAL                            COB-LIST
002015       FILE STATUS IS INPUT-FILE-STATUS.                          COB-LIST
002016     SELECT REPORT-FILE                                           COB-LIST
002017       ASSIGN TO REPORT-FILE-SPECIFICATION                        COB-LIST
002018       ORGANIZATION IS LINE SEQUENTIAL                            COB-LIST
002019       FILE STATUS IS REPORT-FILE-STATUS.                         COB-LIST
002020/                                                                 COB-LIST
003001 DATA DIVISION.                                                   COB-LIST
003002                                                                  COB-LIST
003003 FILE SECTION.                                                    COB-LIST
003004                                                                  COB-LIST
003005 FD  INPUT-FILE                                                   COB-LIST
003006       RECORD CONTAINS 80 CHARACTERS                              COB-LIST
003007       LABEL RECORDS ARE STANDARD                                 COB-LIST
003008       DATA RECORD IS INPUT-RECORD.                               COB-LIST
003009 01  INPUT-RECORD.                                                COB-LIST
003010     05  FILLER                        PIC X(6).                  COB-LIST
003011     05  COMMENT-INDICATOR-INPUT       PIC X(1).                  COB-LIST
003012     05  PROGRAM-AREA-INPUT            PIC X(65).                 COB-LIST
003013     05  FILLER                        PIC X(8).                  COB-LIST
003014                                                                  COB-LIST
003015 FD  REPORT-FILE                                                  COB-LIST
003016       RECORD CONTAINS 80 CHARACTERS                              COB-LIST
003017       LABEL RECORDS ARE OMITTED                                  COB-LIST
003018       DATA RECORD IS REPORT-LINE.                                COB-LIST
003019 01  REPORT-LINE.                                                 COB-LIST
003020     05  PAGE-NUMBER-REPORT            PIC 9(3).                  COB-LIST
003021     05  SEQUENCE-NUMBER-REPORT        PIC 9(3).                  COB-LIST
003022     05  COMMENT-INDICATOR-REPORT      PIC X(1).                  COB-LIST
003023     05  PROGRAM-AREA-A-REPORT         PIC X(4).                  COB-LIST
003024     05  PROGRAM-AREA-B-REPORT         PIC X(61).                 COB-LIST
003025     05  PROGRAM-NAME-REPORT           PIC X(8).                  COB-LIST
003026                                                                  COB-LIST
003027 WORKING-STORAGE SECTION.                                         COB-LIST
003028                                                                  COB-LIST
003029 01  PROGRAM-INDICATORS.                                          COB-LIST
003030     05  ARE-THERE-MORE-RECORDS        PIC X(3) VALUE 'YES'.      COB-LIST
003031                                                                  COB-LIST
003032 01  PROGRAM-COUNTERS.                                            COB-LIST
003033     05  PAGE-NUMBER-WORK              PIC 9(03).                 COB-LIST
003034     05  SEQUENCE-NUMBER-WORK          PIC 9(03).                 COB-LIST
003035                                                                  COB-LIST
003036 01  PROGRAM-VARIABLES.                                           COB-LIST
003037     05  USER-FILE-SPECIFICATION       PIC X(512).                COB-LIST
003038     05  INPUT-FILE-SPECIFICATION      PIC X(512).                COB-LIST
003039     05  INPUT-FILE-STATUS             PIC X(2).                  COB-LIST
003040     05  REPORT-FILE-SPECIFICATION     PIC X(512).                COB-LIST
003041     05  REPORT-FILE-STATUS            PIC X(2).                  COB-LIST
003042     05  COMMENT-INDICATOR-WORK        PIC X(01).                 COB-LIST
003043     05  PROGRAM-AREA-WORK             PIC X(65).                 COB-LIST
003044     05  PROGRAM-AREA-A-WORK           PIC X(04).                 COB-LIST
003045     05  PROGRAM-AREA-B-WORK           PIC X(61).                 COB-LIST
003046     05  PROGRAM-NAME-WORK             PIC X(08).                 COB-LIST
003047/                                                                 COB-LIST
004001 PROCEDURE DIVISION.                                              COB-LIST
004002                                                                  COB-LIST
004003 A000-CREATE-INVENTORY-LABELS.                                    COB-LIST
004004     DISPLAY 'COBOL PROGRAM BASE NAME? ' WITH NO ADVANCING.       COB-LIST
004005     ACCEPT USER-FILE-SPECIFICATION.                              COB-LIST
004006     STRING FUNCTION TRIM(USER-FILE-SPECIFICATION) ".cob"         COB-LIST
004007         INTO INPUT-FILE-SPECIFICATION.                           COB-LIST
004008     STRING FUNCTION TRIM(USER-FILE-SPECIFICATION) ".COB"         COB-LIST
004009         INTO REPORT-FILE-SPECIFICATION.                          COB-LIST
004010     MOVE USER-FILE-SPECIFICATION TO PROGRAM-NAME-WORK.           COB-LIST
004011     OPEN INPUT  INPUT-FILE                                       COB-LIST
004012          OUTPUT REPORT-FILE.                                     COB-LIST
004013     MOVE 1 TO PAGE-NUMBER-WORK.                                  COB-LIST
004014     MOVE 1 TO SEQUENCE-NUMBER-WORK.                              COB-LIST
004015     IF INPUT-FILE-STATUS NOT = "00"                              COB-LIST
004016         DISPLAY "FILE OPEN FAILED, STATUS " INPUT-FILE-STATUS    COB-LIST
004017         CLOSE INPUT-FILE                                         COB-LIST
004018             REPORT-FILE                                          COB-LIST
004019         CONTINUE.                                                COB-LIST
004020     READ INPUT-FILE                                              COB-LIST
004021         AT END                                                   COB-LIST
004022         MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.                    COB-LIST
004023     PERFORM A001-FORMAT-PRINT-LINE                               COB-LIST
004024         UNTIL ARE-THERE-MORE-RECORDS = 'NO '                     COB-LIST
004025     CLOSE INPUT-FILE                                             COB-LIST
004026           REPORT-FILE.                                           COB-LIST
004027     STOP RUN.                                                    COB-LIST
004028                                                                  COB-LIST
004029 A001-FORMAT-PRINT-LINE.                                          COB-LIST
004030     MOVE SPACES TO REPORT-LINE.                                  COB-LIST
004031     MOVE COMMENT-INDICATOR-INPUT                                 COB-LIST
004032         TO COMMENT-INDICATOR-WORK.                               COB-LIST
004033     MOVE PROGRAM-AREA-INPUT                                      COB-LIST
004034         TO PROGRAM-AREA-WORK.                                    COB-LIST
004035     MOVE PROGRAM-AREA-WORK(1:4)                                  COB-LIST
004036         TO PROGRAM-AREA-A-WORK.                                  COB-LIST
004037     MOVE PROGRAM-AREA-WORK(5:61)                                 COB-LIST
004038         TO PROGRAM-AREA-B-WORK.                                  COB-LIST
004039     MOVE PAGE-NUMBER-WORK TO PAGE-NUMBER-REPORT.                 COB-LIST
004040     IF COMMENT-INDICATOR-WORK = '/'                              COB-LIST
004041         ADD 1 TO PAGE-NUMBER-WORK.                               COB-LIST
004042     MOVE SEQUENCE-NUMBER-WORK TO SEQUENCE-NUMBER-REPORT.         COB-LIST
004043     ADD 1 TO SEQUENCE-NUMBER-WORK.                               COB-LIST
004044     IF COMMENT-INDICATOR-WORK = "/"                              COB-LIST
004045             MOVE 1 TO SEQUENCE-NUMBER-WORK.                      COB-LIST
004046     MOVE COMMENT-INDICATOR-INPUT TO COMMENT-INDICATOR-REPORT.    COB-LIST
004047     MOVE PROGRAM-AREA-A-WORK TO PROGRAM-AREA-A-REPORT.           COB-LIST
004048     MOVE PROGRAM-AREA-B-WORK TO PROGRAM-AREA-B-REPORT.           COB-LIST
004049     MOVE PROGRAM-NAME-WORK TO PROGRAM-NAME-REPORT.               COB-LIST
004050     WRITE REPORT-LINE                                            COB-LIST
004051         BEFORE ADVANCING 1 LINES                                 COB-LIST
004052     READ INPUT-FILE                                              COB-LIST
004053           AT END                                                 COB-LIST
004054              MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.               COB-LIST
