       IDENTIFICATION DIVISION.

       PROGRAM-ID.    FORMAT
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  Odyssey.
       DATE-WRITTEN.  .
       DATE-COMPILED. .
       SECURITY.      UNCLASSIFIED.

      ******************************************************************
      * Columns 1-6     Sequence number area (ppplll)                  *
      * Column 7        Indicator area ("*" = comment,- "=" cont.,     *
      *                   "/" = pagefeed listing, "D" = debug)         *
      * Columns 8-11    Area a                                         *
      *                   The following items must begin in Area A:    *
      *                       Division headers                         *
      *                       Section headers                          *
      *                       Paragraph headers or paragraph names     *
      *                       Level indicators/numbers (01 and 77)     *
      *                       DECLARATIVES and END DECLARATIVES        *
      *                       End program/class/method markers         *
      * Columns 12-72   Area b                                         *
      *                   The following items must begin in Area B:    *
      *                       Entries/sentences/statements/clauses     *
      *                       Continuation lines                       *
      *                       Floating comments identified by "*>"     *
      * Columns 73-80   reserved (system generated number)             *
      ******************************************************************
      /
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. homer WITH DEBUGGING MODE.
       OBJECT-COMPUTER. odysseus.
      *[SPECIAL-NAMES.   *> ex., "CURRENCY SIGN IS "$".]

       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
      *     [SELECT [OPTIONAL] logical-filename ASSIGN TO DSName]
      *     [ORGANIZATION IS file-organization]
      *     [ACCESS MODE IS access-mode]
      *     [RECORD KEY IS record-key]
      *     [ALTERNATE RECORD KEY IS alt-record-key WITH DUPLICATES]
      *     [RELATIVE KEY IS relative-key]
      *     [RECORD DELIMITER IS record-delimiter]
      *     [FILE STATUS IS file-status].
       I-O-CONTROL.
      *     [i-o-control-entries]
      /
       DATA DIVISION.
       FILE SECTION.
      *    FD|SD file-description-entry...
      *    data-description-entry...
       WORKING-STORAGE SECTION.
      *        data-description-entry...
       LINKAGE SECTION.
      *        data-description-entry...
      /
       PROCEDURE DIVISION.
      *section-name SECTION.
      *paragraph-name.
      D    DISPLAY "This is a DEBUG display line."
           DISPLAY "This is a NORMAL display line."
           STOP RUN.
