       IDENTIFICATION DIVISION.

       PROGRAM-ID.    ProgName [IS COMMON|INITIAL].
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  Odyssey.
       DATE-WRITTEN.  MM/DD/YY.
       DATE-COMPILED. MM/DD/YY. HH:MM:SS.
       SECURITY.      [UNCLAS|FOUO|CONFIDENTIAL|SECRET|TOP SECRET].

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

       SOURCE-COMPUTER. AMD64 *> also known as x86_64
       OBJECT-COMPUTER. AMD64 *> also known as x86_64
      *SPECIAL-NAMES. special names entries

       INPUT-OUTPUT SECTION.
       FILE-CONTROL. file control entries
       I-O-CONTROL. I/O control entries
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
           STOP RUN.
