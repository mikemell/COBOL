cobc -x <source filespec> [-o <output filespec>]

COBOL code column formatting:
        1-6     Sequence #, leave blank (deprecated)
        7       (continuation, if '-' leave Area A blank, begin at Area B)
        8-11    Area A
        12-72   Area B
        73-80   Program identification (deprecated)
A comment line is any line with an asterisk (*) or slash (/) in the indicator
area (column 7) of the line, or with a floating comment indicator (*>) as the
first character-string in the program text area (Area A plus Area B).

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
