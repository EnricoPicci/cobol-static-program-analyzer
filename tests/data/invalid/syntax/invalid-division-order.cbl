      *> Syntax error - wrong division order
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRONG-ORDER.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "This should come after DATA DIVISION".
           STOP RUN.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WRONG-PLACE PIC X(10).