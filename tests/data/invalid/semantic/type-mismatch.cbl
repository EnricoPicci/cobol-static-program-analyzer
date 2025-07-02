      *> Semantic error - type mismatch
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TYPE-MISMATCH.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMERIC-VAR PIC 9(5).
       01 ALPHA-VAR   PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE ALPHA-VAR TO NUMERIC-VAR.
           ADD ALPHA-VAR TO NUMERIC-VAR.
           STOP RUN.