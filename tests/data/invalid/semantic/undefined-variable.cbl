      *> Semantic error - undefined variable reference
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNDEFINED-VAR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DEFINED-VAR PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "TEST" TO UNDEFINED-VAR.
           DISPLAY DEFINED-VAR.
           STOP RUN.