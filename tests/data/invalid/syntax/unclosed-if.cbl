      *> Syntax error - unclosed IF statement
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNCLOSED-IF.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEST-VALUE PIC 9 VALUE 5.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           IF TEST-VALUE > 0
               DISPLAY "Value is positive"
           *> Missing END-IF
           STOP RUN.