      *> Basic arithmetic operations
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARITHMETIC-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1     PIC 9(3) VALUE 100.
       01 NUM2     PIC 9(3) VALUE 50.
       01 RESULT   PIC 9(4).
       01 RESULT-DISPLAY PIC Z(4).
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           ADD NUM1 TO NUM2 GIVING RESULT.
           MOVE RESULT TO RESULT-DISPLAY.
           DISPLAY "Addition Result: " RESULT-DISPLAY.
           
           SUBTRACT NUM2 FROM NUM1 GIVING RESULT.
           MOVE RESULT TO RESULT-DISPLAY.
           DISPLAY "Subtraction Result: " RESULT-DISPLAY.
           
           MULTIPLY NUM1 BY NUM2 GIVING RESULT.
           MOVE RESULT TO RESULT-DISPLAY.
           DISPLAY "Multiplication Result: " RESULT-DISPLAY.
           
           DIVIDE NUM1 BY NUM2 GIVING RESULT.
           MOVE RESULT TO RESULT-DISPLAY.
           DISPLAY "Division Result: " RESULT-DISPLAY.
           
           STOP RUN.