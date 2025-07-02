      *> Complex PERFORM logic with nested operations
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTED-PERFORMS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTERS.
           05 OUTER-COUNTER PIC 9(3) VALUE 1.
           05 INNER-COUNTER PIC 9(3) VALUE 1.
       01 RESULT-TABLE.
           05 RESULT-ENTRY OCCURS 10 TIMES.
               10 ENTRY-VALUE PIC 9(4).
       
       PROCEDURE DIVISION.
       MAIN-SECTION SECTION.
       MAIN-PARA.
           PERFORM PROCESS-OUTER-LOOP 
               VARYING OUTER-COUNTER FROM 1 BY 1 
               UNTIL OUTER-COUNTER > 5.
           
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       PROCESS-OUTER-LOOP.
           DISPLAY "Processing outer loop: " OUTER-COUNTER.
           PERFORM PROCESS-INNER-LOOP
               VARYING INNER-COUNTER FROM 1 BY 1
               UNTIL INNER-COUNTER > 2.
       
       PROCESS-INNER-LOOP.
           COMPUTE RESULT-ENTRY(OUTER-COUNTER) = 
               OUTER-COUNTER * INNER-COUNTER.
           DISPLAY "  Inner loop: " INNER-COUNTER 
               " Result: " RESULT-ENTRY(OUTER-COUNTER).
       
       DISPLAY-RESULTS.
           DISPLAY "Final Results:".
           PERFORM SHOW-RESULT 
               VARYING OUTER-COUNTER FROM 1 BY 1
               UNTIL OUTER-COUNTER > 5.
       
       SHOW-RESULT.
           DISPLAY "Entry " OUTER-COUNTER ": " 
               RESULT-ENTRY(OUTER-COUNTER).