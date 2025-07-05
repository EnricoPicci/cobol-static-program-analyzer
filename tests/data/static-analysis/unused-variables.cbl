      *> Unused Variables Test Cases
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNUSED-VARIABLES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USED-VARIABLES.
           05 COUNTER           PIC 9(5) VALUE ZERO.
           05 TOTAL-AMOUNT      PIC 9(7)V99 VALUE ZERO.
           05 PROCESS-FLAG      PIC X VALUE 'Y'.
           05 USER-NAME         PIC X(30) VALUE SPACES.
       
       01 UNUSED-VARIABLES.
           05 UNUSED-COUNTER    PIC 9(3) VALUE ZERO.
           05 UNUSED-FLAG       PIC X VALUE 'N'.
           05 UNUSED-AMOUNT     PIC 9(5)V99 VALUE ZERO.
           05 UNUSED-STRING     PIC X(50) VALUE SPACES.
       
       01 PARTIALLY-USED-VARIABLES.
           05 WRITE-ONLY-VAR    PIC X(10) VALUE SPACES.
           05 READ-ONLY-VAR     PIC X(10) VALUE "CONSTANT".
           05 UNINITIALIZED-VAR PIC X(10).
       
       01 COMPLEX-STRUCTURE.
           05 USED-PARENT       PIC X(20) VALUE SPACES.
           05 UNUSED-PARENT     PIC X(20) VALUE SPACES.
           05 MIXED-PARENT.
               10 USED-CHILD    PIC X(10) VALUE SPACES.
               10 UNUSED-CHILD  PIC X(10) VALUE SPACES.
       
       01 ARRAY-VARIABLES.
           05 USED-ARRAY        PIC X(10) OCCURS 10 TIMES.
           05 UNUSED-ARRAY      PIC X(10) OCCURS 5 TIMES.
           05 PARTIALLY-USED-ARRAY PIC X(10) OCCURS 3 TIMES.
       
       01 REDEFINES-VARIABLES.
           05 BASE-VARIABLE     PIC X(10) VALUE SPACES.
           05 REDEFINED-VAR     REDEFINES BASE-VARIABLE PIC 9(10).
           05 UNUSED-BASE       PIC X(10) VALUE SPACES.
           05 UNUSED-REDEFINE   REDEFINES UNUSED-BASE PIC 9(10).
       
       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-DATA.
           PERFORM 300-DISPLAY-RESULTS.
           STOP RUN.
       
       100-INITIALIZE.
           MOVE ZERO TO COUNTER.
           MOVE ZERO TO TOTAL-AMOUNT.
           MOVE 'Y' TO PROCESS-FLAG.
           MOVE "JOHN DOE" TO USER-NAME.
       
       200-PROCESS-DATA.
           PERFORM 210-CALCULATE-TOTAL.
           PERFORM 220-VALIDATE-DATA.
           PERFORM 230-PROCESS-ARRAYS.
           PERFORM 240-HANDLE-REDEFINES.
       
       210-CALCULATE-TOTAL.
           ADD 1 TO COUNTER.
           ADD 100.50 TO TOTAL-AMOUNT.
           
           *> Write-only variable - never read
           MOVE "WRITTEN" TO WRITE-ONLY-VAR.
           
           *> Read uninitialized variable
           DISPLAY "Uninitialized: " UNINITIALIZED-VAR.
       
       220-VALIDATE-DATA.
           IF PROCESS-FLAG = 'Y'
               DISPLAY "Processing for user: " USER-NAME
           END-IF.
           
           *> Read-only variable - never written after initialization
           DISPLAY "Read only: " READ-ONLY-VAR.
       
       230-PROCESS-ARRAYS.
           *> Use only first element of array
           MOVE "ELEMENT1" TO USED-ARRAY(1).
           DISPLAY "First element: " USED-ARRAY(1).
           
           *> Use only some elements of partially used array
           MOVE "PARTIAL1" TO PARTIALLY-USED-ARRAY(1).
           MOVE "PARTIAL2" TO PARTIALLY-USED-ARRAY(2).
           DISPLAY "Partial elements: " PARTIALLY-USED-ARRAY(1) " " PARTIALLY-USED-ARRAY(2).
       
       240-HANDLE-REDEFINES.
           *> Use both base and redefined variables
           MOVE "ABCDEFGHIJ" TO BASE-VARIABLE.
           DISPLAY "Redefined as number: " REDEFINED-VAR.
       
       300-DISPLAY-RESULTS.
           DISPLAY "Counter: " COUNTER.
           DISPLAY "Total: " TOTAL-AMOUNT.
           
           *> Use complex structure partially
           MOVE "USED PARENT" TO USED-PARENT.
           MOVE "USED CHILD" TO USED-CHILD.
           DISPLAY "Used parent: " USED-PARENT.
           DISPLAY "Used child: " USED-CHILD.
       
       *> Additional paragraphs for testing
       400-UNUSED-PROCESSING.
           *> This paragraph is never called, so variables used here
           *> might still be considered unused in the overall program
           MOVE "TEMP" TO UNUSED-STRING.
           ADD 1 TO UNUSED-COUNTER.
           
       500-VARIABLE-SCOPE-TEST.
           *> Test variables used in different scopes
           PERFORM 510-NESTED-SCOPE.
           
       510-NESTED-SCOPE.
           *> Variables used in nested scopes
           MOVE "NESTED" TO WRITE-ONLY-VAR.
           DISPLAY "Nested access: " READ-ONLY-VAR.