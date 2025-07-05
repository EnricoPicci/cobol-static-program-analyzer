      *> GO TO Targets and Infinite Loops Test Cases
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOTO-AND-LOOPS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONTROL-VARIABLES.
           05 CHOICE-NUMBER     PIC 9 VALUE 1.
           05 LOOP-COUNTER      PIC 9(3) VALUE ZERO.
           05 MAX-LOOPS         PIC 9(3) VALUE 10.
           05 CONDITION-FLAG    PIC X VALUE 'Y'.
       
       01 TEST-VARIABLES.
           05 TEST-RESULT       PIC X(10) VALUE SPACES.
           05 ERROR-CODE        PIC 9(3) VALUE ZERO.
           05 RETRY-COUNT       PIC 9(2) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE.
           PERFORM 200-GOTO-TESTS.
           PERFORM 300-LOOP-TESTS.
           STOP RUN.
       
       100-INITIALIZE.
           MOVE ZERO TO LOOP-COUNTER.
           MOVE 'Y' TO CONDITION-FLAG.
           DISPLAY "Starting GO TO and loop tests...".
       
       200-GOTO-TESTS.
           PERFORM 210-SIMPLE-GOTO.
           PERFORM 220-COMPUTED-GOTO.
           PERFORM 230-CONDITIONAL-GOTO.
           PERFORM 240-MISSING-TARGET-GOTO.
       
       *> SIMPLE GO TO TESTS
       210-SIMPLE-GOTO.
           DISPLAY "Simple GO TO test".
           GO TO 211-GOTO-TARGET.
       
       211-GOTO-TARGET.
           DISPLAY "Reached GO TO target".
           GO TO 212-GOTO-END.
       
       212-GOTO-END.
           DISPLAY "Simple GO TO test completed".
       
       *> COMPUTED GO TO TESTS
       220-COMPUTED-GOTO.
           DISPLAY "Computed GO TO test".
           MOVE 2 TO CHOICE-NUMBER.
           GO TO 221-CHOICE-ONE 222-CHOICE-TWO 223-CHOICE-THREE
               DEPENDING ON CHOICE-NUMBER.
           GO TO 224-GOTO-ERROR.
       
       221-CHOICE-ONE.
           DISPLAY "Choice One selected".
           GO TO 224-COMPUTED-END.
       
       222-CHOICE-TWO.
           DISPLAY "Choice Two selected".
           GO TO 224-COMPUTED-END.
       
       223-CHOICE-THREE.
           DISPLAY "Choice Three selected".
           GO TO 224-COMPUTED-END.
       
       224-COMPUTED-END.
           DISPLAY "Computed GO TO test completed".
           GO TO 225-GOTO-CONTINUE.
       
       224-GOTO-ERROR.
           DISPLAY "GO TO error case".
           GO TO 225-GOTO-CONTINUE.
       
       225-GOTO-CONTINUE.
           DISPLAY "Continuing after computed GO TO".
       
       *> CONDITIONAL GO TO TESTS
       230-CONDITIONAL-GOTO.
           DISPLAY "Conditional GO TO test".
           IF CONDITION-FLAG = 'Y'
               GO TO 231-CONDITION-TRUE
           ELSE
               GO TO 232-CONDITION-FALSE
           END-IF.
       
       231-CONDITION-TRUE.
           DISPLAY "Condition was true".
           GO TO 233-CONDITIONAL-END.
       
       232-CONDITION-FALSE.
           DISPLAY "Condition was false".
           GO TO 233-CONDITIONAL-END.
       
       233-CONDITIONAL-END.
           DISPLAY "Conditional GO TO test completed".
       
       *> MISSING TARGET GO TO TESTS
       240-MISSING-TARGET-GOTO.
           DISPLAY "Testing missing GO TO targets".
           *> These GO TO statements reference non-existent targets
           IF ERROR-CODE = 999
               GO TO NONEXISTENT-TARGET
           END-IF.
           
           IF ERROR-CODE = 998
               GO TO ANOTHER-MISSING-TARGET
           END-IF.
           
           IF ERROR-CODE = 997
               GO TO 999-UNDEFINED-PARAGRAPH
           END-IF.
       
       *> LOOP TESTS
       300-LOOP-TESTS.
           PERFORM 310-SIMPLE-LOOPS.
           PERFORM 320-INFINITE-LOOP-RISKS.
           PERFORM 330-COMPLEX-LOOP-PATTERNS.
       
       310-SIMPLE-LOOPS.
           DISPLAY "Simple loop tests".
           MOVE ZERO TO LOOP-COUNTER.
           PERFORM 311-LOOP-BODY UNTIL LOOP-COUNTER >= MAX-LOOPS.
       
       311-LOOP-BODY.
           ADD 1 TO LOOP-COUNTER.
           DISPLAY "Loop iteration: " LOOP-COUNTER.
       
       *> INFINITE LOOP RISKS
       320-INFINITE-LOOP-RISKS.
           DISPLAY "Testing infinite loop risks".
           PERFORM 321-RISKY-LOOP.
       
       321-RISKY-LOOP.
           DISPLAY "In risky loop".
           ADD 1 TO LOOP-COUNTER.
           *> This could be infinite if MAX-LOOPS is never reached
           IF LOOP-COUNTER < MAX-LOOPS
               PERFORM 321-RISKY-LOOP
           END-IF.
       
       *> COMPLEX LOOP PATTERNS
       330-COMPLEX-LOOP-PATTERNS.
           DISPLAY "Complex loop patterns".
           PERFORM 331-NESTED-LOOPS.
           PERFORM 332-GOTO-LOOPS.
       
       331-NESTED-LOOPS.
           DISPLAY "Nested loops test".
           MOVE ZERO TO LOOP-COUNTER.
           PERFORM 331-OUTER-LOOP 3 TIMES.
       
       331-OUTER-LOOP.
           DISPLAY "Outer loop iteration".
           ADD 1 TO LOOP-COUNTER.
           PERFORM 331-INNER-LOOP 2 TIMES.
       
       331-INNER-LOOP.
           DISPLAY "Inner loop iteration".
           ADD 1 TO LOOP-COUNTER.
       
       332-GOTO-LOOPS.
           DISPLAY "GO TO loop patterns".
           MOVE ZERO TO LOOP-COUNTER.
           GO TO 332-LOOP-START.
       
       332-LOOP-START.
           ADD 1 TO LOOP-COUNTER.
           DISPLAY "GO TO loop iteration: " LOOP-COUNTER.
           IF LOOP-COUNTER < 5
               GO TO 332-LOOP-START
           END-IF.
           GO TO 332-LOOP-END.
       
       332-LOOP-END.
           DISPLAY "GO TO loop completed".
       
       *> SECTION-LEVEL PATTERNS
       GOTO-SECTION-TESTS SECTION.
       400-SECTION-GOTO-TESTS.
           DISPLAY "Section-level GO TO tests".
           GO TO 410-SECTION-TARGET.
       
       410-SECTION-TARGET.
           DISPLAY "Reached section target".
           PERFORM 420-SECTION-PERFORM.
       
       420-SECTION-PERFORM.
           DISPLAY "Section perform test".
           GO TO 430-SECTION-END.
       
       430-SECTION-END.
           DISPLAY "Section tests completed".
       
       *> CROSS-SECTION GO TO
       CROSS-SECTION-TESTS SECTION.
       500-CROSS-SECTION-START.
           DISPLAY "Cross-section GO TO test".
           GO TO 410-SECTION-TARGET.
       
       *> ERROR HANDLING PATTERNS
       ERROR-HANDLING-SECTION SECTION.
       600-ERROR-HANDLER.
           DISPLAY "Error handling with GO TO".
           GO TO 610-ERROR-PROCESSING.
       
       610-ERROR-PROCESSING.
           ADD 1 TO ERROR-CODE.
           IF ERROR-CODE > 10
               GO TO 620-ERROR-RECOVERY
           ELSE
               GO TO 630-ERROR-RETRY
           END-IF.
       
       620-ERROR-RECOVERY.
           DISPLAY "Error recovery mode".
           GO TO 640-ERROR-END.
       
       630-ERROR-RETRY.
           DISPLAY "Error retry mode".
           ADD 1 TO RETRY-COUNT.
           IF RETRY-COUNT < 3
               GO TO 610-ERROR-PROCESSING
           ELSE
               GO TO 620-ERROR-RECOVERY
           END-IF.
       
       640-ERROR-END.
           DISPLAY "Error handling completed".
       
       *> POTENTIAL INFINITE LOOPS WITH GO TO
       INFINITE-PATTERNS-SECTION SECTION.
       700-INFINITE-GOTO.
           DISPLAY "Potential infinite GO TO".
           ADD 1 TO LOOP-COUNTER.
           *> This could be infinite without proper exit condition
           IF LOOP-COUNTER < 1000
               GO TO 700-INFINITE-GOTO
           END-IF.
       
       710-CIRCULAR-GOTO.
           DISPLAY "Circular GO TO pattern".
           GO TO 711-GOTO-MIDDLE.
       
       711-GOTO-MIDDLE.
           DISPLAY "Middle of circular GO TO".
           GO TO 712-GOTO-END.
       
       712-GOTO-END.
           DISPLAY "End of circular GO TO".
           *> This creates a circular pattern
           GO TO 710-CIRCULAR-GOTO.
       
       *> UNREACHABLE CODE AFTER GO TO
       UNREACHABLE-SECTION SECTION.
       800-UNREACHABLE-CODE.
           DISPLAY "Before GO TO".
           GO TO 810-GOTO-DESTINATION.
           
           *> This code is unreachable
           DISPLAY "This will never be displayed".
           MOVE 'X' TO TEST-RESULT.
           ADD 1 TO ERROR-CODE.
       
       810-GOTO-DESTINATION.
           DISPLAY "Reached GO TO destination".
       
       *> GOTO with missing targets in different contexts
       MISSING-TARGETS-SECTION SECTION.
       900-MISSING-TARGETS.
           DISPLAY "Testing various missing targets".
           
           *> Missing paragraph target
           IF ERROR-CODE = 1
               GO TO MISSING-PARAGRAPH
           END-IF.
           
           *> Missing section target  
           IF ERROR-CODE = 2
               GO TO MISSING-SECTION
           END-IF.
           
           *> Malformed target names
           IF ERROR-CODE = 3
               GO TO 123-INVALID-NAME
           END-IF.
           
           *> Empty GO TO
           IF ERROR-CODE = 4
               GO TO
           END-IF.