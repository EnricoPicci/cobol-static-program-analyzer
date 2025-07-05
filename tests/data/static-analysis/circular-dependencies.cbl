      *> Circular Dependencies Test Cases
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRCULAR-DEPENDENCIES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONTROL-VARIABLES.
           05 LOOP-COUNTER      PIC 9(3) VALUE ZERO.
           05 MAX-ITERATIONS    PIC 9(3) VALUE 100.
           05 BREAK-FLAG        PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE.
           PERFORM 200-DEMONSTRATE-PATTERNS.
           STOP RUN.
       
       100-INITIALIZE.
           MOVE ZERO TO LOOP-COUNTER.
           MOVE 'N' TO BREAK-FLAG.
           DISPLAY "Demonstrating circular dependency patterns...".
       
       200-DEMONSTRATE-PATTERNS.
           PERFORM 210-SIMPLE-CIRCULAR.
           PERFORM 220-COMPLEX-CIRCULAR.
           PERFORM 230-SECTION-CIRCULAR.
           PERFORM 240-MIXED-CIRCULAR.
       
       *> SIMPLE CIRCULAR DEPENDENCY - Direct A -> B -> A
       210-SIMPLE-CIRCULAR.
           DISPLAY "Starting simple circular test".
           PERFORM 211-CIRCULAR-A.
       
       211-CIRCULAR-A.
           DISPLAY "In Circular A".
           ADD 1 TO LOOP-COUNTER.
           IF LOOP-COUNTER < 3
               PERFORM 212-CIRCULAR-B
           END-IF.
       
       212-CIRCULAR-B.
           DISPLAY "In Circular B".
           IF LOOP-COUNTER < 2
               PERFORM 211-CIRCULAR-A
           END-IF.
       
       *> COMPLEX CIRCULAR DEPENDENCY - A -> B -> C -> D -> A
       220-COMPLEX-CIRCULAR.
           DISPLAY "Starting complex circular test".
           MOVE ZERO TO LOOP-COUNTER.
           PERFORM 221-COMPLEX-A.
       
       221-COMPLEX-A.
           DISPLAY "In Complex A".
           ADD 1 TO LOOP-COUNTER.
           IF LOOP-COUNTER < 5
               PERFORM 222-COMPLEX-B
           END-IF.
       
       222-COMPLEX-B.
           DISPLAY "In Complex B".
           IF LOOP-COUNTER < 4
               PERFORM 223-COMPLEX-C
           END-IF.
       
       223-COMPLEX-C.
           DISPLAY "In Complex C".
           IF LOOP-COUNTER < 3
               PERFORM 224-COMPLEX-D
           END-IF.
       
       224-COMPLEX-D.
           DISPLAY "In Complex D".
           IF LOOP-COUNTER < 2
               PERFORM 221-COMPLEX-A
           END-IF.
       
       *> SECTION-LEVEL CIRCULAR DEPENDENCIES
       230-SECTION-CIRCULAR.
           DISPLAY "Starting section circular test".
           PERFORM 300-SECTION-A.
       
       SECTION-A-CIRCULAR SECTION.
       300-SECTION-A.
           DISPLAY "In Section A".
           PERFORM 310-SECTION-A-PARA.
       
       310-SECTION-A-PARA.
           DISPLAY "In Section A Para".
           PERFORM 400-SECTION-B.
       
       SECTION-B-CIRCULAR SECTION.
       400-SECTION-B.
           DISPLAY "In Section B".
           PERFORM 410-SECTION-B-PARA.
       
       410-SECTION-B-PARA.
           DISPLAY "In Section B Para".
           *> This creates circular dependency back to Section A
           PERFORM 300-SECTION-A.
       
       *> MIXED CIRCULAR - Combination of PERFORM, GO TO, and CALL
       240-MIXED-CIRCULAR.
           DISPLAY "Starting mixed circular test".
           MOVE ZERO TO LOOP-COUNTER.
           PERFORM 241-MIXED-A.
       
       241-MIXED-A.
           DISPLAY "In Mixed A".
           ADD 1 TO LOOP-COUNTER.
           IF LOOP-COUNTER < 3
               GO TO 242-MIXED-B
           END-IF.
           GO TO 244-MIXED-EXIT.
       
       242-MIXED-B.
           DISPLAY "In Mixed B".
           PERFORM 243-MIXED-C.
           GO TO 244-MIXED-EXIT.
       
       243-MIXED-C.
           DISPLAY "In Mixed C".
           IF LOOP-COUNTER < 2
               PERFORM 241-MIXED-A
           END-IF.
       
       244-MIXED-EXIT.
           DISPLAY "Exiting mixed circular test".
       
       *> SELF-REFERENCING PARAGRAPHS - Direct infinite loop potential
       SELF-REFERENCE-SECTION SECTION.
       500-SELF-REFERENCING.
           DISPLAY "In self-referencing paragraph".
           ADD 1 TO LOOP-COUNTER.
           IF LOOP-COUNTER < MAX-ITERATIONS
               PERFORM 500-SELF-REFERENCING
           END-IF.
       
       510-DIRECT-SELF-CALL.
           DISPLAY "Direct self call".
           *> This would create infinite loop without condition
           IF BREAK-FLAG = 'N'
               MOVE 'Y' TO BREAK-FLAG
               PERFORM 510-DIRECT-SELF-CALL
           END-IF.
       
       *> MULTIPLE CIRCULAR CHAINS
       MULTIPLE-CIRCULAR-SECTION SECTION.
       600-CHAIN-ONE-START.
           DISPLAY "Chain One Start".
           PERFORM 601-CHAIN-ONE-MIDDLE.
       
       601-CHAIN-ONE-MIDDLE.
           DISPLAY "Chain One Middle".
           PERFORM 602-CHAIN-ONE-END.
       
       602-CHAIN-ONE-END.
           DISPLAY "Chain One End".
           PERFORM 600-CHAIN-ONE-START.
       
       610-CHAIN-TWO-START.
           DISPLAY "Chain Two Start".
           PERFORM 611-CHAIN-TWO-MIDDLE.
       
       611-CHAIN-TWO-MIDDLE.
           DISPLAY "Chain Two Middle".
           PERFORM 612-CHAIN-TWO-END.
       
       612-CHAIN-TWO-END.
           DISPLAY "Chain Two End".
           PERFORM 610-CHAIN-TWO-START.
       
       *> CONDITIONAL CIRCULAR DEPENDENCIES
       CONDITIONAL-CIRCULAR-SECTION SECTION.
       700-CONDITIONAL-START.
           DISPLAY "Conditional circular start".
           IF LOOP-COUNTER < 10
               PERFORM 701-CONDITIONAL-MIDDLE
           END-IF.
       
       701-CONDITIONAL-MIDDLE.
           DISPLAY "Conditional middle".
           ADD 1 TO LOOP-COUNTER.
           IF LOOP-COUNTER MOD 2 = 0
               PERFORM 702-CONDITIONAL-END
           ELSE
               PERFORM 700-CONDITIONAL-START
           END-IF.
       
       702-CONDITIONAL-END.
           DISPLAY "Conditional end".
           IF LOOP-COUNTER < 5
               PERFORM 700-CONDITIONAL-START
           END-IF.
       
       *> DEEP NESTING CIRCULAR
       DEEP-NESTING-SECTION SECTION.
       800-DEEP-LEVEL-1.
           DISPLAY "Deep Level 1".
           PERFORM 801-DEEP-LEVEL-2.
       
       801-DEEP-LEVEL-2.
           DISPLAY "Deep Level 2".
           PERFORM 802-DEEP-LEVEL-3.
       
       802-DEEP-LEVEL-3.
           DISPLAY "Deep Level 3".
           PERFORM 803-DEEP-LEVEL-4.
       
       803-DEEP-LEVEL-4.
           DISPLAY "Deep Level 4".
           PERFORM 804-DEEP-LEVEL-5.
       
       804-DEEP-LEVEL-5.
           DISPLAY "Deep Level 5".
           *> Deep circular back to level 1
           PERFORM 800-DEEP-LEVEL-1.