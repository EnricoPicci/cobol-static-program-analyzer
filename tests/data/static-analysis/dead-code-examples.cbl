      *> Dead Code Detection Test Cases
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEAD-CODE-EXAMPLES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONTROL-FLAGS.
           05 PROCESS-FLAG      PIC X VALUE 'N'.
           05 ERROR-FLAG        PIC X VALUE 'N'.
           05 UNUSED-FLAG       PIC X VALUE 'N'.
       
       01 COUNTERS.
           05 RECORD-COUNT      PIC 9(5) VALUE ZERO.
           05 ERROR-COUNT       PIC 9(3) VALUE ZERO.
           05 UNUSED-COUNTER    PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-RECORDS.
           PERFORM 300-FINALIZE.
           STOP RUN.
       
       100-INITIALIZE.
           MOVE 'Y' TO PROCESS-FLAG.
           DISPLAY "Starting process...".
       
       200-PROCESS-RECORDS.
           PERFORM 210-READ-RECORD.
           PERFORM 220-PROCESS-RECORD
               UNTIL PROCESS-FLAG = 'N'.
       
       210-READ-RECORD.
           ADD 1 TO RECORD-COUNT.
           IF RECORD-COUNT > 100
               MOVE 'N' TO PROCESS-FLAG
           END-IF.
       
       220-PROCESS-RECORD.
           IF RECORD-COUNT > 50
               PERFORM 230-VALIDATE-RECORD
           END-IF.
           PERFORM 210-READ-RECORD.
       
       230-VALIDATE-RECORD.
           IF RECORD-COUNT > 75
               MOVE 'Y' TO ERROR-FLAG
           END-IF.
       
       300-FINALIZE.
           DISPLAY "Records processed: " RECORD-COUNT.
           IF ERROR-FLAG = 'Y'
               DISPLAY "Errors encountered: " ERROR-COUNT
           END-IF.
       
       *> DEAD CODE SECTIONS - Never called or referenced
       DEAD-SECTION SECTION.
       400-UNREACHABLE-PROCESS.
           DISPLAY "This code is never reached".
           PERFORM 410-ANOTHER-UNREACHABLE.
       
       410-ANOTHER-UNREACHABLE.
           MOVE 'Y' TO UNUSED-FLAG.
           ADD 1 TO UNUSED-COUNTER.
       
       *> DEAD PARAGRAPHS - Not part of any section and never called
       500-ORPHANED-PARAGRAPH.
           DISPLAY "This paragraph is orphaned".
           MOVE 999 TO UNUSED-COUNTER.
       
       510-ANOTHER-ORPHANED.
           DISPLAY "Another orphaned paragraph".
           PERFORM 520-CHAINED-ORPHANED.
       
       520-CHAINED-ORPHANED.
           DISPLAY "Chained orphaned paragraph".
       
       *> PARTIALLY DEAD CODE - Some paragraphs called, others not
       PARTIAL-SECTION SECTION.
       600-ENTRY-POINT.
           PERFORM 610-CALLED-PARAGRAPH.
           *> Note: 620-UNCALLED-PARAGRAPH is never performed
       
       610-CALLED-PARAGRAPH.
           DISPLAY "This paragraph is called".
       
       620-UNCALLED-PARAGRAPH.
           DISPLAY "This paragraph is never called".
           MOVE 'Y' TO UNUSED-FLAG.
       
       *> CONDITIONAL DEAD CODE - Only reachable under certain conditions
       CONDITIONAL-SECTION SECTION.
       700-CONDITIONAL-ENTRY.
           IF ERROR-FLAG = 'Y'
               PERFORM 710-ERROR-HANDLER
           END-IF.
           *> 720-UNREACHABLE-ERROR is never performed
       
       710-ERROR-HANDLER.
           DISPLAY "Handling error".
           ADD 1 TO ERROR-COUNT.
       
       720-UNREACHABLE-ERROR.
           DISPLAY "This error handler is never reached".
           MOVE 'N' TO ERROR-FLAG.