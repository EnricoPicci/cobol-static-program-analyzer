      *> Complex conditional logic and branching
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDITIONAL-LOGIC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENT-RECORD.
           05 STUDENT-NAME PIC X(25).
           05 STUDENT-GRADE PIC 9(3).
           05 STUDENT-STATUS PIC X(10).
       
       01 GRADE-RANGES.
           05 A-GRADE PIC 9(3) VALUE 90.
           05 B-GRADE PIC 9(3) VALUE 80.
           05 C-GRADE PIC 9(3) VALUE 70.
           05 D-GRADE PIC 9(3) VALUE 60.
       
       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           MOVE "ALICE SMITH" TO STUDENT-NAME.
           MOVE 85 TO STUDENT-GRADE.
           
           PERFORM EVALUATE-GRADE.
           
           DISPLAY "Student: " STUDENT-NAME.
           DISPLAY "Grade: " STUDENT-GRADE.
           DISPLAY "Status: " STUDENT-STATUS.
           
           STOP RUN.
       
       EVALUATE-GRADE.
           EVALUATE STUDENT-GRADE
               WHEN A-GRADE THRU 100
                   MOVE "EXCELLENT" TO STUDENT-STATUS
                   PERFORM AWARD-HONORS
               WHEN B-GRADE THRU 89
                   MOVE "GOOD" TO STUDENT-STATUS
                   PERFORM STANDARD-RECOGNITION
               WHEN C-GRADE THRU 79
                   MOVE "AVERAGE" TO STUDENT-STATUS
               WHEN D-GRADE THRU 69
                   MOVE "BELOW AVG" TO STUDENT-STATUS
                   PERFORM REMEDIAL-ACTION
               WHEN OTHER
                   MOVE "FAILING" TO STUDENT-STATUS
                   PERFORM FAILING-ACTION
           END-EVALUATE.
       
       AWARD-HONORS.
           DISPLAY "*** HONORS STUDENT ***".
       
       STANDARD-RECOGNITION.
           DISPLAY "Good job, keep it up!".
       
       REMEDIAL-ACTION.
           DISPLAY "Consider additional study.".
       
       FAILING-ACTION.
           DISPLAY "Please see advisor immediately.".