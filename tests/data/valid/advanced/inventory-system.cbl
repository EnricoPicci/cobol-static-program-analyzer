      *> Advanced inventory management system
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-SYSTEM.
       AUTHOR. Test Suite.
       INSTALLATION. Test Environment.
       DATE-WRITTEN. 2025-07-01.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO "INVENTORY.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-FILE.
       01 INVENTORY-RECORD.
           05 ITEM-CODE     PIC X(10).
           05 ITEM-NAME     PIC X(30).
           05 QUANTITY      PIC 9(5).
           05 UNIT-PRICE    PIC 9(5)V99.
           05 REORDER-LEVEL PIC 9(4).
       
       WORKING-STORAGE SECTION.
       01 WS-INVENTORY-REC.
           05 WS-ITEM-CODE     PIC X(10).
           05 WS-ITEM-NAME     PIC X(30).
           05 WS-QUANTITY      PIC 9(5).
           05 WS-UNIT-PRICE    PIC 9(5)V99.
           05 WS-REORDER-LEVEL PIC 9(4).
       
       01 COUNTERS.
           05 TOTAL-ITEMS      PIC 9(5) VALUE ZERO.
           05 LOW-STOCK-ITEMS  PIC 9(5) VALUE ZERO.
           05 TOTAL-VALUE      PIC 9(8)V99 VALUE ZERO.
       
       01 CALCULATED-VALUES.
           05 ITEM-VALUE       PIC 9(8)V99.
           05 PERCENTAGE       PIC 9(3)V99.
       
       01 REPORT-HEADERS.
           05 REPORT-TITLE PIC X(50) VALUE 
               "INVENTORY ANALYSIS REPORT".
           05 COLUMN-HEADER PIC X(80) VALUE
               "ITEM CODE  ITEM NAME                QUANTITY  PRICE   VALUE".
       
       01 DETAIL-LINE.
           05 DL-ITEM-CODE  PIC X(10).
           05 FILLER        PIC X(2) VALUE SPACES.
           05 DL-ITEM-NAME  PIC X(25).
           05 FILLER        PIC X(2) VALUE SPACES.
           05 DL-QUANTITY   PIC Z(5).
           05 FILLER        PIC X(2) VALUE SPACES.
           05 DL-PRICE      PIC Z(5).99.
           05 FILLER        PIC X(2) VALUE SPACES.
           05 DL-VALUE      PIC Z(8).99.
       
       01 EOF-FLAG         PIC X VALUE 'N'.
       01 REORDER-ALERT    PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
       000-MAIN-CONTROL.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-INVENTORY.
           PERFORM 300-GENERATE-REPORT.
           PERFORM 400-CLEANUP.
           STOP RUN.
       
       100-INITIALIZE.
           OPEN INPUT INVENTORY-FILE.
           MOVE SPACES TO DETAIL-LINE.
           DISPLAY REPORT-TITLE.
           DISPLAY COLUMN-HEADER.
           DISPLAY " ".
       
       200-PROCESS-INVENTORY.
           PERFORM 210-READ-RECORD.
           PERFORM 220-PROCESS-RECORD 
               UNTIL EOF-FLAG = 'Y'.
       
       210-READ-RECORD.
           READ INVENTORY-FILE INTO WS-INVENTORY-REC
               AT END 
                   MOVE 'Y' TO EOF-FLAG
           END-READ.
       
       220-PROCESS-RECORD.
           ADD 1 TO TOTAL-ITEMS.
           
           COMPUTE ITEM-VALUE = WS-QUANTITY * WS-UNIT-PRICE.
           ADD ITEM-VALUE TO TOTAL-VALUE.
           
           IF WS-QUANTITY < WS-REORDER-LEVEL
               ADD 1 TO LOW-STOCK-ITEMS
               MOVE 'Y' TO REORDER-ALERT
           END-IF.
           
           PERFORM 230-FORMAT-DETAIL-LINE.
           DISPLAY DETAIL-LINE.
           
           PERFORM 210-READ-RECORD.
       
       230-FORMAT-DETAIL-LINE.
           MOVE WS-ITEM-CODE TO DL-ITEM-CODE.
           MOVE WS-ITEM-NAME TO DL-ITEM-NAME.
           MOVE WS-QUANTITY TO DL-QUANTITY.
           MOVE WS-UNIT-PRICE TO DL-PRICE.
           MOVE ITEM-VALUE TO DL-VALUE.
       
       300-GENERATE-REPORT.
           DISPLAY " ".
           DISPLAY "SUMMARY STATISTICS:".
           DISPLAY "Total Items: " TOTAL-ITEMS.
           DISPLAY "Low Stock Items: " LOW-STOCK-ITEMS.
           DISPLAY "Total Inventory Value: " TOTAL-VALUE.
           
           IF LOW-STOCK-ITEMS > 0
               COMPUTE PERCENTAGE = 
                   (LOW-STOCK-ITEMS / TOTAL-ITEMS) * 100
               DISPLAY "Percentage Needing Reorder: " PERCENTAGE "%"
           END-IF.
           
           IF REORDER-ALERT = 'Y'
               DISPLAY "*** REORDER ALERT: Low stock items detected ***"
           END-IF.
       
       400-CLEANUP.
           CLOSE INVENTORY-FILE.
           DISPLAY "Processing complete.".