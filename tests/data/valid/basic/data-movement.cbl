      *> Data movement and basic operations
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-MOVEMENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-INFO.
           05 CUSTOMER-NAME    PIC X(30) VALUE SPACES.
           05 CUSTOMER-ID      PIC 9(5) VALUE ZERO.
           05 CUSTOMER-BALANCE PIC 9(7)V99 VALUE ZERO.
       
       01 TEMP-NAME PIC X(30).
       01 TEMP-ID   PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           MOVE "JOHN DOE" TO CUSTOMER-NAME.
           MOVE 12345 TO CUSTOMER-ID.
           MOVE 1500.75 TO CUSTOMER-BALANCE.
           
           MOVE CUSTOMER-NAME TO TEMP-NAME.
           MOVE CUSTOMER-ID TO TEMP-ID.
           
           DISPLAY "Customer Name: " CUSTOMER-NAME.
           DISPLAY "Customer ID: " CUSTOMER-ID.
           DISPLAY "Customer Balance: " CUSTOMER-BALANCE.
           
           STOP RUN.