      *> Customer record copybook
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID       PIC 9(8).
           05 CUSTOMER-NAME     PIC X(30).
           05 CUSTOMER-ADDRESS.
               10 STREET        PIC X(25).
               10 CITY          PIC X(20).
               10 STATE         PIC X(2).
               10 ZIP-CODE      PIC 9(5).
           05 CUSTOMER-PHONE    PIC X(12).
           05 CUSTOMER-EMAIL    PIC X(50).
           05 ACCOUNT-BALANCE   PIC S9(8)V99 COMP-3.
           05 CREDIT-LIMIT      PIC S9(8)V99 COMP-3.
           05 LAST-PAYMENT-DATE PIC 9(8).
           05 STATUS-CODE       PIC X.
               88 ACTIVE-CUSTOMER    VALUE 'A'.
               88 INACTIVE-CUSTOMER  VALUE 'I'.
               88 SUSPENDED-CUSTOMER VALUE 'S'.