      *> Standard error codes copybook
       01 ERROR-CODES.
           05 ERR-SUCCESS           PIC X(4) VALUE '0000'.
           05 ERR-FILE-NOT-FOUND    PIC X(4) VALUE '1001'.
           05 ERR-INVALID-DATA      PIC X(4) VALUE '1002'.
           05 ERR-DUPLICATE-KEY     PIC X(4) VALUE '1003'.
           05 ERR-INSUFFICIENT-MEM  PIC X(4) VALUE '1004'.
           05 ERR-NETWORK-ERROR     PIC X(4) VALUE '1005'.
           05 ERR-TIMEOUT           PIC X(4) VALUE '1006'.
           05 ERR-PERMISSION-DENIED PIC X(4) VALUE '1007'.
           05 ERR-UNKNOWN           PIC X(4) VALUE '9999'.
       
       01 ERROR-MESSAGES.
           05 MSG-SUCCESS           PIC X(40) VALUE 
               'Operation completed successfully'.
           05 MSG-FILE-NOT-FOUND    PIC X(40) VALUE 
               'Required file could not be found'.
           05 MSG-INVALID-DATA      PIC X(40) VALUE 
               'Data format is invalid or corrupted'.
           05 MSG-DUPLICATE-KEY     PIC X(40) VALUE 
               'Duplicate key value not allowed'.
           05 MSG-INSUFFICIENT-MEM  PIC X(40) VALUE 
               'Insufficient memory for operation'.
           05 MSG-NETWORK-ERROR     PIC X(40) VALUE 
               'Network connection error occurred'.
           05 MSG-TIMEOUT           PIC X(40) VALUE 
               'Operation timed out before completion'.
           05 MSG-PERMISSION-DENIED PIC X(40) VALUE 
               'Access denied - insufficient privileges'.
           05 MSG-UNKNOWN           PIC X(40) VALUE 
               'Unknown error occurred'.