       01  :DATA1:-RECORD.
           03  :DATA1:-KEY.
               05  :DATA1:-ORG-ACCT.
                   07  :DATA1:-ORG  PIC 999.
                   07  :DATA1:-ACCT PIC X(19).
               05  :DATA1:-STMT-ID-CODE
                                   PIC S9(7)       BINARY.
               05  :DATA1:-REC-TYPE PIC 9.
                   88  :DATA1:-FILE-HEADER          VALUE ZERO.
                   88  :DATA1:-RECAP-RECORD         VALUE 1 2.
                   88  :DATA1:-RECAP-REC-HIST-SENT  VALUE 2.
                   88  :DATA1:-PLAN-RECORD          VALUE 3.
                   88  :DATA1:-TRANSACTION-RECORD   VALUE 4.
                   88  :DATA1:-FILE-TRAILER         VALUE 9.
               05  :DATA1:-REC-NBR  PIC S9(4)       BINARY.
           03  :DATA1:-TRANSACTION-DATA.
               05  :DATA1:-ATPT-DATA
                                   PIC X(458).
               05  FILLER          PIC X(1916).

