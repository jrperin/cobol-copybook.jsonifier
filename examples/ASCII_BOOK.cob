
      *================================================================*00010000
      *                 COBOL COPYBOOK EXAMPLE - ASCII                 *00020000
      * -------------------------------------------------------------- *00021000
      * NOTE: This copybook handle ascii text content that in general  *00022000
      *       is written in EBCDIC codepage in Mainframes and is       *00023000
      *       converted in transfer methods to ASCII. To convert it,   *00024000
      *       the content needs to be in plain EBCDIC text such as     *00025000
      *       PIC X(n), PIC A(n), PIC +99999.99, PIC +ZZZZZ9.99.       *00026000
      *       In FTP transfer use TEXT.                                *00027000
      *                                Joao Roberto Perin - 2021-09-15 *00028000
      *================================================================*00030000
      *                                           REGISTRY LENGHT: 100 *00040000
      *================================================================*00050000
       01 :DATA1:-DETAIL-REGISTRY.                                      00060000
          03 :DATA1:-REGISTRY-TYPE             PIC 9(002).              00070000
             88 :DATA1:-REGISTRY-TYPE-HEADER     VALUE 01.              00080000
             88 :DATA1:-REGISTRY-TYPE-DETAIL     VALUE 02.              00090000
             88 :DATA1:-REGISTRY-TYPE-TRAILLER   VALUE 99.              00100000
          03 :DATA1:-COMPANY                   PIC 9(003).              00110000
          03 :DATA1:-USER-ACCOUNT              PIC X(019).              00120000
          03 :DATA1:-BIRTH-DATE                PIC X(010).              00130000
COMM  *      BIRTH-DATE: YYYY-MM-DD                                     00140000
          03 :DATA1:-NAME                      PIC X(040).              00150000
          03 :DATA1:-CREDIT-LIMIT              PIC +9999999.            00160000
          03 :DATA1:-LIMIT-USED                PIC +99999.99.           00170000
          03 :DATA1:-STATUS OCCURS 4 TIMES.                             00180000
             05 :DATA1:-STATUS-FLAG            PIC X(001).              00190000
RESERV    03 FILLER                            PIC X(003).              00200000
      *================================================================*00210000
      *                               END                              *00220000
      *================================================================*00230000
