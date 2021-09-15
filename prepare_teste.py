
print('STARTED...')


# --------------------------- EBCDIC BINARY CONTENT ----------------------------
print('Generating for ECBDIC')

copybookfname = 'EBCDIC_BOOK.cob'
datafname     = 'EBCDIC_DATA.bin'

copybook_ebcdic = """
      *================================================================*00010000
      *                 COBOL COPYBOOK EXAMPLE - EBCDIC                *00020000
      * -------------------------------------------------------------- *00021000
      * NOTE: This copybook handle binary content that in general are  *00022000
      *       written in EBCDIC content in Mainframes and to use it    *00023000
      *       in other platforms it's necessary to transfer than in    *00024000
      *       binary to preserv their contents.                        *00025000
      *       In FTP transfer use BINARY.                              *00026000
      *                                Joao Roberto Perin - 2021-09-15 *00027000
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
          03 :DATA1:-CREDIT-LIMIT              PIC S9(07) USAGE BINARY. 00160000
          03 :DATA1:-LIMIT-USED                PIC S9(05)V99 COMP-3.    00170000
          03 :DATA1:-STATUS OCCURS 4 TIMES.                             00180000
             05 :DATA1:-STATUS-FLAG            PIC X(001).              00190000
RESERV    03 FILLER                            PIC X(014).              00200000
      *================================================================*00210000
      *                               END                              *00220000
      *================================================================*00230000
"""

ebcdicdata1 = b'\xF0\xF2\xF0\xF0\xF4\xF0\xF0\xF4\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF9\xF0\xF0\xF0\xF1\xF1\xF1\xF1\xF1\xF9\xF7\xF1\x60\xF0\xF1\x60\xF2\xF1\xD1\xD6\xC8\xD5\x40\xD9\xD6\xC2\xC5\xD9\xE3\x40\xD7\xC5\xD9\xC9\xD5\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x00\x00\x03\xE9\x01\x00\x01\x0D\xF1\xF2\xF3\xF4\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'
ebcdicdata2 = b'\xF0\xF2\xF0\xF0\xF4\xF0\xF0\xF4\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF9\xF0\xF0\xF0\xF2\xF2\xF2\xF2\xF1\xF9\xF8\xF2\x60\xF0\xF2\x60\xF2\xF2\xD4\xC1\xD9\xE8\x40\xC1\xD5\xD5\x40\xC7\xC1\xD9\xC3\xC9\xC1\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\xFF\xFF\xFF\x85\x01\x00\x02\x0C\xF5\xF6\xF7\xF8\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'
ebcdicdata3 = b'\xF0\xF2\xF0\xF0\xF4\xF0\xF0\xF4\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF9\xF0\xF0\xF0\xF3\xF3\xF3\xF3\xF1\xF9\xF9\xF3\x60\xF0\xF3\x60\xF2\xF3\xD7\xC5\xE3\xC5\xD9\x40\xD7\xC1\xD9\xD2\xC5\xD9\x40\xC2\xD9\xC1\xE9\xC9\xD3\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x00\x00\x00\x7B\x03\x00\x03\x0C\xF9\xF0\xC1\xC2\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'

print('Generating EBCDIC Copybook...')
with open(copybookfname, 'w') as f:
    f.write(copybook_ebcdic)

print("Generating Data File...")

with open(datafname, 'wb') as f:
    f.write(ebcdicdata1)
    f.write(ebcdicdata2)
    f.write(ebcdicdata3)


# --------------------------- ASCII TEXT CONTENT -------------------------------

print('Generating for ASCII')

copybookfname = 'ASCII_BOOK.cob'
datafname     = 'ASCII_DATA.bin'

copybook_ascii = """
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
"""

asciidata1 = "0200400400000000900011111971-01-21JOHN ROBERT PERIN                       +0001001-00100.101234   " + "\n"
asciidata2 = "0200400400000000900011111971-01-21MARY ANN GARCIA                         -0000123+01000.205678   " + "\n"
asciidata3 = "0200400400000000900011111971-01-21JPETER PARKER BRAZIL                    +0000123+03000.3090AB   " + "\n"

print('Generating ASCII Copybook...')

with open(copybookfname, 'w') as f:
    f.write(copybook_ascii)

print("Generating Data File...")

with open(datafname, 'w') as f:
    f.write(asciidata1)
    f.write(asciidata2)
    f.write(asciidata3)

print("ENDED...")


exit(0)
# # Content in EBCID will be like this:
# # "0200400400000000593797511980-05-31JORNADA21626234477                      00001231234567          "

# regtype = b'\xF0\xF2'
# company = b'\xF0\xF0\xF4'

# account1 = b'\xF0\xF0\xF4\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF9\xF0\xF0\xF0\xF1\xF1\xF1\xF1'
# account2 = b'\xF0\xF0\xF4\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF9\xF0\xF0\xF0\xF2\xF2\xF2\xF2'
# account3 = b'\xF0\xF0\xF4\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF0\xF9\xF0\xF0\xF0\xF3\xF3\xF3\xF3'

# date1 = b'\xF1\xF9\xF7\xF1\x60\xF0\xF1\x60\xF2\xF1'
# date2 = b'\xF1\xF9\xF8\xF2\x60\xF0\xF2\x60\xF2\xF2'
# date3 = b'\xF1\xF9\xF9\xF3\x60\xF0\xF3\x60\xF2\xF3'

# name1 = b'\xD1\xD6\xC8\xD5\x40\xD9\xD6\xC2\xC5\xD9\xE3\x40\xD7\xC5\xD9\xC9\xD5\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'
# name2 = b'\xD4\xC1\xD9\xE8\x40\xC1\xD5\xD5\x40\xC7\xC1\xD9\xC3\xC9\xC1\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'
# name3 = b'\xD7\xC5\xE3\xC5\xD9\x40\xD7\xC1\xD9\xD2\xC5\xD9\x40\xC2\xD9\xC1\xE9\xC9\xD3x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'
# # JOHN ROBERT PERIN
# # \xD1\xD6\xC8\xD5\x40\xD9\xD6\xC2\xC5\xD9\xE3\x40\xD7\xC5\xD9\xC9\xD5\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40

# # MARY ANN GARCIA
# # \xD4\xC1\xD9\xE8\x40\xC1\xD5\xD5\x40\xC7\xC1\xD9\xC3\xC9\xC1

# # PETER PARKER BRAZIL
# # \xD7\xC5\xE3\xC5\xD9\x40\xD7\xC1\xD9\xD2\xC5\xD9\x40\xC2\xD9\xC1\xE9\xC9\xD3


# creditlimit1 = b'\x00\x00\x03\xE9' # = +1001
# creditlimit2 = b'\xFF\xFF\xFF\x85' # = -123
# creditlimit3 = b'\x00\x00\x00\x7B' # = +123

# limitused1 = b'\x01\x00\x01\x0D'  # -0010010
# limitused2 = b'\x01\x00\x02\x0C'   # +0100020
# limitused3 = b'\x03\x00\x03\x0C'   # +0300030

# status1 = b'\xF1\xF2\xF3\xF4'
# status2 = b'\xF5\xF6\xF7\xF8'
# status3 = b'\xF9\xF0\xC1\xC2'

# filler  = b'\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40\x40'


# reg1 = regtype + company + account1 + date1 + name1 + creditlimit1 + limitused1 + status1 + filler
# reg2 = regtype + company + account2 + date1 + name2 + creditlimit2 + limitused2 + status2 + filler
# reg3 = regtype + company + account3 + date1 + name3 + creditlimit3 + limitused3 + status3 + filler

# # This little python script generates 3 lines in ebcdic
# # To be parsed based on DATA1_BOOK.cob
