      ******************************************************************
      *                    COPYBOOK - CLIDATA                          *
      *----------------------------------------------------------------*
      * DESCRIPTION : CLIENT ADDRESS FILE LAYOUT                       *
      *----------------------------------------------------------------*
      *    FILE LAYOUT:                                                *
      *        - 01 = HEADER                                           *
      *        - 02 = DETAIL                                           *
      *        - 99 = TRAILER                                          *
      *  LENGTH: 2500 BYTES                                            *
      *                                                                *
      *  NOTE: This copybook defines the structure of the CLIDATA      *
      *        file used for storing client address and related        *
      *        information.                                            *
      *        The file consists of three record types: Header,        *
      *        Detail, and Trailer. Each record type has a specific    *
      *        format and purpose.                                     *
      ******************************************************************
      *================================================================*
      *                 HEADER RECORD                                  *
      *================================================================*
       01  :CLIDATA:-HEADER.
           03  :CLIDATA:-HDR-RECTYPE                  PIC 9(002).
               88 :CLIDATA:-RECTYPE-HEADER              VALUE 01.
      *         Condition-name indicating this is a Header Record.
      *         Used for validating the record type during processing.
           03  :CLIDATA:-FILE-TYPE                   PIC X(020).
      *         File Type Description (e.g., "CLIENT ADDRESS FILE").
           03  :CLIDATA:-HDR-DATE-GEN                PIC 9(010).
      *         File Generation Date in YYYYMMDD format.
      *         Example: 20240101 for January 1, 2024.
           03  :CLIDATA:-HDR-TIME-GEN                PIC 9(008).
      *         File Generation Time in HHMMSS format.
      *         Example: 143000 for 2:30:00 PM.
           03  :CLIDATA:-CTROL-CODE                  PIC X(027).
      *         Control Code, used for tracking data.
           03  FILLER                                PIC X(2433).
      *         Filler to ensure the Header Record is 2500 bytes long.
      *================================================================*
      *                 DETAIL RECORD                                  *
      *================================================================*
       01 :CLIDATA:-DETAIL-RECORD.
           03  :CLIDATA:-DTL-RECTYPE                  PIC  9(002).
      *         Record Type Indicator for Detail Record.
      *
               88  :CLIDATA:-RECTYPE-DETAIL            VALUE 02.
      *         Condition-name indicating this is a Detail Record.
      *         Used for validating the record type during processing.
      *
           03  :CLIDATA:-COMPANY-CODE                PIC 9(03).
      *         Organization Code.
           03  :CLIDATA:-ACCOUNT-NUMBER              PIC X(19).
      *         Client Account Number.
           03  :CLIDATA:-DATE-OF-BIRTH               PIC 9(04).
      *         Date of Birth in MMYY format.
           03  :CLIDATA:-NAME-LINE-1                 PIC X(40).
      *         Client Name - Line 1.
           03  :CLIDATA:-DOCUMENT-NUMBER             PIC X(15).
      *         Client Document Number
           03  :CLIDATA:-DOC-ISSUER-ORG              PIC X(08).
      *         Document Issuer Organization
           03  :CLIDATA:-ISSUER-STATE-CODE           PIC X(02).
      *         Document Issuer State (e.g., "SP", "RJ").
           03  :CLIDATA:-DOC-ISSUE-DATE              PIC 9(08).
      *         Document Issue Date in YYYYMMDD format.
           03  :CLIDATA:-USER-DATA-01                PIC X(30).
      *         User Defined Data Field 01.
           03  :CLIDATA:-USER-DATA-02                PIC X(30).
      *         User Defined Data Field 02.
           03  :CLIDATA:-POSITION                    PIC X(04).
      *         Client Position/Job Title.
           03  :CLIDATA:-PEP-CODE                    PIC X(02).
      *         Politically Exposed Person (PEP) Code.
           03  :CLIDATA:-NAME-TYPE-IND               PIC 9(01).
      *         Name Type Indicator (e.g., 1 for Individual,
      *                                    2 for Company).
           03  :CLIDATA:-RELATIVE-NAME               PIC X(40).
      *         Relative's Name.
           03  :CLIDATA:-SPOUSE-NAME                 PIC X(40).
      *         Spouse's Name.
           03  :CLIDATA:-SPOUSE-ID-NUM               PIC X(11).
      *         Spouse's ID Number.
           03  :CLIDATA:-SPOUSE-DOC-NUMBER           PIC X(13).
      *         Spouse's Document Number.
           03  :CLIDATA:-SPOUSE-DOC-ISSUER-ORG       PIC X(10).
      *         Spouse's Document Issuer Organization.
           03  :CLIDATA:-SPOUSE-DOC-ISSUE-DATE       PIC 9(08).
      *         Spouse's Document Issue Date in YYYYMMDD format.
           03  :CLIDATA:-SPOUSE-UPDATE-DATE          PIC 9(08).
      *         Spouse's Information Update Date in YYYYMMDD format.
           03  :CLIDATA:-DOCUMENT-TYPE               PIC X(02).
      *         Document Type Code.
           03  :CLIDATA:-DOCUMENT-NUMBER             PIC X(15).
      *         Client Document Number.
           03  :CLIDATA:-ASSETS-POSSESSION           PIC X(01).
      *         Assets Possession Indicator (Y/N).
           03  :CLIDATA:-EMPLOYER                    PIC X(40).
      *         Employer Name.
           03  :CLIDATA:-EMPLOYER-ADDRESS-1          PIC X(40).
      *         Employer Address - Line 1.
           03  :CLIDATA:-EMAIL                       PIC X(60).
      *         Client Email Address.
           03  :CLIDATA:-ACCOUNT-NUMBER              PIC X(19).
      *         Client Account Number.
           03  :CLIDATA:-SALES-CHANNEL-PAN           PIC X(04).
      *         Sales Channel PAN.
           03  :CLIDATA:-DUE-DAY                     PIC 9(02).
      *         Due Day of the Month for Payments.
           03  :CLIDATA:-BUSINESS-OR-PERSONAL-ID     PIC X(14).
      *         CPF or CNPJ Number.
           03  :CLIDATA:-USER-ACCOUNT-NUMBER         PIC X(19).
      *         User Account Number.
           03  :CLIDATA:-ELECTRONIC-ADDR-NAME        PIC X(60).
      *         Electronic Address Name.
           03  :CLIDATA:-INCOME                      PIC 9(09).
      *         Client Income.
           03  :CLIDATA:-CORPORATE-FLAG              PIC X(01).
      *         Corporate Flag (Y/N).
           03  :CLIDATA:-CREDIT-LIMIT                PIC X(17).
      *         Credit Limit.
           03  :CLIDATA:-SALES-CHANNEL               PIC X(03).
      *         Sales Channel Code.
           03  :CLIDATA:-SOURCE                      PIC X(01).
      *         Data Source Indicator.
           03  :CLIDATA:-CONVERSION-DATE             PIC 9(08).
      *         Data Conversion Date in YYYYMMDD format.
           03  :CLIDATA:-LAST-CREDIT-LIMIT           PIC 9(17).
      *         Last Credit Limit Amount.
           03  :CLIDATA:-CREDIT-LIMIT-DATE           PIC 9(08).
      *         Credit Limit Date in YYYYMMDD format.
           03  :CLIDATA:-LAST-CREDIT-LIMIT-DATE      PIC 9(08).
      *         Last Credit Limit Date in YYYYMMDD format.
           03  :CLIDATA:-PAYMENT-TOTAL-AMOUNT-DUE    PIC 9(17).
      *         Total Payment Amount Due.
           03  :CLIDATA:-LAST-PAYMENT-AMOUNT         PIC 9(17).
      *         Last Payment Amount.
           03  :CLIDATA:-CURRENT-BALANCE             PIC 9(17).
      *         Current Balance.
           03  :CLIDATA:-AVAILABLE-CREDIT            PIC 9(17).
      *         Open to Buy Amount.
           03  :CLIDATA:-LAST-PURCHASE-AMOUNT        PIC 9(17).
      *         Last Purchase Amount.
           03  :CLIDATA:-USER-AMOUNT-1               PIC 9(17).
      *         User Defined Amount 1.
           03  :CLIDATA:-CASH-CREDIT-LIMIT           PIC 9(17).
      *         Cash Credit Limit.
           03  :CLIDATA:-MAINTENANCE-DATE            PIC 9(08).
      *         maintenance Date in YYYYMMDD format.
           03  :CLIDATA:-DISCOUNT-TABLE-CODE         PIC 9(03).
      *         Discount Table Code.
           03  :CLIDATA:-MISC-USER-1                 PIC X(11).
      *         Miscellaneous User Data 1.
           03  :CLIDATA:-FIRST-PURCHASE-DATE         PIC 9(08).
      *         First Purchase Date in YYYYMMDD format.
           03  :CLIDATA:-MAXIMUM-CIP-DATE            PIC 9(08).
      *         Maximum CIP Date in YYYYMMDD format.
           03  :CLIDATA:-CIP-SENT                    PIC X(01).
      *         CIP Sent Indicator (Y/N).
           03  :CLIDATA:-AH-AGREEMENT-STATUS         PIC X(01).
      *         AH Agreement Status.
           03  :CLIDATA:-AH-AGREEMENT-BROKEN-DATE    PIC 9(08).
      *         AH Agreement Broken Date in YYYYMMDD format.
           03  :CLIDATA:-TOTAL-PAYMENT-ON-DAY        PIC 9(17).
      *         Total Payment on Day.
           03  :CLIDATA:-LAST-MINIMUM-PAYMENT        PIC 9(17).
      *         Last Minimum Payment Amount.
           03  :CLIDATA:-VIP-TYPE                    PIC X(02).
      *         VIP Type Code.
           03  :CLIDATA:-PAYMENT-DELINQ-DAYS         PIC 9(03).
      *         Payment Delinquency Days.
           03  :CLIDATA:-CURRENCY-EXCHANGE-HEDGE-SW  PIC X(01).
      *         Currency Exchange Hedge Switch (Y/N).
           03  :CLIDATA:-CURRENCY-EXCHANGE-HEDGE-AMT PIC 9(05).
      *         Currency Exchange Hedge Amount.
           03  :CLIDATA:-LAST-HEDGE-UPLOAD           PIC 9(05).
      *         Last Hedge Upload.
           03  :CLIDATA:-CURRENT-BALANCE-HEDGE       PIC 9(09).
      *         Current Balance Hedge.
           03  :CLIDATA:-RECURRING-CARD-QUANTITY     PIC 9(02).
      *         Recurring Card Quantity.
           03  :CLIDATA:-FLAG                        PIC X(01).
      *         Flag (Y/N).
           03  :CLIDATA:-COMPULS-FLAG                PIC X(01).
      *         Compulsory Flag (Y/N).
           03  :CLIDATA:-ELECTRONIC-INVOICE-FLAG     PIC X(01).
      *         Electronic Invoice Flag (Y/N).
           03  :CLIDATA:-ELECTRONIC-INVOICE-FLAG-DATE PIC 9(08).
      *         Electronic Invoice Flag Date in YYYYMMDD format.
           03  :CLIDATA:-EMERGENCY-CREDIT-AVAIL      PIC X(01).
      *         Emergency Credit Available (Y/N).
           03  :CLIDATA:-EMERGENCY-CREDIT-MAINT-DATE PIC 9(08).
      *         Emergency Credit Maintenance Date in YYYYMMDD format.
           03  :CLIDATA:-PRODUCT-CODE                PIC 9(03).
      *         Product Code.
           03  :CLIDATA:-CARD-NUMBER                 PIC X(19).
      *         Card Number.
           03  :CLIDATA:-CARD-HASH                   PIC X(19).
      *         Card Hash Value.
           03  :CLIDATA:-TABLE-TYPE-ANDD             PIC X(01).
      *         Table Type ANDD.
           03  :CLIDATA:-TABLE-CODE-ANDD             PIC 9(05).
      *         Table Code ANDD.
           03  :CLIDATA:-DISCOUNT-TABLE-CODE         PIC 9(05).
      *         Discount Table Code.
           03  :CLIDATA:-BLOCK-CODE-1                PIC 9(02).
      *         Block Code 1.
           03  :CLIDATA:-PENULTIMATE-INVOICE-BALANCE PIC 9(17).
      *         Penultimate Invoice Balance.
           03  :CLIDATA:-ADSO-RECOMP-PGM-IND         PIC X(01).
      *         ADSO Recomp Program Indicator.
           03  :CLIDATA:-DEPT-GRADE-1                PIC 9(03).
      *         Department Grade 1.
           03  :CLIDATA:-DEPT-GRADE-2                PIC 9(03).
      *         Department Grade 2.
           03  :CLIDATA:-DEPT-GRADE-3                PIC 9(03).
      *         Department Grade 3.
           03  :CLIDATA:-DATE-GRADE-1                PIC 9(08).
      *         Date Grade 1 in YYYYMMDD format.
           03  :CLIDATA:-DATE-GRADE-2                PIC 9(08).
      *         Date Grade 2 in YYYYMMDD format.
           03  :CLIDATA:-DATE-GRADE-3                PIC 9(08).
      *         Date Grade 3 in YYYYMMDD format.
           03  :CLIDATA:-PRODUCT-GRADE-1             PIC 9(05).
      *         Product Grade 1.
           03  :CLIDATA:-PRODUCT-GRADE-2             PIC 9(05).
      *         Product Grade 2.
           03  :CLIDATA:-PRODUCT-GRADE-3             PIC 9(05).
      *         Product Grade 3.
           03  :CLIDATA:-ADSO-DATE-RECOMP-PGM        PIC 9(08).
      *         ADSO Date Recomp Program in YYYYMMDD format.
           03  :CLIDATA:-CLIENT-ID-STATUS            PIC X(01).
      *         Client ID Status.
           03  :CLIDATA:-CLIENT-ID-STATUS-CHANGE-DT  PIC 9(08).
      *         Client ID Status Change Date in YYYYMMDD format.
           03  :CLIDATA:-TOTAL-CREDIT-CIP            PIC 9(17).
      *         Total Credit CIP.
           03  :CLIDATA:-CIP-UPDATE                  PIC X(01).
      *         CIP Update Indicator.
           03  :CLIDATA:-ANNUAL-DISCOUNT             PIC 9(05).
      *         Annual Discount.
           03  :CLIDATA:-SALES-CHANNEL-CODE          PIC 9(03).
      *         Sales Channel Code.
           03  :CLIDATA:-PROTOCOL-NUMBER             PIC 9(17).
      *         Protocol Number.
           03  :CLIDATA:-TEST-CELL                   PIC 9(03).
      *         Test Cell.
           03  :CLIDATA:-SALES-ACTION                PIC 9(09).
      *         Sales Action.
           03  :CLIDATA:-MULTIPLE-ACCOUNTS-FLAG      PIC X(01).
      *         Multiple Accounts Flag (Y/N).
           03  :CLIDATA:-MULTIPLE-STATUS             PIC 9(02).
      *         Multiple Status.
           03  :CLIDATA:-MULTIPLE-STATUS-DATE-CHANGE PIC 9(08).
      *         Multiple Status Date Change in YYYYMMDD format.
           03  :CLIDATA:-MULTIPLE-ADHESION-DATE      PIC 9(08).
      *         Multiple Adhesion Date in YYYYMMDD format.
           03  :CLIDATA:-CREDIT-GRANT-DATE           PIC 9(08).
      *         Credit Grant Date in YYYYMMDD format.
           03  :CLIDATA:-MULTIPLE-AGENCY             PIC 9(05).
      *         Multiple Agency
           03  :CLIDATA:-MULTIPLE-ACCT               PIC 9(09).
      *         Multiple Account
           03  :CLIDATA:-MULTIPLE-ELIGIBILITY-IND    PIC X(01).
      *         Multiple Eligibility Indicator.
           03  :CLIDATA:-FUNCTION-ACCOUNTS-IND       PIC X(01).
      *         Function Accounts Indicator.
           03  :CLIDATA:-SMS-PACKAGE-CODE            PIC 9(02).
      *         SMS Package Code.
           03  :CLIDATA:-SMS-PACKAGE-DATE            PIC 9(08).
      *         SMS Package Date in YYYYMMDD format.
           03  :CLIDATA:-REFIN-STATUS                PIC 9(02).
      *         Refinancing Status.
           03  :CLIDATA:-PENDING-REFIN-STATUS        PIC 9(02).
      *         Pending Refinancing Status.
           03  :CLIDATA:-REFIN-STATUS-DATE           PIC 9(08).
      *         Refinancing Status Date in YYYYMMDD format.
           03  :CLIDATA:-UNBLOCK-REFIN-DATE          PIC 9(08).
      *         Unblock Refinancing Date in YYYYMMDD format.
           03  :CLIDATA:-INCONS-REFIN-DATE           PIC 9(08).
      *         Inconsistent Refinancing Date in YYYYMMDD format.
           03  :CLIDATA:-INCONS-REFIN-AMOUNT         PIC 9(17).
      *         Inconsistent Refinancing Amount.
           03  :CLIDATA:-PRF-CODE                    PIC 9(03).
      *         PRF Code.
           03  :CLIDATA:-ELECTRONIC-ADDR-NAME-2      PIC X(60).
      *         Electronic Address Name 2.
           03  :CLIDATA:-LOGO                        PIC 9(03).
      *         Logo Code.
           03  :CLIDATA:-SHORT-NAME                  PIC X(20).
      *         Short Name.
           03  :CLIDATA:-STATE-OF-RESIDENCE          PIC 9(03).
      *         State of Residence Code.
           03  :CLIDATA:-STATE-OF-ISSUE              PIC 9(03).
      *         State of Issue Code.
           03  :CLIDATA:-INT-STATUS                  PIC X(01).
      *         Internal Status.
           03  :CLIDATA:-BLK-CODE-1                  PIC X.
      *         Block Code 1.
           03  :CLIDATA:-BLK-CODE-2                  PIC A.
      *         Block Code 2.
           03  :CLIDATA:-BILLING-CYCLE               PIC 9(02).
      *         Billing Cycle.
           03  :CLIDATA:-USER-CODE-7                 PIC X(02).
      *         User Code 7.
           03  :CLIDATA:-USER-CODE-8                 PIC X(02).
      *         User Code 8.
           03  :CLIDATA:-DATE-OPENED                 PIC 9(08).
      *         Date Opened in YYYYMMDD format.
           03  :CLIDATA:-BLOCK-CODE-1-DATE           PIC 9(08).
      *         Block Code 1 Date in YYYYMMDD format.
           03  :CLIDATA:-BLOCK-CODE-2-DATE           PIC 9(08).
      *         Block Code 2 Date in YYYYMMDD format.
           03  :CLIDATA:-CARD-FEE-DATE               PIC 9(08).
      *         Card Fee Date in YYYYMMDD format.
           03  :CLIDATA:-LAST-PURCHASE-DATE          PIC 9(08).
      *         Last Purchase Date in YYYYMMDD format.
           03  :CLIDATA:-LAST-DELINQUENCY-DATE       PIC 9(08).
      *         Last Delinquency Date in YYYYMMDD format.
           03  :CLIDATA:-LAST-STATEMENT-DATE         PIC 9(08).
      *         Last Statement Date in YYYYMMDD format.
           03  :CLIDATA:-NEXT-STATEMENT-DATE         PIC 9(08).
      *         Next Statement Date in YYYYMMDD format.
           03  :CLIDATA:-PAYMENT-DUE-DATE            PIC 9(08).
      *         Payment Due Date in YYYYMMDD format.
           03  :CLIDATA:-LAST-PAYMENT-DATE           PIC 9(08).
      *         Last Payment Date in YYYYMMDD format.
           03  :CLIDATA:-CARD-EXPIRATION-DATE        PIC 9(08).
      *         Card Expiration Date in YYYYMMDD format.
           03  :CLIDATA:-AVAILABLE-CASH-CREDIT       PIC 9(17).
      *         Available Cash Credit.
           03  :CLIDATA:-SPM-CYCLE                PIC 9(17).
      *         SPM Cycle.
           03  :CLIDATA:-SPM-ACCUM                PIC 9(17).
      *         SPM Accumulated.
           03  :CLIDATA:-MG-CELL-AREA-CODE-SMS       PIC X(04).
      *         MG Cell Area Code for SMS.
           03  :CLIDATA:-MG-CELLPHONE-SMS            PIC X(10).
      *         MG Cellphone Number for SMS.
           03  :CLIDATA:-FH-STATEMENT-DATE-1         PIC 9(08).
      *         FH Statement Date 1 in YYYYMMDD format.
           03  :CLIDATA:-FH-STATEMENT-DATE-2         PIC 9(08).
      *         FH Statement Date 2 in YYYYMMDD format.
           03  :CLIDATA:-FH-STATEMENT-DATE-3         PIC 9(08).
      *         FH Statement Date 3 in YYYYMMDD format.
           03  :CLIDATA:-FH-STATEMENT-DATE-4         PIC 9(08).
      *         FH Statement Date 4 in YYYYMMDD format.
           03  :CLIDATA:-FH-STATEMENT-DATE-5         PIC 9(08).
      *         FH Statement Date 5 in YYYYMMDD format.
           03  :CLIDATA:-FH-STATEMENT-DATE-6         PIC 9(08).
      *         FH Statement Date 6 in YYYYMMDD format.
           03  :CLIDATA:-RAW-SCORE-1                 PIC 9(03).
      *         Raw Score 1.
           03  :CLIDATA:-RAW-SCORE-2                 PIC 9(03).
      *         Raw Score 2.
           03  :CLIDATA:-RAW-SCORE-3                 PIC 9(03).
      *         Raw Score 3.
           03  :CLIDATA:-RAW-SCORE-4                 PIC 9(03).
      *         Raw Score 4.
           03  :CLIDATA:-RAW-SCORE-5                 PIC 9(03).
      *         Raw Score 5.
           03  :CLIDATA:-RAW-SCORE-6                 PIC 9(03).
      *         Raw Score 6.
           03  :CLIDATA:-BEHAVIOR-SCORE-1            PIC 9(03).
      *         Behavior Score 1.
           03  :CLIDATA:-BEHAVIOR-SCORE-2            PIC 9(03).
      *         Behavior Score 2.
           03  :CLIDATA:-BEHAVIOR-SCORE-3            PIC 9(03).
      *         Behavior Score 3.
           03  :CLIDATA:-BEHAVIOR-SCORE-4            PIC 9(03).
      *         Behavior Score 4.
           03  :CLIDATA:-BEHAVIOR-SCORE-5            PIC 9(03).
      *         Behavior Score 5.
           03  :CLIDATA:-BEHAVIOR-SCORE-6            PIC 9(03).
      *         Behavior Score 6.
           03  :CLIDATA:-SCORE-ID-1                  PIC 9(03).
      *         SCORE ID 1.    
           03  :CLIDATA:-SCORE-ID-2                  PIC 9(03).
      *         SCORE ID 2.    
           03  :CLIDATA:-SCORE-ID-3                  PIC 9(03).
      *         SCORE ID 3.    
           03  :CLIDATA:-SCORE-ID-4                  PIC 9(03).
      *         SCORE ID 4.    
           03  :CLIDATA:-SCORE-ID-5                  PIC 9(03).
      *         SCORE ID 5.    
           03  :CLIDATA:-SCORE-ID-6                  PIC 9(03).
      *         SCORE ID 6.    
           03  :CLIDATA:-ADDRESS-TYPE                PIC X(01).
      *         Address Type Indicator.
               88  :CLIDATA:-RESIDENTIAL-ADDR-TYPE   VALUE 'R'.
      *           Condition-name indicating Residential Address.
               88  :CLIDATA:-COMMERCIAL-ADDR-TYPE    VALUE 'C'.
      *           Condition-name indicating Commercial Address.
               88  :CLIDATA:-BILLING-ADDR-TYPE       VALUE 'F'.
      *           Condition-name indicating Billing Address.
           03  :CLIDATA:-ADDRESS OCCURS 03 TIMES.
      *       Repeating group for up to 3 addresses.
             05 :CLIDATA:-ZIP-CODE                   PIC 9(08).
      *           Zip Code.
             05 :CLIDATA:-STREET-DESC                PIC X(45).
      *           Street Description.
             05 :CLIDATA:-STREET-NUMBER              PIC X(06).
      *           Street Number.
             05 :CLIDATA:-NEIGHBORHOOD-NAME          PIC X(15).
      *           Neighborhood Name.
             05 :CLIDATA:-COMPLEMENT-TEXT            PIC X(15).
      *           Complement Text (e.g., Apartment Number).
             05 :CLIDATA:-CITY-NAME                  PIC X(25).
      *           City Name.
             05 :CLIDATA:-STATE-NAME                 PIC X(02).
      *           State Name.
             05 :CLIDATA:-COUNTRY-NAME               PIC X(20).
      *           Country Name.
             05 :CLIDATA:-CELLPHONE-NUMBER           PIC X(10).
      *           Cellphone Number.
             05 :CLIDATA:-CELLPHONE-AREA-CODE        PIC X(04).
      *           Cellphone Area Code.
           03  :CLIDATA:-UUID                        PIC X(36).
      *         Universally Unique Identifier.
           03  :CLIDATA:-RECORD-STATUS               PIC X(02).
      *         Record Status Code.
           03  :CLIDATA:-EXCLUSION-REASON-FLAG       PIC X(02).
      *         Exclusion Reason Flag.
           03  :CLIDATA:-ALTERATION-ID               PIC X(220).
      *         Alteration ID.
           03  FILLER                                PIC X(249).
      *         Filler to ensure the Detail Record is 2500 bytes long.
      *================================================================*
      *                       TRAILER RECORD                           *
      *================================================================*
       01 :CLIDATA:-TRAILER.
           03  :CLIDATA:-TRL-RECTYPE                  PIC  9(002).
      *         Record Type Indicator for Trailer Record.
               88  :CLIDATA:-RECTYPE-TRAILER           VALUE 99.
      *           Condition-name indicating this is a Trailer Record.
      *           Used for validating the record type during processing.
           03  :CLIDATA:-TOTAL-RECORDS               PIC  9(017).
      *         Total Number of Detail Records in the file.
           03  FILLER                                PIC  X(2481).
      *         Filler to ensure the Trailer Record is 2500 bytes long.
      *---------------------------------------------------------------*
      *                      END OF COPYBOOK                          *
      *---------------------------------------------------------------*