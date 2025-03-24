      *================================================================*
      * Disk file name: MAF016 - ASCII                                  
      *================================================================*
      * The following COBOL file definition shows the contents and      
      * layout of each record. The numbers to the far right under       
      * POSITION shows the relative starting position of that field.    
      * To determine the number of characters in a field, look at the   
      * PIC column. Following the PIC is an "X" or "9". "X" indicates   
      * the field is alphanumeric. "9" indicates the field is numeric   
      * only. The number following the "X" or "9" tells the number of   
      * characters in that field. Fields which are not available will   
      * be space filled for alphanumeric and zero filled for numeric.   
      * If survey, block, section and abstract fields are not available 
      * the survey field will contain the literal 'NO LOCATION DATA     
      * AVAILABLE'. Each record is 240 BYTES (characters) long.         
      * Definitions of each data field can be found on the next few     
      * pages.                                                          
      *
      * Source:                                                         
      * https://www.rrc.texas.gov/media/bkyl5qvx/well-api-manual.pdf    
      *================================================================*
       01 MAF016-QUAD-INFO.                                             POSITION
          05 MAF016-QUAD-NUM             PIC 9(07) VALUE ZEROS.         1       
          05 MAF016-API-NUM              PIC 9(08) VALUE ZEROS.         8       
          05 MAF016-SURVEY               PIC X(55) VALUE SPACES.        16      
          05 MAF016-BLOCK                PIC X(10) VALUE SPACES.        71      
          05 MAF016-SECTION              PIC X(08) VALUE SPACES.        81      
          05 MAF016-ABSTRACT             PIC X(06) VALUE SPACES.        89      
          05 MAF016-OPERATOR             PIC X(32) VALUE SPACES.        95      
          05 MAF016-TOTAL-DEPTH          PIC 9(05) VALUE ZEROS.         127     
          05 MAF016-WELL-NUMBER          PIC X(06) VALUE SPACES.        132     
          05 MAF016-LEASE-NAME           PIC X(32) VALUE SPACES.        138     
          05 MAF016-PERMIT-NUM           PIC 9(06) VALUE ZEROS.         170     
      *----------------------------------------------------------------*
      * REDEFINES NOT SUPPORTED
      * WORKING ARROUND EXPANDING THE SUB FIELDS ISTEAD REDEFINING:
      *   05 MAF016-GAS-RRCID            PIC 9(06) VALUE ZEROS.         176
      *    05 FILLER REDEFINES MAF016-GAS-RRCID.                                 
      *       10 MAF016-OIL-LEASE-NUM     PIC 9(05).                             
      *       10 FILLER                   PIC 9.                                 
      *----------------------------------------------------------------*
          05 MAF016-GAS-RRCID.
             10 MAF016-OIL-LEASE-NUM     PIC 9(05).                     176
             10 FILLER                   PIC 9.                         181
          05 MAF016-FIELD-NAME           PIC X(32) VALUE SPACES.        182     
          05 MAF016-COMPLETION-DATE      PIC 9(08) VALUE ZEROS.         214     
          05 MAF016-PLUG-DATE            PIC 9(08) VALUE ZEROS.         222     
          05 MAF016-REFER-TO-API         PIC 9(08) VALUE ZEROS.         230     
          05 MAF016-ON-OFF-SCHEDULE      PIC X VALUE SPACE.             238     
          05 MAF016-OIL-GAS-CODE         PIC X VALUE SPACE.             239     
          05 FILLER                      PIC X VALUE SPACE.             240     