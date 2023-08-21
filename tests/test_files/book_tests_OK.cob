      ******************************************************************00000100
      *                    COPYBOOK - DT00MIG                          *00000200
      *----------------------------------------------------------------*00000300
      * DESCRICAO : LAYOUT ARQUIVO CLIENTE CONTA E ENDERECO            *00000400
      *              RT MIGRACAO - SQUAD CADASTRAL                     *00000500
      *----------------------------------------------------------------*00000600
      *    LAYOUT DO ARQUIVO DE:                                       *00000700
      *                                                                *00000800
      *       - 01 = HEADER                                            *00000900
      *       - 02 = DETALHE                                           *00001000
      *       - 99 = TRAILLER                                          *00001100
      *                                                                *00001200
      *  TAMANHO: 2500 BYTES                                           *00001300
      ******************************************************************00001400
      *                      A L T E R A C O E S                       *00001500
      ******************************************************************00001600
      *   DATA........:                                                *00001700
      *   AUTOR.......:                                                *00001800
      *   REFERENCIA..:                                                *00001900
      *   DESCRICAO...:                                                *00002000
      ******************************************************************00002100
      *================================================================*00002200
      *                 HEADER DO ARQUIVO                              *00002300
      *================================================================*00002400
       01  :DT00MIG:-HEADER.                                            00002500
           03  :DT00MIG:-HDR-TIPO-REG                 PIC 9(002).       00002600
               88 :DT00MIG:-TIPO-REG                    VALUE 01.       00002700
           03  :DT00MIG:-TIPO-ARQUIVO                 PIC X(020).       00002800
      *         TIPO DE ARQUIVO.                                        00002900
           03  :DT00MIG:-HDR-DTA-GER                  PIC 9(010).       00003000
      *         DT GERACAO ARQUIVO AAAA-MM-DD.                          00003100
           03  :DT00MIG:-HDR-HOR-GER                  PIC 9(008).       00003200
      *         HR GERACAO ARQUIVO HH:MM:SS.                            00003300
           03  :DT00MIG:-COD-ONDA-MIG                 PIC X(027).       00003400
      *         CODIGO ONDA MIGRACAO DATA E HORA.                       00003500
           03  FILLER                                 PIC X(2433).      00003600
      *                                                                 00003700
      *================================================================*00003800
      *                   DADOS DETALHE REGISTRO CONTA                 *00003900
      *================================================================*00004000
      *                                                                 00004100
       01 :DT00MIG:-TIPO-DETALHE.                                       00004200
           03  :DT00MIG:-TP-REG                       PIC  9(002).      00004300
               88  :DT00MIG:-TP-TIPO-DETALHE             VALUE 02.      00004400
           03  :DT00MIG:-XXYY-ORG                     PIC 9(03).        00004500
           03  :DT00MIG:-XXYY-ACCT                    PIC X(19).        00004600
           03  :DT00MIG:-XXYY-DOB                     PIC 9(04).        00004700
           03  :DT00MIG:-XXYY-NAME-LINE-1             PIC X(40).        00004800
           03  :DT00MIG:-XXYY-NUM-DOCUMENTO           PIC X(15).        00004900
           03  :DT00MIG:-XXYY-ORG-EMIS-DOC            PIC X(08).        00005000
           03  :DT00MIG:-XXYY-UF-EXPD-DOCUMENTO       PIC X(02).        00005100
           03  :DT00MIG:-XXYY-DATA-EXPEDICAO          PIC 9(08).        00005200
           03  :DT00MIG:-XXYY-USER-13                 PIC X(30).        00005300
           03  :DT00MIG:-XXYY-USER-12                 PIC X(30).        00005400
           03  :DT00MIG:-XXYY-POSITION                PIC X(04).        00005500
           03  :DT00MIG:-XXYY-CODIGO-PEP              PIC X(02).        00005600
           03  :DT00MIG:-XXYY-NAME-TYPE-IND           PIC 9(01).        00005700
           03  :DT00MIG:-XXYY-RELATIVE-NAME           PIC X(40).        00005800
           03  :DT00MIG:-XXYY-NOME-CONJUGE            PIC X(40).        00005900
           03  :DT00MIG:-XXYY-CPF-CONJUGE             PIC X(11).        00006000
           03  :DT00MIG:-XXYY-NUMDOC-CONJUGE          PIC X(13).        00006100
           03  :DT00MIG:-XXYY-ORG-EMI-DOC-CONJUGE     PIC X(10).        00006200
           03  :DT00MIG:-XXYY-DAT-EXPD-DOC-CONJUGE    PIC 9(08).        00006300
           03  :DT00MIG:-XXYY-DATA-ATU-CONJUGE        PIC 9(08).        00006400
           03  :DT00MIG:-XXYY-TIPO-DOCUMENTO          PIC X(02).        00006500
           03  :DT00MIG:-XXYY-NUM-DOCUMENTO           PIC X(15).        00006600
           03  :DT00MIG:-XXYY-POSSUI-PATRIM           PIC X(01).        00006700
           03  :DT00MIG:-XXYY-EMPLOYER                PIC X(40).        00006800
           03  :DT00MIG:-XXYY-EMPLOYER-ADDR-1         PIC X(40).        00006900
           03  :DT00MIG:-XXYY-EMAIL                   PIC X(60).        00007000
           03  :DT00MIG:-XXYY-ACCT                    PIC X(19).        00007100
           03  :DT00MIG:-XXYY-CANAL-VENDA-PAN         PIC X(04).        00007200
           03  :DT00MIG:-XXYY-DUE-DAY                 PIC 9(02).        00007300
           03  :DT00MIG:-XXYY-NUM-CPF-CNPJ            PIC X(14).        00007400
           03  :DT00MIG:-XXYY-USER-ACCT-NBR           PIC X(19).        00007500
           03  :DT00MIG:-XXYY-NOM-ENDR-ELET           PIC X(60).        00007600
           03  :DT00MIG:-XXYY-INCOME                  PIC 9(09).        00007700
           03  :DT00MIG:-XXYY-FLAG-ESTATUTARIO        PIC X(01).        00007800
           03  :DT00MIG:-XXYY-CRLIM                   PIC X(17).        00007900
           03  :DT00MIG:-XXYY-CANAL-VENDA             PIC X(03).        00008000
           03  :DT00MIG:-XXYY-SOURCE                  PIC X(01).        00008100
           03  :DT00MIG:-XXYY-DATE-CONVERSION         PIC 9(08).        00008200
           03  :DT00MIG:-XXYY-LAST-CRLIM              PIC 9(17).        00008300
           03  :DT00MIG:-XXYY-DATE-CRLIM              PIC 9(08).        00008400
           03  :DT00MIG:-XXYY-DATE-LAST-CRLIM         PIC 9(08).        00008500
           03  :DT00MIG:-XXYY-PMT-TOT-AMT-DUE         PIC 9(17).        00008600
           03  :DT00MIG:-XXYY-PMT-LAST-AMT            PIC 9(17).        00008700
           03  :DT00MIG:-XXYY-CURR-BAL                PIC 9(17).        00008800
           03  :DT00MIG:-XXYY-OPEN-TO-BUY             PIC 9(17).        00008900
           03  :DT00MIG:-XXYY-AMT-LAST-PURCH          PIC 9(17).        00009000
           03  :DT00MIG:-XXYY-USER-AMT-1              PIC 9(17).        00009100
           03  :DT00MIG:-XXYY-CASH-CRLIM              PIC 9(17).        00009200
           03  :DT00MIG:-XXYY-DATE-MANUT-FOPA         PIC 9(08).        00009300
           03  :DT00MIG:-XXYY-COD-TAB-DSCT-FOPA       PIC 9(03).        00009400
           03  :DT00MIG:-XXYY-MISC-USER-4             PIC X(11).        00009500
           03  :DT00MIG:-XXYY-FIRST-PURCH-DTE         PIC 9(08).        00009600
           03  :DT00MIG:-XXYY-DATA-MAX-CIP            PIC 9(08).        00009700
           03  :DT00MIG:-XXYY-ENVIADO-CIP             PIC X(01).        00009800
           03  :DT00MIG:-XXYY-AH-AGRMNT-STATUS        PIC X(01).        00009900
           03  :DT00MIG:-XXYY-AH-AGRMNT-BROKEN-DATE   PIC 9(08).        00010000
           03  :DT00MIG:-XXYY-VQ-TOT-PGTO-EM-DIA      PIC 9(17).        00010100
           03  :DT00MIG:-XXYY-VQ-LAST-PGTO-MIN        PIC 9(17).        00010200
           03  :DT00MIG:-XXYY-VP-VIP-TIPO             PIC X(02).        00010300
           03  :DT00MIG:-XXYY-VQ-PMT-DAYS-DELQ        PIC 9(03).        00010400
           03  :DT00MIG:-XXYY-TROCA-MOEDA-HEDGE-SW    PIC X(01).        00010500
           03  :DT00MIG:-XXYY-TROCA-MOEDA-HEDGE-AMT   PIC 9(05).        00010600
           03  :DT00MIG:-XXYY-LAST-CARGA-HEDGE        PIC 9(05).        00010700
           03  :DT00MIG:-XXYY-CURR-BAL-HEDGE          PIC 9(09).        00010800
           03  :DT00MIG:-XXYY-QTD-CARTAO-RECORRENTE   PIC 9(02).        00010900
           03  :DT00MIG:-XXYY-FLAG-FOPA               PIC X(01).        00011000
           03  :DT00MIG:-XXYY-FLG-COMPULS             PIC X(01).        00011100
           03  :DT00MIG:-XXYY-FLAG-FATURA-ELETR       PIC X(01).        00011200
           03  :DT00MIG:-XXYY-DATA-FLAG-FATURA-ELETR  PIC 9(08).        00011300
           03  :DT00MIG:-XXYY-AVAL-EMERG-CREDITO      PIC X(01).        00011400
           03  :DT00MIG:-XXYY-DATA-MANUT-AEC          PIC 9(08).        00011500
           03  :DT00MIG:-XXYY-CODIGO-PRODUTO          PIC 9(03).        00011600
           03  :DT00MIG:-XXYY-NUM-CART-TIT            PIC X(19).        00011700
           03  :DT00MIG:-XXYY-HASH-CARTAO             PIC X(19).        00011800
           03  :DT00MIG:-XXYY-TIPO-TAB-ANDD           PIC X(01).        00011900
           03  :DT00MIG:-XXYY-COD-TAB-ANDD            PIC 9(05).        00012000
           03  :DT00MIG:-XXYY-COD-TAB-DSCT            PIC 9(05).        00012100
           03  :DT00MIG:-XXYY-NUM-MOT-BLCK-CODE-1     PIC 9(02).        00012200
           03  :DT00MIG:-XXYY-SLD-FATURA-PENULT       PIC 9(17).        00012300
           03  :DT00MIG:-XXYY-IND-ADSO-PGM-RCMP       PIC X(01).        00012400
           03  :DT00MIG:-XXYY-LOGO-GRADE-1            PIC 9(03).        00012500
           03  :DT00MIG:-XXYY-LOGO-GRADE-2            PIC 9(03).        00012600
           03  :DT00MIG:-XXYY-LOGO-GRADE-3            PIC 9(03).        00012700
           03  :DT00MIG:-XXYY-DATE-GRADE-1            PIC 9(08).        00012800
           03  :DT00MIG:-XXYY-DATE-GRADE-2            PIC 9(08).        00012900
           03  :DT00MIG:-XXYY-DATE-GRADE-3            PIC 9(08).        00013000
           03  :DT00MIG:-XXYY-PRODUTO-GRADE-1         PIC 9(05).        00013100
           03  :DT00MIG:-XXYY-PRODUTO-GRADE-2         PIC 9(05).        00013200
           03  :DT00MIG:-XXYY-PRODUTO-GRADE-3         PIC 9(05).        00013300
           03  :DT00MIG:-XXYY-DAT-IND-ADSO-PGM-RCMP   PIC 9(08).        00013400
           03  :DT00MIG:-XXYY-STATUS-CPF              PIC X(01).        00013500
           03  :DT00MIG:-XXYY-DT-ALT-STATUS-CPF       PIC 9(08).        00013600
           03  :DT00MIG:-XXYY-TOTAL-CREDITO-CIP       PIC 9(17).        00013700
           03  :DT00MIG:-XXYY-ATUALIZA-CIP            PIC X(01).        00013800
           03  :DT00MIG:-XXYY-DESC-ANUID              PIC 9(05).        00013900
           03  :DT00MIG:-XXYY-CNL-VENDA               PIC 9(03).        00014000
           03  :DT00MIG:-XXYY-NUM-PROTOCOLO           PIC 9(17).        00014100
           03  :DT00MIG:-XXYY-CELULA-TESTE            PIC 9(03).        00014200
           03  :DT00MIG:-XXYY-ACAO-VENDA              PIC 9(09).        00014300
           03  :DT00MIG:-XXYY-FLAG-CTA-MULTIPLA       PIC X(01).        00014400
           03  :DT00MIG:-XXYY-STATUS-MULTIPLO         PIC 9(02).        00014500
           03  :DT00MIG:-XXYY-DT-ALTER-ST-MULTIPLO    PIC 9(08).        00014600
           03  :DT00MIG:-XXYY-DT-ADESAO-MULTIPLO      PIC 9(08).        00014700
           03  :DT00MIG:-XXYY-DT-CONCESSAO-CRED       PIC 9(08).        00014800
           03  :DT00MIG:-XXYY-AG-MULTIPLO             PIC 9(05).        00014900
           03  :DT00MIG:-XXYY-CC-MULTIPLO             PIC 9(09).        00015000
           03  :DT00MIG:-XXYY-IND-ELEG-MULTIPLO       PIC X(01).        00015100
           03  :DT00MIG:-XXYY-IND-CONTA-FUNC          PIC X(01).        00015200
           03  :DT00MIG:-XXYY-COD-PACOTE-SMS          PIC 9(02).        00015300
           03  :DT00MIG:-XXYY-DAT-PACOTE-SMS          PIC 9(08).        00015400
           03  :DT00MIG:-XXYY-STATUS-REFIN            PIC 9(02).        00015500
           03  :DT00MIG:-XXYY-STATUS-REFIN-PEND       PIC 9(02).        00015600
           03  :DT00MIG:-XXYY-DATE-STATUS-REFIN       PIC 9(08).        00015700
           03  :DT00MIG:-XXYY-DATE-DESBLO-REFIN       PIC 9(08).        00015800
           03  :DT00MIG:-XXYY-DATE-REFIN-INCONS       PIC 9(08).        00015900
           03  :DT00MIG:-XXYY-AMT-REFIN-INCONS        PIC 9(17).        00016000
           03  :DT00MIG:-XXYY-COD-PRF                 PIC 9(03).        00016100
           03  :DT00MIG:-XXYY-NOM-ENDR-ELET-2         PIC X(60).        00016200
           03  :DT00MIG:-XXYY-LOGO                    PIC 9(03).        00016300
           03  :DT00MIG:-XXYY-SHORT-NAME              PIC X(20).        00016400
           03  :DT00MIG:-XXYY-STATE-OF-RESID          PIC 9(03).        00016500
           03  :DT00MIG:-XXYY-STATE-OF-ISSUE          PIC 9(03).        00016600
           03  :DT00MIG:-XXYY-INT-STATUS              PIC X(01).        00016700
           03  :DT00MIG:-XXYY-BLOCK-CODE-1            PIC X(01).        00016800
           03  :DT00MIG:-XXYY-BLOCK-CODE-2            PIC X(01).        00016900
           03  :DT00MIG:-XXYY-BILLING-CYCLE           PIC 9(02).        00017000
           03  :DT00MIG:-XXYY-USER-CODE-7             PIC X(02).        00017100
           03  :DT00MIG:-XXYY-USER-CODE-8             PIC X(02).        00017200
           03  :DT00MIG:-XXYY-DATE-OPENED             PIC 9(08).        00017300
           03  :DT00MIG:-XXYY-DATE-BLOCK-CODE-1       PIC 9(08).        00017400
           03  :DT00MIG:-XXYY-DATE-BLOCK-CODE-2       PIC 9(08).        00017500
           03  :DT00MIG:-XXYY-DATE-CARD-FEE           PIC 9(08).        00017600
           03  :DT00MIG:-XXYY-DATE-LAST-PURCH         PIC 9(08).        00017700
           03  :DT00MIG:-XXYY-DATE-LAST-DELQ          PIC 9(08).        00017800
           03  :DT00MIG:-XXYY-DATE-LAST-STMT          PIC 9(08).        00017900
           03  :DT00MIG:-XXYY-DATE-NEXT-STMT          PIC 9(08).        00018000
           03  :DT00MIG:-XXYY-DATE-PMT-DUE            PIC 9(08).        00018100
           03  :DT00MIG:-XXYY-DATE-LAST-PMT           PIC 9(08).        00018200
           03  :DT00MIG:-XXYY-DATE-CARD-EXPR          PIC 9(08).        00018300
           03  :DT00MIG:-XXYY-CASH-AVAIL-CREDIT       PIC 9(17).        00018400
           03  :DT00MIG:-XXYY-VQ-SPM-CYCLE            PIC 9(17).        00018500
           03  :DT00MIG:-XXYY-VQ-SPM-ACCUM            PIC 9(17).        00018600
           03  :DT00MIG:-DDD-CELULAR-MG-SMS           PIC X(04).        00018700
           03  :DT00MIG:-CELULAR-MG-SMS               PIC X(10).        00018800
           03  :DT00MIG:-XXYY-FH-STMT-DATE-1          PIC 9(08).        00018900
           03  :DT00MIG:-XXYY-FH-STMT-DATE-2          PIC 9(08).        00019000
           03  :DT00MIG:-XXYY-FH-STMT-DATE-3          PIC 9(08).        00019100
           03  :DT00MIG:-XXYY-FH-STMT-DATE-4          PIC 9(08).        00019200
           03  :DT00MIG:-XXYY-FH-STMT-DATE-5          PIC 9(08).        00019300
           03  :DT00MIG:-XXYY-FH-STMT-DATE-6          PIC 9(08).        00019400
           03  :DT00MIG:-XXYY-SH-RAW-SCORE-1          PIC 9(03).        00019500
           03  :DT00MIG:-XXYY-SH-RAW-SCORE-2          PIC 9(03).        00019600
           03  :DT00MIG:-XXYY-SH-RAW-SCORE-3          PIC 9(03).        00019700
           03  :DT00MIG:-XXYY-SH-RAW-SCORE-4          PIC 9(03).        00019800
           03  :DT00MIG:-XXYY-SH-RAW-SCORE-5          PIC 9(03).        00019900
           03  :DT00MIG:-XXYY-SH-RAW-SCORE-6          PIC 9(03).        00020000
           03  :DT00MIG:-XXYY-SH-BEHAVIOR-SCORE-1     PIC 9(03).        00020100
           03  :DT00MIG:-XXYY-SH-BEHAVIOR-SCORE-2     PIC 9(03).        00020200
           03  :DT00MIG:-XXYY-SH-BEHAVIOR-SCORE-3     PIC 9(03).        00020300
           03  :DT00MIG:-XXYY-SH-BEHAVIOR-SCORE-4     PIC 9(03).        00020400
           03  :DT00MIG:-XXYY-SH-BEHAVIOR-SCORE-5     PIC 9(03).        00020500
           03  :DT00MIG:-XXYY-SH-BEHAVIOR-SCORE-6     PIC 9(03).        00020600
           03  :DT00MIG:-XXYY-SH-SCORECARD-ID-1       PIC 9(03).        00020700
           03  :DT00MIG:-XXYY-SH-SCORECARD-ID-2       PIC 9(03).        00020800
           03  :DT00MIG:-XXYY-SH-SCORECARD-ID-3       PIC 9(03).        00020900
           03  :DT00MIG:-XXYY-SH-SCORECARD-ID-4       PIC 9(03).        00021000
           03  :DT00MIG:-XXYY-SH-SCORECARD-ID-5       PIC 9(03).        00021100
           03  :DT00MIG:-XXYY-SH-SCORECARD-ID-6       PIC 9(03).        00021200
           03  :DT00MIG:-TIPO-ENDERECO                PIC X(01).        00021300
               88  :DT00MIG:-TP-END-RESIDENCIAL       VALUE 'R'.        00021400
               88  :DT00MIG:-TP-END-COMERCIAL         VALUE 'C'.        00021500
               88  :DT00MIG:-TP-END-FATURA            VALUE 'F'.        00021600
           03  :DT00MIG:-ENDERECO OCCURS 03 TIMES.                      00021700
             05 :DT00MIG:-XXYY-NUM-CEP                PIC 9(08).        00021800
             05 :DT00MIG:-XXYY-TXT-LOGR               PIC X(45).        00021900
             05 :DT00MIG:-XXYY-NUM-LOGR               PIC X(06).        00022000
             05 :DT00MIG:-XXYY-NOM-BRR                PIC X(15).        00022100
             05 :DT00MIG:-XXYY-TXT-CPL                PIC X(15).        00022200
             05 :DT00MIG:-XXYY-NOM-MNCP               PIC X(25).        00022300
             05 :DT00MIG:-XXYY-NOM-UF                 PIC X(02).        00022400
             05 :DT00MIG:-XXYY-NOM-PAIS               PIC X(20).        00022500
             05 :DT00MIG:-XXYY-NUM-TEL-CEL            PIC X(10).        00022600
             05 :DT00MIG:-XXYY-NUM-DDD-TEL-CEL        PIC X(04).        00022700
           03  :DT00MIG:-UUID                         PIC X(36).        00022800
           03  :DT00MIG:-STATUS-REGISTRO              PIC X(02).        00022900
           03  :DT00MIG:-FLAG-MOT-EXCLUSAO            PIC X(02).        00023000
           03  :DT00MIG:-ID-ALTERACAO-CAD             PIC X(220).       00023100
           03  FILLER                                 PIC X(249).       00023200
      *================================================================*00023300
      *                       TRAILER DO ARQUIVO                       *00023400
      *================================================================*00023500
       01 :DT00MIG:-TRAILER.                                            00023600
           03  :DT00MIG:-TT-REG                       PIC  9(002).      00023700
               88  :DT00MIG:-TP-TRAILER                  VALUE 99.      00023800
           03  :DT00MIG:-QTD-REGIS                    PIC  9(017).      00023900
           03  FILLER                                 PIC  X(2481).     00024000
      *---------------------------------------------------------------* 00024100
      *                      FINAL DO BOOK                            * 00024200
      *---------------------------------------------------------------* 00024300

