      ******************************************************************
      *                                                                *
      *     ----------------CREDIT MANAGEMENT SYSTEM--------------     *
      *                                                                *
      *              VISAO CLIENTE - ENDERECOS                         *
S15742*                      RECORD LAYOUT - BVQ0058C (LRECL: 500)     *      
      *              USED FOR PROGRAMS:                                *
      *                VQOEM00, VQOEM01, VQOEM02                       *
      *              USED FOR FILES:                                   *
      *                                                                *
      *****************************************************************
      *
       01  :DATA2:-RECORD.
           03  :DATA2:-KEY.
               05  :DATA2:-NUM-ORG-X.
                   07  :DATA2:-NUM-ORG          PIC 999.
                       88  :DATA2:-HEADER-REC             VALUE 000.
                       88  :DATA2:-TRAILER-REC            VALUE 999.
               05  :DATA2:-NUM-CPF-CNPJ         PIC X(14).
               05  :DATA2:-IND-TIP-PES          PIC 9(01).
                   88  :DATA2:-FISICO                     VALUE 0.
                   88  :DATA2:-JURIDICO                   VALUE 1.
               05  :DATA2:-IND-QLFC-PES         PIC X(01).
                   88  :DATA2:-CLIENTE                    VALUE 'C'.
                   88  :DATA2:-PORTADOR                   VALUE 'P'.
                   88  :DATA2:-ADICIONAL                  VALUE 'A'.
                   88  :DATA2:-CONTATO                    VALUE 'O'.
               05  :DATA2:-QTD-ENDR             PIC 9(01)V99.
           03  :DATA2:-AREA-DADOS.
               05  :DATA2:-TXT-LOGR                 PIC X(45).
               05  :DATA2:-NUM-LOGR                 PIC X(06).
               05  :DATA2:-TXT-CPL                  PIC X(15).
               05  :DATA2:-NUM-CEP                  PIC 9(08).
               05  :DATA2:-NOM-BRR                  PIC X(15).
               05  :DATA2:-NOM-MNCP                 PIC X(25).
               05  :DATA2:-NOM-UF                   PIC X(02).
               05  :DATA2:-NOM-PAIS                 PIC X(20).
               05  :DATA2:-NUM-DDD-TEL-FIX          PIC X(04).
               05  :DATA2:-NUM-TEL-FIX              PIC X(10).
S15742         05  :DATA2:-NUM-RML-TEL-FIX-ANT      PIC X(05).
               05  :DATA2:-NUM-DDD-TEL-CEL          PIC X(04).
               05  :DATA2:-NUM-TEL-CEL              PIC X(10).
               05  :DATA2:-NUM-DDD-FAX              PIC X(04).
               05  :DATA2:-NUM-FAX                  PIC X(10).
               05  :DATA2:-COD-NUM-BIP              PIC X(20).
               05  :DATA2:-IND-QLDE-ENDR            PIC X(01).
               05  :DATA2:-IND-QLDE-TEL-FIX         PIC X(01).
               05  :DATA2:-IND-QLDE-TEL-CEL         PIC X(01).
               05  :DATA2:-DAT-INI-ENDR             PIC S9(07) COMP-3.
               05  :DATA2:-DAT-ULT-MNT              PIC S9(07) COMP-3.
               05  :DATA2:-IND-OPER-ULT-MNT         PIC X(03).
               05  :DATA2:-DAT-MNT-ENDR-FRUD        PIC S9(07) COMP-3.
               05  :DATA2:-IND-ENDR-CRRS            PIC X(01).
               05  :DATA2:-COD-CID                  PIC 9(03)V99.
               05  :DATA2:-IND-ATLZ-XXBS            PIC X(01).
               05  :DATA2:-NUM-CLI                  PIC X(19).
               05  :DATA2:-DAT-DST-ENDR             PIC S9(07) COMP-3.   
               05  :DATA2:-ORIGEM                   PIC X(01).
                   88  :DATA2:-CONVERSOR-PADRAO           VALUE 'C'.
S10447         05  :DATA2:-TXT-REF                  PIC X(40).
SP9871         05  :DATA2:-NUM-DDI-TEL              PIC X(04).
SP9408         05  :DATA2:-NUM-DDI-CELULAR          PIC X(04).
S15742         05  :DATA2:-NUM-DDI-FAX              PIC X(04).
S15742         05  :DATA2:-NUM-RML-FAX              PIC X(06).
S15742         05  :DATA2:-NUM-RML-TEL-FIX          PIC X(06).
12345          05  :DATA2:-DAT-ALT-ENDR             PIC S9(07) COMP-3.
12345          05  :DATA2:-DAT-ALT-TEL-FIXO         PIC S9(07) COMP-3.
12345          05  :DATA2:-DAT-ALT-TEL-CEL          PIC S9(07) COMP-3.
12345          05  :DATA2:-IDF-DST-ENDR             PIC X(01).
12345              88  :DATA2:-DST-ENDR-RESID            VALUE 'R'.
12345              88  :DATA2:-DST-ENDR-COMERC           VALUE 'C'.
12345              88  :DATA2:-DST-ENDR-CORRESP          VALUE 'F'.
12345              88  :DATA2:-DST-OUTROS                VALUE ' '.
12345          05  :DATA2:-FILLER                   PIC X(149).
12345          05 WS-C OCCURS 2 TIMES.
12345             07 WS-TESTE                      PIC S9(7) COMP-3.
12345             07 WS-LIXO                       PIC X(02).
12345             07 WS-C2 OCCURS 3 TIMES.
12345                09 WS-SEGURO                  PIC S9(7) COMP-3.
12345                09 WS-CLIENTE                 PIC X(2).
12345 *------- 05  :DATA2:-FILLER                   PIC X(140).
12345 *------- 05  :DATA2:-FILLER                   PIC X(162).          
S15742*------- 05  :DATA2:-FILLER                   PIC X(178).          
