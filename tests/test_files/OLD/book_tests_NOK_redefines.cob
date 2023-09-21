      **COMENTARIO*****************************************************
      *
       01  :DT00:-RECORD.
           03  :DT00:-KEY.
               05  :DT00:-NUM-ORG-X.
                   07  :DT00:-NUM-ORG          PIC 999.
                       88  :DT00:-HEADER-REC             VALUE 000.
                       88  :DT00:-TRAILER-REC            VALUE 999.
               05  :DT00:-NUM-CPF-CNPJ         PIC X(14).
               05  :DT00:-IND-TIP-PES          PIC 9(01).
                   88  :DT00:-FISICO                     VALUE 0.
                   88  :DT00:-JURIDICO                   VALUE 1.
               05  :DT00:-IND-QLFC-PES         PIC X(01).
                   88  :DT00:-CLIENTE                    VALUE 'C'.
                   88  :DT00:-PORTADOR                   VALUE 'P'.
                   88  :DT00:-ADICIONAL                  VALUE 'A'.
                   88  :DT00:-CONTATO                    VALUE 'O'.
               05  :DT00:-QTD-ENDR             PIC 9(01)V99.
           03  :DT00:-AREA-DADOS.
               05  :DT00:-TXT-LOGR                 PIC X(45).
               05  :DT00:-NUM-LOGR                 PIC X(06).
               05  :DT00:-TXT-CPL                  PIC X(15).
               05  :DT00:-NUM-CEP                  PIC 9(08).
               05  :DT00:-NOM-BRR                  PIC X(15).
               05  :DT00:-NOM-MNCP                 PIC X(25).
               05  :DT00:-NOM-UF                   PIC X(02).
               05  :DT00:-NOM-PAIS                 PIC X(20).
               05  :DT00:-NUM-DDD-TEL-FIX          PIC X(04).
               05  :DT00:-NUM-TEL-FIX              PIC X(10).
S15742         05  :DT00:-NUM-RML-TEL-FIX-ANT      PIC X(05).
               05  :DT00:-NUM-DDD-TEL-CEL          PIC X(04).
               05  :DT00:-NUM-TEL-CEL              PIC X(10).
               05  :DT00:-NUM-DDD-FAX              PIC X(04).
               05  :DT00:-NUM-FAX                  PIC X(10).
               05  :DT00:-COD-NUM-BIP              PIC X(20).
               05  :DT00:-IND-QLDE-ENDR            PIC X(01).
               05  :DT00:-IND-QLDE-TEL-FIX         PIC X(01).
               05  :DT00:-IND-QLDE-TEL-CEL         PIC X(01).
               05  :DT00:-DAT-INI-ENDR             PIC S9(07) COMP-3.
               05  :DT00:-DAT-ULT-MNT              PIC S9(07) COMP-3.
               05  :DT00:-IND-OPER-ULT-MNT         PIC X(03).
               05  :DT00:-DAT-MNT-ENDR-FRUD        PIC S9(07) COMP-3.
               05  :DT00:-IND-ENDR-CRRS            PIC X(01).
               05  :DT00:-COD-CID                  PIC 9(03)V99.
               05  :DT00:-IND-ATLZ-XXBS            PIC X(01).
               05  :DT00:-NUM-CLI                  PIC X(19).
               05  :DT00:-DAT-DST-ENDR             PIC S9(07) COMP-3.   O@000223
               05  :DT00:-ORIGEM                   PIC X(01).
                   88  :DT00:-CONVERSOR-PADRAO           VALUE 'C'.
S10447         05  :DT00:-TXT-REF                  PIC X(40).
SP9871         05  :DT00:-NUM-DDI-TEL              PIC X(04).
SP9408         05  :DT00:-NUM-DDI-CELULAR          PIC X(04).
S15742         05  :DT00:-NUM-DDI-FAX              PIC X(04).
S15742         05  :DT00:-NUM-RML-FAX              PIC X(06).
S15742         05  :DT00:-NUM-RML-TEL-FIX          PIC X(06).
               05  :DT00:-DAT-ALT-ENDR             PIC S9(07) COMP-3.
               05  :DT00:-DAT-ALT-TEL-FIXO         PIC S9(07) COMP-3.
               05  :DT00:-DAT-ALT-TEL-CEL          PIC S9(07) COMP-3.
               05  :DT00:-IDF-DST-ENDR             PIC X(01).
                   88  :DT00:-DST-ENDR-RESID            VALUE 'R'.
                   88  :DT00:-DST-ENDR-COMERC           VALUE 'C'.
                   88  :DT00:-DST-ENDR-CORRESP          VALUE 'F'.
                   88  :DT00:-DST-OUTROS                VALUE ' '.
               05  :DT00:-FILLER                   PIC X(149).
               05 WS-C OCCURS 2 TIMES.
                  07 WS-TESTE                      PIC S9(7) COMP-3.
                  07 WS-LIXO                       PIC X(02).
                  07 WS-C2 OCCURS 3 TIMES.
                     09 WS-SEGURO                  PIC S9(7) COMP-3.
                     09 WS-CLIENTE                 PIC X(2).
           03  :DT00:-TESTE-REDEFINES REDEFINES :DT00:-KEY.
               05 :DT00:-TESTE-KEY                 PIC X()
               05* :DT00:-NOVES-ALPHABETIC         PIC F(12).
