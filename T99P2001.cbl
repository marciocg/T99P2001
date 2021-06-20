      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
      *
       PROGRAM-ID. T99P2001.
      *
      *AUTHOR.  MARCIO CONC
      *REMARKS. BALANCED LINE COM 4 ARQS: 2E, 1S E MAIS 1 RELATORIO.
      *KNOWN-BUGS. DESLOCA SEQ EM ARQUIVO COM 'Ç' ASTERISCAR NOT AT END!
      *DATE-WRITTEN. 15/10/2013.
      *VERSAO 004-MARCIO CONC-29/04/2017-INCLUI FILE STATUS.
      *VERSAO 003-MARCIO CONC-21/04/2017-ADEQUA PARA GNUCOBOL.
      *VERSAO 002-MARCIO CONC-24/10/2013-TRATA REPETICOES NAS ENTRADAS.
      *VERSAO 001-MARCIO CONC-15/10/2013-IMPLANTACAO.
      *
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
      *
       CONFIGURATION SECTION.
      *
      *SOURCE-COMPUTER. POSIX WITH DEBUGGING MODE.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT T99F100E ASSIGN TO 'T99F100E.txt'
               FILE STATUS IS FS-100E
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT T99F200E ASSIGN TO 'T99F200E.txt'
               FILE STATUS IS FS-200E
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT T99F100S ASSIGN TO 'T99F100S.txt'
               FILE STATUS IS FS-100S
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT T99L701S ASSIGN TO 'T99L701S.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
      *
       FILE SECTION.
      *
       FD  T99F100E
           BLOCK  0
           RECORD 60
           RECORDING F.
      *
       01  100E-REGISTRO-FD            PIC X(60).
      *
       FD  T99F200E
           BLOCK  0
           RECORD 60
           RECORDING F.
      *
       01  200E-REGISTRO-FD            PIC X(60).
      *
       FD  T99F100S
           BLOCK  0
           RECORD 60
           RECORDING F.
      *
       01  100S-REGISTRO-FD            PIC X(60).
      *
       FD  T99L701S
           BLOCK  0
           RECORD 72
           RECORDING F.
      *
       01  701S-REGISTRO-FD            PIC X(72).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       77  CTE-PROG                    PIC  X(16)     VALUE
                                               '*** T99P2001 ***'.
       77  CTE-VERS                    PIC  X(06)     VALUE 'VRS004'.
      *-----------------------------------------------------------------
       LOCAL-STORAGE SECTION.
      *-----------------------------------------------------------------
      *{{ INCLUI BOOKS }}
            COPY 'T99K100E.cpy'.
            COPY 'T99K200E.cpy'.
            COPY 'T99K100S.cpy'.
            COPY 'T99K701S.cpy'.
      *{{  FIM BOOKS  }}
       01  TABELA-FIXA.
           03  FILLER                PIC X(09)     VALUE ' JANEIRO'.
           03  FILLER                PIC X(09)     VALUE 'FEVEREIRO'.
           03  FILLER                PIC X(09)     VALUE '  MARCO'.
           03  FILLER                PIC X(09)     VALUE '  ABRIL'.
           03  FILLER                PIC X(09)     VALUE '  MAIO'.
           03  FILLER                PIC X(09)     VALUE ' JUNHO'.
           03  FILLER                PIC X(09)     VALUE ' JULHO'.
           03  FILLER                PIC X(09)     VALUE ' AGOSTO'.
           03  FILLER                PIC X(09)     VALUE 'SETEMBRO'.
           03  FILLER                PIC X(09)     VALUE 'OUTUBRO'.
           03  FILLER                PIC X(09)     VALUE 'NOVEMBRO'.
           03  FILLER                PIC X(09)     VALUE 'DEZEMBRO'.
       01  TABELA-MES REDEFINES TABELA-FIXA.
           03 TAB-MES
              OCCURS   12 TIMES      PIC X(09).
      *
       01  TAB-ERROS-GERAL.
           03  TAB-ERROS  OCCURS  200  TIMES PIC  X(16).
      *
       77  FS-100E                     PIC  XX.
       77  FS-200E                     PIC  XX.
       77  FS-100S                     PIC  XX.
       77  CNT-SEQ-100E                PIC  9(05)     VALUE 1.
       77  SMT-SEQ-100E                PIC  9(09)     VALUE 1.
       77  CNT-SEQ-200E                PIC  9(09)     VALUE 1.
       77  SMT-SEQ-200E                PIC  9(09)     VALUE 1.
       77  CNT-SEQ-100S                PIC  9(05)     VALUE 1.
       77  SMT-SEQ-100S                PIC  9(09)     VALUE 1.
       77  LINHA-701S                  PIC S9(03)     VALUE 16.
       77  LINHAS-POR-PAGINA           PIC S9(03)     
      *                                               VALUE 16.
                                                      VALUE -1.      
       77  PAG-701S                    PIC  9(05)     VALUE 1.
       77  SBS-GRV                     PIC  9(03)     VALUE 0.
      *
       01  GD-100E-REGISTRO            PIC  X(60).
       01  GD-100E-REG-DADOS REDEFINES GD-100E-REGISTRO.
           03  GD-100E-MATR            PIC  9(07).
           03  GD-100E-NOME            PIC  X(30).
           03  FILLER                  PIC  X(18).
           03  GD-100E-SEQ             PIC  9(05).
      *
       01  GD-200E-REGISTRO            PIC  X(60).
       01  GD-200E-REG-DADOS REDEFINES GD-200E-REGISTRO.
           03  GD-200E-MATR            PIC  9(07).
           03  GD-200E-NOME            PIC  X(30).
           03  GD-200E-SEXO            PIC  X(01).
           03  GD-200E-CPF             PIC  9(11).
           03  GD-200E-EST-CIVIL       PIC  X(01).
           03  FILLER                  PIC  X(03).           
           03  GD-200E-CMDO            PIC  9(02).
           03  GD-200E-SEQ             PIC  9(05).
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *
      *--------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *--------------------------------
      **** INICIALIZA TABELA DE ERROS
           MOVE '001 INCLUIDO   '  TO TAB-ERROS (001)
           MOVE '002 ALTERADO   '  TO TAB-ERROS (002)
           MOVE '003 EXCLUIDO   '  TO TAB-ERROS (003)
           MOVE '091 INC. EXIST.'  TO TAB-ERROS (091)
           MOVE '092 ALT. INEXIS'  TO TAB-ERROS (092)
           MOVE '093 EXC. INEXIS'  TO TAB-ERROS (093)
           MOVE '101 INCL. DUPL.'  TO TAB-ERROS (101)
           MOVE '102 ALT. DUPL. '  TO TAB-ERROS (102)
           MOVE '103 EXCL. DUPL.'  TO TAB-ERROS (103)
      *- - Inclusão Existente Duplicada
           MOVE '191 INC. EX. DU'  TO TAB-ERROS (191)
           MOVE '192 ALT. IN. DU'  TO TAB-ERROS (192)
           MOVE '193 EXC. IN. DU'  TO TAB-ERROS (193)
           MOVE '199 CAD. DUPL. '  TO TAB-ERROS (199)
      *
      **** QUANDO O SBS-GRV = 99 O PROCESSAMENTO EH O NORMAL
      *
      *    CALL SBVERSAO USING CTE-PROG CTE-VERS
           DISPLAY CTE-PROG ' ' CTE-VERS ' *** 000 *** INICIO PROGRAMA' 
      *
           PERFORM 900001-ABRE-ARQUIVOS
           PERFORM 100000-TRATA-HEADER
           
           PERFORM 210000-LER-ARQ-100E
           PERFORM 220000-LER-ARQ-200E
           PERFORM 300000-FAZ-BALANC-LINE
                    UNTIL 100E-TRL AND 200E-TRL                    

           PERFORM 600000-TRATA-TRAILER

           DISPLAY CTE-PROG ' ' CTE-VERS ' *** 999 *** FIM NORMAL'
           MOVE 0 TO RETURN-CODE
           .
       999999-ENCERRA.
           PERFORM 900002-FECHA-ARQUIVOS 
           GOBACK
           .
      *-----------------------------
       100000-TRATA-HEADER SECTION.
      *-----------------------------
           READ T99F100E INTO 100E-REGISTRO
                AT END PERFORM 999001-ERRO-001
                NOT AT END MOVE 100E-SEQ TO GD-100E-SEQ
           END-READ
      *
           READ T99F200E INTO 200E-REGISTRO
                AT END PERFORM 999001-ERRO-001
                NOT AT END MOVE 200E-SEQ TO GD-200E-SEQ
           END-READ
      *
           IF NOT 100E-HDR
               PERFORM 999002-ERRO-002
           ELSE
               IF 100E-SEQ     NOT EQUAL 1
                   PERFORM 999003-ERRO-003
               ELSE
                   IF 100E-NOME-ARQ NOT EQUAL 'T99F100'
                       PERFORM 999006-ERRO-006
                   END-IF
               END-IF
           END-IF
      *
           IF NOT 200E-HDR
               PERFORM 999002-ERRO-002
           ELSE
               IF 200E-SEQ     NOT EQUAL 1
                   PERFORM 999003-ERRO-003
               ELSE
                   IF 200E-NOME-ARQ NOT EQUAL 'T99F200'
                       PERFORM 999006-ERRO-006
                   END-IF
               END-IF
           END-IF
      *
      *ARRUMAR NOME ARQ E DATA DO SISTEMA
           MOVE SPACES       TO 100S-REGISTRO.
           MOVE ZEROS        TO 100S-MATR.
           MOVE 'T99F100'    TO 100S-NOME-ARQ.
           MOVE 20131015     TO 100S-AMD-ARQ.
           MOVE CNT-SEQ-100S TO 100S-SEQ.
           WRITE 100S-REGISTRO-FD FROM 100S-REGISTRO
           .
      *
       100999-SAIDA. EXIT.
      *-----------------------------
       210000-LER-ARQ-100E SECTION.
      *-----------------------------
           READ T99F100E INTO 100E-REGISTRO
               AT END 
                   PERFORM 999005-ERRO-005
               NOT AT END
                   IF (NOT 100E-TRL) AND 
                      100E-SEQ NOT EQUAL GD-100E-SEQ + 1
                      PERFORM 999009-ERRO-009
                   END-IF
           END-READ
           IF NOT 100E-TRL
              ADD 1 TO CNT-SEQ-100E
              COMPUTE SMT-SEQ-100E = SMT-SEQ-100E + CNT-SEQ-100E
           END-IF
           .
       210999-SAIDA. EXIT.
      *-----------------------------
       220000-LER-ARQ-200E SECTION.
      *-----------------------------
           READ T99F200E INTO 200E-REGISTRO
                AT END 
                    PERFORM 999007-ERRO-007
                NOT AT END
                    IF (NOT 200E-TRL) AND 
                       200E-SEQ NOT EQUAL GD-200E-SEQ + 1
                       PERFORM 999010-ERRO-010
                    END-IF
           END-READ
           IF NOT 200E-TRL
              ADD 1 TO CNT-SEQ-200E
              COMPUTE SMT-SEQ-200E = SMT-SEQ-200E + CNT-SEQ-200E
           END-IF
           .
       220999-SAIDA. EXIT.
      *-------------------------------
       300000-FAZ-BALANC-LINE SECTION.
      *-------------------------------
           IF  LINHA-701S EQUAL LINHAS-POR-PAGINA
      *    IF  LINHA-701S EQUAL        16
               PERFORM 440000-GRAVA-CABEC
           END-IF
      *
           PERFORM 350000-TRATA-GRAVA
      *
           IF  GD-100E-MATR LESS THAN GD-200E-MATR
               MOVE  99  TO  SBS-GRV
      *D        DISPLAY '=*** ENTROU LESS THAN'
               PERFORM 210000-LER-ARQ-100E
               PERFORM 340000-TRATA-REPET-100E
           ELSE
               IF  GD-100E-MATR GREATER THAN GD-200E-MATR
      *D         DISPLAY '=*** ENTROU GREATER THAN'
                   PERFORM 310000-TRATA-CMDO-MAIOR
                   PERFORM 220000-LER-ARQ-200E
                   PERFORM 330000-TRATA-REPET-200E
               ELSE
                   PERFORM 320000-TRATA-CMDO-IGUAL
      *D         DISPLAY '=*** ENTROU CMDO IGUAL'
                   PERFORM 210000-LER-ARQ-100E
                   PERFORM 340000-TRATA-REPET-100E
                   PERFORM 220000-LER-ARQ-200E
                   PERFORM 330000-TRATA-REPET-200E
               END-IF
           END-IF
           .
      *
       300999-SAIDA. EXIT.
      *--------------------------------
       310000-TRATA-CMDO-MAIOR SECTION.
      *-----------------------------
           IF  GD-200E-CMDO EQUAL 1
               MOVE 01 TO SBS-GRV
           ELSE
               IF  GD-200E-CMDO EQUAL 2
                   MOVE 92 TO SBS-GRV
               ELSE
                   IF  GD-200E-CMDO EQUAL 3
                       MOVE 93 TO SBS-GRV
                   END-IF
               END-IF
           END-IF
           .
       310999-SAIDA. EXIT.
      *-----------------------------
       320000-TRATA-CMDO-IGUAL SECTION.
      *-----------------------------
           IF  GD-200E-CMDO EQUAL 1
               MOVE 91 TO SBS-GRV
           ELSE
               IF  GD-200E-CMDO EQUAL 2
                   MOVE 02 TO SBS-GRV
               ELSE
                   IF  GD-200E-CMDO EQUAL 3
                       MOVE 03 TO SBS-GRV
                   END-IF
               END-IF
           END-IF
           .
       320999-SAIDA. EXIT.
      *-----------------------------
       330000-TRATA-REPET-200E SECTION.
      *-----------------------------
           ADD 100 TO SBS-GRV
           PERFORM  WITH TEST BEFORE 
                    UNTIL 200E-MATR NOT EQUAL GD-200E-MATR
                    PERFORM 430010-GRAVA-MVTODUP-701S
                    MOVE 200E-REGISTRO TO GD-200E-REGISTRO
                    PERFORM 220000-LER-ARQ-200E
           END-PERFORM
           SUBTRACT 100 FROM SBS-GRV
      *- - SE ENCONTROU OS TRAILERS, ENTAO GRAVA ANTES DE SAIR LOOP
           IF 100E-TRL AND 200E-TRL
              PERFORM 350000-TRATA-GRAVA
           END-IF
           .
      *
       330999-SAIDA. EXIT.
      *-----------------------------
       340000-TRATA-REPET-100E SECTION.
      *-----------------------------
           ADD  100 TO SBS-GRV
           PERFORM  WITH TEST BEFORE
                    UNTIL 100E-MATR NOT EQUAL GD-100E-MATR
                    PERFORM 430020-GRAVA-BASEDUP-701S
                    MOVE 100E-REGISTRO TO GD-100E-REGISTRO
                    PERFORM 210000-LER-ARQ-100E
           END-PERFORM
           SUBTRACT 100 FROM SBS-GRV
      *- - SE ENCONTROU OS TRAILERS, ENTAO GRAVA ANTES DE SAIR LOOP
           IF 100E-TRL AND 200E-TRL
              PERFORM 350000-TRATA-GRAVA
           END-IF
           .
      *
       340999-SAIDA. EXIT.
      *-----------------------------
       350000-TRATA-GRAVA SECTION.
      *-----------------------------
           EVALUATE  SBS-GRV
               WHEN  0
                     CONTINUE
               WHEN  1
                     PERFORM 430010-GRAVA-MVTODUP-701S
                     PERFORM 420000-GRAVA-S-200
               WHEN  2
                     PERFORM 430010-GRAVA-MVTODUP-701S
                     PERFORM 420000-GRAVA-S-200
               WHEN  91
                     PERFORM 410000-GRAVA-S-100
                     PERFORM 430010-GRAVA-MVTODUP-701S
               WHEN  99
      *- - - - - - Evita GRAVAR 100E DUPLIC. SE SBS-GRV=99 da mesma MATR
                   IF 100S-MATR NOT EQUAL GD-100E-MATR
                      PERFORM 410000-GRAVA-S-100
                   END-IF
               WHEN  OTHER
                     PERFORM 430010-GRAVA-MVTODUP-701S
           END-EVALUATE.
      *
      ** ATUALIZA/REINICIALIZA GUARDAS E SBS APOS LEITURAS.
      *
           MOVE 99 TO SBS-GRV
           MOVE 200E-REGISTRO TO GD-200E-REGISTRO
           MOVE 100E-REGISTRO TO GD-100E-REGISTRO
           .
      *
       350999-SAIDA. EXIT.
      *---------------------------
       410000-GRAVA-S-100 SECTION.
      *---------------------------
           MOVE SPACES TO 100S-REGISTRO
           MOVE GD-100E-REGISTRO TO 100S-REGISTRO
           ADD 1 TO CNT-SEQ-100S
           MOVE CNT-SEQ-100S TO 100S-SEQ
           WRITE 100S-REGISTRO-FD FROM 100S-REGISTRO
           ADD CNT-SEQ-100S TO SMT-SEQ-100S
           .
       410999-SAIDA. EXIT.
      *-------------------------------
       420000-GRAVA-S-200 SECTION.
      *-------------------------------
           MOVE GD-200E-MATR      TO  100S-MATR
           MOVE GD-200E-NOME      TO  100S-NOME
           MOVE GD-200E-SEXO      TO  100S-SEXO
           MOVE GD-200E-CPF       TO  100S-CPF
           MOVE GD-200E-EST-CIVIL TO  100S-EST-CIVIL
           ADD 1                  TO  CNT-SEQ-100S
           MOVE CNT-SEQ-100S      TO  100S-SEQ
           WRITE 100S-REGISTRO-FD FROM 100S-REGISTRO
      *
           ADD CNT-SEQ-100S TO SMT-SEQ-100S
           .
      *
       420999-SAIDA. EXIT.
      *-------------------------------
       430010-GRAVA-MVTODUP-701S SECTION.
      *-------------------------------
           MOVE GD-200E-SEQ         TO  701S-SEQ-REG
           MOVE GD-200E-MATR        TO  701S-MATR
           MOVE GD-200E-NOME        TO  701S-NOME
           PERFORM 430001-GRAVA-701S
           .
      *-------------------------------
       430020-GRAVA-BASEDUP-701S SECTION.
      *-------------------------------
           MOVE GD-100E-SEQ         TO  701S-SEQ-REG
           MOVE GD-100E-MATR        TO  701S-MATR
           MOVE GD-100E-NOME        TO  701S-NOME
           PERFORM 430001-GRAVA-701S
           .
      *-------------------------------
       430001-GRAVA-701S SECTION.
      *-------------------------------
           MOVE TAB-ERROS (SBS-GRV) TO  701S-OCORR
           WRITE 701S-REGISTRO-FD FROM 701S-LIN-DET
           ADD 1                    TO LINHA-701S
           .
      *---------------------------
       440000-GRAVA-CABEC SECTION.
      *---------------------------
      *
      *    CALL SBCURDAT USING TIP-FUNC DAT-ATUAL HORA-ATUAL.
      *
      *ARRUMAR A DATA PARA PEGAR DO SISTEMA!
      
           MOVE PAG-701S         TO 701S-PAG
           MOVE 21               TO 701S-DD
           MOVE TAB-MES(4)       TO 701S-MM
           MOVE 2017             TO 701S-AA
      *
           WRITE 701S-REGISTRO-FD FROM 701S-CAB-01
           WRITE 701S-REGISTRO-FD FROM 701S-CAB-02
           WRITE 701S-REGISTRO-FD FROM 701S-CAB-03
           WRITE 701S-REGISTRO-FD FROM 701S-LIN-HIFEN
           WRITE 701S-REGISTRO-FD FROM 701S-CAB-04
           WRITE 701S-REGISTRO-FD FROM 701S-LIN-HIFEN
      *
           ADD  1 TO PAG-701S
           MOVE 6 TO LINHA-701S
           .
      *
       440999-SAIDA. EXIT.
      *-------------------------------------
       600000-TRATA-TRAILER SECTION.
      *-------------------------------------
           IF 100E-SOMAT-SEQ NOT EQUAL SMT-SEQ-100E
               PERFORM 999004-ERRO-004
           END-IF
      *
           MOVE SPACES         TO 100S-REG-TRL 
           MOVE 99999999       TO 100S-REG-TRL 
           MOVE SMT-SEQ-100S   TO 100S-SOMAT-SEQ 
           WRITE 100S-REGISTRO-FD FROM 100S-REG-TRL 
      *
           WRITE 701S-REGISTRO-FD FROM 701S-LIN-HIFEN 
           WRITE 701S-REGISTRO-FD FROM 701S-FIM-REL 
           WRITE 701S-REGISTRO-FD FROM 701S-LIN-HIFEN
           .
      *
       400999-SAIDA. EXIT.
      *-----------------------------
       900001-ABRE-ARQUIVOS SECTION.
      *-----------------------------
           OPEN INPUT  T99F100E
                INPUT  T99F200E
                OUTPUT T99F100S
                OUTPUT T99L701S
           IF FS-100E NOT EQUAL ZEROES OR
              FS-200E NOT EQUAL ZEROES OR
              FS-100S NOT EQUAL ZEROES
              PERFORM 999011-ERRO-011
      *       DISPLAY '*O FS-100E: ' FS-100E
      *       DISPLAY '*O FS-200E: ' FS-200E
      *       DISPLAY '*O FS-100S: ' FS-100S
           END-IF
           .
      *-----------------------------
       900002-FECHA-ARQUIVOS SECTION.
      *-----------------------------
           CLOSE T99F100E T99F200E T99F100S T99L701S
           IF FS-100E NOT EQUAL ZEROES OR
              FS-200E NOT EQUAL ZEROES OR
              FS-100S NOT EQUAL ZEROES
              PERFORM 999012-ERRO-012
           END-IF
           .
      *--------------------------
       999001-ERRO-001 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 001: T99F100E VAZIO'
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *    CALL SBABEND.
      *--------------------------
       999002-ERRO-002 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 002 HEADER INVALIDO :' 100E-MATR
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *    CALL SBABEND.
      *--------------------------
       999003-ERRO-003 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 003 SEQ-HDR INVALIDO :' 100E-SEQ
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *    CALL SBABEND.
      *--------------------------
       999004-ERRO-004 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 004 SOMAT-SEQ NAO CONFERE :' 100E-SOMAT-SEQ
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *    CALL SBABEND.
      *--------------------------
       999005-ERRO-005 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 005 T99F100E EOF INESPERADO.'
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *    CALL SBABEND.
      *--------------------------
       999006-ERRO-006 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 006 T99F100E NOME NAO CONFERE :' 100E-NOME-ARQ
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *    CALL SBABEND.
      *--------------------------
       999007-ERRO-007 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 007 T99F200E EOF INESPERADO.'
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *--------------------------
       999008-ERRO-008 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 008 T99F100E REGISTRO BASE DUPLICADO!' 
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *--------------------------
       999009-ERRO-009 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 009 T99F100E SEQ. FORA DE ORDEM! ' 100E-SEQ
                                ' - ' GD-100E-SEQ
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *--------------------------
       999010-ERRO-010 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 010 T99F200E SEQ. FORA DE ORDEM! ' 200E-SEQ
                                ' - ' GD-200E-SEQ
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *--------------------------
       999011-ERRO-011 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 011 ERRO OPEN ARQUIVOS: ' FS-100E ' ' FS-200E
               ' ' FS-100S
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      *--------------------------
       999012-ERRO-012 SECTION.
      *--------------------------
           DISPLAY CTE-PROG ' ' CTE-VERS
           DISPLAY '*** 012 ERRO CLOSE ARQUIVOS: ' FS-100E ' ' FS-200E
               ' ' FS-100S
           MOVE 888 TO RETURN-CODE
           GO TO 999999-ENCERRA
           .
      ******************************************************************
       END PROGRAM T99P2001.
      ******************************************************************
