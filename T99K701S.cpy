       01 701S-REGISTRO.
      *   03 701S-REG                PIC X(72).
          03 701S-CAB-01.
             05 FILLER               PIC X(24)
                                        VALUE '*** RELATORIO ERROS ***'.
             05 701S-DD              PIC X(02).
             05 701S-MM              PIC X(09).
             05 701S-AA              PIC X(04).
             05 FILLER               PIC X(29).
             05 701S-PAG             PIC ZZZ9.
          03 701S-CAB-02.
             05 FILLER               PIC X(72) VALUE ALL ' - CAB2 -'.
          03 701S-CAB-03.
             05 FILLER               PIC X(72) VALUE ALL ' - CAB3 -'.
          03 701S-CAB-04.
             05 FILLER               PIC X(72) VALUE ALL ' - CAB4 -'.
          03 701S-LIN-DET.
             05 701S-SEQ-REG         PIC X(05).
             05 FILLER               PIC X(01).
             05 701S-MATR            PIC 9(07).
             05 701S-NOME            PIC X(35).
             05 701S-OCORR           PIC X(16).
             05 FILLER               PIC X(08).
          03 701S-FIM-REL.
             05 FILLER               PIC X(72)
                                       VALUE ALL '*** FIM RELATORIO '.
          03 701S-LIN-HIFEN.
             05 FILLER               PIC X(72) VALUE ALL '-'.
