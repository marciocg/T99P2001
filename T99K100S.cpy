       01 100S-REGISTRO.
          03 100S-REG                PIC X(60).
          03 100S-REG-HDR REDEFINES 100S-REG.
             05 FILLER               PIC X(07).
             05 100S-NOME-ARQ        PIC X(07).
             05 100S-AMD-ARQ         PIC 9(08).
             05 FILLER               PIC X(33).
             05 FILLER               PIC X(05).
          03 100S-REG-DET REDEFINES 100S-REG.
             05 100S-MATR            PIC 9(07).
                88 100S-HDR          VALUE ZEROES.
                88 100S-TRL          VALUE ALL 9.
             05 100S-NOME            PIC X(30).
             05 100S-SEXO            PIC X(01).
             05 100S-CPF             PIC 9(11).
             05 100S-EST-CIVIL       PIC X(01).
             05 FILLER               PIC X(03).
             05 100S-CMDO            PIC 9(02).
             05 100S-SEQ             PIC 9(05).
          03 100S-REG-TRL REDEFINES 100S-REG.
             05 FILLER               PIC X(07).
             05 100S-SOMAT-SEQ       PIC 9(09).
             05 FILLER               PIC X(44).
