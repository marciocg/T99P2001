       01 200E-REGISTRO.
          03 200E-REG                PIC X(60).
          03 200E-REG-HDR REDEFINES 200E-REG.
             05 FILLER               PIC X(07).
             05 200E-NOME-ARQ        PIC X(07).
             05 200E-AMD-ARQ         PIC 9(08).
             05 FILLER               PIC X(33).
             05 FILLER               PIC X(05).
          03 200E-REG-DET REDEFINES 200E-REG.
             05 200E-MATR            PIC 9(07).
                88 200E-HDR          VALUE ZEROES.
                88 200E-TRL          VALUE 9999999.
             05 200E-NOME            PIC X(30).
             05 200E-SEXO            PIC X(01).
             05 200E-CPF             PIC 9(11).
             05 200E-EST-CIVIL       PIC X(01).
             05 FILLER               PIC X(03).
             05 200E-CMDO            PIC 9(02).
             05 200E-SEQ             PIC 9(05).
          03 200E-REG-TRL REDEFINES 200E-REG.
             05 FILLER               PIC X(07).
             05 200E-SOMAT-SEQ       PIC 9(09).
             05 FILLER               PIC X(44).
