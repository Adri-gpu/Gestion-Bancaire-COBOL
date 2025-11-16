      *>----------------------------------------------------*
      *> Definition de l'enregistrement OPERATION           *
      *>----------------------------------------------------*
       01  OPERATION-REC.
           05 OPR-ID          PIC 9(8).
           05 OPR-NUM-COMPTE  PIC X(10).
           05 OPR-DATE        PIC 9(8).
           05 OPR-SENS        PIC X(1).
           05 OPR-MONTANT     PIC S9(7)V99.
           05 OPR-LIBELLE     PIC X(30).
