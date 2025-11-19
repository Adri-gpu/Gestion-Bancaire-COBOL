       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQCONS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENTS
               ASSIGN TO "../data/CLIENTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NUM-CLIENT
               FILE STATUS IS WS-FS-CLIENTS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-CLIENTS
           RECORD CONTAINS 126 CHARACTERS
           COPY "Client.cpy".

       WORKING-STORAGE SECTION.
      *> 01  FS-CLIENTS      PIC XX VALUE "00".
       01  WS-FS-CLIENTS          PIC XX VALUE SPACES.
       01  EOF-CLIENTS     PIC X  VALUE "N".

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM OUVRIR-FICHIERS
           IF WS-FS-CLIENTS = "00"
              PERFORM LISTER-CLIENTS
           END-IF
           PERFORM FERMER-FICHIERS
           STOP RUN.

       OUVRIR-FICHIERS.
           OPEN INPUT F-CLIENTS
           IF WS-FS-CLIENTS NOT = "00"
              DISPLAY "ERREUR OUVERTURE CLIENTS.DAT : " WS-FS-CLIENTS
           END-IF.

       LISTER-CLIENTS.
           DISPLAY "==============================================="
           DISPLAY "              LISTE DES CLIENTS                "
           DISPLAY "==============================================="
           PERFORM UNTIL EOF-CLIENTS = "O"
              READ F-CLIENTS NEXT RECORD
                  AT END
                     MOVE "O" TO EOF-CLIENTS
                  NOT AT END
                     PERFORM AFFICHER-CLIENT
              END-READ
           END-PERFORM.

       AFFICHER-CLIENT.
           DISPLAY "-----------------------------------------------"
           DISPLAY "NUMERO  : " NUM-CLIENT
           DISPLAY "NOM     : " NOM-CLIENT
           DISPLAY "PRENOM  : " PRENOM-CLIENT
           DISPLAY "ADRESSE : " ADRESSE-CLIENT
           DISPLAY "CP      : " CP-CLIENT
           DISPLAY "VILLE   : " VILLE-CLIENT
           DISPLAY " ".

       FERMER-FICHIERS.
           CLOSE F-CLIENTS.
