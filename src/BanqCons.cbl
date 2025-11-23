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
           RECORD CONTAINS 126 CHARACTERS.
       01  CLIENT-REC.
           COPY "Client.cpy".

       WORKING-STORAGE SECTION.
       01  WS-FS-CLIENTS      PIC XX  VALUE SPACES.
       01  WS-REPONSE         PIC X   VALUE SPACE.
       01  WS-CLI-RECH        PIC X(6) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-SECTION.

           OPEN INPUT F-CLIENTS

           IF WS-FS-CLIENTS NOT = "00"
              DISPLAY "ERREUR OUVERTURE CLIENTS.DAT : " WS-FS-CLIENTS
              STOP RUN
           END-IF

           PERFORM JUSQUA-FIN-CONSULT

           CLOSE F-CLIENTS

           STOP RUN.
           
       *>----------------------------------------------------*
       *> Boucle de consultation                             *
       *>----------------------------------------------------*
       JUSQUA-FIN-CONSULT.
           PERFORM UNTIL WS-REPONSE = "N" OR WS-REPONSE = "n"

              MOVE SPACES TO WS-CLI-RECH
              DISPLAY "----------------------------------------"
              DISPLAY "Numero de client a consulter (ex: C00001) : "
              ACCEPT WS-CLI-RECH

              IF WS-CLI-RECH = SPACES
                 MOVE "N" TO WS-REPONSE
              ELSE
                 PERFORM CONSULTER-UN-CLIENT

                 DISPLAY "Voulez-vous consulter un autre client ? (O/N) : "
                 ACCEPT WS-REPONSE
              END-IF

           END-PERFORM
           .

       *>----------------------------------------------------*
       *> Lecture d'un client par NUM-CLIENT                 *
       *>----------------------------------------------------*
       CONSULTER-UN-CLIENT.
           MOVE WS-CLI-RECH TO NUM-CLIENT

           READ F-CLIENTS
                KEY IS NUM-CLIENT
                INVALID KEY
                   DISPLAY "Client " WS-CLI-RECH " introuvable."
                NOT INVALID KEY
                   DISPLAY "Client trouve : "
                   DISPLAY "  Numero : " NUM-CLIENT
                   DISPLAY "  Nom    : " NOM-CLIENT
                   DISPLAY "  Prenom : " PRENOM-CLIENT
                   DISPLAY "  Adresse: " ADRESSE-CLIENT
                   DISPLAY "  CP     : " CP-CLIENT
                   DISPLAY "  Ville  : " VILLE-CLIENT
           END-READ
           .
       
       END PROGRAM BANQCONS.
