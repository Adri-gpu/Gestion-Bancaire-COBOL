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

            SELECT F-PARAM   ASSIGN TO "../data/PARAM.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-FS-PARAM.

           SELECT F-COMPTES
               ASSIGN TO "data/COMPTES.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CPT-NUM-COMPTE
               FILE STATUS IS WS-FS-COMPTES.

           SELECT F-OPERATIONS
               ASSIGN TO "data/OPERATIONS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS OPR-ID
               ALTERNATE RECORD KEY IS OPR-NUM-COMPTE WITH DUPLICATES
               FILE STATUS IS WS-FS-OPERATIONS.

       DATA DIVISION.
       FILE SECTION.

       FD  F-CLIENTS
           RECORD CONTAINS 126 CHARACTERS.
       01  CLIENT-REC.
           COPY "Client.cpy".

       FD  F-COMPTES.
       01  COMPTE-REC.
           COPY "Compte.cpy".

       FD  F-OPERATIONS.
       01  OPERATION-REC.
           COPY "Operation.cpy".

       FD  F-PARAM.
       01  PARAM-REC.
           COPY "Param.cpy".


       WORKING-STORAGE SECTION.
       01  WS-FS-CLIENTS      PIC XX  VALUE SPACES.
       01  WS-FS-COMPTES      PIC XX  VALUE SPACES.
       01  WS-FS-OPERATIONS            PIC XX VALUE SPACES.
       01  WS-FS-PARAM            PIC XX VALUE SPACES.
       01  WS-NUM-SEQ-CLIENT      PIC 9(5) VALUE 0.
       01  WS-NUM-SEQ-CLIENT-AL   PIC X(5).
       01  WS-REPONSE         PIC X   VALUE SPACE.
       01  WS-CLI-RECH        PIC X(6) VALUE SPACES.
       01  WS-CHOIX           PIC X   VALUE " ".
       01  WS-NB-CLIENTS      PIC 9(4) VALUE 0.
       01  WS-CONFIRM         PIC X   VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-SECTION.

           OPEN I-O F-CLIENTS

           IF WS-FS-CLIENTS NOT = "00"
              DISPLAY "ERREUR OUVERTURE CLIENTS.DAT : " WS-FS-CLIENTS
              STOP RUN
           END-IF

           PERFORM BOUCLE-MENU

           CLOSE F-CLIENTS

           STOP RUN.
       
       BOUCLE-MENU.
           PERFORM UNTIL WS-CHOIX = "4"
              DISPLAY " "
              DISPLAY "========================================"
              DISPLAY "      MENU CONSULTATION CLIENTS"
              DISPLAY "========================================"
              DISPLAY "  1 - Creer un nouveau client"
              DISPLAY "  2 - Consulter les informations d'un client"
              DISPLAY "  3 - Lister tous les clients"
              DISPLAY "  4 - Supprimer un client"
              DISPLAY "Votre choix : "
              ACCEPT WS-CHOIX

              EVALUATE WS-CHOIX
                 WHEN "1"
                    PERFORM SAISIE-CLIENTS
                 WHEN "2"
                    PERFORM DEMANDER-ET-CONSULTER-CLIENT
                 WHEN "3"
                    PERFORM LISTER-TOUS-LES-CLIENTS
                 WHEN "4"
                    PERFORM SUPPRIMER-UN-CLIENT
                 WHEN "5"
                    DISPLAY "Fin du programme."
                 WHEN OTHER
                    DISPLAY "Choix invalide, merci de recommencer."
              END-EVALUATE
           END-PERFORM
           .

       SAISIE-CLIENTS.
           PERFORM UNTIL WS-REPONSE = "N" OR WS-REPONSE = "n"
              PERFORM CREER-UN-CLIENT
              DISPLAY "Voulez-vous saisir un autre client ? (O/N) : "
              ACCEPT WS-REPONSE
           END-PERFORM
           .

       CREER-UN-CLIENT.
           ADD 1 TO WS-NUM-SEQ-CLIENT

           MOVE WS-NUM-SEQ-CLIENT  TO WS-NUM-SEQ-CLIENT-AL
           MOVE SPACES             TO CLIENT-REC
           MOVE "C"                TO NUM-CLIENT(1:1)
           MOVE WS-NUM-SEQ-CLIENT-AL TO NUM-CLIENT(2:5)

           DISPLAY "----------------------------------------"
           DISPLAY "CREATION DU CLIENT : " NUM-CLIENT

           DISPLAY "Nom du client : "
           ACCEPT NOM-CLIENT

           DISPLAY "Prenom du client : "
           ACCEPT PRENOM-CLIENT

           DISPLAY "Adresse du client : "
           ACCEPT ADRESSE-CLIENT

           DISPLAY "Code postal : "
           ACCEPT CP-CLIENT

           DISPLAY "Ville : "
           ACCEPT VILLE-CLIENT

           WRITE CLIENT-REC
           IF WS-FS-CLIENTS NOT = "00"
              DISPLAY "ERREUR ECRITURE CLIENT : " WS-FS-CLIENTS
           ELSE
              DISPLAY "Client " NUM-CLIENT " cree."
           END-IF
           .

       DEMANDER-ET-CONSULTER-CLIENT.
           MOVE SPACES TO WS-CLI-RECH

           DISPLAY "----------------------------------------"
           DISPLAY "Numero de client a consulter (ex: C00001) : "
           ACCEPT WS-CLI-RECH

           IF WS-CLI-RECH NOT = SPACES
              PERFORM CONSULTER-UN-CLIENT
           ELSE
              DISPLAY "Numero vide, retour au menu."
           END-IF
           .

       AFFICHER-CLIENT.
           DISPLAY "  Numero : " NUM-CLIENT
           DISPLAY "  Nom    : " NOM-CLIENT
           DISPLAY "  Prenom : " PRENOM-CLIENT
           DISPLAY "  Adresse: " ADRESSE-CLIENT
           DISPLAY "  CP     : " CP-CLIENT
           DISPLAY "  Ville  : " VILLE-CLIENT
           DISPLAY "----------------------------------------"
           .

       LISTER-TOUS-LES-CLIENTS.
           DISPLAY " "
           DISPLAY "========== LISTE DES CLIENTS =========="

           MOVE 0      TO WS-NB-CLIENTS
           MOVE "00"   TO WS-FS-CLIENTS

           MOVE SPACES TO NUM-CLIENT

           START F-CLIENTS KEY >= NUM-CLIENT
                INVALID KEY
                   DISPLAY "Aucun client dans le fichier."
                   EXIT PARAGRAPH
           .

           PERFORM UNTIL WS-FS-CLIENTS = "10"
              READ F-CLIENTS NEXT RECORD
                 AT END
                    MOVE "10" TO WS-FS-CLIENTS
                 NOT AT END
                    ADD 1 TO WS-NB-CLIENTS
                    PERFORM AFFICHER-CLIENT
              END-READ
           END-PERFORM

           IF WS-NB-CLIENTS > 0
              DISPLAY "====== FIN DE LA LISTE DES CLIENTS ======"
           END-IF
           .

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

       SUPPRIMER-UN-CLIENT.
           MOVE SPACES TO WS-CLI-RECH

           DISPLAY "----------------------------------------"
           DISPLAY "Numero de client a supprimer (ex: C00001) : "
           ACCEPT WS-CLI-RECH

           IF WS-CLI-RECH = SPACES
              DISPLAY "Numero vide, retour au menu."
              EXIT PARAGRAPH
           END-IF

           MOVE WS-CLI-RECH TO NUM-CLIENT

           READ F-CLIENTS
                KEY IS NUM-CLIENT
                INVALID KEY
                   DISPLAY "Client " WS-CLI-RECH " introuvable."
                   EXIT PARAGRAPH
                NOT INVALID KEY
                   DISPLAY "Client trouve : "
                   PERFORM AFFICHER-CLIENT
           END-READ

           DISPLAY "Confirmez la suppression (O/N) : "
           ACCEPT WS-CONFIRM

           IF WS-CONFIRM NOT = "O" AND WS-CONFIRM NOT = "o"
              DISPLAY "Suppression annulee."
              EXIT PARAGRAPH
           END-IF

           DELETE F-CLIENTS RECORD

           IF WS-FS-CLIENTS = "00"
              DISPLAY "Client " NUM-CLIENT " supprime."
           ELSE
              DISPLAY "Erreur suppression client : " WS-FS-CLIENTS
           END-IF
           .
       
       END PROGRAM BANQCONS.
