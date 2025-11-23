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
              DISPLAY "  1 - Consulter un client par numero"
              DISPLAY "  2 - Lister tous les clients"
              DISPLAY "  3 - Supprimer un client"
              DISPLAY "  4 - Quitter"
              DISPLAY "Votre choix : "
              ACCEPT WS-CHOIX

              EVALUATE WS-CHOIX
                 WHEN "1"
                    PERFORM DEMANDER-ET-CONSULTER-CLIENT
                 WHEN "2"
                    PERFORM LISTER-TOUS-LES-CLIENTS
                 WHEN "3"
                    PERFORM SUPPRIMER-UN-CLIENT
                 WHEN "4"
                    DISPLAY "Fin du programme."
                 WHEN OTHER
                    DISPLAY "Choix invalide, merci de recommencer."
              END-EVALUATE
           END-PERFORM
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

           *> Repositionner le fichier sur la premiere cle
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
