       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQCLI.

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

       DATA DIVISION.
       FILE SECTION.

       FD  F-CLIENTS
           RECORD CONTAINS 126 CHARACTERS.
       01  CLIENT-REC.
           COPY "Client.cpy".

       FD  F-PARAM.
       COPY "Param.cpy".

       WORKING-STORAGE SECTION.
       01  WS-FS-CLIENTS          PIC XX VALUE SPACES.
       01  WS-FS-PARAM            PIC XX VALUE SPACES.

       01  WS-NUM-SEQ-CLIENT      PIC 9(5) VALUE 0.
       01  WS-NUM-SEQ-CLIENT-AL   PIC X(5).

       01  WS-REPONSE             PIC X VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT-PARAM

           IF WS-FS-PARAM NOT = "00"
              DISPLAY "ERREUR OUVERTURE/LECTURE PARAM.DAT : " WS-FS-PARAM
              STOP RUN
           END-IF

           OPEN I-O F-CLIENTS
           IF WS-FS-CLIENTS = "35"
              OPEN OUTPUT F-CLIENTS
              CLOSE F-CLIENTS
           
              OPEN I-O F-CLIENTS
           END-IF
           
           IF WS-FS-CLIENTS NOT = "00"
              DISPLAY "ERREUR OUVERTURE CLIENTS.DAT : " WS-FS-CLIENTS
              CLOSE F-PARAM
              STOP RUN
           END-IF

           PERFORM SAISIE-CLIENTS

           MOVE WS-NUM-SEQ-CLIENT TO PRM-DERNIER-ID-CLIENT
           REWRITE PARAM-FILE-REC

           CLOSE F-CLIENTS
           CLOSE F-PARAM

           STOP RUN.

       *>----------------------------------------------------*
       *> Initialisation : lecture du fichier PARAM          *
       *> pour recuperer PRM-DERNIER-ID-CLIENT               *
       *>----------------------------------------------------*
       INIT-PARAM.
           OPEN I-O F-PARAM

           IF WS-FS-PARAM NOT = "00"
              DISPLAY "ERREUR OUVERTURE I-O PARAM.DAT : " WS-FS-PARAM
              EXIT PARAGRAPH
           END-IF

           READ F-PARAM
              AT END
                 *> Si le fichier est vide, on part de 0
                 MOVE 0 TO WS-NUM-SEQ-CLIENT
              NOT AT END
                 MOVE PRM-DERNIER-ID-CLIENT TO WS-NUM-SEQ-CLIENT
           END-READ
           .

       *>----------------------------------------------------*
       *> Boucle de saisie des clients                       *
       *>----------------------------------------------------*
       SAISIE-CLIENTS.
           PERFORM UNTIL WS-REPONSE = "N" OR WS-REPONSE = "n"
              PERFORM CREER-UN-CLIENT
              DISPLAY "Voulez-vous saisir un autre client ? (O/N) : "
              ACCEPT WS-REPONSE
           END-PERFORM
           .

       *>----------------------------------------------------*
       *> Creation d'un client : generation du NUM-CLIENT     *
       *> et saisie des champs, puis ecriture dans le fichier *
       *>----------------------------------------------------*
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

       END PROGRAM BANQCLI.
