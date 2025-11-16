       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQINIT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENTS
               ASSIGN TO "data/CLIENTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NUM-CLIENT
               FILE STATUS IS WS-FS-CLIENTS.

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

           SELECT F-PARAM
               ASSIGN TO "data/PARAM.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-PARAM.

       DATA DIVISION.
       FILE SECTION.
       FD  F-CLIENTS.
       COPY "Client.cpy".

       FD  F-COMPTES.
       COPY "Compte.cpy".

       FD  F-OPERATIONS.
       COPY "Operation.cpy".

       FD  F-PARAM.
       COPY "Param.cpy".

       WORKING-STORAGE SECTION.
       01  WS-FS-CLIENTS     PIC X(2).
       01  WS-FS-COMPTES     PIC X(2).
       01  WS-FS-OPERATIONS  PIC X(2).
       01  WS-FS-PARAM       PIC X(2).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           DISPLAY "INITIALISATION DES FICHIERS BANCAIRES".

           OPEN OUTPUT F-CLIENTS
                       F-COMPTES
                       F-OPERATIONS
                       F-PARAM.

           IF WS-FS-CLIENTS NOT = "00"
              DISPLAY "ERREUR CREATION CLIENTS, FS=" WS-FS-CLIENTS
           END-IF

           MOVE 0 TO PRM-DERNIER-ID-CLIENT
                    PRM-DERNIER-ID-COMPTE
                    PRM-DERNIER-ID-OPR.

           WRITE PARAM-FILE-REC.

           CLOSE F-CLIENTS
                 F-COMPTES
                 F-OPERATIONS
                 F-PARAM.

           DISPLAY "INITIALISATION TERMINEE.".
           STOP RUN.
