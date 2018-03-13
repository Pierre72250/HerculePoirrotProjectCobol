IDENTIFICATION DIVISION.
PROGRAM-ID. projet.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
SELECT fenquetes ASSIGN TO "enquetes.dat"
    ORGANIZATION INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY fe_idEnq
    FILE STATUS IS fenq_stat.
SELECT fagents ASSIGN TO "agents.dat"
    ORGANIZATION INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY fa_matr
    FILE STATUS IS fagent_stat.
SELECT fpersonnes ASSIGN TO "personnes.dat"
    ORGANIZATION INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY fp_idPers
    FILE STATUS IS fpers_stat.
SELECT fpieces ASSIGN TO "pieces.dat"
    ORGANIZATION INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY fpi_idPiece
    FILE STATUS IS fpiece_stat.
SELECT farchives ASSIGN TO "archives.dat"
    ORGANIZATION INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY far_idArchi
    FILE STATUS IS farch_stat.
SELECT fcrimes ASSIGN TO "crimes.dat"
    ORGANIZATION INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY fc_idCrime
    FILE STATUS IS fcrime_stat.


DATA DIVISION.
FILE SECTION.
FD fenquetes.
    01 enqTampon.
        02 fe_idEnq PIC 9(5).
        02 fe_dateDeb PIC 9(2)A(1)9(2)A(1)9(4).
        02 fe_matrChef PIC A(5).

FD fagents.
    01 agentTampon.
        02 fa_matr PIC A(5).
        02 fa_nom PIC A(20).
        02 fa_prenom PIC A(20).
        02 fa_lieuServ PIC A(50).
        02 fa_EnqEnCours PIC 9(5).

FD fpersonnes.
    01 persTampon.
        02 fp_idPers PIC 9(5).
        02 fp_nom PIC A(20).
        02 fp_prenom PIC A(20).
        02 fp_type PIC A(7).
        02 fp_taille PIC 9(3).
        02 fp_dateNaissance PIC 9(2)A(1)9(2)A(1)9(4).
        02 fp_pointure PIC 9(2).
        02 fp_sexe PIC A(1).

FD fpieces.
    01 pieceTampon.
        02 fpi_idPiece PIC 9(10).
        02 fpi_nature PIC A(10).
        02 fpi_taille PIC A(1).
        02 fpi_idBox PIC 9(10).
        02 fpi_idCrime PIC 9(10).
        02 fpi_idEnq PIC 9(5).

FD farchives.
    01 archTampon.
        02 far_idArchi PIC 9(5).
        02 far_dateDeb PIC 9(2)A(1)9(2)A(1)9(4).
        02 far_chef PIC A(5).
        02 far_status PIC A(7).

FD fcrimes.
    01 crimesTampon.
        02 fc_idCrime PIC 9(5).
        02 fc_lieu PIC A(40).
        02 fc_date PIC 9(2)A(1)9(2)A(1)9(4).
        02 fc_description PIC A(2000).
        02 fc_idEnquetes PIC 9(5).



WORKING-STORAGE SECTION.

    77 fenq_stat PIC 9(2).
    77 fagent_stat PIC 9(2).
    77 fpers_stat PIC 9(2).
    77 fpiece_stat PIC 9(2).
    77 farch_stat PIC 9(2).
    77 fcrime_stat PIC 9(2).
    77 Wfin PIC 9(1).
    77 Wnb PIC 9(4).
    77 Wtrouve PIC 9(1).
    77 Wdecision PIC 9(1).
    77 Wfe_idEnq PIC 9(5).
    77 Wfe_matrChef PIC A(5).
    77 Wfe_dateDeb PIC 9(2)A(1)9(2)A(1)9(4).
    77 Wfc_lieu PIC A(40).
    77 Wfc_desc PIC A(2000).
    77 Wfa_matr PIC A(5).
    77 Wfa_nom PIC A(20).
    77 Wfa_prenom PIC A(20).
    77 Wfa_lieuServ PIC A(50).
    77 Wfa_EnqEnCours PIC 9(5).


PROCEDURE DIVISION.
    OPEN INPUT fenquetes
    IF fenq_stat = 35 THEN
        OPEN OUTPUT fenquetes
    END-IF
    CLOSE fenquetes

    OPEN INPUT fagents
    IF fagent_stat = 35 THEN
        OPEN OUTPUT fagents
    END-IF
    CLOSE fagents

    OPEN INPUT fpersonnes
    IF fpers_stat = 35 THEN
        OPEN OUTPUT fpersonnes
    END-IF
    CLOSE fpersonnes

    OPEN INPUT fpieces
    IF fpiece_stat = 35 THEN
        OPEN OUTPUT fpieces
    END-IF
    CLOSE fpieces

    OPEN INPUT farchives
    IF farch_stat = 35 THEN
        OPEN OUTPUT farchives
    END-IF
    CLOSE farchives

    *>menu et fonctions ici
    *>PERFORM AJOUTER_ENQUETE
    *>PERFORM RECHERCHER_ENQUETE
    PERFORM AJOUTER_AGENT

    STOP RUN.

AJOUTER_ENQUETE.
    DISPLAY "Entrez le matricule du chef d'enquete :"
    ACCEPT Wfe_matrChef
    DISPLAY "Entrez la date de d�but d'enquete : "
    ACCEPT Wfe_dateDeb

    *> Attention il faut parcourir farchives et fenquetes pour cr�er l id
    MOVE 0 TO Wnb
    MOVE 0 TO Wfin
    OPEN INPUT farchives
    PERFORM UNTIL Wfin = 1
        READ farchives NEXT
        AT END
         	MOVE 1 TO Wfin
        NOT AT END
          	COMPUTE Wnb = Wnb + 1
    END-PERFORM
    CLOSE farchives
    MOVE 0 TO Wfin
    OPEN INPUT fenquetes
    PERFORM UNTIL Wfin = 1
        READ fenquetes NEXT
        AT END
         	MOVE 1 TO Wfin
        NOT AT END
          	COMPUTE Wnb = Wnb + 1
    END-PERFORM
    CLOSE fenquetes

    MOVE Wnb TO fe_idEnq
    MOVE Wfe_dateDeb TO fe_dateDeb
    MOVE Wfe_matrChef TO fe_matrChef
    OPEN I-O fenquetes
    WRITE enqTampon
       	INVALID KEY
        	DISPLAY "Existe deja"
	    NOT INVALID KEY
	        DISPLAY "Ajout r�ussi. L'enqu�te porte le num�ro "Wnb
	END-WRITE.
    CLOSE fenquetes

    *>PERFORM AJOUTER_CRIME
.

MODIFIER_ENQUETE.
	DISPLAY "Donner le num�ro de l'enqu�te � modifier"
	ACCEPT Wfe_idEnq

	OPEN I-O fenquetes
	READ fenquetes
	INVALID KEY
		DISPLAY "Enqu�te introuvable."
	NOT INVALID KEY
	    DISPLAY "Modifier la date de l'enqu�te ? 1:OUI, 0:NON"
	    ACCEPT Wdecision
	    IF Wdecision = 1
	    THEN
	    	DISPLAY "Donner la nouvelle date"
	    	ACCEPT Wfe_dateDeb
	    END-IF
	    DISPLAY "Modifier le chef d'enqu�te ? 1:OUI, 0:NON"
	    ACCEPT Wdecision
	    IF Wdecision = 1
	    THEN
	    	DISPLAY "Donner son nouveau matricule"
	    	ACCEPT Wfe_matrChef
	    END-IF

	    MOVE Wfe_dateDeb TO fe_dateDeb
    	MOVE Wfe_matrChef TO fe_matrChef
		REWRITE enqTampon
	END-READ
	CLOSE fenquetes
.

SUPPRIMER_ENQUETE. *> � refaire
	DISPLAY "Donner le num�ro de l'enq�te � archiver"
	ACCEPT Wfe_idEnq

	OPEN I-O fenquetes
	READ fenquetes
	INVALID KEY
		DISPLAY "Enqu�te introuvable."
	NOT INVALID KEY
		DELETE fenquetes
	END-READ
	CLOSE fenquetes
.

RECHERCHER_ENQUETE.
	DISPLAY "Donner l'enqu�te que vous voulez rechercher"
	ACCEPT Wfe_idEnq

	OPEN INPUT fenquetes
	READ fenquetes
	INVALID KEY
		DISPLAY "Enqu�te introuvable."
	NOT INVALID KEY
		DISPLAY "Enqu�te existe."
	END-READ
	CLOSE fenquetes
.

AJOUTER_CRIME.
	DISPLAY "Donner le lieu du d�lit"
	ACCEPT Wfc_lieu
	DISPLAY "Description : "
	ACCEPT Wfc_desc

	MOVE 0 TO Wnb
    MOVE 0 TO Wfin
    OPEN INPUT fcrimes
    PERFORM UNTIL Wfin = 1
        READ fcrimes NEXT
        AT END
         	MOVE 1 TO Wfin
         	COMPUTE Wnb = Wnb + 1
        NOT AT END
          	COMPUTE Wnb = Wnb + 1
    END-PERFORM
    CLOSE fcrimes

    MOVE Wfc_desc TO fc_description
    MOVE Wfc_lieu TO fc_lieu
    MOVE Wfe_dateDeb TO fc_date
    MOVE fe_idEnq TO fc_idEnquetes
    MOVE Wnb TO fc_idCrime

    OPEN I-O fcrimes
    WRITE crimesTampon
       	INVALID KEY
        	DISPLAY "Existe deja"
	    NOT INVALID KEY
	        DISPLAY "Ajout r�ussi"
	END-WRITE.
    CLOSE fcrimes

    PERFORM WITH TEST AFTER UNTIL Wdecision = 1
    	DISPLAY "Ajouter une pi�ce � conviction ? 1:OUI, 0:NON"
    	ACCEPT Wdecision

    	*>PERFORM AJOUTER_PIECE
    END-PERFORM
.

MODIFIER_CRIME.
.
SUPPRIMER_CRIME.
.
RECHERCHER_CRIME.
.
AJOUTER_PIECE.
.
MODIFIER_PIECE.
.
SUPPRIMER_PIECE.
.
RECHERCHER_PIECE.
.
AJOUTER_BOX.
.
MODIFIER_BOX.
.
SUPPRIMER_BOX.
.
RECHERCHE_BOX_VIDE.
.
RECHERCHE_BOX_PLEIN.
.
AJOUTER_PERSONNE.
.
MODIFIER_PERSONNE.
.
SUPPRIMER_PERSONNE.
.
RECHERCHER_PERSONNE.
.
AJOUTER_LIENS.
.
MODIFIER_LIENS.
.
SUPPRIMER_LIENS.
.
AJOUTER_AGENT.
  PERFORM TEST_UNIQUE_MATRICULE_AGENT

  DISPLAY "Nom de l'agent :"
  ACCEPT Wfa_nom

  DISPLAY "Prénom de l'agent :"
  ACCEPT Wfa_prenom

  DISPLAY "Lieu de service de l'agent :"
  ACCEPT Wfa_lieuServ

  DISPLAY "Numéro de l'enquête en charge de l'agent :"
  ACCEPT Wfa_enqEnCours

  MOVE Wfa_matr TO fa_matr
  MOVE Wfa_nom TO fa_nom
  MOVE Wfa_prenom TO fa_prenom
  MOVE Wfa_lieuServ TO fa_lieuServ
  MOVE Wfa_enqEnCours TO fa_EnqEnCours

  OPEN EXTEND fagents
  WRITE agentTampon END-WRITE
  CLOSE fagents.

  TEST_UNIQUE_MATRICULE_AGENT.
  OPEN INPUT fagents
  MOVE 0 TO wFin
  MOVE 0 TO wTrouve

  DISPLAY "Numéro de matricule de l'agent :"
  ACCEPT Wfa_matr

  PERFORM WITH TEST AFTER UNTIL wTrouve = 1 OR wFin = 1
    READ fagents
      AT END
      MOVE 1 TO wFin
      NOT AT END
      IF fa_matr = Wfa_matr THEN
        MOVE 1 TO wTrouve
        DISPLAY "Ce numéro de matricule est déjà attribué. Merci de resaisir un matricule valide."
        CLOSE fagents
        PERFORM TEST_UNIQUE_MATRICULE_AGENT
      END-IF
    END-READ
  END-PERFORM
  CLOSE fagents.
MODIFIER_AGENTS.
.
SUPPRIMER_AGENTS.
.
RECHERCHER_AGENTS.
.
AJOUTER_ARCHIVES.
.
MODIFIER_ARCHIVES.
.
SUPPRIMER_ARCHIVES.
.
RECHERCHER_ARCHIVES.
.
