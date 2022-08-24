#---------------------#
# make avant graphs   #
# selection resultats #
#---------------------#

#-- nettoyage
rm(list = (ls()))

#-- sourcer les script necessaire
source(file.path("R","package-option.R"))
source(file.path("R","Noms-Fichiers.R"))
source(file.path("R","Nbr_Good_simul.R"))
source(file.path("R","Select_bad_simul.R"))
source(file.path("R","Select_resultat_ACP_good.R"))
source(file.path("R","Select_resultat_AFC_good.R"))
source(file.path("R","Select_resultat_PCoA_bray_good.R"))
source(file.path("R","Select_resultat_PCoA_chao_good.R"))
source(file.path("R","Select_resultat_nMDS_bray_good.R"))
source(file.path("R","Select_resultat_nMDS_chao_good.R"))

#-- application de la selection et creation des tableau de resultats utilisables
fct_tab_good_simul(fichier=V_Nom_Fichier)
fct_select_bad_simul(alpha=800)

#-- creation des tableau des resultats selectionnes
fct_select_resultACP()
fct_select_resultAFC()
fct_select_resultPCoA_bray()
fct_select_resultPCoA_chao()
fct_select_resultnMDS_bray()
fct_select_resultnMDS_chao()
