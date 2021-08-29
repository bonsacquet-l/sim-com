#-------------------------------------------------#
# fonction pour produire les graphiques de chaque #
# indicateur (en duo ACP et AFC)                  #
#-------------------------------------------------#
#-- nettoyage
rm(list = (ls()))

#-- chargement des fonctions
source(file.path("R","Noms-Fichiers.R"))

#-- source les fonctions  
source(file.path("R","Graph_RV_Site.R"))
source(file.path("R","Graph_RV_Sp.R"))
source(file.path("R","Graph_G1_G2.R"))
source(file.path("R","Graph_GA_GB.R"))
source(file.path("R","Graph_Detect_D2D4.R"))
source(file.path("R","Graph_Detect_D5.R"))
source(file.path("R","Graph_Inertia_Plan1.R"))
source(file.path("R","Graph_RV_Sp_tiff.R"))
source(file.path("R","Graph_Detect_D2D4_tiff.R"))


#-- executer les fonctions graphiques
Fct_Graph_RV_Site(Detection="D1")
Fct_Graph_RV_Sp(Detection="D5")
Fct_Graph_G1_G2(Detection="D5",couleur_noir="couleur")
Fct_Graph_GA_GB(Detection="D5",couleur_noir="couleur")
Fct_Graph_Detect_D2D4(Detection="D2",couleur_noir="couleur")
Fct_Graph_Detect_D5(Detection="D5",couleur_noir="couleur")
Fct_Graph_Inertia_Plan1(Detection="D1")

Fct_Graph_RV_Sp_tiff(Detection="D1")
Fct_Graph_Detect_D2D4_tiff(Detection="D4",couleur_noir="couleur")
