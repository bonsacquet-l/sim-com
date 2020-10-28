#===========================#
# make.R pour ACP (exemple) #
#===========================#
#-- nettoyage
rm(list = (ls()))

#-------------#
# simulations #
#-------------#
#-- chargement des fonctions
source(file.path("R","package-option.R"))
source(file.path("R","simul-ACP.R"))
source(file.path("R","RV_ACP.R"))
source(file.path("R","Control_Max_Sp_Absente_Perdue.R"))

#-- faire tourner les simulation une a une (exemple)
Sim<-Fct_simulACP(Nsimul=10, Nplot=50, Nsp=20, minAbon=5, maxAbon=50,
                  minSeG1=100000, maxSeG1=100000, minSeG2=50000, maxSeG2=50000,
                  sp1=1, sp2=20, sp11=0, sp22=0,
                  DetectMean=0.1, VarDetect=0,
                  Detect_Mean_On_G1Min=750, Detect_Mean_On_G1Max=750,
                  minSlope=0, maxSlope=0,
                  fichier="Exemple_D1_C1_01")

RV<-Fct_RV_ACP(Sim$Nsimul, "Site", Sim$A_TabCoordACP_Site, Sim$A_TabCoordACP_Site_Naive, Sim$fichier)
Fct_Control_Sp_Absente_Perdue<-function(Analyse="ACP",Fichier=Sim$fichier)
  
#-- une fois que toute les simulation sont faite (et apres avoir modifier si besoin
#-- le script Noms-Fichiers)

#-------------------------------------------------------#
# regrouper les resultats et construire les indicateurs #
#-------------------------------------------------------#
#-- nettoyage
rm(list = (ls()))
  
#-- source les fonctions
source(file.path("R","Regroup_RV_ACP.R"))
source(file.path("R","Regroup_Coord_Sp_ACP.R"))
source(file.path("R","Grp_Plus_Moins_detectable_D2D4_ACP.R"))
source(file.path("R","Grp_Plus_Moins_detectable_D5_ACP.R"))
source(file.path("R","Grp_1_2_ACP.R"))
source(file.path("R","Grp_A_B_ACP.R"))
  
#-- Execution des fonctions de regroupement des resultats et des indicateurs
Fct_Regroup_RV_ACP("Site")
Fct_Regroup_RV_ACP("Sp")
Fct_Regroup_Coord_Sp_ACP()
Fct_Grp_detect_ACP()
Fct_Grp_detectD5_ACP()
Fct_Grp_1_2_ACP()
Fct_Grp_A_B_ACP()

#----------------------------------#
# extraction de donnees numeriques #
#----------------------------------#
#-- nettoyage
rm(list = (ls()))

#-- source les fonctions




