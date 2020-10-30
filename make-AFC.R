#===========================#
# make.R pour AFC (exemple) #
#===========================#
#-- nettoyage
rm(list = (ls()))

#-------------#
# simulations #
#-------------#
#-- chargement des fonctions
source(file.path("R","package-option.R"))
source(file.path("R","simul-AFC.R"))
source(file.path("R","RV_AFC.R"))
source(file.path("R","Control_Max_Sp_Absente_Perdue.R"))

#-- faire tourner les simulation une a une (exemple)
Sim<-Fct_simulAFC(Nsimul=1000, Nplot=100, Nsp=20, minAbon=5, maxAbon=50,
                       minSeG1=100000, maxSeG1=100000, minSeG2=50000, maxSeG2=50000,
                       sp1=1, sp2=20, sp11=0, sp22=0,
                       DetectMean=0.1, VarDetect=0,
                       Detect_Mean_On_G1Min=500, Detect_Mean_On_G1Max=500,
                       minSlope=0, maxSlope=0,
                       fichier="Exemple_D1_C1_01")

RV<-Fct_RV_AFC(Sim$Nsimul, "Site", Sim$A_TabCoordAFC_Site, Sim$A_TabCoordAFC_Site_Naive, Sim$fichier)
Fct_Control_Sp_Absente_Perdue<-function(Analyse="AFC",Fichier=Sim$fichier)
  
#-- une fois que toute les simulation sont faite (et apres avoir modifier si besoin
#-- le script Noms-Fichiers)

#-------------------------------------------------------#
# regrouper les resultats et construire les indicateurs #
#-------------------------------------------------------#
#-- nettoyage
rm(list = (ls()))

#-- source les fonctions
source(file.path("R","Regroup_RV_AFC.R"))
source(file.path("R","Regroup_Coord_Sp_AFC.R"))
source(file.path("R","Grp_Plus_Moins_detectable_D2D4_AFC.R"))
source(file.path("R","Grp_Plus_Moins_detectable_D5_AFC.R"))
source(file.path("R","Grp_1_2_AFC.R"))
source(file.path("R","Grp_A_B_AFC.R"))
  
#-- Execution des fonctions de regroupement des resultats
Fct_Regroup_RV_AFC("Site")
Fct_Regroup_RV_AFC("Sp")
Fct_Regroup_Coord_Sp_AFC()
Fct_Grp_detect_AFC()
Fct_Grp_detectD5_AFC()
Fct_Grp_1_2_AFC()
Fct_Grp_A_B_AFC()



