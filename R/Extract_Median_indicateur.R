#-----------------------------------------------------------------#
# script pour extraire facilement les medianes des indicateur     #
# lionel Bonsacquet                                               #
#-----------------------------------------------------------------#
#-- nettoyage
rm(list = (ls()))

#-- chargement des noms des fichiers
source(file.path("R","Noms-Fichiers.R"))

#-- chargement des donnnees (en fonction des infos recherchees)
  #- les ACP
  load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp.Rdata"))
  data<-M_Resultat_RV_ACP
  
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_1_2.Rdata"))
  data<-M_Grp_1_2_Axe1_ACP
  data<-M_Grp_1_2_Axe1_ACP_Naive
  data<-M_Grp_1_2_Axe2_ACP
  data<-M_Grp_1_2_Axe2_ACP_Naive
  
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B.Rdata"))
  data<-M_Grp_A_B_Axe1_ACP
  data<-M_Grp_A_B_Axe1_ACP_Naive
  data<-M_Grp_A_B_Axe2_ACP
  data<-M_Grp_A_B_Axe2_ACP_Naive
  
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect.Rdata"))
  data<-M_Grp_Detect_Axe1_ACP
  data<-M_Grp_Detect_Axe1_ACP_Naive
  data<-M_Grp_Detect_Axe2_ACP      
  data<-M_Grp_Detect_Axe2_ACP_Naive
  
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5.Rdata"))
  data<-M_Grp_Detect_Axe1_ACP
  data<-M_Grp_Detect_Axe1_ACP_Naive
  data<-M_Grp_Detect_Axe2_ACP      
  data<-M_Grp_Detect_Axe2_ACP_Naive
  
  load(file.path("Outcome","out-regroupement","ACP","ACP_Inertia_Plan1.Rdata"))
  data<-M_Inertia_Plan1_ACP
  
  #- les AFC
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp.Rdata"))
  data<-M_Resultat_RV_AFC
  
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_1_2.Rdata"))
  data<-M_Grp_1_2_Axe1_AFC
  data<-M_Grp_1_2_Axe1_AFC_Naive
  data<-M_Grp_1_2_Axe2_AFC
  data<-M_Grp_1_2_Axe2_AFC_Naive
  
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B.Rdata"))
  data<-M_Grp_A_B_Axe1_AFC
  data<-M_Grp_A_B_Axe1_AFC_Naive
  data<-M_Grp_A_B_Axe2_AFC
  data<-M_Grp_A_B_Axe2_AFC_Naive
  
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect.Rdata"))
  data<-M_Grp_Detect_Axe1_AFC
  data<-M_Grp_Detect_Axe1_AFC_Naive
  data<-M_Grp_Detect_Axe2_AFC      
  data<-M_Grp_Detect_Axe2_AFC_Naive
  
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5.Rdata"))
  data<-M_Grp_Detect_Axe1_AFC
  data<-M_Grp_Detect_Axe1_AFC_Naive
  data<-M_Grp_Detect_Axe2_AFC      
  data<-M_Grp_Detect_Axe2_AFC_Naive
  
  load(file.path("Outcome","out-regroupement","AFC","AFC_Inertia_Plan1.Rdata"))
  data<-M_Inertia_Plan1_AFC

#-- le fichier
    fichier<-V_Fichier_D1
    
    fichier<-V_Fichier_D3_Grp1_2
    
    fichier<-V_Fichier_D1_GrpA_B
    
    fichier<-V_Fichier_D2_GrpDetect
    fichier<-V_Fichier_D4_GrpDetect
    fichier<-V_Fichier_D5_GrpDetect
#-- les medians
    median(data[,"D1_C1_01"])
    
    median(data[,"D3_C2_01_G1"])
    

  
  
