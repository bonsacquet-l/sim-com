#--------------------------------------------------#
# fonction repartition des especes en deux groupes #
# les plus detectable et les moins detectable      #
# script applicable que pour les D5                # 
# pour les ACP                                     #
# lionel.bonsacquet                                #
#--------------------------------------------------# # executer dans le make_ACP
# 11 especes moins detectable donc 11*1000 coordonnees a stocker au plus dans
# une colonne de chaque matrice

Fct_Grp_detectD5_ACP<-function() {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(c(V_Fichier_D5_GrpDetect))
  
  M_Grp_Detect_Axe1_ACP<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe1_ACP)<-as.character(V_Fichier_D5_GrpDetect)
  
  M_Grp_Detect_Axe2_ACP<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe2_ACP)<-as.character(V_Fichier_D5_GrpDetect)
  
  M_Grp_Detect_Axe1_ACP_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe1_ACP_Naive)<-as.character(V_Fichier_D5_GrpDetect)
  
  M_Grp_Detect_Axe2_ACP_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe2_ACP_Naive)<-as.character(V_Fichier_D5_GrpDetect)
  
  #-- chargement des donnees des coordonnees des especes
  load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_Coord_Sp.Rdata"))
  
  #-- la boucle avec appel de la detection des sp a chaque simulation
  for (n in c(V_Fichier_D5)) {
    print(n)
    
    #-- les donnees de detection (des 1000 simulations du fichier)
    Sim<-load(file.path("Outcome","out-simul","ACP",paste("ACP_Simul_",n,".Rdata",sep="")))
    M_MemDetectSp<-M_MemDetectSp
    
    for (z in 1:1000) {
      #-- ordre de detection des sp de moins au plus detectable
      Sp_les_Moins_Detect<-order(M_MemDetectSp[z,])[1:10]+20*(z-1)  # les dix moins detectable
      Sp_les_Plus_Detect<-order(M_MemDetectSp[z,])[11:20]+20*(z-1)  # les dix plus detectable
      
      #les lignes a remplir
      lignes<-(c((10*(z-1)+1):(10*(z-1)+10)))
      
      #-- repartition des coordonnees des especes dans le deux groupes
      ifelse(median(M_Resultat_Coord_Sp_ACP_axe1[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)
      M_Grp_Detect_Axe1_ACP[(lignes),paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_ACP_axe1[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe1_ACP[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_ACP_axe1[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_ACP_Naive_axe1[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1) 
      M_Grp_Detect_Axe1_ACP_Naive[(lignes),paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_ACP_Naive_axe1[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe1_ACP_Naive[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_ACP_Naive_axe1[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_ACP_axe2[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)    
      M_Grp_Detect_Axe2_ACP[(lignes),paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_ACP_axe2[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe2_ACP[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_ACP_axe2[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_ACP_Naive_axe2[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)   
      M_Grp_Detect_Axe2_ACP_Naive[lignes,paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_ACP_Naive_axe2[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe2_ACP_Naive[lignes,paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_ACP_Naive_axe2[Sp_les_Moins_Detect,n]*a
    }
  } 
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5.Rdata")
  
  save(M_Grp_Detect_Axe1_ACP,M_Grp_Detect_Axe2_ACP,
       M_Grp_Detect_Axe1_ACP_Naive,M_Grp_Detect_Axe2_ACP_Naive,
       list = c("M_Grp_Detect_Axe1_ACP","M_Grp_Detect_Axe2_ACP",
                "M_Grp_Detect_Axe1_ACP_Naive","M_Grp_Detect_Axe2_ACP_Naive"),
       file = saveData)
}

################################################################################
################################################################################