#--------------------------------------------------#
# fonction repartition des especes en deux groupes #
# les plus detectable et les moins detectable      #
# script applicable que pour les D5                # 
# pour les AFC                                     #
# lionel.bonsacquet                                #
#--------------------------------------------------# # executer dans le make_AFC
# 11 especes moins detectable donc 11*1000 coordonnees a stocker au plus dans
# une colonne de chaque matrice

Fct_Grp_detectD5_AFC<-function() {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(c(V_Fichier_D5_GrpDetect))
  
  M_Grp_Detect_Axe1_AFC<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe1_AFC)<-as.character(V_Fichier_D5_GrpDetect)
  
  M_Grp_Detect_Axe2_AFC<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe2_AFC)<-as.character(V_Fichier_D5_GrpDetect)
  
  M_Grp_Detect_Axe1_AFC_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe1_AFC_Naive)<-as.character(V_Fichier_D5_GrpDetect)
  
  M_Grp_Detect_Axe2_AFC_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe2_AFC_Naive)<-as.character(V_Fichier_D5_GrpDetect)
  
  #-- chargement des donnees des coordonnees des especes
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_Coord_Sp.Rdata"))
  
  #-- la boucle avec appel de la detection des sp a chaque simulation
  for (n in c(V_Fichier_D5)) {
    print(n)
    
    #-- les donnees de detection (des 1000 simulations du fichier)
    Sim<-load(file.path("Outcome","out-simul","AFC",paste("AFC_Simul_",n,".Rdata",sep="")))
    M_MemDetectSp<-M_MemDetectSp
  
    for (z in 1:1000) {
      #-- ordre de detection des sp de moins au plus detectable
      Sp_les_Moins_Detect<-order(M_MemDetectSp[z,])[1:10]+20*(z-1)  # les dix moins detectable
      Sp_les_Plus_Detect<-order(M_MemDetectSp[z,])[11:20]+20*(z-1)  # les dix plus detectable
      
      #les lignes a remplir
      lignes<-(c((10*(z-1)+1):(10*(z-1)+10)))
      
       #-- repartition des coordonnees des especes dans le deux groupes
      ifelse(median(M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)
      M_Grp_Detect_Axe1_AFC[(lignes),paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe1_AFC[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1) 
      M_Grp_Detect_Axe1_AFC_Naive[(lignes),paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe1_AFC_Naive[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)    
      M_Grp_Detect_Axe2_AFC[(lignes),paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe2_AFC[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)   
      M_Grp_Detect_Axe2_AFC_Naive[lignes,paste(n,"_PlusD",sep="")]<-M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Plus_Detect,n]*a
      M_Grp_Detect_Axe2_AFC_Naive[lignes,paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Moins_Detect,n]*a
    }
  } 
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5.Rdata")
  
  save(M_Grp_Detect_Axe1_AFC,M_Grp_Detect_Axe2_AFC,
       M_Grp_Detect_Axe1_AFC_Naive,M_Grp_Detect_Axe2_AFC_Naive,
       list = c("M_Grp_Detect_Axe1_AFC","M_Grp_Detect_Axe2_AFC",
                "M_Grp_Detect_Axe1_AFC_Naive","M_Grp_Detect_Axe2_AFC_Naive"),
       file = saveData)
}

################################################################################
################################################################################