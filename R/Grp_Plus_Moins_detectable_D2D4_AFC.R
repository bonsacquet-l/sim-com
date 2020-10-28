#--------------------------------------------------#
# fonction repartition des especes en deux groupes #
# les plus detectable et les moins detectable      #
# script applicable que pour les D2, D4            # 
# pour les AFC                                     #
# lionel.bonsacquet                                #
#--------------------------------------------------# # executer dans le make_AFC
# 11 especes moins detectable donc 11*1000 coordonnees a stocker au plus dans
# une colonne de chaque matrice

Fct_Grp_detect_AFC<-function() {
  #-- chargement des donnees (ici les coordonnees uniquement)
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_Coord_Sp.Rdata"))
  
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(c(V_Fichier_D2_GrpDetect,V_Fichier_D4_GrpDetect))
  
  M_Grp_Detect_Axe1_AFC<-matrix(NA,nrow = 11000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe1_AFC)<-as.character(c(V_Fichier_D2_GrpDetect,V_Fichier_D4_GrpDetect))
  
  M_Grp_Detect_Axe2_AFC<-matrix(NA,nrow = 11000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe2_AFC)<-as.character(c(V_Fichier_D2_GrpDetect,V_Fichier_D4_GrpDetect))
  
  M_Grp_Detect_Axe1_AFC_Naive<-matrix(NA,nrow = 11000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe1_AFC_Naive)<-as.character(c(V_Fichier_D2_GrpDetect,V_Fichier_D4_GrpDetect))
  
  M_Grp_Detect_Axe2_AFC_Naive<-matrix(NA,nrow = 11000,ncol = Nbr_Fichier)
  colnames(M_Grp_Detect_Axe2_AFC_Naive)<-as.character(c(V_Fichier_D2_GrpDetect,V_Fichier_D4_GrpDetect))
  
  #-- appel des resultats et mise en matrice par une boucle
  for (n in c(V_Fichier_D2,V_Fichier_D4)) {
    print(n)
    
    i<-0
    
    while (i<=999) {
      Sp_les_Plus_Detect<-c((6+i*20),(7+i*20),(8+i*20),(9+i*20),(10+i*20),(11+i*20),(12+i*20),
                            (19+i*20),(20+i*20))
      Sp_les_Moins_Detect<-c((1+i*20),(2+i*20),(3+i*20),(4+i*20),(5+i*20),
                             (13+i*20),(14+i*20),(15+i*20),(16+i*20),(17+i*20),(18+i*20))
      lignes<-(c((11*i+1):(11*i+11)))
      
      #-- repartition des coordonnees des especes dans le deux groupes
      ifelse(median(M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)
      M_Grp_Detect_Axe1_AFC[(lignes),paste(n,"_PlusD",sep="")]<-c(M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Plus_Detect,n]*a,
                                                                  rep(median(M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Plus_Detect,n])*a,2))
      M_Grp_Detect_Axe1_AFC[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_axe1[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1) 
      M_Grp_Detect_Axe1_AFC_Naive[(lignes),paste(n,"_PlusD",sep="")]<-c(M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Plus_Detect,n]*a,
                                                                        rep(median(M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Plus_Detect,n])*a,2))
      M_Grp_Detect_Axe1_AFC_Naive[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_Naive_axe1[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)    
      M_Grp_Detect_Axe2_AFC[(lignes),paste(n,"_PlusD",sep="")]<-c(M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Plus_Detect,n]*a,
                                                                  rep(median(M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Plus_Detect,n])*a,2))
      M_Grp_Detect_Axe2_AFC[(lignes),paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_axe2[Sp_les_Moins_Detect,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Moins_Detect,n])<0,a<-(-1),a<-1)   
      M_Grp_Detect_Axe2_AFC_Naive[lignes,paste(n,"_PlusD",sep="")]<-c(M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Plus_Detect,n]*a,
                                                                      rep(median(M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Plus_Detect,n])*a,2))
      M_Grp_Detect_Axe2_AFC_Naive[lignes,paste(n,"_MoinsD",sep="")]<-M_Resultat_Coord_Sp_AFC_Naive_axe2[Sp_les_Moins_Detect,n]*a
      
      i<-i+1 
    }
  }
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect.Rdata")
  
  save(M_Grp_Detect_Axe1_AFC,M_Grp_Detect_Axe2_AFC,
       M_Grp_Detect_Axe1_AFC_Naive,M_Grp_Detect_Axe2_AFC_Naive,
       list = c("M_Grp_Detect_Axe1_AFC","M_Grp_Detect_Axe2_AFC",
                "M_Grp_Detect_Axe1_AFC_Naive","M_Grp_Detect_Axe2_AFC_Naive"),
       file = saveData)
}

################################################################################
################################################################################