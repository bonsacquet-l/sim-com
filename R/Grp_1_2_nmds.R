# fonction repartition des especes en deux groupes #
# le groupe 1 (les sp de 1 à 10) et le groupe 2    #
# (les sp de 11 a 20), applicable a tout les simul # 
# pour les nMDS                                    #
# lionel.bonsacquet                                #
#--------------------------------------------------# # executer dans le make_nmds
# 1O especes dans un Groupe donc 10*1000 coordonnees a stocker au plus dans
# une colonne de chaque matrice

Fct_Grp_1_2_nmds<-function(distance="chao") {
  #-- chargement des donnees (ici les coordonnees uniquement)
  load(file.path("Outcome","out-regroupement","nMDS",paste("nMDS_Regroup_Coord_Sp_",distance,".Rdata",sep = "")))
  
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(V_Fichier_Grp1_2)
  
  M_Grp_1_2_Axe1_nmds<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_1_2_Axe1_nmds)<-as.character(V_Fichier_Grp1_2)
  
  M_Grp_1_2_Axe2_nmds<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_1_2_Axe2_nmds)<-as.character(V_Fichier_Grp1_2)
  
  M_Grp_1_2_Axe1_nmds_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_1_2_Axe1_nmds_Naive)<-as.character(V_Fichier_Grp1_2)
  
  M_Grp_1_2_Axe2_nmds_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_1_2_Axe2_nmds_Naive)<-as.character(V_Fichier_Grp1_2)
  
  #-- appel des resultats et mise en matrice par une boucle
  for (n in V_Nom_Fichier) {
    print(n)
    Sp_Grp_1<-vector(length = 0)
    Sp_Grp_2<-vector(length = 0)
    
    i<-0
    while (i<=999)
    {
      Sp_Grp_1<-c((1+i*20),(2+i*20),(3+i*20),(4+i*20),(5+i*20),(6+i*20),(7+i*20),(8+i*20),
                  (9+i*20),(10+i*20))
      Sp_Grp_2<-c((11+i*20),(12+i*20),(13+i*20),(14+i*20),(15+i*20),(16+i*20),(17+i*20),
                  (18+i*20),(19+i*20),(20+i*20))
      
      #les lignes a remplir
      lignes<-(c((10*i+1):(10*i+10)))
      
      #-- repartition des coordonnees des especes dans le deux groupes
      a<-NA
      ifelse(median(M_Resultat_Coord_Sp_nmds_axe1[Sp_Grp_2,n])<0,a<-(-1),a<-1)
      M_Grp_1_2_Axe1_nmds[(lignes),paste(n,"_G1",sep="")]<-M_Resultat_Coord_Sp_nmds_axe1[Sp_Grp_1,n]*a
      M_Grp_1_2_Axe1_nmds[(lignes),paste(n,"_G2",sep="")]<-M_Resultat_Coord_Sp_nmds_axe1[Sp_Grp_2,n]*a
      
      a<-NA
      ifelse(median(M_Resultat_Coord_Sp_nmds_Naive_axe1[Sp_Grp_2,n])<0,a<-(-1),a<-1) 
      M_Grp_1_2_Axe1_nmds_Naive[(lignes),paste(n,"_G1",sep="")]<-M_Resultat_Coord_Sp_nmds_Naive_axe1[Sp_Grp_1,n]*a
      M_Grp_1_2_Axe1_nmds_Naive[(lignes),paste(n,"_G2",sep="")]<-M_Resultat_Coord_Sp_nmds_Naive_axe1[Sp_Grp_2,n]*a
      
      a<-NA
      ifelse(median(M_Resultat_Coord_Sp_nmds_axe2[Sp_Grp_2,n])<0,a<-(-1),a<-1)    
      M_Grp_1_2_Axe2_nmds[(lignes),paste(n,"_G1",sep="")]<-M_Resultat_Coord_Sp_nmds_axe2[Sp_Grp_1,n]*a
      M_Grp_1_2_Axe2_nmds[(lignes),paste(n,"_G2",sep="")]<-M_Resultat_Coord_Sp_nmds_axe2[Sp_Grp_2,n]*a
      
      a<-NA
      ifelse(median(M_Resultat_Coord_Sp_nmds_Naive_axe2[Sp_Grp_2,n])<0,a<-(-1),a<-1)   
      M_Grp_1_2_Axe2_nmds_Naive[lignes,paste(n,"_G1",sep="")]<-M_Resultat_Coord_Sp_nmds_Naive_axe2[Sp_Grp_1,n]*a
      M_Grp_1_2_Axe2_nmds_Naive[lignes,paste(n,"_G2",sep="")]<-M_Resultat_Coord_Sp_nmds_Naive_axe2[Sp_Grp_2,n]*a
      
      i<-i+1 
    }
  } 
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement","nMDS",paste("nMDS_",distance,"_Grp_1_2.Rdata",sep = ""))
  
  save(M_Grp_1_2_Axe1_nmds,M_Grp_1_2_Axe2_nmds,
       M_Grp_1_2_Axe1_nmds_Naive,M_Grp_1_2_Axe2_nmds_Naive,
       list = c("M_Grp_1_2_Axe1_nmds","M_Grp_1_2_Axe2_nmds",
                "M_Grp_1_2_Axe1_nmds_Naive","M_Grp_1_2_Axe2_nmds_Naive"),
       file = saveData)
}

################################################################################
################################################################################