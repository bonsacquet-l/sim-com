#--------------------------------------------------#
# fonction repartition des especes en deux groupes #
# le groupe A (les sp de paire) et le groupe B     #
# (les sp impaire), applicable a tout les simul    # 
# pour les PCoA                                    #
# lionel.bonsacquet                                #
#--------------------------------------------------# # executer dans le make_pcoa
# 1O especes dans un Groupe donc 10*1000 coordonnees a stocker au plus dans
# une colonne de chaque matrice

Fct_Grp_A_B_pcoa<-function(distance="chao") {
  #-- chargement des donnees (ici les coordonnees uniquement)
  load(file.path("Outcome","out-regroupement","PCoA",paste("PCoA_Regroup_Coord_Sp_",distance,".Rdata",sep = "")))
  
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(V_Fichier_GrpA_B)
  
  M_Grp_A_B_Axe1_pcoa<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_A_B_Axe1_pcoa)<-as.character(V_Fichier_GrpA_B)
  
  M_Grp_A_B_Axe2_pcoa<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_A_B_Axe2_pcoa)<-as.character(V_Fichier_GrpA_B)
  
  M_Grp_A_B_Axe1_pcoa_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_A_B_Axe1_pcoa_Naive)<-as.character(V_Fichier_GrpA_B)
  
  M_Grp_A_B_Axe2_pcoa_Naive<-matrix(NA,nrow = 10000,ncol = Nbr_Fichier)
  colnames(M_Grp_A_B_Axe2_pcoa_Naive)<-as.character(V_Fichier_GrpA_B)
  
  #-- appel des resultats et mise en matrice par une boucle
  for (n in V_Nom_Fichier) {
    print(n)
    
    i<-0   
    Sp_Grp_A<-vector(length = 0)
    Sp_Grp_B<-vector(length = 0)
    
    while (i<=999)
    {
      Sp_Grp_A<-c((1+i*20),(3+i*20),(5+i*20),(7+i*20),(9+i*20),(11+i*20),(13+i*20),(15+i*20),
                  (17+i*20),(19+i*20))
      Sp_Grp_B<-c((2+i*20),(4+i*20),(6+i*20),(8+i*20),(10+i*20),(12+i*20),(14+i*20),
                  (16+i*20),(18+i*20),(20+i*20))
      
      #les lignes a remplir
      lignes<-(c((10*i+1):(10*i+10)))
      
      #-- repartition des coordonnees des especes dans le deux groupes
      ifelse(median(M_Resultat_Coord_Sp_pcoa_axe1[Sp_Grp_B,n])<0,a<-(-1),a<-1)
      M_Grp_A_B_Axe1_pcoa[(lignes),paste(n,"_GA",sep="")]<-M_Resultat_Coord_Sp_pcoa_axe1[Sp_Grp_A,n]*a
      M_Grp_A_B_Axe1_pcoa[(lignes),paste(n,"_GB",sep="")]<-M_Resultat_Coord_Sp_pcoa_axe1[Sp_Grp_B,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_pcoa_Naive_axe1[Sp_Grp_B,n])<0,a<-(-1),a<-1) 
      M_Grp_A_B_Axe1_pcoa_Naive[(lignes),paste(n,"_GA",sep="")]<-M_Resultat_Coord_Sp_pcoa_Naive_axe1[Sp_Grp_A,n]*a
      M_Grp_A_B_Axe1_pcoa_Naive[(lignes),paste(n,"_GB",sep="")]<-M_Resultat_Coord_Sp_pcoa_Naive_axe1[Sp_Grp_B,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_pcoa_axe2[Sp_Grp_B,n])<0,a<-(-1),a<-1)    
      M_Grp_A_B_Axe2_pcoa[(lignes),paste(n,"_GA",sep="")]<-M_Resultat_Coord_Sp_pcoa_axe2[Sp_Grp_A,n]*a
      M_Grp_A_B_Axe2_pcoa[(lignes),paste(n,"_GB",sep="")]<-M_Resultat_Coord_Sp_pcoa_axe2[Sp_Grp_B,n]*a
      
      ifelse(median(M_Resultat_Coord_Sp_pcoa_Naive_axe2[Sp_Grp_B,n])<0,a<-(-1),a<-1)   
      M_Grp_A_B_Axe2_pcoa_Naive[lignes,paste(n,"_GA",sep="")]<-M_Resultat_Coord_Sp_pcoa_Naive_axe2[Sp_Grp_A,n]*a
      M_Grp_A_B_Axe2_pcoa_Naive[lignes,paste(n,"_GB",sep="")]<-M_Resultat_Coord_Sp_pcoa_Naive_axe2[Sp_Grp_B,n]*a
      
      i<-i+1 
    }
  } 
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement","PCoA",paste("PCoA_",distance,"_Grp_A_B.Rdata",sep = ""))
  
  save(M_Grp_A_B_Axe1_pcoa,M_Grp_A_B_Axe2_pcoa,
       M_Grp_A_B_Axe1_pcoa_Naive,M_Grp_A_B_Axe2_pcoa_Naive,
       list = c("M_Grp_A_B_Axe1_pcoa","M_Grp_A_B_Axe2_pcoa",
                "M_Grp_A_B_Axe1_pcoa_Naive","M_Grp_A_B_Axe2_pcoa_Naive"),
       file = saveData)
}

################################################################################
################################################################################