#--------------------------------------------------------------#
# fonction de regroupement des coordonnees des especes dans le #
# premier plan de toutes les simulations des nMDS              # 
# lionel.bonsacquet                                            #
#--------------------------------------------------------------#
# executer dans le make_nmds

#-- toutes les coordonnees des 20 especes sur les 1000 simuls, Soit 20000 donnees

Fct_Regroup_Coord_Sp_nmds<-function(distance="chao") {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))    # les noms des fichiers a compiler
  Nbr_Fichier<-length(V_Nom_Fichier)          # nombre de fichier a compiler
  
  M_Resultat_Coord_Sp_nmds_axe1<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier)  # coord des sp sur axe 1
  colnames(M_Resultat_Coord_Sp_nmds_axe1)<-as.character(V_Nom_Fichier)      # dans nmds     
  
  M_Resultat_Coord_Sp_nmds_axe2<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier)  # coord des sp sur axe 2
  colnames(M_Resultat_Coord_Sp_nmds_axe2)<-as.character(V_Nom_Fichier)      # dans nmds
  
  M_Resultat_Coord_Sp_nmds_Naive_axe1<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier) # coord des sp sur axe 1
  colnames(M_Resultat_Coord_Sp_nmds_Naive_axe1)<-as.character(V_Nom_Fichier)     # dans nmds_Naive
  
  M_Resultat_Coord_Sp_nmds_Naive_axe2<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier) # coord des sp sur axe 2
  colnames(M_Resultat_Coord_Sp_nmds_Naive_axe2)<-as.character(V_Nom_Fichier)     # dans nmds_Naive
  
  #-- appel des resultats et mise en matrice par une boucle
  for (i in V_Nom_Fichier) {
    #-- chargement
    load(file.path("Outcome","out-simul","nMDS",paste("nMDS_dist_",distance,"_",fichier,".Rdata",sep="")))
    
    #-- stockage
    M_Resultat_Coord_Sp_nmds_axe1[,i]<-as.vector(A_Coord_nmds_Sp[,1,])
    M_Resultat_Coord_Sp_nmds_axe2[,i]<-as.vector(A_Coord_nmds_Sp[,2,])
    M_Resultat_Coord_Sp_nmds_Naive_axe1[,i]<-as.vector(A_Coord_nmds_Sp_Naive[,1,])
    M_Resultat_Coord_Sp_nmds_Naive_axe2[,i]<-as.vector(A_Coord_nmds_Sp_Naive[,2,])
    
    #-- sauvegarde
    saveData<-file.path("Outcome","out-regroupement","nMDS",paste("nMDS_Regroup_Coord_Sp_",distance,".Rdata",sep = ""))
    
    save(M_Resultat_Coord_Sp_nmds_axe1,M_Resultat_Coord_Sp_nmds_axe2,
         M_Resultat_Coord_Sp_nmds_Naive_axe1,M_Resultat_Coord_Sp_nmds_Naive_axe2,
         list = c("M_Resultat_Coord_Sp_nmds_axe1","M_Resultat_Coord_Sp_nmds_axe2",
                  "M_Resultat_Coord_Sp_nmds_Naive_axe1","M_Resultat_Coord_Sp_nmds_Naive_axe2"),
         file = saveData)
  }
}

################################################################################
################################################################################