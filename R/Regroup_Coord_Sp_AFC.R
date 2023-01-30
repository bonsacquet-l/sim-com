#---------------------------------#
# fonction de regroupement des    #
# coordonnees des especes dans le #
# premier plan de toutes les      #
# simulations des AFC              # 
# lionel.bonsacquet               #
#---------------------------------#     # executer dans le make_AFC

#-- toutes les coordonnees des 20 especes sur les 1000 simuls, Soit 20000 donnees

Fct_Regroup_Coord_Sp_AFC<-function() {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))    # les noms des fichiers a compiler
  Nbr_Fichier<-length(V_Nom_Fichier)          # nombre de fichier a compiler

  M_Resultat_Coord_Sp_AFC_axe1<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier)  # coord des sp sur axe 1
  colnames(M_Resultat_Coord_Sp_AFC_axe1)<-as.character(V_Nom_Fichier)      # dans AFC     
  
  M_Resultat_Coord_Sp_AFC_axe2<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier)  # coord des sp sur axe 2
  colnames(M_Resultat_Coord_Sp_AFC_axe2)<-as.character(V_Nom_Fichier)      # dans AFC
  
  M_Resultat_Coord_Sp_AFC_Naive_axe1<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier) # coord des sp sur axe 1
  colnames(M_Resultat_Coord_Sp_AFC_Naive_axe1)<-as.character(V_Nom_Fichier)     # dans AFC_Naive
  
  M_Resultat_Coord_Sp_AFC_Naive_axe2<-matrix(NA,nrow = 20000,ncol = Nbr_Fichier) # coord des sp sur axe 2
  colnames(M_Resultat_Coord_Sp_AFC_Naive_axe2)<-as.character(V_Nom_Fichier)     # dans AFC_Naive
  
  #-- appel des resultats et mise en matrice par une boucle
  for (i in V_Nom_Fichier) {
    #-- chargement
    print(i)
    load(file.path("Outcome","out-simul","AFC",paste("AFC_Simul_",i,".Rdata",sep="")))
    
    #-- stockage
    M_Resultat_Coord_Sp_AFC_axe1[,i]<-as.vector(A_TabCoordAFC[,1,])
    M_Resultat_Coord_Sp_AFC_axe2[,i]<-as.vector(A_TabCoordAFC[,2,])
    M_Resultat_Coord_Sp_AFC_Naive_axe1[,i]<-as.vector(A_TabCoordAFC_Naive[,1,])
    M_Resultat_Coord_Sp_AFC_Naive_axe2[,i]<-as.vector(A_TabCoordAFC_Naive[,2,])
    
    #-- sauvegarde
    saveData<-file.path("Outcome","out-regroupement","AFC","AFC_Regroup_Coord_Sp.Rdata")
    
    save(M_Resultat_Coord_Sp_AFC_axe1,M_Resultat_Coord_Sp_AFC_axe2,
         M_Resultat_Coord_Sp_AFC_Naive_axe1,M_Resultat_Coord_Sp_AFC_Naive_axe2,
         list = c("M_Resultat_Coord_Sp_AFC_axe1","M_Resultat_Coord_Sp_AFC_axe2",
                  "M_Resultat_Coord_Sp_AFC_Naive_axe1","M_Resultat_Coord_Sp_AFC_Naive_axe2"),
         file = saveData)
  }
}

################################################################################
################################################################################