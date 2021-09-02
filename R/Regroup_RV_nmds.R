#--------------------------------------------------------------#
# fonction de regroupement des RV sur les matrices de distance #
# de toutes les simulations des nMDS pour une distance donnee  #
# lionel.bonsacquet                                            #
#--------------------------------------------------------------#
# executer dans le make_nmds

Fct_Regroup_RV_nmds<-function(Site_Sp="Site",distance="chao") {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(V_Nom_Fichier)
  
  M_Resultat_RV_nmds<-matrix(NA,nrow = 1000,ncol = Nbr_Fichier)
  colnames(M_Resultat_RV_nmds)<-as.character(V_Nom_Fichier)
  
  #-- appel des resultats et mise en matrice par une boucle
  for (i in V_Nom_Fichier) {
    #-- chargement
    load(file.path("Outcome","out-simul","nMDS",paste("nMDS","_RV_",Site_Sp,"_",distance,"_",i,".Rdata",sep="")))
    
    #-- stockage
    M_Resultat_RV_nmds[,i]<-M_RV_Dist_
    
    #-- sauvegarde
    saveData<-file.path("Outcome","out-regroupement","nMDS",paste("nMDS_Regroup_RV_",Site_Sp,"_",distance,".Rdata",sep=""))
    
    save(M_Resultat_RV_nmds,
         list = c("M_Resultat_RV_nmds"),
         file = saveData)
  }
}

################################################################################
################################################################################