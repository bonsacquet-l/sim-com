#---------------------------------#
# fonction de regroupement des    #
# RV sur les matrices de distance #
# de toutes les simulations des   # 
# AFC                             #
# lionel.bonsacquet               #
#---------------------------------#     # executer dans le make_AFC

Fct_Regroup_RV_AFC<-function(Site_Sp="Site") {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(V_Nom_Fichier)
  
  M_Resultat_RV_AFC<-matrix(NA,nrow = 1000,ncol = Nbr_Fichier)
  colnames(M_Resultat_RV_AFC)<-as.character(V_Nom_Fichier)
  
  #-- appel des resultats et mise en matrice par une boucle
  for (i in V_Nom_Fichier) {
    #-- chargement
    load(file.path("Outcome","out-simul","AFC",paste("AFC_RV_",Site_Sp,"_",i,".Rdata",sep="")))
    
    #-- stockage
    M_Resultat_RV_AFC[,i]<-M_RV_Dist_AFC
    
    #-- sauvegarde
    saveData<-file.path("Outcome","out-regroupement","AFC",paste("AFC_Regroup_RV_",Site_Sp,".Rdata",sep=""))
    
    save(M_Resultat_RV_AFC,
         list = c("M_Resultat_RV_AFC"),
         file = saveData)
  }
}

################################################################################
################################################################################