#---------------------------------#
# fonction de regroupement des    #
# RV sur les matrices de distance #
# de toutes les simulations des   # 
# ACP                             #
# lionel.bonsacquet               #
#---------------------------------#     # executer dans le make_ACP

Fct_Regroup_RV_ACP<-function(Site_Sp="Site") {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))
  Nbr_Fichier<-length(V_Nom_Fichier)

  M_Resultat_RV_ACP<-matrix(NA,nrow = 1000,ncol = Nbr_Fichier)
  colnames(M_Resultat_RV_ACP)<-as.character(V_Nom_Fichier)

  #-- appel des resultats et mise en matrice par une boucle
  for (i in V_Nom_Fichier) {
    #-- chargement
    load(file.path("Outcome","out-simul","ACP",paste("ACP_RV_",Site_Sp,"_",i,".Rdata",sep="")))
    
    #-- stockage
    M_Resultat_RV_ACP[,i]<-M_RV_Dist_ACP
    
    #-- sauvegarde
    saveData<-file.path("Outcome","out-regroupement","ACP",paste("ACP_Regroup_RV_",Site_Sp,".Rdata",sep=""))
    
    save(M_Resultat_RV_ACP,
         list = c("M_Resultat_RV_ACP"),
         file = saveData)
  }
}
  
################################################################################
################################################################################