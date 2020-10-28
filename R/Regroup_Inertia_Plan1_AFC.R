#----------------------------------------#
# fonction pour regrouper les resultats  #
# des inerties expliquees par le premier #
# plan de projection dans les AFC        #
# lionel bonsacquet                      #
#----------------------------------------#


Fct_Regroup_Inertia_Plan1_AFC<-function() {
  #-- Pour stocker les resultats
  source(file.path("R","Noms-Fichiers.R"))         # sourcer tout les noms de fichiers
  Nbr_Fichier<-length(V_Nom_Fichier)               # recupere les bon noms
  
  M_Inertia_Plan1_AFC<-matrix(NA,nrow = 1000,ncol = Nbr_Fichier)      # matrice pour stocker les resultats
  colnames(M_Inertia_Plan1_AFC)<-as.character(V_Nom_Fichier)          # noms des colonnes de la matrice (les fichier)
  
  #-- appel des resultats et mise en matrice par une boucle
  for (i in V_Nom_Fichier) {
    #-- chargement
    load(file.path("Outcome","out-simul","AFC",paste("AFC_Simul_",i,".Rdata",sep="")))
    
    #-- stockage
    for (j in 1:1000) {
      M_Inertia_Plan1_AFC[j,i]<-sum(M_TabEigAFC[j,1:2])
    }
  }
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement","AFC","AFC_Inertia_Plan1.Rdata")
  
  save(M_Inertia_Plan1_AFC,
       list = c("M_Inertia_Plan1_AFC"),
       file = saveData)
}

################################################################################
################################################################################