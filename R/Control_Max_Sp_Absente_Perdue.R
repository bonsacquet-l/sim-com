#--------------------------------#
# script pour sortir les donnees #
# Max_Nbr_Sp_Absente et perdues  #
# lors des simulations           #
# lionel bonsacquet              #
#--------------------------------#

#-- chargement des noms des fichiers
source(file.path("R","Noms-Fichiers.R"))

#-- fonction avec pour entree AFC ou ACP et le nom des fichiers
Fct_Control_Sp_Absente_Perdue<-function(Analyse="ACP",Fichier=V_Fichier_D3) {
  for (i in Fichier) {
    #-- chargement du fichier
    load(file.path("Outcome","out-simul",Analyse,paste(Analyse,"_Simul_",i,".Rdata",sep="")))
    
    #-- sorties des infos
    print(paste("max d'especes absentes: ", Max_NbrSpAbsente))
    print(paste("max d'especes perdue: ", Max_NbrSpPerdu))
  }
}


