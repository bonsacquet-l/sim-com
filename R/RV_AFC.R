#========================================================#
# fonction pour le calcul des distances entre les points #
# especes dans le premier plan des AFC simulees          #
# et le coefficient RV entre les matrices de distances   #
# lionel.bonsacquet                                      #
#========================================================#

Fct_RV_AFC<-function(Nsimul, Site_Sp="Site", A_TabCoordAFC_Site_Sp, A_TabCoordAFC_Site_Sp_Naive, fichier)
{
  #---------------------------------------------#
  # les matrices de distance informee et naives #
  #---------------------------------------------#
  #-- pour stocker les RV
  M_RV_Dist_AFC<-matrix(NA,nrow=Nsimul,ncol=1)
  
  #-- calcul des matrices de distance et le RV pour chaque simulation de la serie
  for (zz in 1:Nsimul) {
    #-- calculs
    M_TabDistAFC<-dist(A_TabCoordAFC_Site_Sp[,,zz])
    M_TabDistAFC_Naive<-dist(A_TabCoordAFC_Site_Sp_Naive[,,zz])
    Res_RV<-RVdist.randtest(M_TabDistAFC,M_TabDistAFC_Naive, nrepet = 999)
    
    #-- stockage
    M_RV_Dist_AFC[zz,1]<-Res_RV$o
  }
  
  #-----------------#
  # les sauvegardes #
  #-----------------#
  #-- les chemins de sauvegardes
  if (Site_Sp=="Site") {Debut_Fichier<-"AFC_RV_Site_"} else {Debut_Fichier<-"AFC_RV_Sp_"}
  saveData<-file.path("Outcome","out-simul","AFC",paste(Debut_Fichier,fichier,".Rdata",sep=""))
  
  save(M_RV_Dist_AFC,
       list = c("M_RV_Dist_AFC"),
       file = saveData)
  
}


################################################################################
################################################################################