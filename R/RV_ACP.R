#========================================================#
# fonction pour le calcul des distances entre les points #
# especes dans le premier plan des ACP simulees          #
# et le coefficient RV entre les matrices de distances   #
# lionel.bonsacquet                                      #
#========================================================#

Fct_RV_ACP<-function(Nsimul, Site_Sp="Site", A_TabCoordACP_Site_Sp, A_TabCoordACP_Site_Sp_Naive, fichier)
{
  #---------------------------------------------#
  # les matrices de distance informee et naives #
  #---------------------------------------------#
  #-- pour stocker les RV
  M_RV_Dist_ACP<-matrix(NA,nrow=Nsimul,ncol=1)
  
  #-- calcul des matrices de distance et le RV pour chaque simulation de la serie
  for (zz in 1:Nsimul) {
    #-- calculs
    M_TabDistACP<-dist(A_TabCoordACP_Site_Sp[,,zz])
    M_TabDistACP_Naive<-dist(A_TabCoordACP_Site_Sp_Naive[,,zz])
    Res_RV<-RVdist.randtest(M_TabDistACP,M_TabDistACP_Naive, nrepet = 999)
    
    #-- stockage
    M_RV_Dist_ACP[zz,1]<-Res_RV$o
  }
  
  #-----------------#
  # les sauvegardes #
  #-----------------#
  #-- les chemins de sauvegardes
  if (Site_Sp=="Site") {Debut_Fichier<-"ACP_RV_Site_"} else {Debut_Fichier<-"ACP_RV_Sp_"}
  saveData<-file.path("Outcome","out-simul","ACP",paste(Debut_Fichier,fichier,".Rdata",sep=""))
  
  save(M_RV_Dist_ACP,
       list = c("M_RV_Dist_ACP"),
       file = saveData)
}


################################################################################
################################################################################