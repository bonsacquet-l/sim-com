#========================================================#
# fonction pour le calcul des distances entre les points #
# especes dans le premier plan des AFC simulees          #
# et le coefficient RV entre les matrices de distances   #
# lionel.bonsacquet                                      #
#========================================================#

Fct_RV_Nmds<-function(Nsimul,
                      analyse,
                      A_Coord_Nmds,
                      A_Coord_Nmds_Naive, 
                      fichier,
                      info_save="site_chao")
{
  #---------------------------------------------#
  # les matrices de distance informee et naives #
  #---------------------------------------------#
  #-- pour stocker les RV
  M_RV_Dist_<-matrix(NA,nrow=Nsimul,ncol=1)
  
  #-- passage des warning en error
  #options(warn = 2)
  #-- ou non
  options(warn = 0)
  
  #-- calcul des matrices de distance et le RV pour chaque simulation de la serie
  for (zz in 1:Nsimul) {
    #-- initialisation table null
    Res_RV_nmds<-NULL
    M_Tab_Dist_nmds<-NULL
    M_Tab_Dist_nmds_Naive<-NULL
    
    #-- calculs pour les sites
    try(M_Tab_Dist_nmds<-dist(A_Coord_Nmds[,,zz]))
    try(M_Tab_Dist_nmds_Naive<-dist(A_Coord_Nmds_Naive[,,zz]))
    try(Res_RV_nmds<-RVdist.randtest(M_Tab_Dist_nmds,M_Tab_Dist_nmds_Naive, nrepet = 999))
    
    #-- stockage
    if (is.null(Res_RV_nmds)==FALSE){
      M_RV_Dist_[zz,1]<-Res_RV_nmds$o}
  }
  
  #-----------------#
  # les sauvegardes #
  #-----------------#
  #-- les chemins de sauvegardes
  saveData<-file.path("Outcome","out-simul",analyse,paste(analyse,"_RV_warning",info_save,"_",fichier,".Rdata",sep=""))
  
  save(M_RV_Dist_,
       list = c("M_RV_Dist_"),
       file = saveData)
  
}


################################################################################
################################################################################