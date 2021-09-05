#========================================================#
# fonction pour le calcul des distances entre les points #
# site et espece dans le premier plan des nmds simulees  #
# et le coefficient RV entre les matrices de distances   #
# lionel.bonsacquet                                      #
#========================================================#

Fct_RV_nmds<-function(distance="chao",
                      fichier)
{
  #---------------------------------------------#
  # les matrices de distance informee et naives #
  #---------------------------------------------#
  #-- charger les resultats des nMDS
  load(file.path("Outcome","out-simul","nMDS",paste("nMDS_dist_",distance,"_",fichier,".Rdata",sep="")))
  
  #-- pour stocker les RV
  M_RV_Dist_Sp<-matrix(NA,nrow=Nsimul,ncol=1)
  M_RV_Dist_Site<-matrix(NA,nrow=Nsimul,ncol=1)
  
  #-- passage des warning en error
  #options(warn = 2)
  #-- ou non
  options(warn = 0)
  
  #-- calcul des matrices de distance et le RV pour chaque simulation de la serie
  for (zz in 1:Nsimul) {
    #-- initialisation table null pour les Sp
    Res_RV_nmds_Sp<-NULL
    M_Tab_Dist_nmds_Sp<-NULL
    M_Tab_Dist_nmds_Sp_Naive<-NULL
    
    #-- initialisation table null pour les Site
    Res_RV_nmds_Site<-NULL
    M_Tab_Dist_nmds_Site<-NULL
    M_Tab_Dist_nmds_Site_Naive<-NULL
    
    #-- calculs pour les Sp
    try(M_Tab_Dist_nmds_Sp<-dist(A_Coord_nmds_Sp[,,zz]))
    try(M_Tab_Dist_nmds_Sp_Naive<-dist(A_Coord_nmds_Sp_Naive[,,zz]))
    try(Res_RV_nmds_Sp<-RVdist.randtest(M_Tab_Dist_nmds_Sp,M_Tab_Dist_nmds_Sp_Naive, nrepet = 999))
    
    #-- calculs pour les Site
    try(M_Tab_Dist_nmds_Site<-dist(A_Coord_nmds_Site[,,zz]))
    try(M_Tab_Dist_nmds_Site_Naive<-dist(A_Coord_nmds_Site_Naive[,,zz]))
    try(Res_RV_nmds_Site<-RVdist.randtest(M_Tab_Dist_nmds_Site,M_Tab_Dist_nmds_Site_Naive, nrepet = 999))
    
    #-- stockage
    if (is.null(Res_RV_nmds_Sp)==FALSE){
      M_RV_Dist_Sp[zz,1]<-Res_RV_nmds_Sp$o}
    
    if (is.null(Res_RV_nmds_Site)==FALSE){
      M_RV_Dist_Site[zz,1]<-Res_RV_nmds_Site$o}
  }
  
  #-----------------#
  # les sauvegardes #
  #-----------------#
  #-- les chemins de sauvegardes
  saveData1<-file.path("Outcome","out-simul","nMDS",paste("nMDS","_RV_Sp_",distance,"_",fichier,".Rdata",sep=""))
  saveData2<-file.path("Outcome","out-simul","nMDS",paste("nMDS","_RV_Site_",distance,"_",fichier,".Rdata",sep=""))
  
  M_RV_Dist_<-M_RV_Dist_Sp
  save(M_RV_Dist_,
       list = c("M_RV_Dist_"),
       file = saveData1)
  
  M_RV_Dist_<-M_RV_Dist_Site
  save(M_RV_Dist_,
       list = c("M_RV_Dist_"),
       file = saveData2)
  
}

################################################################################
################################################################################