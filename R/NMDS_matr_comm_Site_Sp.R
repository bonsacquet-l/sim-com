#---------------------------------------#
# script pour analyse nmds              #
# non metrique multidimensional scaling #
# matrice de la communaute              # 
# avec indice de chao type jaccard et   #
# de bray curtis                        #
#---------------------------------------#

library(vegan)

#-- une fonction pour appliquer cette analyse au donnees simuler
fct_nmds_comm_site_sp<-function(analyse="AFC",fichier="D4_C3_05") {   # sur AFC de base et sur D4_C3-05 pour test
  
  #-- appel des donnees de simulations
  load(file.path("Outcome","out-simul",analyse,paste(analyse,"_Simul_",fichier,".Rdata",sep="")))
  
  #-- recup des infos
  Nsimul<-dim(A_MemAbon)[3]  #A_MemAbon Array des 1000 simulations (Nsimul) d'abondance de Nsp (espece) sur Nplot (site)
  Nplot<-dim(A_MemAbon)[1]   #A_MemAbon Array des abondance informees
  Nsp<-dim(A_MemAbon)[2]
  
  #-- creation des array pour sauvegarder les infos
  A_Coord_Nmds_com_Site_chao<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_Nmds_com_Site_Naive_chao<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  A_Coord_Nmds_com_Sp_chao<-array(data = NA, dim = c(Nsp,2,Nsimul))
  A_Coord_Nmds_com_Sp_Naive_chao<-array(data = NA, dim = c(Nsp,2,Nsimul))
  
  A_Coord_Nmds_com_Site_bray<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_Nmds_com_Site_Naive_bray<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  A_Coord_Nmds_com_Sp_bray<-array(data = NA, dim = c(Nsp,2,Nsimul))
  A_Coord_Nmds_com_Sp_Naive_bray<-array(data = NA, dim = c(Nsp,2,Nsimul))
  
  #-- passage des warning en error
  options(warn = 2)
  
  #-- une boucle pour recuperer les resultats des 1000 simulations du fichier
  for (zz in 1:Nsimul) {
    print(cat("simul num: ", zz))
    #-- calculs des racine carree des abondances (pas besoin pour chao car calcul de 2005 sur abondance)
    M_MemAbonSqrt<-sqrt(A_MemAbon[,,zz])
    M_MemAbonSqrt_Naive<-sqrt(A_MemAbon_Naive[,,zz])
    
    #-- reinitialisation a chaque boucle
    Nmds_Abon_chao<-NULL
    Nmds_Abon_Naive_chao<-NULL
    Nmds_Abon_bray<-NULL
    Nmds_Abon_Naive_bray<-NULL
    
    #-- le nmds sur les matrices de distance
    try(Nmds_Abon_chao<-metaMDS(A_MemAbon[,,zz],
                            distance = "chao", 
                            k = 2,
                            maxit = 999, 
                            try = 100,
                            trymax = 1000,
                            autotransform = FALSE,
                            wascores = TRUE),
        silent=FALSE)
    
    try(Nmds_Abon_Naive_chao<-metaMDS(A_MemAbon_Naive[,,zz],
                                  distance = "chao",
                                  k = 2,
                                  maxit = 999, 
                                  try = 100,
                                  trymax = 1000,
                                  autotransform = FALSE,
                                  wascores = TRUE),
        silent=FALSE)
    
    try(Nmds_Abon_bray<-metaMDS(M_MemAbonSqrt,
                            distance = "bray", 
                            k = 2,
                            maxit = 999, 
                            try = 100,
                            trymax = 1000,
                            autotransform = FALSE,  # car sqrt des abondances
                            wascores = TRUE),
        silent=FALSE)
    
    try(Nmds_Abon_Naive_bray<-metaMDS(M_MemAbonSqrt_Naive,
                                  distance = "bray",
                                  k = 2,
                                  maxit = 999, 
                                  try = 100,
                                  trymax = 1000,
                                  autotransform = FALSE, # car sqrt des abondances
                                  wascores = TRUE),
        silent=FALSE)

    #-- recup des coord des pts sites dans le premier plan
    if (is.null(Nmds_Abon_chao)==FALSE){
      A_Coord_Nmds_com_Site_chao[,,zz]<-Nmds_Abon_chao$points[1:Nplot,1:2]}
    if (is.null(Nmds_Abon_Naive_chao)==FALSE){
      A_Coord_Nmds_com_Site_Naive_chao[,,zz]<-Nmds_Abon_Naive_chao$points[1:Nplot,1:2]}
    
    
    if (is.null(Nmds_Abon_chao)==FALSE){
      A_Coord_Nmds_com_Sp_chao[,,zz]<-Nmds_Abon_chao$species[1:Nsp,1:2]}
    if (is.null(Nmds_Abon_Naive_chao)==FALSE){
      A_Coord_Nmds_com_Sp_Naive_chao[,,zz]<-Nmds_Abon_Naive_chao$species[1:Nsp,1:2]}
    
    if (is.null(Nmds_Abon_bray)==FALSE){
      A_Coord_Nmds_com_Site_bray[,,zz]<-Nmds_Abon_bray$points[1:Nplot,1:2]}
    if (is.null(Nmds_Abon_Naive_bray)==FALSE){
    A_Coord_Nmds_com_Site_Naive_bray[,,zz]<-Nmds_Abon_Naive_bray$points[1:Nplot,1:2]}
    
    if (is.null(Nmds_Abon_bray)==FALSE){
      A_Coord_Nmds_com_Sp_bray[,,zz]<-Nmds_Abon_bray$species[1:Nsp,1:2]}
    if (is.null(Nmds_Abon_Naive_bray)==FALSE){
      A_Coord_Nmds_com_Sp_Naive_bray[,,zz]<-Nmds_Abon_Naive_bray$species[1:Nsp,1:2]}
  }
  
  #-- les sauvegardes
  #-- les chemins de sauvegardes
  saveData<-file.path("Outcome","out-simul",analyse,paste(analyse,"_nmds_site_sp_",fichier,".Rdata",sep=""))
  
  save(Nsimul,Nplot,Nsp,
       A_Coord_Nmds_com_Site_chao,A_Coord_Nmds_com_Site_Naive_chao,
       A_Coord_Nmds_com_Sp_chao,A_Coord_Nmds_com_Sp_Naive_chao,
       A_Coord_Nmds_com_Site_bray,A_Coord_Nmds_com_Site_Naive_bray,
       A_Coord_Nmds_com_Sp_bray,A_Coord_Nmds_com_Sp_Naive_bray,
       list = c("Nsimul","Nplot","Nsp",
                "A_Coord_Nmds_com_Site_chao","A_Coord_Nmds_com_Site_Naive_chao",
                "A_Coord_Nmds_com_Sp_chao","A_Coord_Nmds_com_Sp_Naive_chao",
                "A_Coord_Nmds_com_Site_bray","A_Coord_Nmds_com_Site_Naive_bray",
                "A_Coord_Nmds_com_Sp_bray","A_Coord_Nmds_com_Sp_Naive_bray"),
       file = saveData)
  
}  
