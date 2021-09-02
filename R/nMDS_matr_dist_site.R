#---------------------------------------#
# script pour analyse nmds              #
# non metrique multidimensional scaling #
# matrice de la communaute              # 
# avec indice de chao type jaccard et   #
# de bray curtis                        #
#---------------------------------------#

library(vegan)

#-- une fonction pour appliquer cette analyse au donnees simuler
fct_nmds_site<-function(analyse="AFC", # sur simul faite pour ACP
                        distance="chao", # pour test
                        fichier="D4_C3_05") {   # sur D4_C3_05 pour test
  
  #-- appel des donnees de simulations
  load(file.path("Outcome","out-simul",analyse,paste(analyse,"_Simul_",fichier,".Rdata",sep="")))
  
  #-- passage des warning en error
  options(warn = 2)
  
  #-- recup des infos
  Nsimul<-dim(A_MemAbon)[3]  #A_MemAbon Array des 1000 simulations (Nsimul) d'abondance de Nsp (espece) sur Nplot (site)
  Nplot<-dim(A_MemAbon)[1]   #A_MemAbon Array des abondance informees
  Nsp<-dim(A_MemAbon)[2]
  
  #-- creation des array pour sauvegarder les infos
  A_Coord_nmds_Site<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_nmds_Site_Naive<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  A_Coord_nmds_Sp<-array(data = NA, dim = c(Nsp,2,Nsimul))
  A_Coord_nmds_Sp_Naive<-array(data = NA, dim = c(Nsp,2,Nsimul))
  
  A_Coord_nmds_Site_TEST<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_nmds_Site_Naive_TEST<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  #-- une boucle pour recuperer les resultats des 1000 simulations du fichier
  for (zz in 1:Nsimul) {
    #-- calculs des racine carree des abondances (pas besoin pour chao car calcul de 2005 sur abondance)
    M_MemAbonSqrt<-sqrt(A_MemAbon[,,zz])
    M_MemAbonSqrt_Naive<-sqrt(A_MemAbon_Naive[,,zz])
    
    #-- reinitialisation a chaque boucle
    Nmds_Abon<-NULL
    Nmds_Abon_Naive<-NULL

    #-- le nmds sur les matrices de distance
    if (distance=="chao"){
      try(Nmds_Abon<-metaMDS(A_MemAbon[,,zz],
                            distance = "chao", 
                            k = 2,
                            maxit = 999, 
                            try = 100,
                            trymax = 1000,
                            autotransform = FALSE,
                            wascores = TRUE),
        silent=FALSE)
      
      try(Nmds_Abon_Naive<-metaMDS(A_MemAbon_Naive[,,zz],
                                  distance = "chao",
                                  k = 2,
                                  maxit = 999, 
                                  try = 100,
                                  trymax = 1000,
                                  autotransform = FALSE,
                                  wascores = TRUE),
        silent=FALSE)
    }
    
    if (distance=="bray"){
      try(Nmds_Abon<-metaMDS(M_MemAbonSqrt,
                            distance = "bray", 
                            k = 2,
                            maxit = 999, 
                            try = 100,
                            trymax = 1000,
                            autotransform = FALSE,  # car sqrt des abondances
                            wascores = TRUE),
        silent=FALSE)
    
    try(Nmds_Abon_Naive<-metaMDS(M_MemAbonSqrt_Naive,
                                  distance = "bray",
                                  k = 2,
                                  maxit = 999, 
                                  try = 100,
                                  trymax = 1000,
                                  autotransform = FALSE, # car sqrt des abondances
                                  wascores = TRUE),
        silent=FALSE)
    }
    
    #-- recup des coord des pts sites dans le premier plan
    if (is.null(Nmds_Abon)==FALSE & is.null(Nmds_Abon_Naive)==FALSE){
      A_Coord_nmds_Site[,,zz]<-Nmds_Abon$points[1:Nplot,1:2]
      A_Coord_nmds_Site_Naive[,,zz]<-Nmds_Abon_Naive$points[1:Nplot,1:2]}
    
    if (is.null(Nmds_Abon)==FALSE){
      A_Coord_nmds_Site_TEST[,,zz]<-Nmds_Abon$points[1:Nplot,1:2]}
    if (is.null(Nmds_Abon_Naive)==FALSE){
      A_Coord_nmds_Site_Naive_TEST[,,zz]<-Nmds_Abon_Naive$points[1:Nplot,1:2]}
    
    #-- recup des coord des pts sp dans le premier plan
    if (is.null(Nmds_Abon)==FALSE & is.null(Nmds_Abon_Naive)==FALSE){
      A_Coord_nmds_Sp[,,zz]<-Nmds_Abon$species[1:Nsp,1:2]
      A_Coord_nmds_Sp_Naive[,,zz]<-Nmds_Abon_Naive$species[1:Nsp,1:2]}
    
  }
  
  #-- les sauvegardes
  #-- les chemins de sauvegardes
  saveData<-file.path("Outcome","out-simul","nMDS",paste("nMDS_dist_",distance,"_",fichier,".Rdata",sep=""))
  
  save(Nsimul,Nplot,Nsp,
       A_Coord_nmds_Site,A_Coord_nmds_Site_Naive,
       A_Coord_nmds_Sp,A_Coord_nmds_Sp_Naive,
       A_Coord_nmds_Site_TEST,A_Coord_nmds_Site_Naive_TEST,
       list = c("Nsimul","Nplot","Nsp",
                "A_Coord_nmds_Site","A_Coord_nmds_Site_Naive",
                "A_Coord_nmds_Sp","A_Coord_nmds_Sp_Naive",
                "A_Coord_nmds_Site_TEST","A_Coord_nmds_Site_Naive_TEST"),
       file = saveData)
  
}  
