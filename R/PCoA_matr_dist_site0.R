#-----------------------------------------#
# calacul PCoA focalise sur les sites     #
# matrice de distance entre les sites     # 
# avec distance de chao et de bray curtis #
#-----------------------------------------#

library(vegan)

#-- une fonction pour appliquer cette analyse au donnees simuler
fct_pcoa_site<-function(analyse="ACP",fichier="D4_C3_05") {   # sur ACP de base et sur D4_C3_05 pour test
  
  #-- appel des donnees de simulations
  load(file.path("Outcome","out-simul",analyse,paste(analyse,"_Simul_",fichier,".Rdata",sep="")))
  
  #-- passage des warning en error
  options(warn = 2)
  
  #-- recup des infos
  Nsimul<-dim(A_MemAbon)[3]  #A_MemAbon Array des 1000 simulations (Nsimul) d'abondance de Nsp (espece) sur Nplot (site)
  Nplot<-dim(A_MemAbon)[1]   #A_MemAbon Array des abondance informees
  Nsp<-dim(A_MemAbon)[2]
  
  #-- creation des array pour sauvegarder les infos
  A_Coord_pcoa_chao_Site<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_pcoa_chao_Site_Naive<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  A_Coord_pcoa_chao_Sp<-array(data = NA, dim = c(Nsp,2,Nsimul))
  A_Coord_pcoa_chao_Sp_Naive<-array(data = NA, dim = c(Nsp,2,Nsimul))
  
  A_Coord_pcoa_bray_Site<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_pcoa_bray_Site_Naive<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  A_Coord_pcoa_bray_Sp<-array(data = NA, dim = c(Nsp,2,Nsimul))
  A_Coord_pcoa_bray_Sp_Naive<-array(data = NA, dim = c(Nsp,2,Nsimul))
  
  #-- une boucle pour recuperer les resultats des 1000 simulations du fichier
  for (zz in 1:Nsimul) {
    #-- initialisation de la boucle
    M_DistSite_chao<-NULL
    M_DistSite_Naive_chao<-NULL
    M_DistSite_bray<-NULL
    M_DistSite_Naive_bray<-NULL
    
    Pcoa_chao<-NULL
    Pcoa_chao_Naive<-NULL
    Pcoa_bray<-NULL
    Pcoa_bray_Naive<-NULL
    
    #-- calculs des transformation des abondances (pas besoin pour chao car calcul de 2005 sur abondance)
    M_MemAbonSqrt<-sqrt(A_MemAbon[,,zz])
    M_MemAbonSqrt_Naive<-sqrt(A_MemAbon_Naive[,,zz])
    
    #-- calcul des distances a partir des abondances ou abondances transformees
    try(M_DistSite_chao<-vegdist(A_MemAbon[,,zz] ,method = "chao"))  # distance de jaccard modifier par chao pour les unseen species
    try(M_DistSite_Naive_chao<-vegdist(A_MemAbon_Naive[,,zz] ,method = "chao"))
    
    try(M_DistSite_bray<-vegdist(M_MemAbonSqrt, method = "bray"))  # distance de bray-curtis
    try(M_DistSite_Naive_bray<-vegdist(M_MemAbonSqrt_Naive, method = "bray")) 
    
    # pas besoin de faire le passage en distance euclidienne car commande integree dans cmdscal
    #M_DistSite_bray<-quasieuclid(M_DistSite_bray)  # passage en matrice euclidienne car distance de Bray curtis non euclidienne (mais ça racine carré oui)
    #M_DistSite_Naive_bray<-quasieuclid(M_DistSite_Naive_bray)
    
    #-- le PCoA sur les matrices de distance avec cmdscale de vegan
    if (max(M_DistSite_chao)!=0 & max(M_DistSite_Naive_chao)!=0){            # si que des distances nulle, PCoA impossible
      try(Pcoa_chao<-cmdscale(d=M_DistSite_chao,k=2,add = TRUE))
      try(Pcoa_chao_Naive<-cmdscale(d=M_DistSite_Naive_chao,k=2,add = TRUE))}
    
    if (max(M_DistSite_bray)!=0 & max(M_DistSite_Naive_bray)!=0){
      try(Pcoa_bray<-cmdscale(d=M_DistSite_bray,k=2,add = TRUE))
      try(Pcoa_bray_Naive<-cmdscale(d=M_DistSite_Naive_bray,k=2,add = TRUE))}
    
    #-- recup des coord des pts sites dans le premier plan
    if (is.null(Pcoa_chao)==FALSE){
    A_Coord_pcoa_chao_Site[,,zz]<-Pcoa_chao$points[1:Nplot,1:2]
    A_Coord_pcoa_chao_Site_Naive[,,zz]<-Pcoa_chao_Naive$points[1:Nplot,1:2]
    } else {
      A_Coord_pcoa_chao_Site[,,zz]<-NA
      A_Coord_pcoa_chao_Site_Naive[,,zz]<-NA
    }
    
    if (is.null(Pcoa_bray)==FALSE){
    A_Coord_pcoa_bray_Site[,,zz]<-Pcoa_bray$points[1:Nplot,1:2]
    A_Coord_pcoa_bray_Site_Naive[,,zz]<-Pcoa_bray_Naive$points[1:Nplot,1:2]
    } else {
      A_Coord_pcoa_bray_Site[,,zz]<-NA
      A_Coord_pcoa_bray_Site_Naive[,,zz]<-NA
    }
    
    #-- recup des coord des sp avec wascore de vegan
    if (is.null(Pcoa_chao)==FALSE){
      A_Coord_pcoa_chao_Sp[,,zz]<-wascores(Pcoa_chao$points[1:Nplot,1:2],A_MemAbon[,,zz])
      A_Coord_pcoa_chao_Sp_Naive[,,zz]<-wascores(Pcoa_chao_Naive$points[1:Nplot,1:2],A_MemAbon_Naive[,,zz])
    } else {
      A_Coord_pcoa_chao_Sp[,,zz]<-NA
      A_Coord_pcoa_chao_Sp_Naive[,,zz]<-NA
    }
    
    if (is.null(Pcoa_bray)==FALSE){
    A_Coord_pcoa_bray_Sp[,,zz]<-wascores(Pcoa_bray$points[1:Nplot,1:2],M_MemAbonSqrt)
    A_Coord_pcoa_bray_Sp_Naive[,,zz]<-wascores(Pcoa_bray_Naive$points[1:Nplot,1:2],M_MemAbonSqrt_Naive)
    } else {
      A_Coord_pcoa_bray_Sp[,,zz]<-NA
      A_Coord_pcoa_bray_Sp_Naive[,,zz]<-NA
    }
  }
  
  #-- les sauvegardes
  #-- les chemins de sauvegardes
  saveData<-file.path("Outcome","out-simul",PCoA,paste("PCoA_matr_dist_site_sp_",fichier,".Rdata",sep=""))
  
  save(A_Coord_pcoa_chao_Site,A_Coord_pcoa_chao_Site_Naive,
       A_Coord_pcoa_chao_Sp,A_Coord_pcoa_chao_Sp_Naive,
       A_Coord_pcoa_bray_Site,A_Coord_pcoa_bray_Site_Naive,
       A_Coord_pcoa_bray_Sp,A_Coord_pcoa_bray_Sp_Naive,
       list = c("A_Coord_pcoa_chao_Site","A_Coord_pcoa_chao_Site_Naive",
                "A_Coord_pcoa_chao_Sp","A_Coord_pcoa_chao_Sp_Naive",
                "A_Coord_pcoa_bray_Site","A_Coord_pcoa_bray_Site_Naive",
                "A_Coord_pcoa_bray_Sp","A_Coord_pcoa_bray_Sp_Naive"),
       file = saveData)
  
}
