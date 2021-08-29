#-----------------------------------------#
# calacul PCoA focalise sur la matrice    #
# de distance entre les sites. recup des  #
# coord des sites et especes (wascores)   #
# avec distance de chao ou de bray curtis #
#-----------------------------------------#

library(vegan)

#-- une fonction pour appliquer cette analyse au donnees simuler
fct_pcoa_site<-function(analyse="ACP", # sur simul faite pour ACP
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
  A_Coord_pcoa_Site<-array(data = NA, dim = c(Nplot,2,Nsimul))
  A_Coord_pcoa_Site_Naive<-array(data = NA, dim = c(Nplot,2,Nsimul))
  
  A_Coord_pcoa_Sp<-array(data = NA, dim = c(Nsp,2,Nsimul))
  A_Coord_pcoa_Sp_Naive<-array(data = NA, dim = c(Nsp,2,Nsimul))
  
  #-- une boucle pour recuperer les resultats des 1000 simulations du fichier
  for (zz in 1:Nsimul) {
    #-- initialisation de la boucle
    M_DistSite<-NULL
    M_DistSite_Naive<-NULL
    
    pcoa<-NULL
    pcoa_Naive<-NULL
    
    #-- calculs des transformation des abondances (pas besoin pour chao car calcul de 2005 sur abondance)
    M_MemAbonSqrt<-sqrt(A_MemAbon[,,zz])
    M_MemAbonSqrt_Naive<-sqrt(A_MemAbon_Naive[,,zz])
    
    #-- calcul des distances a partir des abondances ou abondances transformees
    if(distance="chao"){
      try(M_DistSite<-vegdist(A_MemAbon[,,zz] ,method = "chao"))  # distance de jaccard modifier par chao pour les unseen species
      try(M_DistSite_Naive<-vegdist(A_MemAbon_Naive[,,zz] ,method = "chao"))
    }
    
    if(distance="bray"){
      try(M_DistSite<-vegdist(M_MemAbonSqrt, method = "bray"))  # distance de bray-curtis
      try(M_DistSite_Naive<-vegdist(M_MemAbonSqrt_Naive, method = "bray")) 
    }
    
    
    # pas besoin de faire le passage en distance euclidienne car commande integree dans cmdscal
    #M_DistSite_bray<-quasieuclid(M_DistSite_bray)  # passage en matrice euclidienne car distance de Bray curtis non euclidienne (mais ça racine carré oui)
    #M_DistSite_Naive_bray<-quasieuclid(M_DistSite_Naive_bray)
    
    #-- le PCoA sur les matrices de distance avec cmdscale de vegan
    if (max(M_DistSite)!=0 & max(M_DistSite_Naive)!=0){            # si que des distances nulle, PCoA impossible
      try(pcoa<-cmdscale(d=M_DistSite,k=2,add = TRUE))
      try(pcoa_Naive<-cmdscale(d=M_DistSite_Naive,k=2,add = TRUE))}
    
    #-- recup des coord des pts sites dans le premier plan
    if (is.null(Pcoa)==FALSE){
      A_Coord_pcoa_Site[,,zz]<-pcoa$points[1:Nplot,1:2]
      A_Coord_pcoa_Site_Naive[,,zz]<-pcoa_Naive$points[1:Nplot,1:2]
    } else {
      A_Coord_pcoa_Site[,,zz]<-NA
      A_Coord_pcoa_Site_Naive[,,zz]<-NA
    }
    
    #-- recup des coord des sp avec wascore de vegan
    if (distance="chao"){
      if (is.null(Pcoa)==FALSE){
        A_Coord_pcoa_Sp[,,zz]<-wascores(pcoa$points[1:Nplot,1:2],A_MemAbon[,,zz])
        A_Coord_pcoa_Sp_Naive[,,zz]<-wascores(pcoa_Naive$points[1:Nplot,1:2],A_MemAbon_Naive[,,zz])
      } else {
        A_Coord_pcoa_Sp[,,zz]<-NA
        A_Coord_pcoa_Sp_Naive[,,zz]<-NA
      }
    }
    
    if (distance="bray"){
      if (is.null(pcoa)==FALSE){
        A_Coord_pcoa_Sp[,,zz]<-wascores(pcoa$points[1:Nplot,1:2],M_MemAbonSqrt)
        A_Coord_pcoa_Sp_Naive[,,zz]<-wascores(pcoa_Naive$points[1:Nplot,1:2],M_MemAbonSqrt_Naive)
      } else {
        A_Coord_pcoa_Sp[,,zz]<-NA
        A_Coord_pcoa_Sp_Naive[,,zz]<-NA
      }
    }
  }
  
  #-- les sauvegardes
  #-- les chemins de sauvegardes
  saveData<-file.path("Outcome","out-simul","PCoA",paste("PCoA_dist_",distance,"_",fichier,".Rdata",sep=""))
  
  save(pcoa,pcoa_Naive,
       A_Coord_pcoa_Site,A_Coord_pcoa_Site_Naive,
       A_Coord_pcoa_Sp,A_Coord_pcoa_Sp_Naive,
       list = c("pcoa","pcoa_Naive",
                "A_Coord_pcoa_Site","A_Coord_pcoa_Site_Naive",
                "A_Coord_pcoa_Sp","A_Coord_pcoa_Sp_Naive"),
       file = saveData)
  
}
