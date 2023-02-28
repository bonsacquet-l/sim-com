#------------------------------------------------#
# fct qui dans chaque fichier de                 #
# regroupement des PCoA_bray remplace par des NA #
# les resultats des simuls qui sont "bad"        #
# en fonction de Select_bad_simul.R              #
#------------------------------------------------#

fct_select_resultPCoA_bray<-function() {
  #-- chargement de la tables des simuls a passer en NA
  load(file.path("Outcome","out-regroupement","PCoA","Table_bad_Simul_PCoA_bray.Rdata"))
  
  #---------------------------le fichier PCoA_bray_Grp_1_2.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_bray_Grp_1_2.Rdata a modifier
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_1_2.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
    i<-T_bad_simul_PCoA_Bray%>%pull("Simul")
    ###
      M_Grp_1_2_Axe1_pcoa_good<-as_tibble(M_Grp_1_2_Axe1_pcoa)
      for (z in i){
        M_Grp_1_2_Axe1_pcoa_good<-M_Grp_1_2_Axe1_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe1_pcoa_good<-M_Grp_1_2_Axe1_pcoa_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe1_pcoa_good)<-colnames(M_Grp_1_2_Axe1_pcoa)
      ###
      
      M_Grp_1_2_Axe1_pcoa_Naive_good<-as_tibble(M_Grp_1_2_Axe1_pcoa_Naive)
      for (z in i){
        M_Grp_1_2_Axe1_pcoa_Naive_good<-M_Grp_1_2_Axe1_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe1_pcoa_Naive_good<-M_Grp_1_2_Axe1_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe1_pcoa_Naive_good)<-colnames(M_Grp_1_2_Axe1_pcoa_Naive)
      ###
      
      M_Grp_1_2_Axe2_pcoa_good<-as_tibble(M_Grp_1_2_Axe2_pcoa)
      for (z in i){
        M_Grp_1_2_Axe2_pcoa_good<-M_Grp_1_2_Axe2_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe2_pcoa_good<-M_Grp_1_2_Axe2_pcoa_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe2_pcoa_good)<-colnames(M_Grp_1_2_Axe2_pcoa)
      ###
      
      M_Grp_1_2_Axe2_pcoa_Naive_good<-as_tibble(M_Grp_1_2_Axe2_pcoa_Naive)
      for (z in i){
        M_Grp_1_2_Axe2_pcoa_Naive_good<-M_Grp_1_2_Axe2_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe2_pcoa_Naive_good<-M_Grp_1_2_Axe2_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe2_pcoa_Naive_good)<-colnames(M_Grp_1_2_Axe2_pcoa_Naive)
      
  } else
  {
    M_Grp_1_2_Axe1_pcoa_good<-M_Grp_1_2_Axe1_pcoa
    M_Grp_1_2_Axe1_pcoa_Naive_good<-M_Grp_1_2_Axe1_pcoa_Naive
    M_Grp_1_2_Axe2_pcoa_good<-M_Grp_1_2_Axe2_pcoa
    M_Grp_1_2_Axe2_pcoa_Naive_good<-M_Grp_1_2_Axe2_pcoa_Naive
  }
  
  #-- la sauvegarde
  saveData1<-file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_1_2_good.Rdata")
  
  save(M_Grp_1_2_Axe1_pcoa_good,M_Grp_1_2_Axe1_pcoa_Naive_good,
       M_Grp_1_2_Axe2_pcoa_good,M_Grp_1_2_Axe2_pcoa_Naive_good,
       list = c("M_Grp_1_2_Axe1_pcoa_good","M_Grp_1_2_Axe1_pcoa_Naive_good",
                "M_Grp_1_2_Axe2_pcoa_good","M_Grp_1_2_Axe2_pcoa_Naive_good"),
       file = saveData1)
  
  #---------------------------le fichier PCoA_bray_Grp_A_B.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_bray_Grp_A_B.Rdata a modifier
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_A_B.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
    i<-T_bad_simul_PCoA_Bray%>%pull("Simul")
    ###
      M_Grp_A_B_Axe1_pcoa_good<-as_tibble(M_Grp_A_B_Axe1_pcoa)
      for (z in i){
        M_Grp_A_B_Axe1_pcoa_good<-M_Grp_A_B_Axe1_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe1_pcoa_good<-M_Grp_A_B_Axe1_pcoa_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe1_pcoa_good)<-colnames(M_Grp_A_B_Axe1_pcoa)
      ###
      
      M_Grp_A_B_Axe1_pcoa_Naive_good<-as_tibble(M_Grp_A_B_Axe1_pcoa_Naive)
      for (z in i){
        M_Grp_A_B_Axe1_pcoa_Naive_good<-M_Grp_A_B_Axe1_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe1_pcoa_Naive_good<-M_Grp_A_B_Axe1_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe1_pcoa_Naive_good)<-colnames(M_Grp_A_B_Axe1_pcoa_Naive)
      ###
      
      M_Grp_A_B_Axe2_pcoa_good<-as_tibble(M_Grp_A_B_Axe2_pcoa)
      for (z in i){
        M_Grp_A_B_Axe2_pcoa_good<-M_Grp_A_B_Axe2_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe2_pcoa_good<-M_Grp_A_B_Axe2_pcoa_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe2_pcoa_good)<-colnames(M_Grp_A_B_Axe2_pcoa)
      ###
      
      M_Grp_A_B_Axe2_pcoa_Naive_good<-as_tibble(M_Grp_A_B_Axe2_pcoa_Naive)
      for (z in i){
        M_Grp_A_B_Axe2_pcoa_Naive_good<-M_Grp_A_B_Axe2_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe2_pcoa_Naive_good<-M_Grp_A_B_Axe2_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe2_pcoa_Naive_good)<-colnames(M_Grp_A_B_Axe2_pcoa_Naive)
      
  } else
  {
    M_Grp_A_B_Axe1_pcoa_good<-M_Grp_A_B_Axe1_pcoa
    M_Grp_A_B_Axe1_pcoa_Naive_good<-M_Grp_A_B_Axe1_pcoa_Naive
    M_Grp_A_B_Axe2_pcoa_good<-M_Grp_A_B_Axe2_pcoa
    M_Grp_A_B_Axe2_pcoa_Naive_good<-M_Grp_A_B_Axe2_pcoa_Naive
  }
  
  #-- la sauvegarde
  saveData2<-file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_A_B_good.Rdata")
  
  save(M_Grp_A_B_Axe1_pcoa_good,M_Grp_A_B_Axe1_pcoa_Naive_good,
       M_Grp_A_B_Axe2_pcoa_good,M_Grp_A_B_Axe2_pcoa_Naive_good,
       list = c("M_Grp_A_B_Axe1_pcoa_good","M_Grp_A_B_Axe1_pcoa_Naive_good",
                "M_Grp_A_B_Axe2_pcoa_good","M_Grp_A_B_Axe2_pcoa_Naive_good"),
       file = saveData2)
  
  #---------------------------le fichier PCoA_bray_Grp_Detect.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_bray_Grp_Detect.Rdata a modifier
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
    i<-T_bad_simul_PCoA_Bray%>%pull("Simul")
    ###
      M_Grp_Detect_Axe1_pcoa_good<-as_tibble(M_Grp_Detect_Axe1_pcoa)
      for (z in i){
        M_Grp_Detect_Axe1_pcoa_good<-M_Grp_Detect_Axe1_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_pcoa_good<-M_Grp_Detect_Axe1_pcoa_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_pcoa_good)<-colnames(M_Grp_Detect_Axe1_pcoa)
      ###
      
      M_Grp_Detect_Axe1_pcoa_Naive_good<-as_tibble(M_Grp_Detect_Axe1_pcoa_Naive)
      for (z in i){
        M_Grp_Detect_Axe1_pcoa_Naive_good<-M_Grp_Detect_Axe1_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_pcoa_Naive_good<-M_Grp_Detect_Axe1_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_pcoa_Naive_good)<-colnames(M_Grp_Detect_Axe1_pcoa_Naive)
      ###
      
      M_Grp_Detect_Axe2_pcoa_good<-as_tibble(M_Grp_Detect_Axe2_pcoa)
      for (z in i){
        M_Grp_Detect_Axe2_pcoa_good<-M_Grp_Detect_Axe2_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_pcoa_good<-M_Grp_Detect_Axe2_pcoa_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_pcoa_good)<-colnames(M_Grp_Detect_Axe2_pcoa)
      ###
      
      M_Grp_Detect_Axe2_pcoa_Naive_good<-as_tibble(M_Grp_Detect_Axe2_pcoa_Naive)
      for (z in i){
        M_Grp_Detect_Axe2_pcoa_Naive_good<-M_Grp_Detect_Axe2_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_pcoa_Naive_good<-M_Grp_Detect_Axe2_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_pcoa_Naive_good)<-colnames(M_Grp_Detect_Axe2_pcoa_Naive)
      
  } else
  {
    M_Grp_Detect_Axe1_pcoa_good<-M_Grp_Detect_Axe1_pcoa
    M_Grp_Detect_Axe1_pcoa_Naive_good<-M_Grp_Detect_Axe1_pcoa_Naive
    M_Grp_Detect_Axe2_pcoa_good<-M_Grp_Detect_Axe2_pcoa
    M_Grp_Detect_Axe2_pcoa_Naive_good<-M_Grp_Detect_Axe2_pcoa_Naive
  }
  
  #-- la sauvegarde
  saveData3<-file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_good.Rdata")
  
  save(M_Grp_Detect_Axe1_pcoa_good,M_Grp_Detect_Axe1_pcoa_Naive_good,
       M_Grp_Detect_Axe2_pcoa_good,M_Grp_Detect_Axe2_pcoa_Naive_good,
       list = c("M_Grp_Detect_Axe1_pcoa_good","M_Grp_Detect_Axe1_pcoa_Naive_good",
                "M_Grp_Detect_Axe2_pcoa_good","M_Grp_Detect_Axe2_pcoa_Naive_good"),
       file = saveData3)
  
  #---------------------------le fichier PCoA_bray_Grp_Detect_D5.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_bray_Grp_Detect_D5.Rdata a modifier
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_D5.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
    i<-T_bad_simul_PCoA_Bray%>%pull("Simul")
    ###
      M_Grp_Detect_Axe1_pcoa_good<-as_tibble(M_Grp_Detect_Axe1_pcoa)
      for (z in i){
        M_Grp_Detect_Axe1_pcoa_good<-M_Grp_Detect_Axe1_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_pcoa_good<-M_Grp_Detect_Axe1_pcoa_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_pcoa_good)<-colnames(M_Grp_Detect_Axe1_pcoa)
      ###
      
      M_Grp_Detect_Axe1_pcoa_Naive_good<-as_tibble(M_Grp_Detect_Axe1_pcoa_Naive)
      for (z in i){
        M_Grp_Detect_Axe1_pcoa_Naive_good<-M_Grp_Detect_Axe1_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_pcoa_Naive_good<-M_Grp_Detect_Axe1_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_pcoa_Naive_good)<-colnames(M_Grp_Detect_Axe1_pcoa_Naive)
      ###
      
      M_Grp_Detect_Axe2_pcoa_good<-as_tibble(M_Grp_Detect_Axe2_pcoa)
      for (z in i){
        M_Grp_Detect_Axe2_pcoa_good<-M_Grp_Detect_Axe2_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_pcoa_good<-M_Grp_Detect_Axe2_pcoa_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_pcoa_good)<-colnames(M_Grp_Detect_Axe2_pcoa)
      ###
      
      M_Grp_Detect_Axe2_pcoa_Naive_good<-as_tibble(M_Grp_Detect_Axe2_pcoa_Naive)
      for (z in i){
        M_Grp_Detect_Axe2_pcoa_Naive_good<-M_Grp_Detect_Axe2_pcoa_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_pcoa_Naive_good<-M_Grp_Detect_Axe2_pcoa_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_pcoa_Naive_good)<-colnames(M_Grp_Detect_Axe2_pcoa_Naive)
      
  } else
  {
    M_Grp_Detect_Axe1_pcoa_good<-M_Grp_Detect_Axe1_pcoa
    M_Grp_Detect_Axe1_pcoa_Naive_good<-M_Grp_Detect_Axe1_pcoa_Naive
    M_Grp_Detect_Axe2_pcoa_good<-M_Grp_Detect_Axe2_pcoa
    M_Grp_Detect_Axe2_pcoa_Naive_good<-M_Grp_Detect_Axe2_pcoa_Naive
  }
  
  #-- la sauvegarde
  saveData4<-file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_D5_good.Rdata")
  
  save(M_Grp_Detect_Axe1_pcoa_good,M_Grp_Detect_Axe1_pcoa_Naive_good,
       M_Grp_Detect_Axe2_pcoa_good,M_Grp_Detect_Axe2_pcoa_Naive_good,
       list = c("M_Grp_Detect_Axe1_pcoa_good","M_Grp_Detect_Axe1_pcoa_Naive_good",
                "M_Grp_Detect_Axe2_pcoa_good","M_Grp_Detect_Axe2_pcoa_Naive_good"),
       file = saveData4)
  
  #---------------------------le fichier PCoA_bray_Inertia_Plan1.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_bray_Inertia_Plan1.Rdata a modifier
#  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Inertia_Plan1.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
#  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
#    for ( i in T_bad_simul_PCoA_Bray$Simul){
#      M_Inertia_Plan1_pcoa_good<-M_Inertia_Plan1_pcoa%>%mutate_at(vars(all_of(starts_with(i))),
#                                                                .=NA)
#    }
#  } else 
#  {
#    M_Inertia_Plan1_pcoa_good<-M_Inertia_Plan1_pcoa
#  }
  
#  #-- la sauvegarde
#  saveData5<-file.path("Outcome","out-regroupement","PCoA","PCoA_Inertia_Plan1_good.Rdata")
  
#  save(M_Inertia_Plan1_pcoa_good,
#       list = c("M_Inertia_Plan1_pcoa_good"),
#       file = saveData5)
  
  #---------------------------le fichier PCoA_bray_Regroup_RV_Site.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_Regroup_RV_Site_bray.Rdata a modifier
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Site_bray.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
    i<-T_bad_simul_PCoA_Bray%>%pull("Simul")
    ###
      M_Resultat_RV_pcoa_good<-as_tibble(M_Resultat_RV_pcoa)
      for (z in i){
        M_Resultat_RV_pcoa_good<-M_Resultat_RV_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Resultat_RV_pcoa_good<-M_Resultat_RV_pcoa_good%>% as.matrix()
      colnames(M_Resultat_RV_pcoa_good)<-colnames(M_Resultat_RV_pcoa)
      
  } else
  {
    M_Resultat_RV_pcoa_good<-M_Resultat_RV_pcoa
  }
  
  #-- la sauvegarde
  saveData6<-file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Site_bray_good.Rdata")
  
  save(M_Resultat_RV_pcoa_good,
       list = c("M_Resultat_RV_pcoa_good"),
       file = saveData6)
  
  #---------------------------le fichier PCoA_Regroup_RV_Sp.Rdata---------------------------------------------------#
  #-- chargement des fichiers de PCoA_Regroup_RV_Sp_bray.Rdata a modifier
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray.Rdata"))
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCoA_Bray$Simul)!=0){
    i<-T_bad_simul_PCoA_Bray%>%pull("Simul")
    ###
      M_Resultat_RV_pcoa_good<-as_tibble(M_Resultat_RV_pcoa)
      for (z in i){
        M_Resultat_RV_pcoa_good<-M_Resultat_RV_pcoa_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Resultat_RV_pcoa_good<-M_Resultat_RV_pcoa_good%>% as.matrix()
      colnames(M_Resultat_RV_pcoa_good)<-colnames(M_Resultat_RV_pcoa)
      
  } else
  {
    M_Resultat_RV_pcoa_good<-M_Resultat_RV_pcoa
  }
  
  #-- la sauvegarde
  saveData7<-file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata")
  
  save(M_Resultat_RV_pcoa_good,
       list = c("M_Resultat_RV_pcoa_good"),
       file = saveData7)
}