#------------------------------------------#
# fct qui dans chaque fichier de           #
# regroupement des ACP remplace par des NA #
# les resultats des simuls qui sont "bad"  #
# en fonction de Select_bad_simul.R        #
#------------------------------------------#

fct_select_resultACP<-function() {
  #-- chargement de la tables des simuls a passer en NA
  load(file.path("Outcome","out-regroupement","ACP","Table_bad_Simul_ACP.Rdata"))

#---------------------------le fichier ACP_Grp_1_2.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Grp_1_2.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_1_2.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
      ###
      M_Grp_1_2_Axe1_ACP_good<-as_tibble(M_Grp_1_2_Axe1_ACP)
      for (z in i){
        M_Grp_1_2_Axe1_ACP_good<-M_Grp_1_2_Axe1_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe1_ACP_good<-M_Grp_1_2_Axe1_ACP_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe1_ACP_good)<-colnames(M_Grp_1_2_Axe1_ACP)
      ###
      
      M_Grp_1_2_Axe1_ACP_Naive_good<-as_tibble(M_Grp_1_2_Axe1_ACP_Naive)
      for (z in i){
        M_Grp_1_2_Axe1_ACP_Naive_good<-M_Grp_1_2_Axe1_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe1_ACP_Naive_good<-M_Grp_1_2_Axe1_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe1_ACP_Naive_good)<-colnames(M_Grp_1_2_Axe1_ACP_Naive)
      ###
      
      M_Grp_1_2_Axe2_ACP_good<-as_tibble(M_Grp_1_2_Axe2_ACP)
      for (z in i){
        M_Grp_1_2_Axe2_ACP_good<-M_Grp_1_2_Axe2_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe2_ACP_good<-M_Grp_1_2_Axe2_ACP_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe2_ACP_good)<-colnames(M_Grp_1_2_Axe2_ACP)
      ###
      
      M_Grp_1_2_Axe2_ACP_Naive_good<-as_tibble(M_Grp_1_2_Axe2_ACP_Naive)
      for (z in i){
        M_Grp_1_2_Axe2_ACP_Naive_good<-M_Grp_1_2_Axe2_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_1_2_Axe2_ACP_Naive_good<-M_Grp_1_2_Axe2_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_1_2_Axe2_ACP_Naive_good)<-colnames(M_Grp_1_2_Axe2_ACP_Naive)
      
  } else
  {
    M_Grp_1_2_Axe1_ACP_good<-M_Grp_1_2_Axe1_ACP
    M_Grp_1_2_Axe1_ACP_Naive_good<-M_Grp_1_2_Axe1_ACP_Naive
    M_Grp_1_2_Axe2_ACP_good<-M_Grp_1_2_Axe2_ACP
    M_Grp_1_2_Axe2_ACP_Naive_good<-M_Grp_1_2_Axe2_ACP_Naive
  }
  
  #-- la sauvegarde
  saveData1<-file.path("Outcome","out-regroupement","ACP","ACP_Grp_1_2_good.Rdata")
  
  save(M_Grp_1_2_Axe1_ACP_good,M_Grp_1_2_Axe1_ACP_Naive_good,
       M_Grp_1_2_Axe2_ACP_good,M_Grp_1_2_Axe2_ACP_Naive_good,
       list = c("M_Grp_1_2_Axe1_ACP_good","M_Grp_1_2_Axe1_ACP_Naive_good",
                "M_Grp_1_2_Axe2_ACP_good","M_Grp_1_2_Axe2_ACP_Naive_good"),
       file = saveData1)
  
#---------------------------le fichier ACP_Grp_A_B.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Grp_A_B.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
    ###
      M_Grp_A_B_Axe1_ACP_good<-as_tibble(M_Grp_A_B_Axe1_ACP)
      for (z in i){
        M_Grp_A_B_Axe1_ACP_good<-M_Grp_A_B_Axe1_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe1_ACP_good<-M_Grp_A_B_Axe1_ACP_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe1_ACP_good)<-colnames(M_Grp_A_B_Axe1_ACP)
      ###
      
      M_Grp_A_B_Axe1_ACP_Naive_good<-as_tibble(M_Grp_A_B_Axe1_ACP_Naive)
      for (z in i){
        M_Grp_A_B_Axe1_ACP_Naive_good<-M_Grp_A_B_Axe1_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe1_ACP_Naive_good<-M_Grp_A_B_Axe1_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe1_ACP_Naive_good)<-colnames(M_Grp_A_B_Axe1_ACP_Naive)
      ###
      
      M_Grp_A_B_Axe2_ACP_good<-as_tibble(M_Grp_A_B_Axe2_ACP)
        for (z in i){
          M_Grp_A_B_Axe2_ACP_good<-M_Grp_A_B_Axe2_ACP_good%>%
            mutate_at(vars(all_of(starts_with(z))),~NA)
        }
      
      M_Grp_A_B_Axe2_ACP_good<-M_Grp_A_B_Axe2_ACP_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe2_ACP_good)<-colnames(M_Grp_A_B_Axe2_ACP)
      ###
      
      M_Grp_A_B_Axe2_ACP_Naive_good<-as_tibble(M_Grp_A_B_Axe2_ACP_Naive)
      for (z in i){
        M_Grp_A_B_Axe2_ACP_Naive_good<-M_Grp_A_B_Axe2_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_A_B_Axe2_ACP_Naive_good<-M_Grp_A_B_Axe2_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_A_B_Axe2_ACP_Naive_good)<-colnames(M_Grp_A_B_Axe2_ACP_Naive)
      
  } else
  {
    M_Grp_A_B_Axe1_ACP_good<-M_Grp_A_B_Axe1_ACP
    M_Grp_A_B_Axe1_ACP_Naive_good<-M_Grp_A_B_Axe1_ACP_Naive
    M_Grp_A_B_Axe2_ACP_good<-M_Grp_A_B_Axe2_ACP
    M_Grp_A_B_Axe2_ACP_Naive_good<-M_Grp_A_B_Axe2_ACP_Naive
  }
  
  #-- la sauvegarde
  saveData2<-file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B_good.Rdata")
  
  save(M_Grp_A_B_Axe1_ACP_good,M_Grp_A_B_Axe1_ACP_Naive_good,
       M_Grp_A_B_Axe2_ACP_good,M_Grp_A_B_Axe2_ACP_Naive_good,
       list = c("M_Grp_A_B_Axe1_ACP_good","M_Grp_A_B_Axe1_ACP_Naive_good",
                "M_Grp_A_B_Axe2_ACP_good","M_Grp_A_B_Axe2_ACP_Naive_good"),
       file = saveData2)
  
#---------------------------le fichier ACP_Grp_Detect.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Grp_Detect.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
    ###
      M_Grp_Detect_Axe1_ACP_good<-as_tibble(M_Grp_Detect_Axe1_ACP)
      for (z in i){
        M_Grp_Detect_Axe1_ACP_good<-M_Grp_Detect_Axe1_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_ACP_good<-M_Grp_Detect_Axe1_ACP_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_ACP_good)<-colnames(M_Grp_Detect_Axe1_ACP)
      ###
      
      M_Grp_Detect_Axe1_ACP_Naive_good<-as_tibble(M_Grp_Detect_Axe1_ACP_Naive)
      for (z in i){
        M_Grp_Detect_Axe1_ACP_Naive_good<-M_Grp_Detect_Axe1_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_ACP_Naive_good<-M_Grp_Detect_Axe1_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_ACP_Naive_good)<-colnames(M_Grp_Detect_Axe1_ACP_Naive)
      ###
      
      M_Grp_Detect_Axe2_ACP_good<-as_tibble(M_Grp_Detect_Axe2_ACP)
      for (z in i){
        M_Grp_Detect_Axe2_ACP_good<-M_Grp_Detect_Axe2_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_ACP_good<-M_Grp_Detect_Axe2_ACP_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_ACP_good)<-colnames(M_Grp_Detect_Axe2_ACP)
      ###
      
      M_Grp_Detect_Axe2_ACP_Naive_good<-as_tibble(M_Grp_Detect_Axe2_ACP_Naive)
      for (z in i){
        M_Grp_Detect_Axe2_ACP_Naive_good<-M_Grp_Detect_Axe2_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_ACP_Naive_good<-M_Grp_Detect_Axe2_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_ACP_Naive_good)<-colnames(M_Grp_Detect_Axe2_ACP_Naive)
      
  } else
  {
    M_Grp_Detect_Axe1_ACP_good<-M_Grp_Detect_Axe1_ACP
    M_Grp_Detect_Axe1_ACP_Naive_good<-M_Grp_Detect_Axe1_ACP_Naive
    M_Grp_Detect_Axe2_ACP_good<-M_Grp_Detect_Axe2_ACP
    M_Grp_Detect_Axe2_ACP_Naive_good<-M_Grp_Detect_Axe2_ACP_Naive
  }

  #-- la sauvegarde
  saveData3<-file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_good.Rdata")
  
  save(M_Grp_Detect_Axe1_ACP_good,M_Grp_Detect_Axe1_ACP_Naive_good,
       M_Grp_Detect_Axe2_ACP_good,M_Grp_Detect_Axe2_ACP_Naive_good,
       list = c("M_Grp_Detect_Axe1_ACP_good","M_Grp_Detect_Axe1_ACP_Naive_good",
                "M_Grp_Detect_Axe2_ACP_good","M_Grp_Detect_Axe2_ACP_Naive_good"),
       file = saveData3)
  
#---------------------------le fichier ACP_Grp_Detect_D5.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Grp_Detect_D5.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
    ###
      M_Grp_Detect_Axe1_ACP_good<-as_tibble(M_Grp_Detect_Axe1_ACP)
      for (z in i){
        M_Grp_Detect_Axe1_ACP_good<-M_Grp_Detect_Axe1_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_ACP_good<-M_Grp_Detect_Axe1_ACP_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_ACP_good)<-colnames(M_Grp_Detect_Axe1_ACP)
      ###
      
      M_Grp_Detect_Axe1_ACP_Naive_good<-as_tibble(M_Grp_Detect_Axe1_ACP_Naive)
      for (z in i){
        M_Grp_Detect_Axe1_ACP_Naive_good<-M_Grp_Detect_Axe1_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe1_ACP_Naive_good<-M_Grp_Detect_Axe1_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe1_ACP_Naive_good)<-colnames(M_Grp_Detect_Axe1_ACP_Naive)
      ###
      
      M_Grp_Detect_Axe2_ACP_good<-as_tibble(M_Grp_Detect_Axe2_ACP)
      for (z in i){
        M_Grp_Detect_Axe2_ACP_good<-M_Grp_Detect_Axe2_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_ACP_good<-M_Grp_Detect_Axe2_ACP_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_ACP_good)<-colnames(M_Grp_Detect_Axe2_ACP)
      ###
      
      M_Grp_Detect_Axe2_ACP_Naive_good<-as_tibble(M_Grp_Detect_Axe2_ACP_Naive)
      for (z in i){
        M_Grp_Detect_Axe2_ACP_Naive_good<-M_Grp_Detect_Axe2_ACP_Naive_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Grp_Detect_Axe2_ACP_Naive_good<-M_Grp_Detect_Axe2_ACP_Naive_good%>% as.matrix()
      colnames(M_Grp_Detect_Axe2_ACP_Naive_good)<-colnames(M_Grp_Detect_Axe2_ACP_Naive)
      
  } else
  {
    M_Grp_Detect_Axe1_ACP_good<-M_Grp_Detect_Axe1_ACP
    M_Grp_Detect_Axe1_ACP_Naive_good<-M_Grp_Detect_Axe1_ACP_Naive
    M_Grp_Detect_Axe2_ACP_good<-M_Grp_Detect_Axe2_ACP
    M_Grp_Detect_Axe2_ACP_Naive_good<-M_Grp_Detect_Axe2_ACP_Naive
  }
  
  #-- la sauvegarde
  saveData4<-file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5_good.Rdata")
  
  save(M_Grp_Detect_Axe1_ACP_good,M_Grp_Detect_Axe1_ACP_Naive_good,
       M_Grp_Detect_Axe2_ACP_good,M_Grp_Detect_Axe2_ACP_Naive_good,
       list = c("M_Grp_Detect_Axe1_ACP_good","M_Grp_Detect_Axe1_ACP_Naive_good",
                "M_Grp_Detect_Axe2_ACP_good","M_Grp_Detect_Axe2_ACP_Naive_good"),
       file = saveData4)

#---------------------------le fichier ACP_Inertia_Plan1.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Inertia_Plan1.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Inertia_Plan1.Rdata"))

  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
    ###
      M_Inertia_Plan1_ACP_good<-as_tibble(M_Inertia_Plan1_ACP)
      for (z in i){
        M_Inertia_Plan1_ACP_good<-M_Inertia_Plan1_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Inertia_Plan1_ACP_good<-M_Inertia_Plan1_ACP_good%>% as.matrix()
      colnames(M_Inertia_Plan1_ACP_good)<-colnames(M_Inertia_Plan1_ACP)
      
  } else 
  {
    M_Inertia_Plan1_ACP_good<-M_Inertia_Plan1_ACP
  }
  
  #-- la sauvegarde
  saveData5<-file.path("Outcome","out-regroupement","ACP","ACP_Inertia_Plan1_good.Rdata")
  
  save(M_Inertia_Plan1_ACP_good,
       list = c("M_Inertia_Plan1_ACP_good"),
       file = saveData5)

#---------------------------le fichier ACP_Regroup_RV_Site.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Regroup_RV_Site.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Site.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
    ###
      M_Resultat_RV_ACP_good<-as_tibble(M_Resultat_RV_ACP)
      for (z in i){
        M_Resultat_RV_ACP_good<-M_Resultat_RV_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Resultat_RV_ACP_good<-M_Resultat_RV_ACP_good%>% as.matrix()
      colnames(M_Resultat_RV_ACP_good)<-colnames(M_Resultat_RV_ACP)
      
  } else
  {
    M_Resultat_RV_ACP_good<-M_Resultat_RV_ACP
  }
  
  #-- la sauvegarde
  saveData6<-file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Site_good.Rdata")
  
  save(M_Resultat_RV_ACP_good,
       list = c("M_Resultat_RV_ACP_good"),
       file = saveData6)
  
  #---------------------------le fichier ACP_Regroup_RV_Sp.Rdata---------------------------------------------------#
  #-- chargement des fichiers de ACP_Regroup_RV_Sp.Rdata a modifier
  load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp.Rdata"))
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_PCA$Simul)!=0){
    i<-T_bad_simul_PCA%>%pull("Simul")
    ###
      M_Resultat_RV_ACP_good<-as_tibble(M_Resultat_RV_ACP)
      for (z in i){
        M_Resultat_RV_ACP_good<-M_Resultat_RV_ACP_good%>%
          mutate_at(vars(all_of(starts_with(z))),~NA)
      }
      
      M_Resultat_RV_ACP_good<-M_Resultat_RV_ACP_good%>% as.matrix()
      colnames(M_Resultat_RV_ACP_good)<-colnames(M_Resultat_RV_ACP)
      
  } else
  {
    M_Resultat_RV_ACP_good<-M_Resultat_RV_ACP
  }
  
  #-- la sauvegarde
  saveData7<-file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp_good.Rdata")
  
  save(M_Resultat_RV_ACP_good,
       list = c("M_Resultat_RV_ACP_good"),
       file = saveData7)
}