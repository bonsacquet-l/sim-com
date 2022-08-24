#------------------------------------------------#
# fct qui dans chaque fichier de                 #
# regroupement des nMDS_chao remplace par des NA #
# les resultats des simuls qui sont "bad"        #
# en fonction de Select_bad_simul.R              #
#------------------------------------------------#


fct_select_resultnMDS_chao<-function() {
  #-- chargement de la tables des simuls a passer en NA
  load(file.path("Outcome","out-regroupement","nMDS","Table_bad_Simul_nMDS_chao.Rdata"))
  
  #---------------------------le fichier nMDS_chao_Grp_1_2.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_chao_Grp_1_2.Rdata a modifier
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_1_2.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
    i<-T_bad_simul_nMDS_Chao%>%pull("Simul")
    M_Grp_1_2_Axe1_nmds_good<-tibble(M_Grp_1_2_Axe1_nmds)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
    colnames(M_Grp_1_2_Axe1_nmds_good)<-colnames(M_Grp_1_2_Axe1_nmds)
    
    M_Grp_1_2_Axe1_nmds_Naive_good<-tibble(M_Grp_1_2_Axe1_nmds_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
    colnames(M_Grp_1_2_Axe1_nmds_Naive_good)<-colnames(M_Grp_1_2_Axe1_nmds_Naive)
    
    M_Grp_1_2_Axe2_nmds_good<-tibble(M_Grp_1_2_Axe2_nmds)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
    colnames(M_Grp_1_2_Axe2_nmds_good)<-colnames(M_Grp_1_2_Axe2_nmds)
    
    M_Grp_1_2_Axe2_nmds_Naive_good<-tibble(M_Grp_1_2_Axe2_nmds_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
    colnames(M_Grp_1_2_Axe2_nmds_Naive_good)<-colnames(M_Grp_1_2_Axe2_nmds_Naive)
    
  } else
  {
    M_Grp_1_2_Axe1_nmds_good<-M_Grp_1_2_Axe1_nmds
    M_Grp_1_2_Axe1_nmds_Naive_good<-M_Grp_1_2_Axe1_nmds_Naive
    M_Grp_1_2_Axe2_nmds_good<-M_Grp_1_2_Axe2_nmds
    M_Grp_1_2_Axe2_nmds_Naive_good<-M_Grp_1_2_Axe2_nmds_Naive
  }
  
  #-- la sauvegarde
  saveData1<-file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_1_2_good.Rdata")
  
  save(M_Grp_1_2_Axe1_nmds_good,M_Grp_1_2_Axe1_nmds_Naive_good,
       M_Grp_1_2_Axe2_nmds_good,M_Grp_1_2_Axe2_nmds_Naive_good,
       list = c("M_Grp_1_2_Axe1_nmds_good","M_Grp_1_2_Axe1_nmds_Naive_good",
                "M_Grp_1_2_Axe2_nmds_good","M_Grp_1_2_Axe2_nmds_Naive_good"),
       file = saveData1)
  
  #---------------------------le fichier nMDS_chao_Grp_A_B.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_chao_Grp_A_B.Rdata a modifier
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_A_B.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
    i<-T_bad_simul_nMDS_Chao%>%pull("Simul")
    M_Grp_A_B_Axe1_nmds_good<-tibble(M_Grp_A_B_Axe1_nmds)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
    colnames(M_Grp_A_B_Axe1_nmds_good)<-colnames(M_Grp_A_B_Axe1_nmds)
    
    M_Grp_A_B_Axe1_nmds_Naive_good<-tibble(M_Grp_A_B_Axe1_nmds_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
    colnames(M_Grp_A_B_Axe1_nmds_Naive_good)<-colnames(M_Grp_A_B_Axe1_nmds_Naive)
    
    M_Grp_A_B_Axe2_nmds_good<-tibble(M_Grp_A_B_Axe2_nmds)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
    colnames(M_Grp_A_B_Axe2_nmds_good)<-colnames(M_Grp_A_B_Axe2_nmds)
    
    M_Grp_A_B_Axe2_nmds_Naive_good<-tibble(M_Grp_A_B_Axe2_nmds_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
    colnames(M_Grp_A_B_Axe2_nmds_Naive_good)<-colnames(M_Grp_A_B_Axe2_nmds_Naive)
    
  } else
  {
    M_Grp_A_B_Axe1_nmds_good<-M_Grp_A_B_Axe1_nmds
    M_Grp_A_B_Axe1_nmds_Naive_good<-M_Grp_A_B_Axe1_nmds_Naive
    M_Grp_A_B_Axe2_nmds_good<-M_Grp_A_B_Axe2_nmds
    M_Grp_A_B_Axe2_nmds_Naive_good<-M_Grp_A_B_Axe2_nmds_Naive
  }
  
  #-- la sauvegarde
  saveData2<-file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_A_B_good.Rdata")
  
  save(M_Grp_A_B_Axe1_nmds_good,M_Grp_A_B_Axe1_nmds_Naive_good,
       M_Grp_A_B_Axe2_nmds_good,M_Grp_A_B_Axe2_nmds_Naive_good,
       list = c("M_Grp_A_B_Axe1_nmds_good","M_Grp_A_B_Axe1_nmds_Naive_good",
                "M_Grp_A_B_Axe2_nmds_good","M_Grp_A_B_Axe2_nmds_Naive_good"),
       file = saveData2)
  
  #---------------------------le fichier nMDS_chao_Grp_Detect.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_chao_Grp_Detect.Rdata a modifier
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
    i<-T_bad_simul_nMDS_Chao%>%pull("Simul")
    M_Grp_Detect_Axe1_nmds_good<-tibble(M_Grp_Detect_Axe1_nmds)%>%
                                  mutate(across(all_of(starts_with(i)),~NA))%>%
                                  as.matrix()
    colnames(M_Grp_Detect_Axe1_nmds_good)<-colnames(M_Grp_Detect_Axe1_nmds)
    
    M_Grp_Detect_Axe1_nmds_Naive_good<-tibble(M_Grp_Detect_Axe1_nmds_Naive)%>%
                                        mutate(across(all_of(starts_with(i)),~NA))%>%
                                        as.matrix()
    colnames(M_Grp_Detect_Axe1_nmds_Naive_good)<-colnames(M_Grp_Detect_Axe1_nmds_Naive)
    
    M_Grp_Detect_Axe2_nmds_good<-tibble(M_Grp_Detect_Axe2_nmds)%>%
                                  mutate(across(all_of(starts_with(i)),~NA))%>%
                                  as.matrix()
    colnames(M_Grp_Detect_Axe2_nmds_good)<-colnames(M_Grp_Detect_Axe2_nmds)
    
    M_Grp_Detect_Axe2_nmds_Naive_good<-tibble(M_Grp_Detect_Axe2_nmds_Naive)%>%
                                        mutate(across(all_of(starts_with(i)),~NA))%>%
                                        as.matrix()
    colnames(M_Grp_Detect_Axe2_nmds_Naive_good)<-colnames(M_Grp_Detect_Axe2_nmds_Naive)
    
  } else
  {
    M_Grp_Detect_Axe1_nmds_good<-M_Grp_Detect_Axe1_nmds
    M_Grp_Detect_Axe1_nmds_Naive_good<-M_Grp_Detect_Axe1_nmds_Naive
    M_Grp_Detect_Axe2_nmds_good<-M_Grp_Detect_Axe2_nmds
    M_Grp_Detect_Axe2_nmds_Naive_good<-M_Grp_Detect_Axe2_nmds_Naive
  }
  
  #-- la sauvegarde
  saveData3<-file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_good.Rdata")
  
  save(M_Grp_Detect_Axe1_nmds_good,M_Grp_Detect_Axe1_nmds_Naive_good,
       M_Grp_Detect_Axe2_nmds_good,M_Grp_Detect_Axe2_nmds_Naive_good,
       list = c("M_Grp_Detect_Axe1_nmds_good","M_Grp_Detect_Axe1_nmds_Naive_good",
                "M_Grp_Detect_Axe2_nmds_good","M_Grp_Detect_Axe2_nmds_Naive_good"),
       file = saveData3)
  
  #---------------------------le fichier nMDS_chao_Grp_Detect_D5.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_chao_Grp_Detect_D5.Rdata a modifier
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_D5.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
    i<-T_bad_simul_nMDS_Chao%>%pull("Simul")
    M_Grp_Detect_Axe1_nmds_good<-tibble(M_Grp_Detect_Axe1_nmds)%>%
                                  mutate(across(all_of(starts_with(i)),~NA))%>%
                                  as.matrix()
    colnames(M_Grp_Detect_Axe1_nmds_good)<-colnames(M_Grp_Detect_Axe1_nmds)
    
    M_Grp_Detect_Axe1_nmds_Naive_good<-tibble(M_Grp_Detect_Axe1_nmds_Naive)%>%
                                        mutate(across(all_of(starts_with(i)),~NA))%>%
                                        as.matrix()
    colnames(M_Grp_Detect_Axe1_nmds_Naive_good)<-colnames(M_Grp_Detect_Axe1_nmds_Naive)
    
    M_Grp_Detect_Axe2_nmds_good<-tibble(M_Grp_Detect_Axe2_nmds)%>%
                                  mutate(across(all_of(starts_with(i)),~NA))%>%
                                  as.matrix()
    colnames(M_Grp_Detect_Axe2_nmds_good)<-colnames(M_Grp_Detect_Axe2_nmds)
    
    M_Grp_Detect_Axe2_nmds_Naive_good<-tibble(M_Grp_Detect_Axe2_nmds_Naive)%>%
                                        mutate(across(all_of(starts_with(i)),~NA))%>%
                                        as.matrix()
    colnames(M_Grp_Detect_Axe2_nmds_Naive_good)<-colnames(M_Grp_Detect_Axe2_nmds_Naive)
    
  } else
  {
    M_Grp_Detect_Axe1_nmds_good<-M_Grp_Detect_Axe1_nmds
    M_Grp_Detect_Axe1_nmds_Naive_good<-M_Grp_Detect_Axe1_nmds_Naive
    M_Grp_Detect_Axe2_nmds_good<-M_Grp_Detect_Axe2_nmds
    M_Grp_Detect_Axe2_nmds_Naive_good<-M_Grp_Detect_Axe2_nmds_Naive
  }
  
  #-- la sauvegarde
  saveData4<-file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_D5_good.Rdata")
  
  save(M_Grp_Detect_Axe1_nmds_good,M_Grp_Detect_Axe1_nmds_Naive_good,
       M_Grp_Detect_Axe2_nmds_good,M_Grp_Detect_Axe2_nmds_Naive_good,
       list = c("M_Grp_Detect_Axe1_nmds_good","M_Grp_Detect_Axe1_nmds_Naive_good",
                "M_Grp_Detect_Axe2_nmds_good","M_Grp_Detect_Axe2_nmds_Naive_good"),
       file = saveData4)
  
  #---------------------------le fichier nMDS_chao_Inertia_Plan1.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_chao_Inertia_Plan1.Rdata a modifier
  #  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Inertia_Plan1.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  #  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
  #    for ( i in T_bad_simul_nMDS_Chao$Simul){
  #      M_Inertia_Plan1_nmds_good<-M_Inertia_Plan1_nmds%>%mutate_at(vars(all_of(starts_with(i))),
  #                                                                .=NA)
  #    }
  #  } else 
  #  {
  #    M_Inertia_Plan1_nmds_good<-M_Inertia_Plan1_nmds
  #  }
  
  #  #-- la sauvegarde
  #  saveData5<-file.path("Outcome","out-regroupement","nMDS","nMDS_Inertia_Plan1_good.Rdata")
  
  #  save(M_Inertia_Plan1_nmds_good,
  #       list = c("M_Inertia_Plan1_nmds_good"),
  #       file = saveData5)
  
  #---------------------------le fichier nMDS_chao_Regroup_RV_Site.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_Regroup_RV_Site_chao.Rdata a modifier
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Site_chao.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
    i<-T_bad_simul_nMDS_Chao%>%pull("Simul")
    M_Resultat_RV_nmds_good<-tibble(M_Resultat_RV_nmds)%>%
                              mutate(across(all_of(starts_with(i)),~NA))%>%
                              as.matrix()
    colnames(M_Resultat_RV_nmds_good)<-colnames(M_Resultat_RV_nmds)
    
  } else
  {
    M_Resultat_RV_nmds_good<-M_Resultat_RV_nmds
  }
  
  #-- la sauvegarde
  saveData6<-file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Site_chao_good.Rdata")
  
  save(M_Resultat_RV_nmds_good,
       list = c("M_Resultat_RV_nmds_good"),
       file = saveData6)
  
  #---------------------------le fichier nMDS_Regroup_RV_Sp.Rdata---------------------------------------------------#
  #-- chargement des fichiers de nMDS_Regroup_RV_Sp_chao.Rdata a modifier
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao.Rdata"))
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_nMDS_Chao$Simul)!=0){
    i<-T_bad_simul_nMDS_Chao%>%pull("Simul")
    M_Resultat_RV_nmds_good<-tibble(M_Resultat_RV_nmds)%>%
                              mutate(across(all_of(starts_with(i)),~NA))%>%
                              as.matrix()
    colnames(M_Resultat_RV_nmds_good)<-colnames(M_Resultat_RV_nmds)
    
  } else
  {
    M_Resultat_RV_nmds_good<-M_Resultat_RV_nmds
  }
  
  #-- la sauvegarde
  saveData7<-file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata")
  
  save(M_Resultat_RV_nmds_good,
       list = c("M_Resultat_RV_nmds_good"),
       file = saveData7)
}