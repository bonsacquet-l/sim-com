#------------------------------------------#
# fct qui dans chaque fichier de           #
# regroupement des AFC remplace par des NA #
# les resultats des simuls qui sont "bad"  #
# en fonction de Select_bad_simul.R        #
#------------------------------------------#

fct_select_resultAFC<-function() {
  #-- chargement de la tables des simuls a passer en NA
  load(file.path("Outcome","out-regroupement","AFC","Table_bad_Simul_AFC.Rdata"))
  
  #---------------------------le fichier AFC_Grp_1_2.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Grp_1_2.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_1_2.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Grp_1_2_Axe1_AFC_good<-tibble(M_Grp_1_2_Axe1_AFC)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
      colnames(M_Grp_1_2_Axe1_AFC_good)<-colnames(M_Grp_1_2_Axe1_AFC)
      
      M_Grp_1_2_Axe1_AFC_Naive_good<-tibble(M_Grp_1_2_Axe1_AFC_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
      colnames(M_Grp_1_2_Axe1_AFC_Naive_good)<-colnames(M_Grp_1_2_Axe1_AFC_Naive)
      
      M_Grp_1_2_Axe2_AFC_good<-tibble(M_Grp_1_2_Axe2_AFC)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
      colnames(M_Grp_1_2_Axe2_AFC_good)<-colnames(M_Grp_1_2_Axe2_AFC)
      
      M_Grp_1_2_Axe2_AFC_Naive_good<-tibble(M_Grp_1_2_Axe2_AFC_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
      colnames(M_Grp_1_2_Axe2_AFC_Naive_good)<-colnames(M_Grp_1_2_Axe2_AFC_Naive)
      
  } else
  {
    M_Grp_1_2_Axe1_AFC_good<-M_Grp_1_2_Axe1_AFC
    M_Grp_1_2_Axe1_AFC_Naive_good<-M_Grp_1_2_Axe1_AFC_Naive
    M_Grp_1_2_Axe2_AFC_good<-M_Grp_1_2_Axe2_AFC
    M_Grp_1_2_Axe2_AFC_Naive_good<-M_Grp_1_2_Axe2_AFC_Naive
  }
  
  #-- la sauvegarde
  saveData1<-file.path("Outcome","out-regroupement","AFC","AFC_Grp_1_2_good.Rdata")
  
  save(M_Grp_1_2_Axe1_AFC_good,M_Grp_1_2_Axe1_AFC_Naive_good,
       M_Grp_1_2_Axe2_AFC_good,M_Grp_1_2_Axe2_AFC_Naive_good,
       list = c("M_Grp_1_2_Axe1_AFC_good","M_Grp_1_2_Axe1_AFC_Naive_good",
                "M_Grp_1_2_Axe2_AFC_good","M_Grp_1_2_Axe2_AFC_Naive_good"),
       file = saveData1)
  
  #---------------------------le fichier AFC_Grp_A_B.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Grp_A_B.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Grp_A_B_Axe1_AFC_good<-tibble(M_Grp_A_B_Axe1_AFC)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
      colnames(M_Grp_A_B_Axe1_AFC_good)<-colnames(M_Grp_A_B_Axe1_AFC)
      
      M_Grp_A_B_Axe1_AFC_Naive_good<-tibble(M_Grp_A_B_Axe1_AFC_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
      colnames(M_Grp_A_B_Axe1_AFC_Naive_good)<-colnames(M_Grp_A_B_Axe1_AFC_Naive)
      
      M_Grp_A_B_Axe2_AFC_good<-tibble(M_Grp_A_B_Axe2_AFC)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
      colnames(M_Grp_A_B_Axe2_AFC_good)<-colnames(M_Grp_A_B_Axe2_AFC)
      
      M_Grp_A_B_Axe2_AFC_Naive_good<-tibble(M_Grp_A_B_Axe2_AFC_Naive)%>%
                                      mutate(across(all_of(starts_with(i)),~NA))%>%
                                      as.matrix()
      colnames(M_Grp_A_B_Axe2_AFC_Naive_good)<-colnames(M_Grp_A_B_Axe2_AFC_Naive)
      
  } else
  {
    M_Grp_A_B_Axe1_AFC_good<-M_Grp_A_B_Axe1_AFC
    M_Grp_A_B_Axe1_AFC_Naive_good<-M_Grp_A_B_Axe1_AFC_Naive
    M_Grp_A_B_Axe2_AFC_good<-M_Grp_A_B_Axe2_AFC
    M_Grp_A_B_Axe2_AFC_Naive_good<-M_Grp_A_B_Axe2_AFC_Naive
  }
  
  #-- la sauvegarde
  saveData2<-file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B_good.Rdata")
  
  save(M_Grp_A_B_Axe1_AFC_good,M_Grp_A_B_Axe1_AFC_Naive_good,
       M_Grp_A_B_Axe2_AFC_good,M_Grp_A_B_Axe2_AFC_Naive_good,
       list = c("M_Grp_A_B_Axe1_AFC_good","M_Grp_A_B_Axe1_AFC_Naive_good",
                "M_Grp_A_B_Axe2_AFC_good","M_Grp_A_B_Axe2_AFC_Naive_good"),
       file = saveData2)
  
  #---------------------------le fichier AFC_Grp_Detect.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Grp_Detect.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Grp_Detect_Axe1_AFC_good<-tibble(M_Grp_Detect_Axe1_AFC)%>%
                                    mutate(across(all_of(starts_with(i)),~NA))%>%
                                    as.matrix()
      colnames(M_Grp_Detect_Axe1_AFC_good)<-colnames(M_Grp_Detect_Axe1_AFC)
      
      M_Grp_Detect_Axe1_AFC_Naive_good<-tibble(M_Grp_Detect_Axe1_AFC_Naive)%>%
                                          mutate(across(all_of(starts_with(i)),~NA))%>%
                                          as.matrix()
      colnames(M_Grp_Detect_Axe1_AFC_Naive_good)<-colnames(M_Grp_Detect_Axe1_AFC_Naive)
      
      M_Grp_Detect_Axe2_AFC_good<-tibble(M_Grp_Detect_Axe2_AFC)%>%
                                    mutate(across(all_of(starts_with(i)),~NA))%>%
                                    as.matrix()
      colnames(M_Grp_Detect_Axe2_AFC_good)<-colnames(M_Grp_Detect_Axe2_AFC)
      
      M_Grp_Detect_Axe2_AFC_Naive_good<-tibble(M_Grp_Detect_Axe2_AFC_Naive)%>%
                                          mutate(across(all_of(starts_with(i)),~NA))%>%
                                          as.matrix()
      colnames(M_Grp_Detect_Axe2_AFC_Naive_good)<-colnames(M_Grp_Detect_Axe2_AFC_Naive)
      
  } else
  {
    M_Grp_Detect_Axe1_AFC_good<-M_Grp_Detect_Axe1_AFC
    M_Grp_Detect_Axe1_AFC_Naive_good<-M_Grp_Detect_Axe1_AFC_Naive
    M_Grp_Detect_Axe2_AFC_good<-M_Grp_Detect_Axe2_AFC
    M_Grp_Detect_Axe2_AFC_Naive_good<-M_Grp_Detect_Axe2_AFC_Naive
  }
  
  #-- la sauvegarde
  saveData3<-file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_good.Rdata")
  
  save(M_Grp_Detect_Axe1_AFC_good,M_Grp_Detect_Axe1_AFC_Naive_good,
       M_Grp_Detect_Axe2_AFC_good,M_Grp_Detect_Axe2_AFC_Naive_good,
       list = c("M_Grp_Detect_Axe1_AFC_good","M_Grp_Detect_Axe1_AFC_Naive_good",
                "M_Grp_Detect_Axe2_AFC_good","M_Grp_Detect_Axe2_AFC_Naive_good"),
       file = saveData3)
  
  #---------------------------le fichier AFC_Grp_Detect_D5.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Grp_Detect_D5.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Grp_Detect_Axe1_AFC_good<-tibble(M_Grp_Detect_Axe1_AFC)%>%
                                    mutate(across(all_of(starts_with(i)),~NA))%>%
                                    as.matrix()
      colnames(M_Grp_Detect_Axe1_AFC_good)<-colnames(M_Grp_Detect_Axe1_AFC)
      
      M_Grp_Detect_Axe1_AFC_Naive_good<-tibble(M_Grp_Detect_Axe1_AFC_Naive)%>%
                                          mutate(across(all_of(starts_with(i)),~NA))%>%
                                          as.matrix()
      colnames(M_Grp_Detect_Axe1_AFC_Naive_good)<-colnames(M_Grp_Detect_Axe1_AFC_Naive)
      
      M_Grp_Detect_Axe2_AFC_good<-tibble(M_Grp_Detect_Axe2_AFC)%>%
                                    mutate(across(all_of(starts_with(i)),~NA))%>%
                                    as.matrix()
      colnames(M_Grp_Detect_Axe2_AFC_good)<-colnames(M_Grp_Detect_Axe2_AFC)
      
      M_Grp_Detect_Axe2_AFC_Naive_good<-tibble(M_Grp_Detect_Axe2_AFC_Naive)%>%
                                          mutate(across(all_of(starts_with(i)),~NA))%>%
                                          as.matrix()
      colnames(M_Grp_Detect_Axe2_AFC_Naive_good)<-colnames(M_Grp_Detect_Axe2_AFC_Naive)
      
  } else
  {
    M_Grp_Detect_Axe1_AFC_good<-M_Grp_Detect_Axe1_AFC
    M_Grp_Detect_Axe1_AFC_Naive_good<-M_Grp_Detect_Axe1_AFC_Naive
    M_Grp_Detect_Axe2_AFC_good<-M_Grp_Detect_Axe2_AFC
    M_Grp_Detect_Axe2_AFC_Naive_good<-M_Grp_Detect_Axe2_AFC_Naive
  }
  
  #-- la sauvegarde
  saveData4<-file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata")
  
  save(M_Grp_Detect_Axe1_AFC_good,M_Grp_Detect_Axe1_AFC_Naive_good,
       M_Grp_Detect_Axe2_AFC_good,M_Grp_Detect_Axe2_AFC_Naive_good,
       list = c("M_Grp_Detect_Axe1_AFC_good","M_Grp_Detect_Axe1_AFC_Naive_good",
                "M_Grp_Detect_Axe2_AFC_good","M_Grp_Detect_Axe2_AFC_Naive_good"),
       file = saveData4)
  
  #---------------------------le fichier AFC_Inertia_Plan1.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Inertia_Plan1.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Inertia_Plan1.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Inertia_Plan1_AFC_good<-tibble(M_Inertia_Plan1_AFC)%>%
                                  mutate(across(all_of(starts_with(i)),~NA))%>%
                                  as.matrix()
      colnames(M_Inertia_Plan1_AFC_good)<-colnames(M_Inertia_Plan1_AFC)
      
  } else 
  {
    M_Inertia_Plan1_AFC_good<-M_Inertia_Plan1_AFC
  }
  
  #-- la sauvegarde
  saveData5<-file.path("Outcome","out-regroupement","AFC","AFC_Inertia_Plan1_good.Rdata")
  
  save(M_Inertia_Plan1_AFC_good,
       list = c("M_Inertia_Plan1_AFC_good"),
       file = saveData5)
  
  #---------------------------le fichier AFC_Regroup_RV_Site.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Regroup_RV_Site.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Site.Rdata"))
  
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Resultat_RV_AFC_good<-tibble(M_Resultat_RV_AFC)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
      colnames(M_Resultat_RV_AFC_good)<-colnames(M_Resultat_RV_AFC)
      
  } else
  {
    M_Resultat_RV_AFC_good<-M_Resultat_RV_AFC
  }
  
  #-- la sauvegarde
  saveData6<-file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Site_good.Rdata")
  
  save(M_Resultat_RV_AFC_good,
       list = c("M_Resultat_RV_AFC_good"),
       file = saveData6)
  
  #---------------------------le fichier AFC_Regroup_RV_Sp.Rdata---------------------------------------------------#
  #-- chargement des fichiers de AFC_Regroup_RV_Sp.Rdata a modifier
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp.Rdata"))
  #-- passage en NA des resultats des simulations conciderees comme insufisament informees
  if(length(T_bad_simul_CA$Simul)!=0){
    i<-T_bad_simul_CA%>%pull("Simul")
      M_Resultat_RV_AFC_good<-tibble(M_Resultat_RV_AFC)%>%
                                mutate(across(all_of(starts_with(i)),~NA))%>%
                                as.matrix()
      colnames(M_Resultat_RV_AFC_good)<-colnames(M_Resultat_RV_AFC)
      
  } else
  {
    M_Resultat_RV_AFC_good<-M_Resultat_RV_AFC
  }
  
  #-- la sauvegarde
  saveData7<-file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata")
  
  save(M_Resultat_RV_AFC_good,
       list = c("M_Resultat_RV_AFC_good"),
       file = saveData7)
}