#--------------------------------------------#
# script pour selectionner les simulations   #
# non valides pour les PCoA et les nMDS      #
# et qui ne sont pas dans les resultats      #
# lionel.bonsacquet                          #
#--------------------------------------------#

fct_select_bad_simul<-function(alpha=800) {
  #-- charger le tableau des bonnes simulations
  load(file.path("Outcome","out-regroupement","Good_simul_all.Rdata"))
  
  #-- selection des hypothèse (simulations) ou il y a moin de alpha (800) bonnes simuls
  T_bad_simul_PCA<-as_tibble(M_good_simul_coord,rownames="Simul")%>%
                   select_at(vars(Simul,PCA,PCA_RV))%>%
                   filter_at(vars(PCA),~.<alpha)%>%
  
  T_bad_simul_CA<-as_tibble(M_good_simul_coord,rownames="Simul")%>%
    select_at(vars(Simul,CA,CA_RV))%>%
    filter_at(vars(CA),~.<alpha)
  
  T_bad_simul_PCoA_Bray<-as_tibble(M_good_simul_coord,rownames="Simul")%>%
    select_at(vars(Simul,PCoA_Bray_sp,PCoA_Bray_site,PCoA_Bray_site_naive,
                   PCoA_Bray_RV))%>%
    filter_at(vars(PCoA_Bray_sp),~.<alpha)
  
  T_bad_simul_PCoA_Chao<-as_tibble(M_good_simul_coord,rownames="Simul")%>%
    select_at(vars(Simul,PCoA_Chao_sp,PCoA_Chao_site,PCoA_Chao_site_naive,
                   PCoA_Chao_RV))%>%
    filter_at(vars(PCoA_Chao_sp),~.<alpha)
  
  T_bad_simul_nMDS_Bray<-as_tibble(M_good_simul_coord,rownames="Simul")%>%
    select_at(vars(Simul,nMDS_Bray_sp,nMDS_Bray_site,nMDS_Bray_site_naive,
                   nMDS_Bray_RV))%>%
    filter_at(vars(nMDS_Bray_sp),~.<alpha)
  
  T_bad_simul_nMDS_Chao<-as_tibble(M_good_simul_coord,rownames="Simul")%>%
    select_at(vars(Simul,nMDS_Chao_sp,nMDS_Chao_site,nMDS_Chao_site_naive,
                   nMDS_Chao_RV))%>%
    filter_at(vars(nMDS_Chao_sp),~.<alpha)
  
  #-- sauvegarde des tableaux
  saveDataPCA<-file.path("Outcome","out-regroupement","ACP","Table_bad_Simul_PCA.Rdata")
  saveDataCA<-file.path("Outcome","out-regroupement","AFC","Table_bad_Simul_CA.Rdata")
  saveDataPCoA_bray<-file.path("Outcome","out-regroupement","PCoA","Table_bad_Simul_PCoA_bray.Rdata")
  saveDataPCoA_chao<-file.path("Outcome","out-regroupement","PCoA","Table_bad_Simul_PCoA_chao.Rdata")
  saveDatanMDS_bray<-file.path("Outcome","out-regroupement","nMDS","Table_bad_Simul_nMDS_bray.Rdata")
  saveDatanMDS_chao<-file.path("Outcome","out-regroupement","nMDS","Table_bad_Simul_nMDS_chao.Rdata")
  
  save(T_bad_simul_PCA,
       list = c("T_bad_simul_PCA"),
       file = saveDataPCA)
  
  save(T_bad_simul_CA,
       list = c("T_bad_simul_CA"),
       file = saveDataCA)
  
  save(T_bad_simul_PCoA_Bray,
       list = c("T_bad_simul_PCoA_Bray"),
       file = saveDataPCoA_bray)
  save(T_bad_simul_PCoA_Chao,
       list = c("T_bad_simul_PCoA_Chao"),
       file = saveDataPCoA_chao)
  
  save(T_bad_simul_nMDS_Bray,
       list = c("T_bad_simul_nMDS_Bray"),
       file = saveDatanMDS_bray)
  save(T_bad_simul_nMDS_Chao,
       list = c("T_bad_simul_nMDS_Chao"),
       file = saveDatanMDS_chao)

  #-- enregistrement des tableau en csv
  savePCACsv<-file.path("Outcome","out-regroupement","ACP","T_bad_simul_PCA.csv")
  saveCACsv<-file.path("Outcome","out-regroupement","AFC","T_bad_Simul_CA.csv")
  savePCoA_brayCsv<-file.path("Outcome","out-regroupement","PCoA","T_bad_Simul_PCoA_bray.csv")
  savePCoA_chaoCsv<-file.path("Outcome","out-regroupement","PCoA","T_bad_Simul_PCoA_chao.csv")
  savenMDS_brayCsv<-file.path("Outcome","out-regroupement","nMDS","T_bad_Simul_nMDS_bray.csv")
  savenMDS_chaoCsv<-file.path("Outcome","out-regroupement","nMDS","T_bad_Simul_nMDS_chao.csv")
  
  write.csv2(T_bad_simul_PCA,
             file = savePCACsv)

  write.csv2(T_bad_simul_CA,
       file = saveCACsv)
  
  write.csv2(T_bad_simul_PCoA_Bray,
       file = savePCoA_brayCsv)
  write.csv2(T_bad_simul_PCoA_Chao,
       file = savePCoA_chaoCsv)
  
  write.csv2(T_bad_simul_nMDS_Bray,
       file = savenMDS_brayCsv)
  write.csv2(T_bad_simul_nMDS_Chao,
       file = savenMDS_chaoCsv)
  
}