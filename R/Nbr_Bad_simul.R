#--------------------------------------------------#
# script pour regrouper le nombre de simulations   #
# qui ne sont pas valide pour les PCoA et les nMDS #
# et differentes comparaisons                      #
# lionel.bonsacquet                                #
#--------------------------------------------------#
source(file.path("R","Noms-Fichiers.R"))

fct_tab_bad_simul<-function(fichier=V_Nom_Fichier)
{
  #-- creation des tableaux recapitulatifs
  M_good_simul_coord<-matrix(NA,ncol =20,nrow = length(fichier))
  colnames(M_good_simul_coord)<-c("PCA","CA","PCA_RV","CA_RV",
                                     "PCoA_Bray_sp","PCoA_Bray_site","PCoA_Bray_site_naive","PCoA_Bray_RV",
                                     "PCoA_Chao_sp","PCoA_Chao_site","PCoA_Chao_site_naive","PCoA_Chao_RV",
                                     "nMDS_Bray_sp","nMDS_Bray_site","nMDS_Bray_site_naive","nMDS_Bray_RV",
                                     "nMDS_Chao_sp","nMDS_Chao_site","nMDS_Chao_site_naive","nMDS_Chao_RV")
  
  rownames(M_good_simul_coord)<-fichier
  
  #-- chargement des premieres donnees utiles
  load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_Coord_Sp.Rdata"))
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_Coord_Sp.Rdata"))
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_Coord_Sp_bray.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_Coord_Sp_bray.Rdata"))
  
  load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp.Rdata"))
  load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp.Rdata"))
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray.Rdata"))
  
  #-- remplissage premiere partie du tableau
  for (i in fichier){
    #-- pour les coord
    M_good_simul_coord[i,"PCA"]<-1000-length(which(is.na(M_Resultat_Coord_Sp_ACP_axe1[,i])))/20
    M_good_simul_coord[i,"CA"]<-1000-length(which(is.na(M_Resultat_Coord_Sp_AFC_axe1[,i])))/20
    M_good_simul_coord[i,"PCA_RV"]<-1000-length(which(is.na(M_Resultat_RV_ACP[,i])))
    M_good_simul_coord[i,"CA_RV"]<-1000-length(which(is.na(M_Resultat_RV_AFC[,i])))
    
    M_good_simul_coord[i,"PCoA_Bray_sp"]<-1000-length(which(is.na(M_Resultat_Coord_Sp_pcoa_axe1[,i])))/20
    M_good_simul_coord[i,"PCoA_Bray_site"]<-1000-length(which(is.na(M_Resultat_Coord_Site_pcoa_TEST_axe1[,i])))/50
    M_good_simul_coord[i,"PCoA_Bray_site_naive"]<-1000-length(which(is.na(M_Resultat_Coord_Site_pcoa_TEST_Naive_axe1[,i])))/50
    M_good_simul_coord[i,"PCoA_Bray_RV"]<-1000-length(which(is.na(M_Resultat_RV_pcoa[,i])))
    
    M_good_simul_coord[i,"nMDS_Bray_sp"]<-1000-length(which(is.na(M_Resultat_Coord_Sp_nmds_axe1[,i])))/20
    M_good_simul_coord[i,"nMDS_Bray_site"]<-1000-length(which(is.na(M_Resultat_Coord_Site_nmds_TEST_axe1[,i])))/100
    M_good_simul_coord[i,"nMDS_Bray_site_naive"]<-1000-length(which(is.na(M_Resultat_Coord_Site_nmds_TEST_Naive_axe1[,i])))/100
    M_good_simul_coord[i,"nMDS_Bray_RV"]<-1000-length(which(is.na(M_Resultat_RV_nmds[,i])))
  }
  
  #-- chargement des donnees utiles suite
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_Coord_Sp_chao.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_Coord_Sp_chao.Rdata"))
  
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao.Rdata"))
  
  #-- remplissage premiere partie du tableau
  for (i in fichier){
    #-- pour les coord
    M_good_simul_coord[i,"PCoA_Chao_sp"]<-1000-length(which(is.na(M_Resultat_Coord_Sp_pcoa_axe1[,i])))/20
    M_good_simul_coord[i,"PCoA_Chao_site"]<-1000-length(which(is.na(M_Resultat_Coord_Site_pcoa_TEST_axe1[,i])))/50
    M_good_simul_coord[i,"PCoA_Chao_site_naive"]<-1000-length(which(is.na(M_Resultat_Coord_Site_pcoa_TEST_Naive_axe1[,i])))/50
    M_good_simul_coord[i,"PCoA_Chao_RV"]<-1000-length(which(is.na(M_Resultat_RV_pcoa[,i])))
    
    M_good_simul_coord[i,"nMDS_Chao_sp"]<-1000-length(which(is.na(M_Resultat_Coord_Sp_nmds_axe1[,i])))/20
    M_good_simul_coord[i,"nMDS_Chao_site"]<-1000-length(which(is.na(M_Resultat_Coord_Site_nmds_TEST_axe1[,i])))/100
    M_good_simul_coord[i,"nMDS_Chao_site_naive"]<-1000-length(which(is.na(M_Resultat_Coord_Site_nmds_TEST_Naive_axe1[,i])))/100
    M_good_simul_coord[i,"nMDS_Chao_RV"]<-1000-length(which(is.na(M_Resultat_RV_nmds[,i])))
  }
  
  #-- sauvegarde
  saveData<-file.path("Outcome","out-regroupement",paste("Good_simul_all.Rdata",sep = ""))
  saveDataCsv<-file.path("Outcome","out-regroupement",paste("Good_simul_all.csv",sep = ""))
    
  save(M_good_simul_coord,
       list = c("M_good_simul_coord"),
       file = saveData)
  
  write.csv2(M_good_simul_coord,
             file = saveDataCsv)
}