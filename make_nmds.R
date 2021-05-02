#---------------------------------------------#
# Make pour appliquer la fonction nmds        #
# a toutes les simuls                         #
#---------------------------------------------#

#-- nettoyage
rm(list = (ls()))

#-- sourcer les script necessaire
source(file.path("R","package-option.R"))
source(file.path("R","Noms-Fichiers.R"))
source(file.path("R","NMDS_matr_comm_Site_Sp.R"))
source(file.path("R","RV_Nmds.R")) 

#AFC
#-- nmds sur les matrice de communaute --#
#-- appliquer en boucle sur les noms des fichiers (au cas ou plus de 1) 
  for (i in V_Nom_Fichier[73]) {
    fct_nmds_comm_site_sp(analyse="AFC",fichier=i)}

  for (i in V_Nom_Fichier[83]) {
    fct_nmds_comm_site_sp(analyse="AFC",fichier=i)}

#-- calcul des Rv sue les differentes matrice de distances (reresentation des points dans un 
#   espace euclidien)
  # chargement des donnees
  load("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_nmds_site_sp_D4_C3_05.Rdata")
  i<-V_Nom_Fichier[73]
  
  load("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_nmds_site_sp_D5_C3.Rdata")
  i<-V_Nom_Fichier[83]
  
  #application de la fonction effectuant le RV entre matrice de distance
  Fct_RV_Nmds(Nsimul=1000,
              analyse="AFC",
              A_Coord_Nmds=A_Coord_Nmds_com_Site_chao,
              A_Coord_Nmds_Naive=A_Coord_Nmds_com_Site_Naive_chao,
              fichier=i,
              info_save="Nmds_site_chao")
  
  Fct_RV_Nmds(Nsimul=1000,
              analyse="AFC",
              A_Coord_Nmds=A_Coord_Nmds_com_Sp_chao,
              A_Coord_Nmds_Naive=A_Coord_Nmds_com_Sp_Naive_chao,
              fichier=i,
              info_save="Nmds_sp_chao")
  
  Fct_RV_Nmds(Nsimul=1000,
              analyse="AFC",
              A_Coord_Nmds=A_Coord_Nmds_com_Site_bray,
              A_Coord_Nmds_Naive=A_Coord_Nmds_com_Site_Naive_bray,
              fichier=i,
              info_save="Nmds_site_bray")
  
  Fct_RV_Nmds(Nsimul=1000,
              analyse="AFC",
              A_Coord_Nmds=A_Coord_Nmds_com_Sp_bray,
              A_Coord_Nmds_Naive=A_Coord_Nmds_com_Sp_Naive_bray,
              fichier=i,
              info_save="Nmds_sp_bray")
  