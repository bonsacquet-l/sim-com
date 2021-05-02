#---------------------------------------------#
# Make pour appliquer les fonction PCoA       #
# a toutes les simuls                         #
#---------------------------------------------#

#-- nettoyage
rm(list = (ls()))

#-- sourcer les script necessaire
source(file.path("R","package-option.R"))
source(file.path("R","Noms-Fichiers.R"))
source(file.path("R","PCoA_matr_dist_site.R"))
source(file.path("R","RV_Nmds.R")) 

# ACP
#-- Pcoa sur les matrice de distance --#
#-- appliquer en boucle sur les noms des fichiers (au cas ou plus de 1)
for (i in V_Nom_Fichier[73]) {
  fct_pcoa_site(analyse="ACP",fichier=i)}

for (i in V_Nom_Fichier[83]) {
  fct_pcoa_site(analyse="ACP",fichier=i)}

#-- calcul des Rv sue les differentes matrice de distances (reresentation des points dans un 
#   espace euclidien)
# chargement des donnees
load("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_pcoa_matr_dist_site_spD4_C3_05.Rdata")
i<-V_Nom_Fichier[73]

load("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_pcoa_matr_dist_site_spD5_C3.Rdata")
i<-V_Nom_Fichier[83]

#application de la fonction effectuant le RV entre matrice de distance
Fct_RV_Nmds(Nsimul=1000,
            analyse="ACP",
            A_Coord_Nmds=A_Coord_pcoa_chao_Site,
            A_Coord_Nmds_Naive=A_Coord_pcoa_chao_Site_Naive,
            fichier=i,
            info_save="Pcoa_site_chao")

Fct_RV_Nmds(Nsimul=1000,
            analyse="ACP",
            A_Coord_Nmds=A_Coord_pcoa_chao_Sp,
            A_Coord_Nmds_Naive=A_Coord_pcoa_chao_Sp_Naive,
            fichier=i,
            info_save="Pcoa_sp_chao")

Fct_RV_Nmds(Nsimul=1000,
            analyse="ACP",
            A_Coord_Nmds=A_Coord_pcoa_bray_Site,
            A_Coord_Nmds_Naive=A_Coord_pcoa_bray_Site_Naive,
            fichier=i,
            info_save="Pcoa_site_bray")

Fct_RV_Nmds(Nsimul=1000,
            analyse="ACP",
            A_Coord_Nmds=A_Coord_pcoa_bray_Sp,
            A_Coord_Nmds_Naive=A_Coord_pcoa_bray_Sp_Naive,
            fichier=i,
            info_save="Pcoa_sp_bray")

#------------------------------