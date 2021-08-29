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
source(file.path("R","RV_PCoA.R")) 
source(file.path("R","Regroup_RV_pcoa.R")) 
source(file.path("R","Regroup_Coord_Sp_pcoa.R")) 
source(file.path("R","Grp_Plus_Moins_detectable_D2D4_pcoa.R")) 
source(file.path("R","Grp_Plus_Moins_detectable_D5_pcoa.R")) 
source(file.path("R","Grp_1_2_pcoa.R")) 
source(file.path("R","Grp_A_B_pcoa.R")) 

# PCoA (sur les meme simuls que celle des ACP)
#-- Pcoa sur les matrice de distance --#
#-- appliquer en boucle sur les noms des fichiers (au cas ou plus de 1)
for (i in V_Nom_Fichier) {
  fct_pcoa_site(analyse="ACP",distance="chao",fichier=i)}

for (i in V_Nom_Fichier) {
  fct_pcoa_site(analyse="ACP",distance="bray",fichier=i)}

#-- calcul des Rv sur les differentes matrice de distances (reresentation des points dans un 
#   espace euclidien)
for (i in V_Nom_Fichier) {
  Fct_RV_pcoa(distance="chao",fichier=i)}

for (i in V_Nom_Fichier) {
  Fct_RV_pcoa(distance="bray",fichier=i)}

#-- regoupment des RV sur les resultats des PCoA pour les graphiques
Fct_Regroup_RV_pcoa(Site_Sp="Site",distance="chao")
Fct_Regroup_RV_pcoa(Site_Sp="Sp",distance="chao")
Fct_Regroup_RV_pcoa(Site_Sp="Site",distance="bray")
Fct_Regroup_RV_pcoa(Site_Sp="Sp",distance="bray")

#-- regroupement des coordonnees des Sp (que les especes) en fonction des groupe 1 et 2
Fct_Regroup_Coord_Sp_pcoa(distance="chao") 
Fct_Regroup_Coord_Sp_pcoa(distance="bray") 

#-- regroupement des coordonnees des espece plus et moins detectable
Fct_Grp_detect_pcoa(distance="chao")
Fct_Grp_detect_pcoa(distance="bray")

Fct_Grp_detectD5_pcoa(distance="chao")
Fct_Grp_detectD5_pcoa(distance="bray")

#-- regroupement des coordonnees des espece des groupe 1 et 2
Fct_Grp_1_2_pcoa(distance="chao")
Fct_Grp_1_2_pcoa(distance="bray")

#-- regroupement des coordonnees des espece des groupe A et B
Fct_Grp_A_B_pcoa(distance="chao")
Fct_Grp_A_B_pcoa(distance="bray")  

#------------------------------