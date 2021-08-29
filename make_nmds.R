#---------------------------------------------#
# Make pour appliquer la fonction nmds        #
# a toutes les simuls                         #
#---------------------------------------------#

#-- nettoyage
rm(list = (ls()))

#-- sourcer les script necessaire
source(file.path("R","package-option.R"))
source(file.path("R","Noms-Fichiers.R"))
source(file.path("R","nMDS_matr_dist_site.R"))
source(file.path("R","RV_nmds.R")) 
source(file.path("R","Regroup_RV_nmds.R")) 
source(file.path("R","Regroup_Coord_Sp_nmds.R")) 
source(file.path("R","Grp_Plus_Moins_detectable_D2D4_nmds.R")) 
source(file.path("R","Grp_Plus_Moins_detectable_D5_nmds.R")) 
source(file.path("R","Grp_1_2_nmds.R")) 
source(file.path("R","Grp_A_B_nmds.R")) 

# nMDS (sur les meme simuls que celle des AFC)
#-- nMDS sur les matrice de distance --#
#-- appliquer en boucle sur les noms des fichiers (au cas ou plus de 1)
for (i in V_Nom_Fichier) {
  fct_nmds_site(analyse="AFC",distance="chao",fichier=i)}

for (i in V_Nom_Fichier) {
  fct_nmds_site(analyse="AFC",distance="bray",fichier=i)}

#-- calcul des Rv sur les differentes matrice de distances (reresentation des points dans un 
#   espace euclidien)
for (i in V_Nom_Fichier) {
  Fct_RV_nmds(distance="chao",fichier=i)}

for (i in V_Nom_Fichier) {
  Fct_RV_nmds(distance="bray",fichier=i)}

#-- regoupment des RV sur les resultats des nMDS pour les graphiques
Fct_Regroup_RV_nmds(Site_Sp="Site",distance="chao")
Fct_Regroup_RV_nmds(Site_Sp="Sp",distance="chao")
Fct_Regroup_RV_nmds(Site_Sp="Site",distance="bray")
Fct_Regroup_RV_nmds(Site_Sp="Sp",distance="bray")

#-- regroupement des coordonnees des Sp (que les especes) en fonction des groupe 1 et 2
Fct_Regroup_Coord_Sp_nmds(distance="chao") 
Fct_Regroup_Coord_Sp_nmds(distance="bray") 

#-- regroupement des coordonnees des espece plus et moins detectable
Fct_Grp_detect_nmds(distance="chao")
Fct_Grp_detect_nmds(distance="bray")

Fct_Grp_detectD5_nmds(distance="chao")
Fct_Grp_detectD5_nmds(distance="bray")

#-- regroupement des coordonnees des espece des groupe 1 et 2
Fct_Grp_1_2_nmds(distance="chao")
Fct_Grp_1_2_nmds(distance="bray")

#-- regroupement des coordonnees des espece des groupe A et B
Fct_Grp_A_B_nmds(distance="chao")
Fct_Grp_A_B_nmds(distance="bray")  

#------------------------------