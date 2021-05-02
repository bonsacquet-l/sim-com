---
output:
  pdf_document: default
  html_document: default
---
# Le projet "sim-com"
Ensemble des scripts utilisés pour le projet d'étude porté par L. Bonscaquet et A. Besnard.

## Version précédente V1.0.0
### 1 les makes  
Tout le projet est exécutable à partir des 3 make.R : make-ACP, make-AFC et make-Graph. 

### 2 la simulations des données
A l'aide de make-ACP() et make-AFC(). Ces deux makes.R comportent deux parties, la première pour simuler les données qui ont les même paramètres et stocker ces simulations dans un dossier, puis exécuter les calculs et vérifications de toutes ces simulations appartenant au même dossier. La deuxième partie permet de regrouper les données (de toutes les simulations, donc de tout les fichiers) et de produire les indicateurs.  

#### 2.1 les fonction pour les ACP et les AFC
Les fonctions simulACP() et simulAFC() permettent de simuler les "informed data", les "naïve data" et les ACP et AFC correspondantes, avec enregistrement des résultats.
Les résultats des simulations faites avec les même paramètres sont stockés dans un même fichier dont le nom est donné dans la fonction. La nomenclature utilisé est: "D1_C1_01" pour famille de probabilité de détection "D1" sur la communauté type "C2" et avec une probabilité de détection moyenne de "0.1".

*Les paramètres à entrer: *   
Nsimul : le nombre de simulation (1000 par défaut),  
Nplot : le nombre de sites suivis (100 pour les AFC),  
Nsp : le nombre d'espèces dans la communauté (20 par défaut),  
minAbon : l'abondance moyenne la plus basse possible dans la communauté,  
maxAbon : l'abondance moyenne la plus haute possible dans la communauté,  
minSeG1 : l'ecart type min des distributions des especes sur G1,  
maxSeG1 : l'ecart type max des distributions des especes sur G1,  
minSeG2 : l'ecart type min des distributions des especes sur G2,  
maxSeG2 : l'ecart type max des distributions des especes sur G2,  
sp1 : numéro de la première espèce du premier groupe impactée par un problème de détection,  
sp2 : numéro de la dernière espèce du premier groupe impactée par un problème de détection,  
sp11 : numéro de la première espèce du deuxième groupe impactée par un problème de détection,  
sp22 : numéro de la dernière espèce du deuxième groupe impactée par un problème de détection,   
DetectMean : moyenne de la loi normale dans laquelle la probabilité détection moyenne de chaque espèce est tirée,  
VarDetect : ecart type de la loi normale dans laquelle la probabilité détection moyenne de chaque espèce est tirée, 
Detect_Mean_On_G1Min= minimum de la position sur G1 de la detection moyenne (par défault =500), 
Detect_Mean_On_G1Max= maximum de la position sur G1 de la detection moyenne (par défault =500),  
minSlope : minimum de la loi uniforme dans laquelle la pente de la probabilité de detection de chaque espèce est tirée,   
maxSlope : maximum de la loi uniforme dans laquelle la pente de la probabilité de detection de chaque espèce est tirée,   
fichier : nom du fichier de sauvegarde.

#### 2.2 les fonctions pour les RV et les vérifications.
Elles sont réalisées pour l'ensemble des simulations qui sont dans le même fichier.

### 3 La construction des indicateurs.
En premier lieu il faut adapter le script "Noms-Fichiers.R" en fonction des besoins. Les fonctions "Fct_Regroup_RV_ACP("Sp")", "Fct_Regroup_Coord_Sp_ACP()", "Fct_Grp_detect_ACP()", "Fct_Grp_detectD5_ACP()", "Fct_Grp_1_2_ACP()" et "Fct_Grp_A_B_ACP()" s'exécutent sur cette base (les noms des fichiers qui ont été renseignés dans le scripts "Noms-Fichiers.R").
 
### 4 La réalisation des graphiques
A l'aide du make.Graph.R

## Nouvelle version V1.0.1
Cette version se voit ajouter des scripts pour effectuer des PCoA et des nMDS avec des matrices de distances entre les sites. Les distances choisies sont les distances de Chao et la distance de Bray-Curtis sur la racine carrée des abondances (ou different percentage).  

Un script est dédier a la fonction pour le calcul des RV entre matrices de distance entre les ordination informée et les ordinations naives (aussi bien pour les sites ou les especes, les PCoA et les nMDS, la distance de chao ou de Bray-Curtis).  

Un script permet de produire les boxplots des RV cités ci-dessus (utilisé dans l'appendix S3).

Un script permet de produire les images des ordinations informees et naives côte à côte.



