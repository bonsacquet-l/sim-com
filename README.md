---
output:
  pdf_document: default
  html_document: default
---
# Le projet "sim-com"
Ensemble des scripts utiliser pour le projet d'étude porter par L. Bonscaquet et A. Besnard.

### 1 les AFC
#### 1.1
La fonction simulAFC() pour simuler les "informed data" et les "naïve data" et les AFC correspondantes, avec enregistrement des résltats des AFC et des tables de distances entre les espèces dans le premier plan de projection.  

*Les paramètres à entrer:  *
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

#### 1.2
calcul des RV entre les matrices de distance avec la fonction 


