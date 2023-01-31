#------------------------------------------#
# script pour recuperer l'ensemble des     #
# medianes pour les indicateurs des AFC    #
# et les inscrire dans un tableau          #
# Rdata et csv                             #
#------------------------------------------#
#
#-- nettoyage
rm(list = (ls()))

#--packages
library(tidyverse)

#-- chargement des noms des fichiers
source(file.path("R","Noms-Fichiers.R"))

#-- chargement des donnnees (en fonction des infos recherchees)
#-- les AFC
#-- le RV
load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata"))
data<-as.tibble(M_Resultat_RV_AFC_good)
T_median_RV_prov<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_RV<-tibble(T_median_RV_prov[,1],tibble(NA))

for (i in 2:ncol(T_median_RV_prov)) {
  T_median_RV<-bind_cols(T_median_RV,tibble(T_median_RV_prov[,i],tibble(NA)))
}

colnames(T_median_RV)<-V_Fichier_Grp1_2

T_median<-bind_cols(tibble(analyse="RV_AFC"),T_median_RV)

#-- les groupe 1 et 2
load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_1_2_good.Rdata"))

data<-as.tibble(M_Grp_1_2_Axe1_AFC_good)
T_median_G12_Ax1<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_G12_Ax1)<-V_Fichier_Grp1_2
T_median_G12_Ax1<-bind_cols(tibble(analyse="G12_Ax1"),T_median_G12_Ax1)

data<-as.tibble(M_Grp_1_2_Axe2_AFC_good)
T_median_G12_Ax2<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_G12_Ax2)<-V_Fichier_Grp1_2
T_median_G12_Ax2<-bind_cols(tibble(analyse="G12_Ax2"),T_median_G12_Ax2)

data<-as.tibble(M_Grp_1_2_Axe1_AFC_Naive_good)
T_median_G12_Na_Ax1<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_G12_Na_Ax1)<-V_Fichier_Grp1_2
T_median_G12_Na_Ax1<-bind_cols(tibble(analyse="G12_Na_Ax1"),T_median_G12_Na_Ax1)

data<-as.tibble(M_Grp_1_2_Axe2_AFC_Naive_good)
T_median_G12_Na_Ax2<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_G12_Na_Ax2)<-V_Fichier_Grp1_2
T_median_G12_Na_Ax2<-bind_cols(tibble(analyse="G12_Na_Ax2"),T_median_G12_Na_Ax2)

T_median<-bind_rows(T_median,T_median_G12_Ax1,T_median_G12_Na_Ax1,
                    T_median_G12_Ax2,T_median_G12_Na_Ax2)

#-- les groupe A et B
load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B_good.Rdata"))

data<-as.tibble(M_Grp_A_B_Axe1_AFC_good)
T_median_GAB_Ax1<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_GAB_Ax1)<-V_Fichier_Grp1_2
T_median_GAB_Ax1<-bind_cols(tibble(analyse="GAB_Ax1"),T_median_GAB_Ax1)

data<-as.tibble(M_Grp_A_B_Axe2_AFC_good)
T_median_GAB_Ax2<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_GAB_Ax2)<-V_Fichier_Grp1_2
T_median_GAB_Ax2<-bind_cols(tibble(analyse="GAB_Ax2"),T_median_GAB_Ax2)

data<-as.tibble(M_Grp_A_B_Axe1_AFC_Naive_good)
T_median_GAB_Na_Ax1<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_GAB_Na_Ax1)<-V_Fichier_Grp1_2
T_median_GAB_Na_Ax1<-bind_cols(tibble(analyse="GAB_Na_Ax1"),T_median_GAB_Na_Ax1)

data<-as.tibble(M_Grp_A_B_Axe2_AFC_Naive_good)
T_median_GAB_Na_Ax2<-data%>%summarise_all(~median(.,na.rm=TRUE))
colnames(T_median_GAB_Na_Ax2)<-V_Fichier_Grp1_2
T_median_GAB_Na_Ax2<-bind_cols(tibble(analyse="GAB_Na_Ax2"),T_median_GAB_Na_Ax2)

T_median<-bind_rows(T_median,T_median_GAB_Ax1,T_median_GAB_Na_Ax1,
                    T_median_GAB_Ax2,T_median_GAB_Na_Ax2)

#-- les groupe plus et moins detectables
#--axes1
load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe1_AFC_good)
T_median_G_detect_Ax1<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Ax1<-bind_cols(tibble(analyse="G_det_Ax1"),T_median_G_detect_Ax1)

load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe1_AFC_good)
T_median_G_detect_Ax1_bis<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Ax1<-bind_cols(T_median_G_detect_Ax1,T_median_G_detect_Ax1_bis)

#--axes2
load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe2_AFC_good)
T_median_G_detect_Ax2<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Ax2<-bind_cols(tibble(analyse="G_det_Ax2"),T_median_G_detect_Ax2)

load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe2_AFC_good)
T_median_G_detect_Ax2_bis<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Ax2<-bind_cols(T_median_G_detect_Ax2,T_median_G_detect_Ax2_bis)

#--axes1 naif
load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe1_AFC_Naive_good)
T_median_G_detect_Na_Ax1<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Na_Ax1<-bind_cols(tibble(analyse="G_det_Na_Ax1"),T_median_G_detect_Na_Ax1)

load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe1_AFC_Naive_good)
T_median_G_detect_Na_Ax1_bis<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Na_Ax1<-bind_cols(T_median_G_detect_Na_Ax1,T_median_G_detect_Na_Ax1_bis)

#--axes2 naif
load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe2_AFC_Naive_good)
T_median_G_detect_Na_Ax2<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Na_Ax2<-bind_cols(tibble(analyse="G_det_Na_Ax2"),T_median_G_detect_Na_Ax2)

load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
data<-as.tibble(M_Grp_Detect_Axe2_AFC_Naive_good)
T_median_G_detect_Na_Ax2_bis<-data%>%summarise_all(~median(.,na.rm=TRUE))
T_median_G_detect_Na_Ax2<-bind_cols(T_median_G_detect_Na_Ax2,T_median_G_detect_Na_Ax2_bis)

#-- regroupement les noms des colonnes
T_median_G_detect<-bind_rows(T_median_G_detect_Ax1,T_median_G_detect_Na_Ax1,
                             T_median_G_detect_Ax2,T_median_G_detect_Na_Ax2)

#-- les noms des colonnes
colnames(T_median_G_detect)<-c("analyse",V_Fichier_D2_Grp1_2,V_Fichier_D4_Grp1_2,
                               V_Fichier_D5_Grp1_2)

T_median<-bind_rows(T_median,T_median_G_detect)

#-- Sauvegarde de la table pour les AFC et en fonction de la detection
#-- partition en fonction de la detection
T_median_D1<-T_median%>%select_at(vars(analyse,starts_with("D1")))
T_median_D2<-T_median%>%select_at(vars(analyse,starts_with("D2")))
T_median_D3<-T_median%>%select_at(vars(analyse,starts_with("D3")))
T_median_D4<-T_median%>%select_at(vars(analyse,starts_with("D4")))
T_median_D5<-T_median%>%select_at(vars(analyse,starts_with("D5")))

#-- chemin de sauvegarde en R.data
saveData<-file.path("Outcome","out_median","T_median_AFC.Rdata")

save(T_median_D1,T_median_D2,
     T_median_D3,T_median_D4,T_median_D5,
     list = c("T_median_D1","T_median_D2",
              "T_median_D3","T_median_D4","T_median_D5"),
     file = saveData)

#-- chemin de sauvegarde en .csv
write_excel_csv2(T_median_D1,file.path("Outcome","out_median","T_median_D1_AFC.csv"))
write_excel_csv2(T_median_D2,file.path("Outcome","out_median","T_median_D2_AFC.csv"))
write_excel_csv2(T_median_D3,file.path("Outcome","out_median","T_median_D3_AFC.csv"))
write_excel_csv2(T_median_D4,file.path("Outcome","out_median","T_median_D4_AFC.csv"))
write_excel_csv2(T_median_D5,file.path("Outcome","out_median","T_median_D5_AFC.csv"))


