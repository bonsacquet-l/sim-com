#---------------------#
# liste des nom des   #
# fichiers ACP et AFC #
#---------------------#

#-------------------------------------------------------------------#
# creation de la liste des fichiers a compiler pour le regroupement #
# des resultats                                                     #
#-------------------------------------------------------------------#

V_Debut_Fichier<-c("D1","D2","D3","D4")
V_Milieu_Fichier<-c("C1","C2","C3","C4")
V_Fin_Fichier<-c("01","03","05","07","09")
V_Nom_Fichier_prov2<-vector(length = 0)
V_Nom_Fichier<-vector(length = 0)

for (i in 1:4) {
  V_Nom_Fichier_prov1<-V_Debut_Fichier[i]
  
  V_Nom_Fichier_prov1<-paste(V_Nom_Fichier_prov1,"_",V_Milieu_Fichier,sep = "")
  V_Nom_Fichier_prov2<-c(V_Nom_Fichier_prov2,V_Nom_Fichier_prov1)
}

for (j in 1:16) {
  V_Nom_Fichier_prov3<-V_Nom_Fichier_prov2[j]
  
  V_Nom_Fichier_prov3<-paste(V_Nom_Fichier_prov3,"_",V_Fin_Fichier,sep = "")
  V_Nom_Fichier<-c(V_Nom_Fichier,V_Nom_Fichier_prov3)
}

V_Nom_Fichier_D5<-c("D5_C1","D5_C2","D5_C3","D5_C4")
V_Nom_Fichier<-c(V_Nom_Fichier,V_Nom_Fichier_D5)

#------------------------------------------------#
# liste des dossiers par type de proba de detect #
# pour la creation des indicateurs et des graphs #
#------------------------------------------------#
V_Fichier_D1<-V_Nom_Fichier[1:20]
V_Fichier_D2<-V_Nom_Fichier[21:40]
V_Fichier_D3<-V_Nom_Fichier[41:60]
V_Fichier_D4<-V_Nom_Fichier[61:80]
V_Fichier_D5<-V_Nom_Fichier[81:84]

#-------------------------------------------------#
# doublement des noms en fonction des indicateurs #
# a creer et les graphique associe                #
#-------------------------------------------------#
#-- pour les groupes + et - detectables
V_Fichier_D2_GrpDetect<-c(paste(V_Fichier_D2,"_PlusD",sep = ""),
                          paste(V_Fichier_D2,"_MoinsD",sep = ""))
V_Fichier_D2_GrpDetect<-V_Fichier_D2_GrpDetect[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]


V_Fichier_D4_GrpDetect<-c(paste(V_Fichier_D4,"_PlusD",sep = ""),
                          paste(V_Fichier_D4,"_MoinsD",sep = ""))
V_Fichier_D4_GrpDetect<-V_Fichier_D4_GrpDetect[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D5_GrpDetect<-c(paste(V_Fichier_D5,"_PlusD",sep = ""),
                          paste(V_Fichier_D5,"_MoinsD",sep = ""))
V_Fichier_D5_GrpDetect<-V_Fichier_D5_GrpDetect[c(1,5,2,6,3,7,4,8)]

#-- pour les groupes 1 et 2
V_Fichier_D1_Grp1_2<-c(paste(V_Fichier_D1,"_G1",sep = ""),
                       paste(V_Fichier_D1,"_G2",sep = ""))
V_Fichier_D1_Grp1_2<-V_Fichier_D1_Grp1_2[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D2_Grp1_2<-c(paste(V_Fichier_D2,"_G1",sep = ""),
                       paste(V_Fichier_D2,"_G2",sep = ""))
V_Fichier_D2_Grp1_2<-V_Fichier_D2_Grp1_2[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D3_Grp1_2<-c(paste(V_Fichier_D3,"_G1",sep = ""),
                       paste(V_Fichier_D3,"_G2",sep = "")) 
V_Fichier_D3_Grp1_2<-V_Fichier_D3_Grp1_2[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D4_Grp1_2<-c(paste(V_Fichier_D4,"_G1",sep = ""),
                       paste(V_Fichier_D4,"_G2",sep = ""))
V_Fichier_D4_Grp1_2<-V_Fichier_D4_Grp1_2[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D5_Grp1_2<-c(paste(V_Fichier_D5,"_G1",sep = ""),
                       paste(V_Fichier_D5,"_G2",sep = ""))
V_Fichier_D5_Grp1_2<-V_Fichier_D5_Grp1_2[c(1,5,2,6,3,7,4,8)]

V_Fichier_Grp1_2<-c(V_Fichier_D1_Grp1_2,V_Fichier_D2_Grp1_2,V_Fichier_D3_Grp1_2,
                    V_Fichier_D4_Grp1_2,V_Fichier_D5_Grp1_2)

#-- pour les groupes A et B
V_Fichier_D1_GrpA_B<-c(paste(V_Fichier_D1,"_GA",sep = ""),
                       paste(V_Fichier_D1,"_GB",sep = ""))
V_Fichier_D1_GrpA_B<-V_Fichier_D1_GrpA_B[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D2_GrpA_B<-c(paste(V_Fichier_D2,"_GA",sep = ""),
                       paste(V_Fichier_D2,"_GB",sep = ""))
V_Fichier_D2_GrpA_B<-V_Fichier_D2_GrpA_B[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D3_GrpA_B<-c(paste(V_Fichier_D3,"_GA",sep = ""),
                       paste(V_Fichier_D3,"_GB",sep = ""))
V_Fichier_D3_GrpA_B<-V_Fichier_D3_GrpA_B[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D4_GrpA_B<-c(paste(V_Fichier_D4,"_GA",sep = ""),
                       paste(V_Fichier_D4,"_GB",sep = ""))
V_Fichier_D4_GrpA_B<-V_Fichier_D4_GrpA_B[c(1,21,2,22,3,23,4,24,5,25,6,26,
                                                 7,27,8,28,9,29,10,30,11,31,12,32,
                                                 13,33,14,34,15,35,16,36,17,37,
                                                 18,38,19,39,20,40)]

V_Fichier_D5_GrpA_B<-c(paste(V_Fichier_D5,"_GA",sep = ""),
                       paste(V_Fichier_D5,"_GB",sep = ""))
V_Fichier_D5_GrpA_B<-V_Fichier_D5_GrpA_B[c(1,5,2,6,3,7,4,8)]

V_Fichier_GrpA_B<-c(V_Fichier_D1_GrpA_B,V_Fichier_D2_GrpA_B,V_Fichier_D3_GrpA_B,
                    V_Fichier_D4_GrpA_B,V_Fichier_D5_GrpA_B)

################################################################################  
################################################################################    