#-------------------------------------#
# graphique des groupes d'espece 1 et #
# 2 soit GA et GB                     #
# pour 4 analyses                     #
# lionel.bonsacquet                   #
#-------------------------------------#

Fct_Graph_GA_GB_par_4_tiff<-function(Detection="D1",couleur_noir="couleur",
                                     graph_1="ACP",graph_2="AFC",
                                     graph_3="PCoA_chao",graph_4="nMDS_chao",
                                     ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                     ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                                     ylimG2_1=c(-1.2,1.5),ylimG2_2=c(-1.2,1.5),
                                     ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8),
                                     ylimG3_1=c(-0.05,0.05),ylimG3_2=c(-0.05,0.05),
                                     ylimG3_3=c(-0.05,0.05),ylimG3_4=c(-0.05,0.05),
                                     ylimG4_1=c(-0.25,0.25),ylimG4_2=c(-0.25,0.25),
                                     ylimG4_3=c(-0.25,0.25),ylimG4_4=c(-0.25,0.25)) {
  
  #-- chargement des donnees pour le graph_1 (ici les coordonnees uniquement)
  if (graph_1=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G1<-M_Grp_A_B_Axe1_ACP_good
    M_Grp_A_B_Axe1_Naive_G1<-M_Grp_A_B_Axe1_ACP_Naive_good
    M_Grp_A_B_Axe2_G1<-M_Grp_A_B_Axe2_ACP_good
    M_Grp_A_B_Axe2_Naive_G1<-M_Grp_A_B_Axe2_ACP_Naive_good
    text_1<-"PCA"
  }
  
  if (graph_1=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G1<-M_Grp_A_B_Axe1_AFC_good
    M_Grp_A_B_Axe1_Naive_G1<-M_Grp_A_B_Axe1_AFC_Naive_good
    M_Grp_A_B_Axe2_G1<-M_Grp_A_B_Axe2_AFC_good
    M_Grp_A_B_Axe2_Naive_G1<-M_Grp_A_B_Axe2_AFC_Naive_good
    text_1<-"CA"
  }
  
  if (graph_1=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G1<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G1<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G1<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G1<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_1<-graph_1
  }
  
  if (graph_1=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G1<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G1<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G1<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G1<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_1<-graph_1
  }
  
  if (graph_1=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G1<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G1<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G1<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G1<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_1<-graph_1
  }
  
  if (graph_1=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G1<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G1<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G1<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G1<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_1<-graph_1
  }
  
  #-- chargement des donnees pour le graph_2 (ici les coordonnees uniquement)
  if (graph_2=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G2<-M_Grp_A_B_Axe1_ACP_good
    M_Grp_A_B_Axe1_Naive_G2<-M_Grp_A_B_Axe1_ACP_Naive_good
    M_Grp_A_B_Axe2_G2<-M_Grp_A_B_Axe2_ACP_good
    M_Grp_A_B_Axe2_Naive_G2<-M_Grp_A_B_Axe2_ACP_Naive_good
    text_2<-"PCA"
  }
  
  if (graph_2=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G2<-M_Grp_A_B_Axe1_AFC_good
    M_Grp_A_B_Axe1_Naive_G2<-M_Grp_A_B_Axe1_AFC_Naive_good
    M_Grp_A_B_Axe2_G2<-M_Grp_A_B_Axe2_AFC_good
    M_Grp_A_B_Axe2_Naive_G2<-M_Grp_A_B_Axe2_AFC_Naive_good
    text_2<-"CA"
  }
  
  if (graph_2=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G2<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G2<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G2<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G2<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_2<-graph_2
  }
  
  if (graph_2=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G2<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G2<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G2<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G2<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_2<-graph_2
  }
  
  if (graph_2=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G2<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G2<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G2<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G2<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_2<-graph_2
  }
  
  if (graph_2=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G2<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G2<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G2<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G2<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_2<-graph_2
  }
  
  #-- chargement des donnees pour le graph_3 (ici les coordonnees uniquement)
  if (graph_3=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G3<-M_Grp_A_B_Axe1_ACP_good
    M_Grp_A_B_Axe1_Naive_G3<-M_Grp_A_B_Axe1_ACP_Naive_good
    M_Grp_A_B_Axe2_G3<-M_Grp_A_B_Axe2_ACP_good
    M_Grp_A_B_Axe2_Naive_G3<-M_Grp_A_B_Axe2_ACP_Naive_good
    text_3<-"PCA"
  }
  
  if (graph_3=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G3<-M_Grp_A_B_Axe1_AFC_good
    M_Grp_A_B_Axe1_Naive_G3<-M_Grp_A_B_Axe1_AFC_Naive_good
    M_Grp_A_B_Axe2_G3<-M_Grp_A_B_Axe2_AFC_good
    M_Grp_A_B_Axe2_Naive_G3<-M_Grp_A_B_Axe2_AFC_Naive_good
    text_3<-"CA"
  }
  
  if (graph_3=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G3<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G3<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G3<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G3<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_3<-graph_3
  }
  
  if (graph_3=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G3<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G3<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G3<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G3<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_3<-graph_3
  }
  
  if (graph_3=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G3<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G3<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G3<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G3<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_3<-graph_3
  }
  
  if (graph_3=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G3<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G3<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G3<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G3<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_3<-graph_3
  }
  
  #-- chargement des donnees pour le graph_4 (ici les coordonnees uniquement)
  if (graph_4=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G4<-M_Grp_A_B_Axe1_ACP_good
    M_Grp_A_B_Axe1_Naive_G4<-M_Grp_A_B_Axe1_ACP_Naive_good
    M_Grp_A_B_Axe2_G4<-M_Grp_A_B_Axe2_ACP_good
    M_Grp_A_B_Axe2_Naive_G4<-M_Grp_A_B_Axe2_ACP_Naive_good
    text_4<-"PCA"
  }
  
  if (graph_4=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G4<-M_Grp_A_B_Axe1_AFC_good
    M_Grp_A_B_Axe1_Naive_G4<-M_Grp_A_B_Axe1_AFC_Naive_good
    M_Grp_A_B_Axe2_G4<-M_Grp_A_B_Axe2_AFC_good
    M_Grp_A_B_Axe2_Naive_G4<-M_Grp_A_B_Axe2_AFC_Naive_good
    text_4<-"CA"
  }
  
  if (graph_4=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G4<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G4<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G4<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G4<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_4<-graph_4
  }
  
  if (graph_4=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G4<-M_Grp_A_B_Axe1_pcoa_good
    M_Grp_A_B_Axe1_Naive_G4<-M_Grp_A_B_Axe1_pcoa_Naive_good
    M_Grp_A_B_Axe2_G4<-M_Grp_A_B_Axe2_pcoa_good
    M_Grp_A_B_Axe2_Naive_G4<-M_Grp_A_B_Axe2_pcoa_Naive_good
    text_4<-graph_4
  }
  
  if (graph_4=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G4<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G4<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G4<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G4<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_4<-graph_4
  }
  
  if (graph_4=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_A_B_good.Rdata"))
    M_Grp_A_B_Axe1_G4<-M_Grp_A_B_Axe1_nmds_good
    M_Grp_A_B_Axe1_Naive_G4<-M_Grp_A_B_Axe1_nmds_Naive_good
    M_Grp_A_B_Axe2_G4<-M_Grp_A_B_Axe2_nmds_good
    M_Grp_A_B_Axe2_Naive_G4<-M_Grp_A_B_Axe2_nmds_Naive_good
    text_4<-graph_4
  }
  
  #-- labels des axes x
  xlab1=paste("(a1) informed data ",Detection,sep="")
  xlab2=paste("(a2) naïve data ",Detection,sep="")
  xlab3=paste("(a3) informed data ",Detection,sep="")
  xlab4=paste("(a4) naïve data ",Detection,sep="")
  xlab5=paste("(b1) informed data ",Detection,sep="")
  xlab6=paste("(b2) naïve data ",Detection,sep="")
  xlab7=paste("(b3) informed data ",Detection,sep="")
  xlab8=paste("(b4) naïve data ",Detection,sep="")
  xlab9=paste("(c1) informed data ",Detection,sep="")
  xlab10=paste("(c2) naïve data ",Detection,sep="")
  xlab11=paste("(c3) informed data ",Detection,sep="")
  xlab12=paste("(c4) naïve data ",Detection,sep="")
  xlab13=paste("(d1) informed data ",Detection,sep="")
  xlab14=paste("(d2) naïve data ",Detection,sep="")
  xlab15=paste("(d3) informed data ",Detection,sep="")
  xlab16=paste("(d4) naïve data ",Detection,sep="")
  
  
  #-- autres donnees utiles
  source(file.path("R","Noms-Fichiers.R"))
  
  #-- gestion des fichiers
  if (Detection=="D1") {colonnes<-V_Fichier_D1_GrpA_B}
  if (Detection=="D2") {colonnes<-V_Fichier_D2_GrpA_B}
  if (Detection=="D3") {colonnes<-V_Fichier_D3_GrpA_B}
  if (Detection=="D4") {colonnes<-V_Fichier_D4_GrpA_B}
  if (Detection=="D5") {colonnes<-V_Fichier_D5_GrpA_B}
  
  #-- pour la sauvegarde
  if(couleur_noir=="couleur") {
    saveData<-file.path("Outcome","out-graph",
                        paste("GA_GB_",graph_1,"_",graph_2,"_",graph_3,"_",graph_4,"_",Detection,"_couleur.tiff",sep=""))
  } else {
    saveData<-file.path("Outcome","out-graph",
                        paste("GA_GB_",graph_1,"_",graph_2,"_",graph_3,"_",graph_4,"_",Detection,"_noir_blanc.tiff",sep=""))}
  
  #-- gestion des couleurs 
  if (Detection=="D5") {
    bordure<-c("black","black","black","black","black","black","black","black")
    if (couleur_noir=="couleur") {
      couleur<-c("white","gray","rosybrown1","red","skyblue1","blue","moccasin","orange")
      couleur_Axis<-c("black","red","blue","orange")
    } else {
      couleur<-c("white","gray","white","gray","white","gray","white","gray")
      couleur_Axis<-c("black","black","black","black")}
  } else { 
    bordure<-c(rep(c("black","black"),20))
    if (couleur_noir=="couleur") {
      couleur<-c(rep(c("white","gray"),5),rep(c("rosybrown1","red"),5),
                 rep(c("skyblue1","blue"),5),rep(c("moccasin","orange"),5))
      couleur_Axis<-c("black","red","blue","orange")
    } else {
      couleur<-c(rep(c("white","gray"),20))
      couleur_Axis<-c("black","black","black","black")}
  }
  
  #-- gestion des espaces 
  if (Detection=="D5") {
    espbox<-c(1:2,4:5,7:8,10:11)
  } else
  {espbox<-c(1:10,13:22,25:34,37:46)}
  
  #----------------# 
  # les graphiques #
  #----------------#
  #-- Si ce n'est pas D5
  if (Detection!="D5") {
    tiff(file=saveData, units="in", width = 10.1, height = 15, pointsize = 7,res=300)
    
    layout(matrix(c(1,2,11,12,13,3,4,11,14,15,5,5,11,16,16,
                    6,7,11,17,18,8,9,11,19,20,10,10,11,21,21),ncol=5,byrow=TRUE),
           widths=c(2.5,2.5,0.1,2.5,2.5),heights=c(3.7,3.7,0.1,3.7,3.7,0.1))
    par(bty="o")
    
    #-- graph_1
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G1[,colonnes],
            ylim=ylimG1_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab1,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="a", side=2, line = 3.5, at=ylimG1_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G1[,colonnes],
            ylim=ylimG1_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab2,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G1[,colonnes],
            ylim=ylimG1_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab3,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G1[,colonnes],
            ylim=ylimG1_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab4,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5) 
    
    mtext(text =text_1, side=1, line = 3.5, at=par('usr')[1]-8, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_2
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G2[,colonnes],
            ylim=ylimG2_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab5,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3  ,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="b", side=2, line = 3.5, at=ylimG2_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G2[,colonnes],
            ylim=ylimG2_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab6,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3  ,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G2[,colonnes],
            ylim=ylimG2_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab7,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G2[,colonnes],
            ylim=ylimG2_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab8,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.7) 
    
    mtext(text =text_2, side=1, line = 3.5, at=par('usr')[1]-8, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(v=0,col="black",lty=1,lwd=1.2) 
    
    #-- graph_3
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G3[,colonnes],
            ylim=ylimG3_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab9,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="c", side=2, line = 3.5, at=ylimG3_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G3[,colonnes],
            ylim=ylimG3_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab10,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G3[,colonnes],
            ylim=ylimG3_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab11,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G3[,colonnes],
            ylim=ylimG3_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab12,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5) 
    
    mtext(text =text_3, side=1, line = 3.5, at=par('usr')[1]-8, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_4
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G4[,colonnes],
            ylim=ylimG4_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab13,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="d", side=2, line = 3.5, at=ylimG4_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G4[,colonnes],
            ylim=ylimG4_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab14,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G4[,colonnes],
            ylim=ylimG4_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab15,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G4[,colonnes],
            ylim=ylimG4_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab16,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.7) 
    
    mtext(text =text_4, side=1, line = 3.5, at=par('usr')[1]-8, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    dev.off()
  }
  
  #-- Si c'est D5
  if (Detection=="D5") {
    tiff(file=saveData, units="in", width = 10.1, height = 15, pointsize = 7,res=300)
    
    layout(matrix(c(1,2,11,12,13,3,4,11,14,15,5,5,11,16,16,
                    6,7,11,17,18,8,9,11,19,20,10,10,11,21,21),ncol=5,byrow=TRUE),
           widths=c(2.5,2.5,0.1,2.5,2.5),heights=c(3.7,3.7,0.1,3.7,3.7,0.1))
    par(bty="o")
    
    #-- graph_1
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G1[,colonnes],
            ylim=ylimG1_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab1,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="a", side=2, line = 3.5, at=ylimG1_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G1[,colonnes],
            ylim=ylimG1_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab2,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G1[,colonnes],
            ylim=ylimG1_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab3,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G1[,colonnes],
            ylim=ylimG1_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab4,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5) 
    
    mtext(text =text_1, side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_2
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G2[,colonnes],
            ylim=ylimG2_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab5,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3  ,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="b", side=2, line = 3.5, at=ylimG2_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G2[,colonnes],
            ylim=ylimG2_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab6,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3  ,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G2[,colonnes],
            ylim=ylimG2_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab7,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G2[,colonnes],
            ylim=ylimG2_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab8,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.7) 
    
    mtext(text =text_2, side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(v=0,col="black",lty=1,lwd=1.2) 
    
    #-- graph_3
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G3[,colonnes],
            ylim=ylimG3_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab9,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="c", side=2, line = 3.5, at=ylimG3_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G3[,colonnes],
            ylim=ylimG3_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab10,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G3[,colonnes],
            ylim=ylimG3_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab11,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G3[,colonnes],
            ylim=ylimG3_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab12,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5) 
    
    mtext(text =text_3, side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.7)
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_4
    #- axe 1 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_G4[,colonnes],
            ylim=ylimG4_1,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab13,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    mtext(text ="d", side=2, line = 3.5, at=ylimG4_1[2], las=2 ,cex = 2.2)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 1 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe1_Naive_G4[,colonnes],
            ylim=ylimG4_2,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab14,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis1, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 informed data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_G4[,colonnes],
            ylim=ylimG4_3,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab15,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.5)
    
    #- axe 2 naive data
    par(mar=c(4,5,0.5,1))
    boxplot(M_Grp_A_B_Axe2_Naive_G4[,colonnes],
            ylim=ylimG4_4,
            col=couleur, border=bordure,outline=FALSE,
            cex.axis=1.6,
            xlab=xlab16,
            xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
    title(ylab="axis2, groups A & B",line = 3,cex.lab=2, adj=0.5)
    axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
         col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
         col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
         col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.6)
    axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
         col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.6)
    abline(h=0,col="black",lty=2,lwd=0.7) 
    
    mtext(text =text_4, side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.7)
    
    #-- une marge
    #par(mar=c(0,0,0,0))
    #plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    dev.off()
  }
  
}


################################################################################
################################################################################
