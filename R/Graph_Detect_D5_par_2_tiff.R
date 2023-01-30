#----------------------------------------#
# graphique des groupes d'espece plus et #
# moins detectables (more/less)          #
# pour 2 analyses                        #
# lionel.bonsacquet                      #
#----------------------------------------#

Fct_Graph_Detect_D5_par_2_tiff<-function(Detection="D5",couleur_noir="couleur",
                              graph_1="PCoA_bray",graph_2="nMDS_chao",
                              ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.05,0.05),
                              ylimG1_3=c(-0.4,0.4),ylimG1_4=c(-0.15,0.15),
                              ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.05,0.05),
                              ylimG2_3=c(-0.4,0.4),ylimG2_4=c(-0.15,0.15)) {
  
  #-- chargement des donnees pour le graph_1 (ici les coordonnees uniquement)
  if (graph_1=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G1<-M_Grp_Detect_Axe1_ACP_good
    M_Grp_Detect_Axe1_Naive_G1<-M_Grp_Detect_Axe1_ACP_Naive_good
    M_Grp_Detect_Axe2_G1<-M_Grp_Detect_Axe2_ACP_good
    M_Grp_Detect_Axe2_Naive_G1<-M_Grp_Detect_Axe2_ACP_Naive_good
    text_1<-"PCA"
  }
  
  if (graph_1=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G1<-M_Grp_Detect_Axe1_AFC_good
    M_Grp_Detect_Axe1_Naive_G1<-M_Grp_Detect_Axe1_AFC_Naive_good
    M_Grp_Detect_Axe2_G1<-M_Grp_Detect_Axe2_AFC_good
    M_Grp_Detect_Axe2_Naive_G1<-M_Grp_Detect_Axe2_AFC_Naive_good
    text_1<-"CA"
  }
  
  if (graph_1=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G1<-M_Grp_Detect_Axe1_pcoa_good
    M_Grp_Detect_Axe1_Naive_G1<-M_Grp_Detect_Axe1_pcoa_Naive_good
    M_Grp_Detect_Axe2_G1<-M_Grp_Detect_Axe2_pcoa_good
    M_Grp_Detect_Axe2_Naive_G1<-M_Grp_Detect_Axe2_pcoa_Naive_good
    text_1<-graph_1
  }
  
  if (graph_1=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G1<-M_Grp_Detect_Axe1_pcoa_good
    M_Grp_Detect_Axe1_Naive_G1<-M_Grp_Detect_Axe1_pcoa_Naive_good
    M_Grp_Detect_Axe2_G1<-M_Grp_Detect_Axe2_pcoa_good
    M_Grp_Detect_Axe2_Naive_G1<-M_Grp_Detect_Axe2_pcoa_Naive_good
    text_1<-graph_1
  }
  
  if (graph_1=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G1<-M_Grp_Detect_Axe1_nmds_good
    M_Grp_Detect_Axe1_Naive_G1<-M_Grp_Detect_Axe1_nmds_Naive_good
    M_Grp_Detect_Axe2_G1<-M_Grp_Detect_Axe2_nmds_good
    M_Grp_Detect_Axe2_Naive_G1<-M_Grp_Detect_Axe2_nmds_Naive_good
    text_1<-graph_1
  }
  
  if (graph_1=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G1<-M_Grp_Detect_Axe1_nmds_good
    M_Grp_Detect_Axe1_Naive_G1<-M_Grp_Detect_Axe1_nmds_Naive_good
    M_Grp_Detect_Axe2_G1<-M_Grp_Detect_Axe2_nmds_good
    M_Grp_Detect_Axe2_Naive_G1<-M_Grp_Detect_Axe2_nmds_Naive_good
    text_1<-graph_1
  }
  
  #-- chargement des donnees pour le graph_2 (ici les coordonnees uniquement)
  if (graph_2=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G2<-M_Grp_Detect_Axe1_ACP_good
    M_Grp_Detect_Axe1_Naive_G2<-M_Grp_Detect_Axe1_ACP_Naive_good
    M_Grp_Detect_Axe2_G2<-M_Grp_Detect_Axe2_ACP_good
    M_Grp_Detect_Axe2_Naive_G2<-M_Grp_Detect_Axe2_ACP_Naive_good
    text_2<-"PCA"
  }
  
  if (graph_2=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G2<-M_Grp_Detect_Axe1_AFC_good
    M_Grp_Detect_Axe1_Naive_G2<-M_Grp_Detect_Axe1_AFC_Naive_good
    M_Grp_Detect_Axe2_G2<-M_Grp_Detect_Axe2_AFC_good
    M_Grp_Detect_Axe2_Naive_G2<-M_Grp_Detect_Axe2_AFC_Naive_good
    text_2<-"CA"
  }
  
  if (graph_2=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G2<-M_Grp_Detect_Axe1_pcoa_good
    M_Grp_Detect_Axe1_Naive_G2<-M_Grp_Detect_Axe1_pcoa_Naive_good
    M_Grp_Detect_Axe2_G2<-M_Grp_Detect_Axe2_pcoa_good
    M_Grp_Detect_Axe2_Naive_G2<-M_Grp_Detect_Axe2_pcoa_Naive_good
    text_2<-graph_2
  }
  
  if (graph_2=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G2<-M_Grp_Detect_Axe1_pcoa_good
    M_Grp_Detect_Axe1_Naive_G2<-M_Grp_Detect_Axe1_pcoa_Naive_good
    M_Grp_Detect_Axe2_G2<-M_Grp_Detect_Axe2_pcoa_good
    M_Grp_Detect_Axe2_Naive_G2<-M_Grp_Detect_Axe2_pcoa_Naive_good
    text_2<-graph_2
  }
  
  if (graph_2=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G2<-M_Grp_Detect_Axe1_nmds_good
    M_Grp_Detect_Axe1_Naive_G2<-M_Grp_Detect_Axe1_nmds_Naive_good
    M_Grp_Detect_Axe2_G2<-M_Grp_Detect_Axe2_nmds_good
    M_Grp_Detect_Axe2_Naive_G2<-M_Grp_Detect_Axe2_nmds_Naive_good
    text_2<-graph_2
  }
  
  if (graph_2=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_D5_good.Rdata"))
    M_Grp_Detect_Axe1_G2<-M_Grp_Detect_Axe1_nmds_good
    M_Grp_Detect_Axe1_Naive_G2<-M_Grp_Detect_Axe1_nmds_Naive_good
    M_Grp_Detect_Axe2_G2<-M_Grp_Detect_Axe2_nmds_good
    M_Grp_Detect_Axe2_Naive_G2<-M_Grp_Detect_Axe2_nmds_Naive_good
    text_2<-graph_2
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
  
  #-- autres donnees utiles
  source(file.path("R","Noms-Fichiers.R"))
  
  #-- gestion des fichiers
  
  if (Detection=="D5") {colonnes<-V_Fichier_D5_GrpDetect}
  
  #-- pour la sauvegarde
  if(couleur_noir=="couleur") {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_",graph_1,"_",graph_2,"_",Detection,"_couleur.tiff",sep=""))
  } else {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_",graph_1,"_",graph_2,"_",Detection,"_noir_blanc.tiff",sep=""))}
  
  #-- gestion des couleurs 
  bordure<-c("black","black","black","black","black","black","black","black")
  if (couleur_noir=="couleur") {
    couleur<-c("white","gray","rosybrown1","red","skyblue1","blue","moccasin","orange")
    couleur_Axis<-c("black","red","blue","orange")
  } else {
    couleur<-c("white","gray","white","gray","white","gray","white","gray")
    couleur_Axis<-c("black","black","black","black")}
  
  #-- gestion des espaces 
  espbox<-c(1:2,4:5,7:8,10:11)
  
  #----------------# 
  # les graphiques #
  #----------------#
  tiff(file=saveData, units="in", width = 3.5, height = 15, pointsize = 7,res=300)
  #pdf(file=saveData, width = 3.5, height = 6.1, pointsize = 7)
  
  layout(matrix(c(1,2,3,4,5,5,6,7,8,9,10,10),ncol=2,byrow=TRUE),
         widths=c(2.5,2.5),heights=c(3.7,3.7,0.1,3.7,3.7,0.1))
  par(bty="o")
  
  #-- graph_1
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_G1[,colonnes],
          ylim=ylimG1_1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab1,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  mtext(text ="a", side=2, line = 3.5, at=ylimG1_1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_Naive_G1[,colonnes],
          ylim=ylimG1_2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab2,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_G1[,colonnes],
          ylim=ylimG1_3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab3,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_Naive_G1[,colonnes],
          ylim=ylimG1_4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab4,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5) 
  
  mtext(text =text_1, side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- graph_2
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_G2[,colonnes],
          ylim=ylimG2_1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab5,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  mtext(text ="b", side=2, line = 3.5, at=ylimG2_1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_Naive_G2[,colonnes],
          ylim=ylimG2_2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab6,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_G2[,colonnes],
          ylim=ylimG2_3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab7,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_Naive_G2[,colonnes],
          ylim=ylimG2_4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=xlab8,
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0.5)
  axis(1,at=c(0.5,1.75,3),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(3,4.5,6),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(6,7.5,9),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(9,10.25,11.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.7) 
  
  mtext(text =text_2, side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
  dev.off()
}


################################################################################
################################################################################
