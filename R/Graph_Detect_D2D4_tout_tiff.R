#----------------------------------------#
# graphique des groupes d'espece plus et #
# moins detectable (More/Less)           #
# pour les ACP et les AFC                #
# sortie en .tiff                        #
# lionel.bonsacquet                      #
#----------------------------------------#

Fct_Graph_Detect_D2D4_tout_tiff<-function(Detection="D4",couleur_noir="couleur",
                                     ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                                     ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                                     ylimAFC1=c(-1.2,1.5),ylimAFC2=c(-0.8,0.8),
                                     ylimAFC3=c(-1.2,1.5),ylimAFC4=c(-0.8,0.8),
                                     ylimPCoAbray1=c(-0.05,0.05),ylimPCoAbray2=c(-0.05,0.05),
                                     ylimPCoAbray3=c(-0.05,0.05),ylimPCoAbray4=c(-0.05,0.05),
                                     ylimnMDSbray1=c(-0.25,0.25),ylimnMDSbray2=c(-0.25,0.25),
                                     ylimnMDSbray3=c(-0.25,0.25),ylimnMDSbray4=c(-0.25,0.25),
                                     ylimPCoAchao1=c(-0.05,0.05),ylimPCoAchao2=c(-0.05,0.05),
                                     ylimPCoAchao3=c(-0.05,0.05),ylimPCoAchao4=c(-0.05,0.05),
                                     ylimnMDSchao1=c(-0.25,0.25),ylimnMDSchao2=c(-0.25,0.25),
                                     ylimnMDSchao3=c(-0.25,0.25),ylimnMDSchao4=c(-0.25,0.25)) {
  
  #-- chargement des donnees (ici les coordonnees uniquement) (autres chargements plus loin)
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_good.Rdata"))
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_good.Rdata"))
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_good.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_Detect_good.Rdata"))

  #-- autres donnees utiles
  source(file.path("R","Noms-Fichiers.R"))
  
  #-- gestion des fichiers
  if (Detection=="D2") {colonnes<-V_Fichier_D2_GrpDetect}
  if (Detection=="D4") {colonnes<-V_Fichier_D4_GrpDetect}
  
  #-- pour la sauvegarde
  if(couleur_noir=="couleur") {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_tt_",Detection,"_couleur.tiff",sep=""))
  } else {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_tt_",Detection,"_noir_blanc.tiff",sep=""))}
  
  #-- gestion des couleurs 
  bordure<-c(rep(c("black","black"),20))
  if (couleur_noir=="couleur") {
    couleur<-c(rep(c("white","gray"),5),rep(c("rosybrown1","red"),5),
               rep(c("skyblue1","blue"),5),rep(c("moccasin","orange"),5))
    couleur_Axis<-c("black","red","blue","orange")
  } else {
    couleur<-c(rep(c("white","gray"),20))
    couleur_Axis<-c("black","black","black","black")}
  
  
  #-- gestion des espaces 
  espbox<-c(1:10,13:22,25:34,37:46)
  
  #----------------# 
  # les graphiques #
  #----------------#
  tiff(file=saveData, units="in", width = 15, height = 15, pointsize = 7,res=300)
  
  layout(matrix(c(1,2,11,12,13,22,23,24,3,4,11,14,15,22,25,26,5,5,11,16,16,22,27,27,
                  6,7,11,17,18,22,28,29,8,9,11,19,20,22,30,31,10,10,11,21,21,22,32,32),ncol=8,byrow=TRUE),
         widths=c(2.5,2.5,0.1,2.5,2.5,0.1,2.5,2.5),heights=c(3.7,3.7,0.1,3.7,3.7,0.1))
  par(bty="o")
  
  #-- ACP
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_ACP_good[,colonnes],
          ylim=ylimACP1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a1) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  mtext(text ="a", side=2, line = 3.5, at=ylimACP1[2], las=2 ,cex = 2)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_ACP_Naive_good[,colonnes],
          ylim=ylimACP2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a2) naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_ACP_good[,colonnes],
          ylim=ylimACP3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a3) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_ACP_Naive_good[,colonnes],
          ylim=ylimACP4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a4) naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5) 
  
  mtext(text ="PCA", side=1, line = 3.5, at=-7, las=1 ,cex = 1.5)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- AFC
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_AFC_good[,colonnes],
          ylim=ylimAFC1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b1) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  mtext(text ="b", side=2, line = 3.5, at=ylimAFC1[2], las=2 ,cex = 2)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_AFC_Naive_good[,colonnes],
          ylim=ylimAFC2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b2) naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_AFC_good[,colonnes],
          ylim=ylimAFC3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b3) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_AFC_Naive_good[,colonnes],
          ylim=ylimAFC4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b4) naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.7) 
  
  mtext(text ="CA", side=1, line = 3.5, at=-7, las=1 ,cex = 1.5)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  #abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(v=0,col="black",lty=1,lwd=1.2) 
  
  #-- PCoA_bray
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_good[,colonnes],
          ylim=ylimPCoAbray1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a1) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  mtext(text ="c", side=2, line = 3.5, at=ylimPCoAbray1[2], las=2 ,cex = 2)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAbray2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a2) naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_pcoa_good[,colonnes],
          ylim=ylimPCoAbray3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a3) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAbray4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a4) naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5) 
  
  mtext(text ="PCoA_Bray", side=1, line = 3.5, at=-7, las=1 ,cex = 1.5)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- nMDS_bray
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_good[,colonnes],
          ylim=ylimnMDSbray1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b1) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  mtext(text ="d", side=2, line = 3.5, at=ylimnMDSbray1[2], las=2 ,cex = 2)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSbray2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b2) naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_nmds_good[,colonnes],
          ylim=ylimnMDSbray3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b3) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSbray4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b4) naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.7) 
  
  mtext(text ="nMDS_Bray", side=1, line = 3.5, at=-7, las=1 ,cex = 1.5)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  #abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(v=0,col="black",lty=1,lwd=1.2) 
  
  #-- PCoA_Chao
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_Detect_good.Rdata"))
  
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_good[,colonnes],
          ylim=ylimPCoAchao1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a1) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  mtext(text ="e", side=2, line = 3.5, at=ylimPCoAchao1[2], las=2 ,cex = 2)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAchao2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a2) naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_pcoa_good[,colonnes],
          ylim=ylimPCoAchao3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a3) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAchao4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(a4) naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5) 
  
  mtext(text ="PCoA_Chao", side=1, line = 3.5, at=-7, las=1 ,cex = 1.5)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- nMDS_chao
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_good.Rdata"))
  
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_good[,colonnes],
          ylim=ylimnMDSchao1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b1) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  mtext(text ="f", side=2, line = 3.5, at=ylimnMDSchao1[2], las=2 ,cex = 2)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSchao2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b2) naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis1, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_nmds_good[,colonnes],
          ylim=ylimnMDSchao3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b3) informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSchao4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1.5,
          xlab=paste("(b4) naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=2,at=espbox)
  title(ylab="axis2, more/less detectable",line = 3,cex.lab=2.2, adj=0.5)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1.5)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1.5)
  abline(h=0,col="black",lty=2,lwd=0.7) 
  
  mtext(text ="nMDS_Chao", side=1, line = 3.5, at=-7, las=1 ,cex = 1.5)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  #abline(h=0,col="black",lty=1,lwd=1) 
   
  dev.off()
  
}


################################################################################
################################################################################
