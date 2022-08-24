#----------------------------------------#
# graphique des groupes d'espece plus et #
# moins detectable (More/Less)           #
# pour les ACP et les AFC                #
# sortie en .tiff                        #
# lionel.bonsacquet                      #
#----------------------------------------#

Fct_Graph_Detect_D5_tout_tiff<-function(Detection="D5",couleur_noir="couleur",
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
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect_D5_good.Rdata"))
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect_D5_good.Rdata"))
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_bray_Grp_Detect_D5_good.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_bray_Grp_Detect_D5_good.Rdata"))
  
  #-- autres donnees utiles
  source(file.path("R","Noms-Fichiers.R"))
  
  #-- gestion des fichiers
  #-- gestion des fichiers
  
  if (Detection=="D5") {colonnes<-V_Fichier_D5_GrpDetect}
  
  #-- pour la sauvegarde
  if(couleur_noir=="couleur") {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_tt_",Detection,"_couleur.tiff",sep=""))
  } else {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_",Detection,"_noir_blanc.tiff",sep=""))}
  
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
  tiff(file=saveData, units="in", width = 15, height = 15, pointsize = 7,res=300)
  
  layout(matrix(c(1,2,11,12,21,22,3,4,13,14,23,24,5,5,15,15,25,25,
                  6,7,16,17,26,27,8,9,18,19,28,29,10,10,20,20,30,30),ncol=6,byrow=TRUE),
         widths=c(2.5,2.5,2.5,2.5,2.5,2.5),heights=c(3.7,3.7,0.1,3.7,3.7,0.1))
  par(bty="o")
  
  #-- ACP
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_ACP_good[,colonnes],
          ylim=ylimACP1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a1) informed data ",Detection,sep=""),
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
  mtext(text ="a", side=2, line = 3.5, at=ylimACP1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_ACP_Naive_good[,colonnes],
          ylim=ylimACP2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a2) naïve data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_ACP_good[,colonnes],
          ylim=ylimACP3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a3) informed data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_ACP_Naive_good[,colonnes],
          ylim=ylimACP4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a4) naive data ",Detection,sep=""),
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
  
  mtext(text ="PCA", side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
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
          cex.axis=1,
          xlab=paste("(b1) informed data ",Detection,sep=""),
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
  mtext(text ="b", side=2, line = 3.5, at=ylimAFC1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_AFC_Naive_good[,colonnes],
          ylim=ylimAFC2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b2) naïve data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_AFC_good[,colonnes],
          ylim=ylimAFC3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b3) informed data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_AFC_Naive_good[,colonnes],
          ylim=ylimAFC4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b4) naive data ",Detection,sep=""),
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
  
  mtext(text ="CA", side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- PCoA_bray
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_good[,colonnes],
          ylim=ylimPCoAbray1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a1) informed data ",Detection,sep=""),
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
  mtext(text ="c", side=2, line = 3.5, at=ylimPCoAbray1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAbray2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a2) naïve data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_pcoa_good[,colonnes],
          ylim=ylimPCoAbray3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a3) informed data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAbray4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a4) naive data ",Detection,sep=""),
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
  
  mtext(text ="PCoA_Bray", side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
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
          cex.axis=1,
          xlab=paste("(b1) informed data ",Detection,sep=""),
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
  mtext(text ="d", side=2, line = 3.5, at=ylimnMDSbray1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSbray2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b2) naïve data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_nmds_good[,colonnes],
          ylim=ylimnMDSbray3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b3) informed data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSbray4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b4) naive data ",Detection,sep=""),
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
  
  mtext(text ="nMDS_Bray", side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- PCoA_Chao
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_chao_Grp_Detect_D5_good.Rdata"))
  
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_good[,colonnes],
          ylim=ylimPCoAchao1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a1) informed data ",Detection,sep=""),
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
  mtext(text ="e", side=2, line = 3.5, at=ylimPCoAchao1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAchao2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a2) naïve data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_pcoa_good[,colonnes],
          ylim=ylimPCoAchao3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a3) informed data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_pcoa_Naive_good[,colonnes],
          ylim=ylimPCoAchao4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a4) naive data ",Detection,sep=""),
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
  
  mtext(text ="PCoA_Chao", side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- nMDS_chao
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_chao_Grp_Detect_D5_good.Rdata"))
  
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_good[,colonnes],
          ylim=ylimnMDSchao1,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b1) informed data ",Detection,sep=""),
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
  mtext(text ="f", side=2, line = 3.5, at=ylimnMDSchao1[2], las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSchao2,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b2) naïve data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_nmds_good[,colonnes],
          ylim=ylimnMDSchao3,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b3) informed data ",Detection,sep=""),
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
  boxplot(M_Grp_Detect_Axe2_nmds_Naive_good[,colonnes],
          ylim=ylimnMDSchao4,
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b4) naive data ",Detection,sep=""),
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
  
  mtext(text ="nMDS_Chao", side=1, line = 3.5, at=par('usr')[1]-2, las=1 ,cex = 1.3)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  dev.off()
  
}


################################################################################
################################################################################
