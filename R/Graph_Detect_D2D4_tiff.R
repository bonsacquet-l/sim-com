#----------------------------------------#
# graphique des groupes d'espece plus et #
# moins detectable (More/Less)           #
# pour les ACP et les AFC                #
# sortie en .tiff                        #
# lionel.bonsacquet                      #
#----------------------------------------#

Fct_Graph_Detect_D2D4_tiff<-function(Detection="D2",couleur_noir="couleur") {
  #-- chargement des donnees (ici les coordonnees uniquement)
  load(file.path("Outcome","out-regroupement","ACP","ACP_Grp_Detect.Rdata"))
  load(file.path("Outcome","out-regroupement","AFC","AFC_Grp_Detect.Rdata"))
  
  #-- autres donnees utiles
  source(file.path("R","Noms-Fichiers.R"))
  
  #-- gestion des fichiers
  if (Detection=="D2") {colonnes<-V_Fichier_D2_GrpDetect}
  if (Detection=="D4") {colonnes<-V_Fichier_D4_GrpDetect}
  
  #-- pour la sauvegarde
  if(couleur_noir=="couleur") {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_",Detection,"_couleur.tiff",sep=""))
  } else {
    saveData<-file.path("Outcome","out-graph",
                        paste("MoreLess_Detect_",Detection,"_noir_blanc.tiff",sep=""))}
  
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
  tiff(file=saveData, units="in", width = 3.5, height = 6.1, pointsize = 7,res=300)
  
  layout(matrix(c(1,2,3,4,5,5,6,7,8,9,10,10),ncol=2,byrow=TRUE),
         widths=c(2.5,2.5),heights=c(3.7,3.7,0.1,3.7,3.7,0.1))
  par(bty="o")
  
  #-- ACP
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_ACP[,colonnes],
          ylim=c(-1,1),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a1) PCA on informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  mtext(text ="a", side=2, line = 3.5, at=par('usr')[4]-0.02, las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_ACP_Naive[,colonnes],
          ylim=c(-1,1),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a2) PCA on naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_ACP[,colonnes],
          ylim=c(-1,1),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a3) PCA on informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_ACP_Naive[,colonnes],
          ylim=c(-1,1),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(a4) PCA on naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5) 
  
  mtext(text ="PCA", side=1, line = 3.5, at=par('usr')[1]-10, las=1 ,cex = 1.3)
  
  #-- une marge
  par(mar=c(0,0,0,0))
  plot.new()
  abline(h=0,col="black",lty=1,lwd=1) 
  
  #-- AFC
  #- axe 1 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_AFC[,colonnes],
          ylim=c(-1.2,1.5),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b1) CA on informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  mtext(text ="b", side=2, line = 3.5, at=par('usr')[4]-0.05, las=2 ,cex = 1.5)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 1 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe1_AFC_Naive[,colonnes],
          ylim=c(-1.2,1.5),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b2) CA on naïve data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis1, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 informed data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_AFC[,colonnes],
          ylim=c(-0.8,0.8),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b3) CA on informed data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.5)
  
  #- axe 2 naive data
  par(mar=c(4,5,0.5,1))
  boxplot(M_Grp_Detect_Axe2_AFC_Naive[,colonnes],
          ylim=c(-0.8,0.8),
          col=couleur, border=bordure,outline=FALSE,
          cex.axis=1,
          xlab=paste("(b4) CA on naive data ",Detection,sep=""),
          xaxt="n",lwd=0.5,cex.lab=1.5,at=espbox)
  title(ylab="axis2, more/less detectable",line = 2,cex.lab=1.5, adj=0)
  axis(1,at=c(0.5,6,11.5),labels = c("","C1",""),col=couleur_Axis[1],
       col.axis=couleur_Axis[1],lwd=0.7,cex.axis=1)
  axis(1,at=c(11.5,17.5,23.5),labels = c("","C2",""),col=couleur_Axis[2],
       col.axis=couleur_Axis[2],lwd=0.7,cex.axis=1)
  axis(1,at=c(23.5,29.5,35.5),labels = c("","C3",""),col=couleur_Axis[3],
       col.axis=couleur_Axis[3],lwd=0.7,cex.axis=1)
  axis(1,at=c(35.5,41,46.5),labels = c("","C4",""),col=couleur_Axis[4],
       col.axis=couleur_Axis[4],lwd=0.7,cex.axis=1)
  abline(h=0,col="black",lty=2,lwd=0.7) 
  
  mtext(text ="CA", side=1, line = 3.5, at=par('usr')[1]-10, las=1 ,cex = 1.3)
  
  dev.off()
  
}


################################################################################
################################################################################
