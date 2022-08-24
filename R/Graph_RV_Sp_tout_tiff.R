#-------------------------------------#
# graphique des RV entre les matrices #
# de distance entre les especes       #
# pour toutes analyses                #
# sortie en .tiff                     #
# lionel.bonsacquet                   #
#-------------------------------------#

Fct_Graph_RV_Sp_tt_tiff<-function(Detection="D1") {
  
  #-- chargement des donnees (ici les coordonnees uniquement)
    load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp_good.Rdata"))
    load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata"))
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata"))
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray_good.Rdata"))

  #-- labels des axes x
  xlab1=paste("PCA simulation ",Detection,sep="")
  xlab2=paste("CA simulation ",Detection,sep="")
  xlab3=paste("PCoA_bray simulation ",Detection,sep="")
  xlab4=paste("nMDS_bray simulation ",Detection,sep="")
  xlab5=paste("PCoA_chao simulation ",Detection,sep="")
  xlab6=paste("nMDS_chao simulation ",Detection,sep="")
  
  #-- labels des axes y
  ylab1<-"RV of distance matrices among \n species PCA_i/PCA_n"
  ylab2<-"RV of distance matrices among \n species CA_i/CA_n"
  ylab3<-"RV of distance matrices among \n species PCoA_bray_i/PCoA_bray_n"
  ylab4<-"RV of distance matrices among \n species nMDS_bray_i/nMDS_bray_n"
  ylab5<-"RV of distance matrices among \n species PCoA_chao_i/PCoA_chao_n"
  ylab6<-"RV of distance matrices among \n species nMDS_chao_i/nMDS_chao_n"
  
  #-- autres donnees utiles
  source(file.path("R","Noms-Fichiers.R"))
  
  #-- gestion des fichiers
  if (Detection=="D1") {colonnes<-V_Fichier_D1}
  if (Detection=="D2") {colonnes<-V_Fichier_D2}
  if (Detection=="D3") {colonnes<-V_Fichier_D3}
  if (Detection=="D4") {colonnes<-V_Fichier_D4}
  if (Detection=="D5") {colonnes<-V_Fichier_D5}
  
  #-- pour la sauvegarde
  saveData_couleur<-file.path("Outcome","out-graph",
                              paste("RV_Sp_tt_",Detection,"_couleur.tiff",sep=""))
  saveData_noir<-file.path("Outcome","out-graph",
                           paste("RV_Sp_tt_",Detection,"_noir_blanc.tiff",sep=""))
  
  #-- gestion des couleurs 
  if (Detection=="D5") {
    couleur<-c("gray","red","blue","orange")
    noirblanc<-c("white","white","white","white")
  } else
  {couleur<-c(rep("gray",5),rep("red",5),rep("blue",5),rep("orange",5))
  noirblanc<-c(rep("white",5),rep("white",5),rep("white",5),rep("white",5))}
  
  #-- gestion des espaces 
  if (Detection=="D5") {
    espbox<-c(1,3,5,7)
  } else
  {espbox<-c(1:5,8:12,15:19,22:26)}
  
  #----------------# 
  # les graphiques #
  #----------------#
  ##-- COULEURS 
  #-- Si ce n'est pas D5
  if (Detection!="D5") {
    tiff(file=saveData_couleur, units="in",width = 7.7, height = 7.6, pointsize = 7,res=300)
    
    layout(matrix(c(1,5,6,10,11,2,5,7,10,12,3,5,8,10,13,4,5,9,10,14),ncol=5,byrow=TRUE),
           widths=c(2.5,0.1,2.5,0.1,2.5),heights=c(3.7,0.1,3.7,0.1))
    par(bty="o")
    
    #-- graph_1
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_ACP_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab1,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab1,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="a", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_2
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_AFC_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab2,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab2,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1)
    mtext(text ="b", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(v=0,col="black",lty=1,lwd=1.2) 
    
    #-- graph_3
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_pcoa_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab3,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab3,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="c", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_4
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_nmds_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab4,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab4,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="d", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao_good.Rdata"))
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata"))
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(v=0,col="black",lty=1,lwd=1.2) 
    
    #-- graph_5
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_pcoa_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab5,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab5,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="e", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_6
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_nmds_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab6,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab6,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="f", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    dev.off()
  }
  
  #-- chargement des donnees (ici les coordonnees uniquement)
  load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata"))
  load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray_good.Rdata"))
  
  #-- Si c'est D5
  if (Detection=="D5") {
    tiff(file=saveData_couleur, units="in",width = 7.7, height = 7.6, pointsize = 7,res=300)
    
    layout(matrix(c(1,5,6,10,11,2,5,7,10,12,3,5,8,10,13,4,5,9,10,14),ncol=5,byrow=TRUE),
           widths=c(2.5,0.1,2.5,0.1,2.5),heights=c(3.7,0.1,3.7,0.1))
    par(bty="o")
    par(bty="o")
    
    #-- graph_1
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_ACP_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab1,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab1,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="a", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_2
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_AFC_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab2,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab2,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="b", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(v=0,col="black",lty=1,lwd=1.2) 
    
    #-- graph_3
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_pcoa_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab3,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab3,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="c", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_4
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_nmds_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab4,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab4,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="d", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao_good.Rdata"))
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata"))
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    #abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(v=0,col="black",lty=1,lwd=1.2) 
    
    #-- graph_5
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_pcoa_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab5,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab5,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="e", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_6
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_nmds_good[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab6,
            xaxt="n",lwd=0.5,cex.lab=1.8,at=espbox)
    
    title(ylab=ylab6,line = 2.5,cex.lab=1.8)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="f", side=2, line = 3.5, at=1, las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    dev.off()
  }
  
}


################################################################################
################################################################################
