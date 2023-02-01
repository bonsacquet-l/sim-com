#-------------------------------------#
# graphique des RV entre les matrices #
# de distance entre les especes       #
# pour 4 analyses                     #
# sortie en .tiff                     #
# lionel.bonsacquet                   #
#-------------------------------------#

Fct_Graph_RV_Sp_par_4_tiff<-function(Detection="D1",
                                     graph_1="ACP",graph_2="PCoA_bray",
                                     graph_3="PCoA_chao",graph_4="nMDS_chao",
                                     ylimG1=c(0,1),ylimG2=c(0,1),
                                     ylimG3=c(0,1),ylimG4=c(0,1)) {
  
  #-- chargement des donnees pour le graph_1 (ici les coordonnees uniquement)
  if (graph_1=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G1<-M_Resultat_RV_ACP_good
    text_1<-"PCA"
  }
  
  if (graph_1=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G1<-M_Resultat_RV_AFC_good
    text_1<-"CA"
  }
  
  if (graph_1=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G1<-M_Resultat_RV_pcoa_good
    text_1<-graph_1
  }
  
  if (graph_1=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G1<-M_Resultat_RV_pcoa_good
    text_1<-graph_1
  }
  
  if (graph_1=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G1<-M_Resultat_RV_nmds_good
    text_1<-graph_1
  }
  
  if (graph_1=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G1<-M_Resultat_RV_nmds_good
    text_1<-graph_1
  }
  
  #-- chargement des donnees pour le graph_2 (ici les coordonnees uniquement)
  if (graph_2=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G2<-M_Resultat_RV_ACP_good
    text_2<-"PCA"
  }
  
  if (graph_2=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G2<-M_Resultat_RV_AFC_good
    text_2<-"CA"
  }
  
  if (graph_2=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G2<-M_Resultat_RV_pcoa_good
    text_2<-graph_2
  }
  
  if (graph_2=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G2<-M_Resultat_RV_pcoa_good
    text_2<-graph_2
  }
  
  if (graph_2=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G2<-M_Resultat_RV_nmds_good
    text_2<-graph_2
  }
  
  if (graph_2=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G2<-M_Resultat_RV_nmds_good
    text_2<-graph_2
  }

  #-- chargement des donnees pour le graph_3 (ici les coordonnees uniquement)
  if (graph_3=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G3<-M_Resultat_RV_ACP_good
    text_3<-"PCA"
  }
  
  if (graph_3=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G3<-M_Resultat_RV_AFC_good
    text_3<-"CA"
  }
  
  if (graph_3=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G3<-M_Resultat_RV_pcoa_good
    text_3<-graph_3
  }
  
  if (graph_3=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G3<-M_Resultat_RV_pcoa_good
    text_3<-graph_3
  }
  
  if (graph_3=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G3<-M_Resultat_RV_nmds_good
    text_3<-graph_3
  }
  
  if (graph_3=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G3<-M_Resultat_RV_nmds_good
    text_3<-graph_3
  }
  
  #-- chargement des donnees pour le graph_4 (ici les coordonnees uniquement)
  if (graph_4=="ACP"){
    load(file.path("Outcome","out-regroupement","ACP","ACP_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G4<-M_Resultat_RV_ACP_good
    text_4<-"PCA"
  }
  
  if (graph_4=="AFC"){
    load(file.path("Outcome","out-regroupement","AFC","AFC_Regroup_RV_Sp_good.Rdata"))
    M_Resultat_RV_G4<-M_Resultat_RV_AFC_good
    text_4<-"CA"
  }
  
  if (graph_4=="PCoA_bray"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G4<-M_Resultat_RV_pcoa_good
    text_4<-graph_4
  }
  
  if (graph_4=="PCoA_chao"){
    load(file.path("Outcome","out-regroupement","PCoA","PCoA_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G4<-M_Resultat_RV_pcoa_good
    text_4<-graph_4
  }
  
  if (graph_4=="nMDS_bray"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_bray_good.Rdata"))
    M_Resultat_RV_G4<-M_Resultat_RV_nmds_good
    text_4<-graph_4
  }
  
  if (graph_4=="nMDS_chao"){
    load(file.path("Outcome","out-regroupement","nMDS","nMDS_Regroup_RV_Sp_chao_good.Rdata"))
    M_Resultat_RV_G4<-M_Resultat_RV_nmds_good
    text_4<-graph_4
  }
  
  #-- labels des axes x
  xlab1=paste(text_1, " simulation ",Detection,sep="")
  xlab2=paste(text_2," simulation ",Detection,sep="")
  xlab3=paste(text_3, " simulation ",Detection,sep="")
  xlab4=paste(text_4," simulation ",Detection,sep="")
  
  #-- labels des axes y
  ylab1=paste("RV of distance matrices among \n species ",text_1,"_i/",text_1,"_n",sep="")
  ylab2=paste("RV of distance matrices among \n species ",text_2,"_i/",text_2,"_n",sep="")
  ylab3=paste("RV of distance matrices among \n species ",text_3,"_i/",text_3,"_n",sep="")
  ylab4=paste("RV of distance matrices among \n species ",text_4,"_i/",text_4,"_n",sep="")
  
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
                              paste("RV_Sp_",graph_1,"_",graph_2,"_",graph_3,"_",graph_4,"_",Detection,"_couleur.tiff",sep=""))
  saveData_noir<-file.path("Outcome","out-graph",
                           paste("RV_Sp_",graph_1,"_",graph_2,"_",graph_3,"_",graph_4,"_",Detection,"_noir_blanc.tiff",sep=""))
  
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
    tiff(file=saveData_couleur, units="in",width = 5.1, height = 7, pointsize = 9,res=300)
    
    layout(matrix(c(1,5,6,2,5,7,3,5,8,4,5,9),ncol=3,byrow=TRUE),
           widths=c(2.5,0.1,2.5),heights=c(3.7,0.1,3.7,0.1))
    par(bty="o")
    
    #-- graph_1
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_G1[,colonnes],
            ylim=ylimG1,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab1,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab1,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="a", side=2, line = 3.5, at=ylimG1[2], las=2 ,cex = 1.6)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_2
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_G2[,colonnes],
            ylim=ylimG2,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab2,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab2,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1)
    mtext(text ="b", side=2, line = 3.5, at=ylimG2[2], las=2 ,cex = 1.6)
    
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
    boxplot(M_Resultat_RV_G3[,colonnes],
            ylim=ylimG3,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab3,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab3,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="c", side=2, line = 3.5, at=ylimG3[2], las=2 ,cex = 1.6)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_4
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_G4[,colonnes],
            ylim=ylimG4,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab4,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab4,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="d", side=2, line = 3.5, at=ylimG4[2], las=2 ,cex = 1.6)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    dev.off()
  }
  
  #-- Si c'est D5
  if (Detection=="D5") {
    tiff(file=saveData_couleur, units="in",width = 5.1, height = 7, pointsize = 9,res=300)
    
    layout(matrix(c(1,5,6,2,5,7,3,5,8,4,5,9),ncol=3,byrow=TRUE),
           widths=c(2.5,0.1,2.5),heights=c(3.7,0.1,3.7,0.1))
    par(bty="o")
    
    #-- graph_1
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_G1[,colonnes],
            ylim=ylimG1,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab1,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab1,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="a", side=2, line = 3.5, at=ylimG1[2], las=2 ,cex = 1.6)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_2
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_G2[,colonnes],
            ylim=ylimG2,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab2,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab2,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="b", side=2, line = 3.5, at=ylimG2[2], las=2 ,cex = 1.6)
    
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
    boxplot(M_Resultat_RV_G3[,colonnes],
            ylim=ylimG3,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab3,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab3,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="c", side=2, line = 3.5, at=ylimG3[2], las=2 ,cex = 1.6)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
    
    #-- une marge
    par(mar=c(0,0,0,0))
    plot.new()
    abline(h=0,col="black",lty=1,lwd=1) 
    
    #-- graph_4
    par(mar=c(4,6,0.5,0.5))
    boxplot(M_Resultat_RV_G4[,colonnes],
            ylim=ylimG4,
            col=couleur,outline=FALSE,
            cex.axis=1.3,
            xlab=xlab4,
            xaxt="n",lwd=0.5,cex.lab=1.6,at=espbox)
    
    title(ylab=ylab4,line = 2.5,cex.lab=1.6)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1.3)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1.3)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1.3)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1.3)
    mtext(text ="d", side=2, line = 3.5, at=ylimG4[2], las=2 ,cex = 1.6)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    #box(which = "figure",col="black")
  
    dev.off()
  }
  
}


################################################################################
################################################################################
