#-------------------------------------#
# graphique des inertie expliquees    #
# par le plan 1 des deux analyses     #
# ACP et AFC                          #
# lionel.bonsacquet                   #
#-------------------------------------#

Fct_Graph_Inertia_Plan1<-function(Detection="D1") {
  #-- chargement des donnees (ici les coordonnees uniquement)
  load(file.path("Outcome","out-regroupement","ACP","ACP_Inertia_Plan1.Rdata"))
  load(file.path("Outcome","out-regroupement","AFC","AFC_Inertia_Plan1.Rdata"))
  
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
                              paste("Explained_Inertia_Informed_",Detection,"_couleur.tiff",sep=""))
  saveData_noir<-file.path("Outcome","out-graph",
                           paste("Explained_Inertia_Informed",Detection,"_noir_blanc.tiff",sep=""))
  
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
    tiff(file=saveData_couleur, units="in",width = 5, height = 3.5, pointsize = 9,res=300)
    
    layout(matrix(c(1,2),ncol=2,byrow=TRUE))
    par(bty="o")
    
    #-- ACP
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_ACP[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
            cex.axis=1,
            xlab=paste("PCA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1,at=espbox)
    
    title(ylab="Explained inertia (PCA)",line = 2.5,cex.lab=1)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1)
    mtext(text ="a", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 1.5)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    #-- AFC
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_AFC[,colonnes],
            ylim=c(0,1.0),
            col=couleur,outline=FALSE,
           cex.axis=1,
            xlab=paste("CA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1,at=espbox)
    
    title(ylab="Explained inertia (CA)",line = 2.5,cex.lab=1)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1)
    mtext(text ="b", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 1.5)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    dev.off()
  }
  
  #-- Si c'est D5
  if (Detection=="D5") {
    tiff(file=saveData_couleur, units="in",width = 2.5, height = 7, pointsize = 9,res=600)
    
    layout(matrix(c(1,2),ncol=1,byrow=TRUE))
    par(bty="o")
    
    #-- ACP
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_ACP[,colonnes],
            ylim=c(0,1),
            col=couleur,outline=FALSE,
           cex.axis=1.3,
            xlab=paste("PCA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
    
    title(ylab="Explained inertia (PCA)",line = 2.5,cex.lab=1.3)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1)
    mtext(text ="a", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    #-- AFC
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_AFC[,colonnes],
            ylim=c(0,1.0),
            col=couleur,outline=FALSE,
           cex.axis=1.3,
            xlab=paste("CA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
    
    title(ylab="Explained inertia (CA)",line = 2.5,cex.lab=1.3)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
         col.axis="red",lwd=1,cex.axis=1)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
         col.axis="blue",lwd=1,cex.axis=1)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
         col.axis="orange",lwd=1,cex.axis=1)
    mtext(text ="b", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    dev.off()
  }
  
  ##-- NOIR ET BLANC 
  #-- Si ce n'est pas D5
  if (Detection!="D5") {
    tiff(file=saveData_couleur, units="in",width = 2.5, height = 7, pointsize = 9,res=300)
    
    layout(matrix(c(1,2),ncol=1,byrow=TRUE))
    par(bty="o")
    
    #-- ACP
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_ACP[,colonnes],
            ylim=c(0,1),
            col=noirblanc,outline=FALSE,
           cex.axis=1.3,
            xlab=paste("PCA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
    
    title(ylab="Explained inertia (PCA)",line = 2,cex.lab=1.3)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    mtext(text ="a", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    #-- AFC
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_AFC[,colonnes],
            ylim=c(0,1.0),
            col=noirblanc,outline=FALSE,
           cex.axis=1.3,
            xlab=paste("CA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
    
    title(ylab="Explained inertia (CA)",line = 2,cex.lab=1.3)
    axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    mtext(text ="b", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 2)
    
    abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    dev.off()
  }
  
  #-- Si c'est D5
  if (Detection=="D5") {
    tiff(file=saveData_couleur, units="in",width = 2.5, height = 7, pointsize = 9,res=300)
    
    layout(matrix(c(1,2),ncol=2,byrow=TRUE))
    par(bty="o")
    
    #-- ACP
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_ACP[,colonnes],
            ylim=c(0,1),
            col=noirblanc,outline=FALSE,
           cex.axis=1.3,
            xlab=paste("PCA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
    
    title(ylab="Explained inertia (PCA)",line = 2,cex.lab=1.3)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    mtext(text ="a", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    #-- AFC
    par(mar=c(4,4,0.5,0.5))
    boxplot(M_Inertia_Plan1_AFC[,colonnes],
            ylim=c(0,1.0),
            col=noirblanc,outline=FALSE,
           cex.axis=1.3,
            xlab=paste("CA simulation ",Detection,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
    
    title(ylab="Explained inertia (CA)",line = 2,cex.lab=1.3)
    axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(2,3,4),labels = c("","C2",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(4,5,6),labels = c("","C3",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="black",
         col.axis="black",lwd=1,cex.axis=1)
    mtext(text ="b", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 2)
    
    abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
    abline(h=0,col="black",lty=2,lwd=0.5)
    box(which = "figure",col="black")
    
    dev.off()
  }  
}


################################################################################
################################################################################
