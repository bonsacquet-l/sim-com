    #-------------------------------------#
    # graphique des RV entre les matrices #
    # de distance entre les especes       #
    # pour 2 analyses                     #
    # sortie en .tiff                     #
    # lionel.bonsacquet                   #
    #-------------------------------------#
    
    Fct_Graph_RV_Sp_par_2_tiff<-function(Detection="D1",
                                         graph_1="PCoA_bray",graph_2="nMDS_bray",
                                         ylimG1=c(0,1),ylimG2=c(0,1)) {
      
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
      
      #-- labels des axes x
      xlab1=paste(text_1, " simulation ",Detection,sep="")
      xlab2=paste(text_2," simulation ",Detection,sep="")
      
      #-- labels des axes y
      ylab1=paste("RV of distance matrices among \n species ",text_1,"_i/",text_1,"_n",sep="")
      ylab2=paste("RV of distance matrices among \n species ",text_2,"_i/",text_2,"_n",sep="")
      
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
                                  paste("RV_Sp_",Detection,"_",graph_1,"_",graph_2,"_couleur_L.tiff",sep=""))
      saveData_noir<-file.path("Outcome","out-graph",
                               paste("RV_Sp_",Detection,"_",graph_1,"_",graph_2,"_noir_blanc_L.tiff",sep=""))
      
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
        tiff(file=saveData_couleur, units="in", width = 5, height = 3.7, pointsize = 10,res=600)
        
        layout(matrix(c(1,2,3),ncol=3,byrow=TRUE),
               widths=c(2.4,0.1,2.4),heights=c(3.7))
        par(bty="o")
        
        #-- graph_1
        par(mar=c(4,6,0.5,0.5))
        boxplot(M_Resultat_RV_G1[,colonnes],
                ylim=ylimG1,
                col=couleur,outline=FALSE,
                cex.axis=1,
                xlab=xlab1,
                xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
        
        title(ylab=ylab1,line = 2.5,cex.lab=1.3)
        axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
             col.axis="black",lwd=1,cex.axis=1)
        axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
             col.axis="red",lwd=1,cex.axis=1)
        axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
             col.axis="blue",lwd=1,cex.axis=1)
        axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
             col.axis="orange",lwd=1,cex.axis=1)
        mtext(text ="a", side=2, line = 3.5, at=ylimG1[2], las=2 ,cex = 1.3)
        
        abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
        abline(h=0,col="black",lty=2,lwd=0.5)
        #box(which = "figure",col="black")
        
        #-- une marge
        par(mar=c(0,0,0,0))
        plot.new()
        abline(v=0,col="black",lty=1,lwd=1) 
        
        #-- graph_2
        par(mar=c(4,6,0.5,0.5))
        boxplot(M_Resultat_RV_G2[,colonnes],
                ylim=ylimG2,
                col=couleur,outline=FALSE,
                cex.axis=1,
                xlab=xlab2,
                xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
        
        title(ylab=ylab2,line = 2.5,cex.lab=1.3)
        axis(1,at=c(0.5,3.5,6.5),labels = c("","C1",""),col="black",
             col.axis="black",lwd=1,cex.axis=1)
        axis(1,at=c(6.5,10,13.5),labels = c("","C2",""),col="red",
             col.axis="red",lwd=1,cex.axis=1)
        axis(1,at=c(13.5,17,20.5),labels = c("","C3",""),col="blue",
             col.axis="blue",lwd=1,cex.axis=1)
        axis(1,at=c(20.5,23.5,26.5),labels = c("","C4",""),col="orange",
             col.axis="orange",lwd=1,cex.axis=1)
        mtext(text ="b", side=2, line = 3, at=ylimG2[2], las=2 ,cex = 1.3)
        
        abline(v = c(6.5,13.5,20.5),col="gray",lty=3,lwd=1)
        abline(h=0,col="black",lty=2,lwd=0.5)
        #box(which = "figure",col="black")
        
        dev.off()
      }
      
      #-- Si c'est D5
      if (Detection=="D5") {
        tiff(file=saveData_couleur, units="in", width = 5, height = 3.7, pointsize = 10,res=600)
        
        layout(matrix(c(1,2,3),ncol=3,byrow=TRUE),
               widths=c(2.4,0.1,2.4),heights=c(3.7))
        par(bty="o")
        
        #-- graph_1
        par(mar=c(4,6,0.5,0.5))
        boxplot(M_Resultat_RV_G1[,colonnes],
                ylim=ylimG1,
                col=couleur,outline=FALSE,
                cex.axis=1,
                xlab=xlab1,
                xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
        
        title(ylab=ylab1,line = 2.5,cex.lab=1.3)
        axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
             col.axis="black",lwd=1,cex.axis=1)
        axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
             col.axis="red",lwd=1,cex.axis=1)
        axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
             col.axis="blue",lwd=1,cex.axis=1)
        axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
             col.axis="orange",lwd=1,cex.axis=1)
        mtext(text ="a", side=2, line = 3, at=ylimG1[2], las=2 ,cex = 1.3)
        
        abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
        abline(h=0,col="black",lty=2,lwd=0.5)
        #box(which = "figure",col="black")
        
        #-- une marge
        par(mar=c(0,0,0,0))
        plot.new()
        abline(v=0,col="black",lty=1,lwd=1) 
        
        #-- graph_2
        par(mar=c(4,6,0.5,0.5))
        boxplot(M_Resultat_RV_G2[,colonnes],
                ylim=ylimG2,
                col=couleur,outline=FALSE,
                cex.axis=1,
                xlab=xlab2,
                xaxt="n",lwd=0.5,cex.lab=1.3,at=espbox)
        
        title(ylab=ylab2,line = 2.5,cex.lab=1.3)
        axis(1,at=c(0.5,1.25,2),labels = c("","C1",""),col="black",
             col.axis="black",lwd=1,cex.axis=1)
        axis(1,at=c(2,3,4),labels = c("","C2",""),col="red",
             col.axis="red",lwd=1,cex.axis=1)
        axis(1,at=c(4,5,6),labels = c("","C3",""),col="blue",
             col.axis="blue",lwd=1,cex.axis=1)
        axis(1,at=c(6,6.75,7.5),labels = c("","C4",""),col="orange",
             col.axis="orange",lwd=1,cex.axis=1)
        mtext(text ="b", side=2, line = 3.5, at=ylimG2[2], las=2 ,cex = 1.3)
        
        abline(v = c(2,4,6),col="gray",lty=3,lwd=1)
        abline(h=0,col="black",lty=2,lwd=0.5)
        #box(which = "figure",col="black")
        
        dev.off()
      }
      
    }

################################################################################
################################################################################
