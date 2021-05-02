#---------------------------------------------------------------#
# graphique des resultats des RV entre les matrice de distances #
# issues de l'ordination des sites ou des especes par Nmds ou   #
# Pcoa #--------------------------------------------------------#
#------#

#-------------------------------------------------------------------------------
#-- regroupement des differentes donnees
  # les RV venant de CA, Pcoa ou Nmds des memes simulations (nom de fichier)
    fichier<-"D4_C3_05"
    fichier<-"D5_C3"
    
  # les matrices de regroupement des donnees
    M_RV_CA_Nmds_sites<-matrix(NA,nrow = 1000,ncol = 3)
    M_RV_CA_Nmds_sp<-matrix(NA,nrow = 1000,ncol = 3)
    
    M_RV_ACP_Pcoa_sites<-matrix(NA,nrow = 1000,ncol = 3)
    M_RV_ACP_Pcoa_sp<-matrix(NA,nrow = 1000,ncol = 3)
    
  #-- chargement des donnees
    #-- CA et Nmds
    # sur les sites 
    load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_Site_",fichier,".Rdata",sep = ""))
    M_RV_CA_Nmds_sites[,1]<-M_RV_Dist_AFC
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_Nmds_site_chao_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_warningNmds_site_chao_",fichier,".Rdata",sep = ""))
    M_RV_CA_Nmds_sites[,2]<-M_RV_Dist_
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_Nmds_site_bray_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_warningNmds_site_bray_",fichier,".Rdata",sep = ""))
    M_RV_CA_Nmds_sites[,3]<-M_RV_Dist_
    
    # sur les sp
    load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_Sp_",fichier,".Rdata",sep = ""))
    M_RV_CA_Nmds_sp[,1]<-M_RV_Dist_AFC
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_Nmds_sp_chao_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_warningNmds_sp_chao_",fichier,".Rdata",sep = ""))
    M_RV_CA_Nmds_sp[,2]<-M_RV_Dist_
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_Nmds_sp_bray_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/AFC/AFC_RV_warningNmds_sp_bray_",fichier,".Rdata",sep = ""))
    M_RV_CA_Nmds_sp[,3]<-M_RV_Dist_
    
  #-- PCA et PCoA 
    # sur les sites 
    load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_Site_",fichier,".Rdata",sep = ""))
    M_RV_ACP_Pcoa_sites[,1]<-M_RV_Dist_ACP
  
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_Pcoa_site_chao_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_warningPcoa_site_chao_",fichier,".Rdata",sep = ""))
    M_RV_ACP_Pcoa_sites[,2]<-M_RV_Dist_
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_Pcoa_site_bray_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_warningPcoa_site_bray_",fichier,".Rdata",sep = ""))
    M_RV_ACP_Pcoa_sites[,3]<-M_RV_Dist_
    
    # sur les sp
    load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_Sp_",fichier,".Rdata",sep = ""))
    M_RV_ACP_Pcoa_sp[,1]<-M_RV_Dist_ACP
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_Pcoa_sp_chao_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_warningPcoa_sp_chao_",fichier,".Rdata",sep = ""))
    M_RV_ACP_Pcoa_sp[,2]<-M_RV_Dist_
    
    #load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_Pcoa_sp_bray_",fichier,".Rdata",sep = ""))
    load(paste("~/Desktop/sim-com/Outcome/out-simul/ACP/ACP_RV_warningPcoa_sp_bray_",fichier,".Rdata",sep = ""))
    M_RV_ACP_Pcoa_sp[,3]<-M_RV_Dist_ 
    
    #-- Non prise en compte de ligne NA qui sont dans site ou sp
    #--1 dans les nMDS
    #-- Les NA ici correspondent a toute les simuls ou il y a eu un warning
    options(warn = 0)
    V_isna_CA_Site<-which(is.na(M_RV_CA_Nmds_sites))
    length(V_isna_CA_Site)
    V_isna_CA_Sp<-which(is.na(M_RV_CA_Nmds_sp))
    length(V_isna_CA_Sp)
    
    setdiff(V_isna_CA_Sp,intersect(V_isna_CA_Site, V_isna_CA_Sp))
    
    V_isna_PCA_Site<-which(is.na(M_RV_ACP_Pcoa_sites))
    length(V_isna_PCA_Site)
    V_isna_PCA_Sp<-which(is.na(M_RV_ACP_Pcoa_sp))
    length(V_isna_PCA_Sp)

    setdiff(V_isna_PCA_Sp,intersect(V_isna_PCA_Site, V_isna_PCA_Sp))
    
    # les Na dans les sp correspondent a des Na dans les sites
    # donc passage cellules des sp en Na qui correspondent a des cellules des sites qui st déjà en Na
    M_RV_CA_Nmds_sp[V_isna_CA_Site]<-NA
    M_RV_ACP_Pcoa_sp[V_isna_PCA_Site]<-NA
    
#-------------------------------------------------------------------------------
#-- les differents graphiques
    # pour les ACP et PCoA
    saveData<-file.path("Outcome","out-graph",
                                paste("RV_ACP_Pcoa_",fichier,".pdf",sep=""))
    
    pdf(file=saveData, width = 5.5, height = 2, pointsize = 7)
    
    layout(matrix(c(1,2),ncol=2,byrow=TRUE))
    par(bty="o")
    
    #-- les sites
    par(mar=c(4,5,0.5,0.5))
    boxplot(M_RV_ACP_Pcoa_sites[,1:3],
            ylim=c(0,1),
            col=c("bisque","coral","brown4"),outline=FALSE,
            ylab="RV of distance matrices among \n sites Analysis_i /Analysis_n",cex.axis=1,
            xlab=paste("analyses on simulations ",fichier,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1)
    
    axis(1,at=c(0.5,1,1.5),labels = c("","PCA",""),col="bisque",
         col.axis="black",lwd=1,cex.axis=0.7)
    axis(1,at=c(1.5,2,2.5),labels = c("","PCoA Chao",""),col="coral",
         col.axis="black",lwd=1,cex.axis=0.7)
    axis(1,at=c(2.5,3,3.5),labels = c("","PCoA Bray",""),col="brown4",
         col.axis="black",lwd=1,cex.axis=0.7)
    mtext(text ="a", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 1.5)
    
    box(which = "figure",col="black")
    
    #-- les especes
    par(mar=c(4,5,0.5,0.5))
    boxplot(M_RV_ACP_Pcoa_sp[,1:3],
            ylim=c(0,1),
            col=c("bisque","coral","brown4"),outline=FALSE,
            ylab="RV of distance matrices among \n species Analysis_i /Analysis_n",cex.axis=1,
            xlab=paste("analyses on simulations ",fichier,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1)
    
    axis(1,at=c(0.5,1,1.5),labels = c("","PCA",""),col="bisque",
         col.axis="black",lwd=1,cex.axis=0.8)
    axis(1,at=c(1.5,2,2.5),labels = c("","PCoA Chao",""),col="coral",
         col.axis="black",lwd=1,cex.axis=0.8)
    axis(1,at=c(2.5,3,3.5),labels = c("","PCoA Bray",""),col="brown4",
         col.axis="black",lwd=1,cex.axis=0.8)
    mtext(text ="b", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 1.5)
    
    box(which = "figure",col="black")
    
    dev.off()

  #-----------------------------------------------------------------------------
    # pour les CA et nMDS
    saveData<-file.path("Outcome","out-graph",
                        paste("RV_CA_Nmds_",fichier,".pdf",sep=""))
    
    pdf(file=saveData, width = 5.5, height = 2, pointsize = 7)
    
    layout(matrix(c(1,2),ncol=2,byrow=TRUE))
    par(bty="o")
    
    #-- les sites
    par(mar=c(4,5,0.5,0.5))
    boxplot(M_RV_CA_Nmds_sites[,1:3],
            ylim=c(0,1),
            col=c("bisque","coral","brown4"),outline=FALSE,
            ylab="RV of distance matrices among \n sites Analysis_i /Analysis_n",cex.axis=1,
            xlab=paste("analyses on simulations ",fichier,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1)
    
    axis(1,at=c(0.5,1,1.5),labels = c("","CA",""),col="bisque",
         col.axis="black",lwd=1,cex.axis=0.8)
    axis(1,at=c(1.5,2,2.5),labels = c("","nMDS Chao",""),col="coral",
         col.axis="black",lwd=1,cex.axis=0.8)
    axis(1,at=c(2.5,3,3.5),labels = c("","nMDS Bray",""),col="brown4",
         col.axis="black",lwd=1,cex.axis=0.8)
    mtext(text ="a", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 1.5)
    
    box(which = "figure",col="black")
    
    #-- les sp
    par(mar=c(4,5,0.5,0.5))
    boxplot(M_RV_CA_Nmds_sp[,1:3],
            ylim=c(0,1),
            col=c("bisque","coral","brown4"),outline=FALSE,
            ylab="RV of distance matrices among  \n species Analysis_i /Analysis_n",cex.axis=1,
            xlab=paste("analyses on simulations ",fichier,sep=""),
            xaxt="n",lwd=0.5,cex.lab=1)
    
    axis(1,at=c(0.5,1,1.5),labels = c("","CA",""),col="bisque",
         col.axis="black",lwd=1,cex.axis=0.8)
    axis(1,at=c(1.5,2,2.5),labels = c("","nMDS Chao",""),col="coral",
         col.axis="black",lwd=1,cex.axis=0.8)
    axis(1,at=c(2.5,3,3.5),labels = c("","nMDS Bray",""),col="brown4",
         col.axis="black",lwd=1,cex.axis=0.8)
    mtext(text ="b", side=2, line = 3, at=par('usr')[4], las=2 ,cex = 1.5)
    
    box(which = "figure",col="black")
    
    dev.off()
    