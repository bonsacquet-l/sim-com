#--------------------------------------#
# graphique des exemples d'ordinations #
# Avec les nMDS                        #
#--------------------------------------#

#-- sourcer les script necessaire

#-- packages
library(ggplot2)
library(ggrepel)
library(cowplot)

#-- fonction pour recuperer la legende d'un ggplot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#--------------------#
# pour les nMDS bray #
#--------------------#
rm(list=ls())

#-- les parametres
analyse<-"nMDS"
distan<-"bray"
fichier<-"D4_C3_05"
#fichier<-"D1_C3_01"

simul<-900

#-- les donnees
load(file.path("Outcome","out-simul","AFC",paste("AFC_Simul_",fichier,".Rdata",sep="")))
load(file.path("Outcome","out-simul",analyse,paste(analyse,"_dist_",distan,"_",fichier,".Rdata",sep="")))

DF_data_i<-as.data.frame(A_Coord_nmds_Sp[,,simul])
DF_data_i<-cbind(DF_data_i,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_i)<-c("nMDS_bray_i_Axis1","nMDS_bray_i_Axis2","mean_detectability")

DF_data_n<-as.data.frame(A_Coord_nmds_Sp_Naive[,,simul])
DF_data_n<-cbind(DF_data_n,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_n)<-c("nMDS_bray_n_Axis1","nMDS_bray_n_Axis2","mean_detectability")

V_label<-c(1:20)
xlim<-c(min(c(min(DF_data_i[,1]),min(DF_data_n[,1])))-0.1,max(c(max(DF_data_i[,1]),max(DF_data_n[,1])))+0.1)
ylim<-c(min(c(min(DF_data_i[,2]),min(DF_data_n[,2])))-0.1,max(c(max(DF_data_i[,2]),max(DF_data_n[,2])))+0.1)

#-- les graphiques
G_nMDS_i<-ggplot()
G_nMDS_i<-G_nMDS_i+ coord_cartesian()
G_nMDS_i<-G_nMDS_i+ geom_hline(aes(yintercept=0))
G_nMDS_i<-G_nMDS_i+ geom_vline(aes(xintercept=0))
G_nMDS_i<-G_nMDS_i+ geom_point(data=DF_data_i,
                             aes(x=nMDS_bray_i_Axis1,
                                 y=nMDS_bray_i_Axis2,
                                 color=mean_detectability))
G_nMDS_i<-G_nMDS_i+ geom_text_repel(data=DF_data_i,
                                  aes(x=nMDS_bray_i_Axis1,
                                      y=nMDS_bray_i_Axis2,
                                      label=V_label),
                                  size=3)
G_nMDS_i<-G_nMDS_i+ labs(x="Axis 1 of nMDS_bray_i",y="Axis 2 of nMDS_bray_i")
G_nMDS_i<-G_nMDS_i+scale_color_manual(values = c("red","black"))

G_nMDS_i<-G_nMDS_i+theme(legend.position = "none") 


G_nMDS_n<-ggplot()
G_nMDS_n<-G_nMDS_n+ coord_cartesian()
G_nMDS_n<-G_nMDS_n+ geom_hline(aes(yintercept=0))
G_nMDS_n<-G_nMDS_n+ geom_vline(aes(xintercept=0))
G_nMDS_n<-G_nMDS_n+ geom_point(data=DF_data_n,
                             aes(x=nMDS_bray_n_Axis1,
                                 y=nMDS_bray_n_Axis2,
                                 color=mean_detectability))
G_nMDS_n<-G_nMDS_n+ geom_text_repel(data=DF_data_n,
                                  aes(x=nMDS_bray_n_Axis1,
                                      y=nMDS_bray_n_Axis2,
                                      label=V_label),
                                  size=3)
G_nMDS_n<-G_nMDS_n+ labs(x="Axis 1 of nMDS_bray_n",y="Axis 2 of nMDS_bray_n")
G_nMDS_n<-G_nMDS_n+scale_color_manual(values = c("red","black"))


legend<-get_legend(G_nMDS_n)

G_nMDS_n<-G_nMDS_n+theme(legend.position = "none") 

G_nMDS<-ggdraw() + draw_plot(G_nMDS_i,0,0,0.40,1)
G_nMDS<-G_nMDS+draw_plot(G_nMDS_n,0.40,0,0.40,1)
G_nMDS<-G_nMDS+draw_plot(legend,0.9,0,0.01,0.9)
G_nMDS<-G_nMDS+draw_plot_label(c("a", "b"), c(0,0.40), c(1, 1), size = 14)

G_nMDS

#-- sauvegarde
saveData<-file.path("Outcome","out-graph",
                    paste("G_nMDS_bray_",fichier,"_",simul,".tiff",sep=""))
save_plot(saveData,G_nMDS,ncol = 1,nrow = 1,base_width=7,dpi=300)

#--------------------#
# pour les nMDS chao #
#--------------------#
rm(list=ls())

#-- les parametres
analyse<-"nMDS"
distan<-"chao"
fichier<-"D4_C3_05"
#fichier<-"D1_C3_01"

simul<-900

#-- les donnees
load(file.path("Outcome","out-simul","AFC",paste("AFC_Simul_",fichier,".Rdata",sep="")))
load(file.path("Outcome","out-simul",analyse,paste(analyse,"_dist_",distan,"_",fichier,".Rdata",sep="")))

DF_data_i<-as.data.frame(A_Coord_nmds_Sp[,,simul])
DF_data_i<-cbind(DF_data_i,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_i)<-c("nMDS_chao_i_Axis1","nMDS_chao_i_Axis2","mean_detectability")

DF_data_n<-as.data.frame(A_Coord_nmds_Sp_Naive[,,simul])
DF_data_n<-cbind(DF_data_n,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_n)<-c("nMDS_chao_n_Axis1","nMDS_chao_n_Axis2","mean_detectability")

V_label<-c(1:20)
xlim<-c(min(c(min(DF_data_i[,1]),min(DF_data_n[,1])))-0.1,max(c(max(DF_data_i[,1]),max(DF_data_n[,1])))+0.1)
ylim<-c(min(c(min(DF_data_i[,2]),min(DF_data_n[,2])))-0.1,max(c(max(DF_data_i[,2]),max(DF_data_n[,2])))+0.1)

#-- les graphiques
G_nMDS_i<-ggplot()
G_nMDS_i<-G_nMDS_i+ coord_cartesian()
G_nMDS_i<-G_nMDS_i+ geom_hline(aes(yintercept=0))
G_nMDS_i<-G_nMDS_i+ geom_vline(aes(xintercept=0))
G_nMDS_i<-G_nMDS_i+ geom_point(data=DF_data_i,
                               aes(x=nMDS_chao_i_Axis1,
                                   y=nMDS_chao_i_Axis2,
                                   color=mean_detectability))
G_nMDS_i<-G_nMDS_i+ geom_text_repel(data=DF_data_i,
                                    aes(x=nMDS_chao_i_Axis1,
                                        y=nMDS_chao_i_Axis2,
                                        label=V_label),
                                    size=3)
G_nMDS_i<-G_nMDS_i+ labs(x="Axis 1 of nMDS_chao_i",y="Axis 2 of nMDS_chao_i")
G_nMDS_i<-G_nMDS_i+scale_color_manual(values = c("red","black"))

G_nMDS_i<-G_nMDS_i+theme(legend.position = "none") 


G_nMDS_n<-ggplot()
G_nMDS_n<-G_nMDS_n+ coord_cartesian()
G_nMDS_n<-G_nMDS_n+ geom_hline(aes(yintercept=0))
G_nMDS_n<-G_nMDS_n+ geom_vline(aes(xintercept=0))
G_nMDS_n<-G_nMDS_n+ geom_point(data=DF_data_n,
                               aes(x=nMDS_chao_n_Axis1,
                                   y=nMDS_chao_n_Axis2,
                                   color=mean_detectability))
G_nMDS_n<-G_nMDS_n+ geom_text_repel(data=DF_data_n,
                                    aes(x=nMDS_chao_n_Axis1,
                                        y=nMDS_chao_n_Axis2,
                                        label=V_label),
                                    size=3)
G_nMDS_n<-G_nMDS_n+ labs(x="Axis 1 of nMDS_chao_n",y="Axis 2 of nMDS_chao_n")
G_nMDS_n<-G_nMDS_n+scale_color_manual(values = c("red","black"))


legend<-get_legend(G_nMDS_n)

G_nMDS_n<-G_nMDS_n+theme(legend.position = "none") 

G_nMDS<-ggdraw() + draw_plot(G_nMDS_i,0,0,0.40,1)
G_nMDS<-G_nMDS+draw_plot(G_nMDS_n,0.40,0,0.40,1)
G_nMDS<-G_nMDS+draw_plot(legend,0.9,0,0.01,0.9)
G_nMDS<-G_nMDS+draw_plot_label(c("a", "b"), c(0,0.40), c(1, 1), size = 14)

G_nMDS

#-- sauvegarde
saveData<-file.path("Outcome","out-graph",
                    paste("G_nMDS_chao_",fichier,"_",simul,".tiff",sep=""))
save_plot(saveData,G_nMDS,ncol = 1,nrow = 1,base_width=7,dpi=300)

