#--------------------------------------#
# graphique des exemples d'ordinations #
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

#--------------#
# pour les PCA #
#--------------#
rm(list=ls())

#-- les parametres
analyse<-"ACP"
fichier<-"D4_C3_05"
fichier<-"D1_C3_01"

simul<-1000

#-- les donnees
load(file.path("Outcome","out-simul",analyse,paste(analyse,"_Simul_",fichier,".Rdata",sep="")))

DF_data_i<-as.data.frame(A_TabCoordACP[,,simul])
DF_data_i<-cbind(DF_data_i,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_i)<-c("PCA_i_Axis1","PCA_i_Axis2","mean_detectability")

DF_data_n<-as.data.frame(A_TabCoordACP_Naive[,,simul])
DF_data_n<-cbind(DF_data_n,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_n)<-c("PCA_n_Axis1","PCA_n_Axis2","mean_detectability")

V_label<-c(1:20)
xlim<-c(min(c(min(DF_data_i[,1]),min(DF_data_n[,1])))-0.1,max(c(max(DF_data_i[,1]),max(DF_data_n[,1])))+0.1)
ylim<-c(min(c(min(DF_data_i[,2]),min(DF_data_n[,2])))-0.1,max(c(max(DF_data_i[,2]),max(DF_data_n[,2])))+0.1)

#-- les graphiques
G_ACP_i<-ggplot()
G_ACP_i<-G_ACP_i+ coord_cartesian(xlim = xlim,ylim = ylim)
G_ACP_i<-G_ACP_i+ geom_hline(aes(yintercept=0))
G_ACP_i<-G_ACP_i+ geom_vline(aes(xintercept=0))
G_ACP_i<-G_ACP_i+ geom_point(data=DF_data_i,
                             aes(x=PCA_i_Axis1,
                                 y=PCA_i_Axis2,
                                 color=mean_detectability))
G_ACP_i<-G_ACP_i+ geom_text_repel(data=DF_data_i,
                                  aes(x=PCA_i_Axis1,
                                      y=PCA_i_Axis2,
                                      label=V_label),
                                  size=3)
G_ACP_i<-G_ACP_i+ labs(x="Axis 1 of PCA_i",y="Axis 2 of PCA_i")
G_ACP_i<-G_ACP_i+scale_color_manual(values = c("black","red"))

G_ACP_i<-G_ACP_i+theme(legend.position = "none") 


G_ACP_n<-ggplot()
G_ACP_n<-G_ACP_n+ coord_cartesian(xlim = xlim,ylim = ylim)
G_ACP_n<-G_ACP_n+ geom_hline(aes(yintercept=0))
G_ACP_n<-G_ACP_n+ geom_vline(aes(xintercept=0))
G_ACP_n<-G_ACP_n+ geom_point(data=DF_data_n,
                             aes(x=PCA_n_Axis1,
                                 y=PCA_n_Axis2,
                                 color=mean_detectability))
G_ACP_n<-G_ACP_n+ geom_text_repel(data=DF_data_n,
                                  aes(x=PCA_n_Axis1,
                                      y=PCA_n_Axis2,
                                      label=V_label),
                                  size=3)
G_ACP_n<-G_ACP_n+ labs(x="Axis 1 of PCA_n",y="Axis 2 of PCA_n")
G_ACP_n<-G_ACP_n+scale_color_manual(values = c("black","red"))


legend<-get_legend(G_ACP_n)

G_ACP_n<-G_ACP_n+theme(legend.position = "none") 

G_ACP<-ggdraw() + draw_plot(G_ACP_i,0,0,0.40,1)
G_ACP<-G_ACP+draw_plot(G_ACP_n,0.40,0,0.40,1)
G_ACP<-G_ACP+draw_plot(legend,0.9,0,0.01,0.9)
G_ACP<-G_ACP+draw_plot_label(c("a", "b"), c(0,0.40), c(1, 1), size = 14)

G_ACP

#-- sauvegarde
saveData<-file.path("Outcome","out-graph",
                    paste("G_ACP_",fichier,"_",simul,".tiff",sep=""))
save_plot(saveData,G_ACP,ncol = 1,nrow = 1,base_width=7,dpi=300)

#-------------#
# pour les CA #
#-------------#
rm(list=ls())

#-- les parametres
analyse<-"AFC"
fichier<-"D4_C3_05"
fichier<-"D1_C3_01"

simul<-900

#-- les donnees
load(file.path("Outcome","out-simul",analyse,paste(analyse,"_Simul_",fichier,".Rdata",sep="")))

DF_data_i<-as.data.frame(A_TabCoordAFC[,,simul])
DF_data_i<-cbind(DF_data_i,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_i)<-c("CA_i_Axis1","CA_i_Axis2","mean_detectability")

DF_data_n<-as.data.frame(A_TabCoordAFC_Naive[,,simul])
DF_data_n<-cbind(DF_data_n,as.factor(M_MemDetectSp[simul,]))
colnames(DF_data_n)<-c("CA_n_Axis1","CA_n_Axis2","mean_detectability")

V_label<-c(1:20)
xlim<-c(min(c(min(DF_data_i[,1]),min(DF_data_n[,1])))-0.1,max(c(max(DF_data_i[,1]),max(DF_data_n[,1])))+0.1)
ylim<-c(min(c(min(DF_data_i[,2]),min(DF_data_n[,2])))-0.1,max(c(max(DF_data_i[,2]),max(DF_data_n[,2])))+0.1)

#-- les graphiques
G_AFC_i<-ggplot()
G_AFC_i<-G_AFC_i+ coord_cartesian(xlim = xlim,ylim = ylim)
G_AFC_i<-G_AFC_i+ geom_hline(aes(yintercept=0))
G_AFC_i<-G_AFC_i+ geom_vline(aes(xintercept=0))
G_AFC_i<-G_AFC_i+ geom_point(data=DF_data_i,
                             aes(x=CA_i_Axis1,
                                 y=CA_i_Axis2,
                                 color=mean_detectability))
G_AFC_i<-G_AFC_i+ geom_text_repel(data=DF_data_i,
                                  aes(x=CA_i_Axis1,
                                      y=CA_i_Axis2,
                                      label=V_label),
                                  size=3)
G_AFC_i<-G_AFC_i+ labs(x="Axis 1 of CA_i",y="Axis 2 of CA_i")
G_AFC_i<-G_AFC_i+scale_color_manual(values = c("black","red"))

G_AFC_i<-G_AFC_i+theme(legend.position = "none") 


G_AFC_n<-ggplot()
G_AFC_n<-G_AFC_n+ coord_cartesian(xlim = xlim,ylim = ylim)
G_AFC_n<-G_AFC_n+ geom_hline(aes(yintercept=0))
G_AFC_n<-G_AFC_n+ geom_vline(aes(xintercept=0))
G_AFC_n<-G_AFC_n+ geom_point(data=DF_data_n,
                             aes(x=CA_n_Axis1,
                                 y=CA_n_Axis2,
                                 color=mean_detectability))
G_AFC_n<-G_AFC_n+ geom_text_repel(data=DF_data_n,
                                  aes(x=CA_n_Axis1,
                                      y=CA_n_Axis2,
                                      label=V_label),
                                  size=3)
G_AFC_n<-G_AFC_n+ labs(x="Axis 1 of CA_n",y="Axis 2 of CA_n")
G_AFC_n<-G_AFC_n+scale_color_manual(values = c("black","red"))


legend<-get_legend(G_AFC_n)

G_AFC_n<-G_AFC_n+theme(legend.position = "none") 

G_AFC<-ggdraw() + draw_plot(G_AFC_i,0,0,0.40,1)
G_AFC<-G_AFC+draw_plot(G_AFC_n,0.40,0,0.40,1)
G_AFC<-G_AFC+draw_plot(legend,0.9,0,0.01,0.9)
G_AFC<-G_AFC+draw_plot_label(c("a", "b"), c(0,0.40), c(1, 1), size = 14)

G_AFC

#-- sauvegarde
saveData<-file.path("Outcome","out-graph",
                    paste("G_AFC_",fichier,"_",simul,".tiff",sep=""))
save_plot(saveData,G_AFC,ncol = 1,nrow = 1,base_width=7,dpi=300)

