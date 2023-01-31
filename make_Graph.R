#-------------------------------------------------#
# fonction pour produire les graphiques de chaque #
# indicateur (en duo ACP et AFC)                  #
#-------------------------------------------------#
#-- nettoyage
rm(list = (ls()))

#-- chargement des fonctions
source(file.path("R","package-option.R"))
source(file.path("R","Noms-Fichiers.R"))

#-- source les fonctions  
source(file.path("R","Graph_Detect_D2D4_tout_tiff.R"))
source(file.path("R","Graph_Detect_D2D4_par_4_tiff.R"))
source(file.path("R","Graph_Detect_D2D4_par_2_tiff.R"))
source(file.path("R","Graph_Detect_D5_tout_tiff.R"))
source(file.path("R","Graph_Detect_D5_par_4_tiff.R"))
source(file.path("R","Graph_Detect_D5_par_2_tiff.R"))

source(file.path("R","Graph_G1_G2_tout_tiff.R"))
source(file.path("R","Graph_G1_G2_par_4_tiff.R"))
source(file.path("R","Graph_G1_G2_par_2_tiff.R"))

source(file.path("R","Graph_GA_GB_tout_tiff.R"))
source(file.path("R","Graph_GA_GB_par_4_tiff.R"))
source(file.path("R","Graph_GA_GB_par_2_tiff.R"))

source(file.path("R","Graph_RV_Sp_tout_tiff.R"))
source(file.path("R","Graph_RV_Sp_par_4_tiff.R"))
source(file.path("R","Graph_RV_Sp_par_2_tiff.R"))

source(file.path("R","Graph_RV_Site_tout_tiff.R"))
source(file.path("R","Graph_RV_Site_par_4_tiff.R"))
source(file.path("R","Graph_RV_Site_par_2_tiff.R"))

#-- executer les fonctions graphiques
  # les groupes plus ou moins detectables
  # D4 -----------------
  Fct_Graph_Detect_D2D4_tout_tiff(Detection="D4",couleur_noir="couleur",
                                  ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                                  ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                                  ylimAFC1=c(-1.2,1.5),ylimAFC2=c(-1.2,1.5),
                                  ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                                  ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.04,0.15),
                                  ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.02,0.025),
                                  ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.4,0.7),
                                  ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.25,0.3),
                                  ylimPCoAchao1=c(-0.02,0.02),ylimPCoAchao2=c(-0.1,0.35),
                                  ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.09,0.09),
                                  ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.2,0.6),
                                  ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.5,0.5))

  Fct_Graph_Detect_D2D4_par_4_tiff(Detection="D4",couleur_noir="couleur",
                                   graph_1="ACP",graph_2="PCoA_bray",
                                   graph_3="nMDS_chao",graph_4="PCoA_chao",
                                   ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                   ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                                   ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.04,0.15),
                                   ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.02,0.025),
                                   ylimG3_1=c(-0.2,0.2),ylimG3_2=c(-0.2,0.6),
                                   ylimG3_3=c(-0.1,0.1),ylimG3_4=c(-0.5,0.5),
                                   ylimG4_1=c(-0.02,0.02),ylimG4_2=c(-0.1,0.35),
                                   ylimG4_3=c(-0.01,0.01),ylimG4_4=c(-0.09,0.09))

  Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D4",couleur_noir="couleur",
                                   graph_1="AFC",graph_2="nMDS_bray",
                                   ylimG1_1=c(-1.2,1.5),ylimG1_2=c(-1.2,1.5),
                                   ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                                   ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.4,0.7),
                                   ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.25,0.3))
  # D2 -----------------
  
  Fct_Graph_Detect_D2D4_tout_tiff(Detection="D2",couleur_noir="couleur",
                                  ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                                  ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                                  ylimAFC1=c(-1.2,1.2),ylimAFC2=c(-1.2,1.2),
                                  ylimAFC3=c(-0.7,0.7),ylimAFC4=c(-0.7,0.7),
                                  ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.05,0.05),
                                  ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.03,0.03),
                                  ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.5,0.5),
                                  ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.35,0.35),
                                  ylimPCoAchao1=c(-0.02,0.02),ylimPCoAchao2=c(-0.05,0.06),
                                  ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.045,0.045),
                                  ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.4,0.4),
                                  ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.4,0.4))

  Fct_Graph_Detect_D2D4_par_4_tiff(Detection="D2",couleur_noir="couleur",
                                   graph_1="ACP",graph_2="PCoA_bray",
                                   graph_3="PCoA_chao",graph_4="nMDS_chao",
                                   ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                   ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                                   ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.05,0.05),
                                   ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.03,0.03),
                                   ylimG3_1=c(-0.02,0.02),ylimG3_2=c(-0.05,0.06),
                                   ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.045,0.045),
                                   ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.4,0.4),
                                   ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.4,0.4))

  Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D2",couleur_noir="couleur",
                                   graph_1="AFC",graph_2="nMDS_bray",
                                   ylimG1_1=c(-1.2,1.2),ylimG1_2=c(-1.2,1.2),
                                   ylimG1_3=c(-0.7,0.7),ylimG1_4=c(-0.7,0.7),
                                   ylimG2_1=c(-0.45,0.45),ylimG2_2=c(-0.5,0.5),
                                   ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.35,0.35))
  # D5 -----------------
  
  Fct_Graph_Detect_D5_tout_tiff(Detection="D5",couleur_noir="couleur",
                                ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                                ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                                ylimAFC1=c(-1,1),ylimAFC2=c(-1.2,1.2),
                                ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                                ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.3,0.4),
                                ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.05,0.05),
                                ylimnMDSbray1=c(-0.3,0.3),ylimnMDSbray2=c(-0.8,1.1),
                                ylimnMDSbray3=c(-0.25,0.25),ylimnMDSbray4=c(-0.25,0.25),
                                ylimPCoAchao1=c(-0.015,0.015),ylimPCoAchao2=c(-0.2,0.8),
                                ylimPCoAchao3=c(-0.005,0.005),ylimPCoAchao4=c(-0.1,0.15),
                                ylimnMDSchao1=c(-0.15,0.15),ylimnMDSchao2=c(-0.5,0.6),
                                ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.35,0.35))

  Fct_Graph_Detect_D5_par_4_tiff(Detection="D5",couleur_noir="couleur",
                                  graph_1="ACP",graph_2="PCoA_bray",
                                  graph_3="PCoA_chao",graph_4="nMDS_chao",
                                  ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                  ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                                  ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.3,0.4),
                                  ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.05,0.05),
                                  ylimG3_1=c(-0.015,0.015),ylimG3_2=c(-0.2,0.8),
                                  ylimG3_3=c(-0.005,0.005),ylimG3_4=c(-0.1,0.15),
                                  ylimG4_1=c(-0.15,0.15),ylimG4_2=c(-0.5,0.6),
                                  ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.3,0.35))

  Fct_Graph_Detect_D5_par_2_tiff(Detection="D5",couleur_noir="couleur",
                                  graph_1="AFC",graph_2="nMDS_bray",
                                  ylimG1_1=c(-1,1),ylimG1_2=c(-1.2,1.2),
                                  ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                                  ylimG2_1=c(-0.3,0.3),ylimG2_2=c(-0.8,1.1),
                                  ylimG2_3=c(-0.25,0.25),ylimG2_4=c(-0.25,0.25))
  #--------------------------------------------------------------
  
  # les groupes 1 et 2
    # D1  # -----------------

    Fct_Graph_G1_G2_tout_tiff(Detection="D1",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.2,1.5),ylimAFC2=c(-1.2,1.5),
                              ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                              ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.1,0.1),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.07,0.07),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.5,0.5),
                              ylimnMDSbray3=c(-0.25,0.25),ylimnMDSbray4=c(-0.4,0.4),
                              ylimPCoAchao1=c(-0.03,0.03),ylimPCoAchao2=c(-0.25,0.25),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.2,0.2),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.8,0.8),
                              ylimnMDSchao3=c(-0.2,0.2),ylimnMDSchao4=c(-0.6,0.6))
    
    Fct_Graph_G1_G2_par_4_tiff(Detection="D1",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.1,0.1),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.07,0.07),
                               ylimG3_1=c(-0.03,0.03),ylimG3_2=c(-0.25,0.25),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.2,0.2),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.8,0.8),
                               ylimG4_3=c(-0.2,0.2),ylimG4_4=c(-0.6,0.6)) 
    
    Fct_Graph_G1_G2_par_2_tiff(Detection="D1",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.2,1.5),ylimG1_2=c(-1.2,1.5),
                               ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.5,0.5),
                               ylimG2_3=c(-0.25,0.25),ylimG2_4=c(-0.4,0.4)) 

    # D2  # -----------------

    Fct_Graph_G1_G2_tout_tiff(Detection="D2",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1,1),ylimAFC2=c(-1,1),
                              ylimAFC3=c(-0.6,0.6),ylimAFC4=c(-0.6,0.6),
                              ylimPCoAbray1=c(-0.025,0.025),ylimPCoAbray2=c(-0.04,0.04),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.018,0.018),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.4,0.4),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.25,0.25),
                              ylimPCoAchao1=c(-0.025,0.025),ylimPCoAchao2=c(-0.045,0.045),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.025,0.025),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.3,0.3),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.2,0.2))
    
    Fct_Graph_G1_G2_par_4_tiff(Detection="D2",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.04,0.04),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.018,0.018),
                               ylimG3_1=c(-0.025,0.025),ylimG3_2=c(-0.045,0.045),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.025,0.025),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.3,0.3),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.2,0.2)) 
    
    Fct_Graph_G1_G2_par_2_tiff(Detection="D2",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-0.6,0.6),ylimG1_4=c(-0.6,0.6),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.4,0.4),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.25,0.25)) 
    
    # D3  # -----------------

    Fct_Graph_G1_G2_tout_tiff(Detection="D3",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.2,1),ylimAFC2=c(-1.2,1),
                              ylimAFC3=c(-0.7,0.7),ylimAFC4=c(-0.7,0.7),
                              ylimPCoAbray1=c(-0.025,0.025),ylimPCoAbray2=c(-0.025,0.35),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.08,0.08),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.8,1.6),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.7,0.7),
                              ylimPCoAchao1=c(-0.025,0.025),ylimPCoAchao2=c(-0.2,0.5),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.2,0.35),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.7,0.7),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.7,0.7))
    
    Fct_Graph_G1_G2_par_4_tiff(Detection="D3",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.025,0.35),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.08,0.08),
                               ylimG3_1=c(-0.025,0.025),ylimG3_2=c(-0.2,0.5),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.2,0.35),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.7,0.7),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.7,0.7)) 
    
    Fct_Graph_G1_G2_par_2_tiff(Detection="D3",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.2,1),ylimG1_2=c(-1.2,1),
                               ylimG1_3=c(-0.7,0.7),ylimG1_4=c(-0.7,0.7),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.8,1.6),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.7,0.7)) 
    
    # D4  # -----------------

    Fct_Graph_G1_G2_tout_tiff(Detection="D4",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.5,1.5),ylimAFC2=c(-1.5,1.5),
                              ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                              ylimPCoAbray1=c(-0.025,0.025),ylimPCoAbray2=c(-0.15,0.25),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.015,0.015),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.8,0.8),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.3,0.3),
                              ylimPCoAchao1=c(-0.025,0.025),ylimPCoAchao2=c(-0.3,0.5),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.05,0.05),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.8,0.8),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.4,0.4))
    
    Fct_Graph_G1_G2_par_4_tiff(Detection="D4",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.15,0.25),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.015,0.015),
                               ylimG3_1=c(-0.025,0.025),ylimG3_2=c(-0.3,0.5),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.05,0.05),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.8,0.8),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.4,0.4)) 
    
    Fct_Graph_G1_G2_par_2_tiff(Detection="D4",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.5,1.5),ylimG1_2=c(-1.5,1.5),
                               ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.8,0.8),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.3,0.3)) 
    
    # D5  # -----------------

    Fct_Graph_G1_G2_tout_tiff(Detection="D5",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.5,1.5),ylimAFC2=c(-1.5,1.5),
                              ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                              ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.2,0.4),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.05,0.05),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-1,1),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.3,0.3),
                              ylimPCoAchao1=c(-0.03,0.03),ylimPCoAchao2=c(-0.2,0.7),
                              ylimPCoAchao3=c(-0.005,0.005),ylimPCoAchao4=c(-0.1,0.15),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.6,0.6),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.4,0.4))
    
    Fct_Graph_G1_G2_par_4_tiff(Detection="D5",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.2,0.4),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.05,0.05),
                               ylimG3_1=c(-0.03,0.03),ylimG3_2=c(-0.2,0.7),
                               ylimG3_3=c(-0.005,0.005),ylimG3_4=c(-0.1,0.15),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.6,0.6),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.4,0.4)) 
    
    Fct_Graph_G1_G2_par_2_tiff(Detection="D5",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.5,1.5),ylimG1_2=c(-1.5,1.5),
                               ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-1,1),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.3,0.3)) 
#--------------------------------------------------------------
    
  # les groupes A et B
    # D1  # -----------------

    Fct_Graph_GA_GB_tout_tiff(Detection="D1",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.2,1.5),ylimAFC2=c(-1.2,1.5),
                              ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                              ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.1,0.1),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.07,0.07),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.5,0.5),
                              ylimnMDSbray3=c(-0.25,0.25),ylimnMDSbray4=c(-0.4,0.4),
                              ylimPCoAchao1=c(-0.03,0.03),ylimPCoAchao2=c(-0.25,0.25),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.2,0.2),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.8,0.8),
                              ylimnMDSchao3=c(-0.2,0.2),ylimnMDSchao4=c(-0.6,0.6))
    
    Fct_Graph_GA_GB_par_4_tiff(Detection="D1",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.1,0.1),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.07,0.07),
                               ylimG3_1=c(-0.03,0.03),ylimG3_2=c(-0.25,0.25),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.2,0.2),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.8,0.8),
                               ylimG4_3=c(-0.2,0.2),ylimG4_4=c(-0.6,0.6)) 
    
    Fct_Graph_GA_GB_par_2_tiff(Detection="D1",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.2,1.5),ylimG1_2=c(-1.2,1.5),
                               ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.5,0.5),
                               ylimG2_3=c(-0.25,0.25),ylimG2_4=c(-0.4,0.4)) 
    
    # D2  # -----------------

    Fct_Graph_GA_GB_tout_tiff(Detection="D2",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1,1),ylimAFC2=c(-1,1),
                              ylimAFC3=c(-0.6,0.6),ylimAFC4=c(-0.6,0.6),
                              ylimPCoAbray1=c(-0.025,0.025),ylimPCoAbray2=c(-0.04,0.04),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.018,0.018),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.4,0.4),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.25,0.25),
                              ylimPCoAchao1=c(-0.025,0.025),ylimPCoAchao2=c(-0.045,0.045),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.025,0.025),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.3,0.3),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.2,0.2))
    
    Fct_Graph_GA_GB_par_4_tiff(Detection="D2",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.04,0.04),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.018,0.018),
                               ylimG3_1=c(-0.025,0.025),ylimG3_2=c(-0.045,0.045),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.025,0.025),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.3,0.3),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.2,0.2)) 
    
    Fct_Graph_GA_GB_par_2_tiff(Detection="D2",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-0.6,0.6),ylimG1_4=c(-0.6,0.6),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.4,0.4),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.25,0.25)) 
    
    # D3  # -----------------

    Fct_Graph_GA_GB_tout_tiff(Detection="D3",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.2,1),ylimAFC2=c(-1.2,1),
                              ylimAFC3=c(-0.7,0.7),ylimAFC4=c(-0.7,0.7),
                              ylimPCoAbray1=c(-0.025,0.025),ylimPCoAbray2=c(-0.025,0.35),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.08,0.08),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.8,1.6),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.7,0.7),
                              ylimPCoAchao1=c(-0.025,0.025),ylimPCoAchao2=c(-0.2,0.5),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.2,0.35),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.7,0.7),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.7,0.7))
    
    Fct_Graph_GA_GB_par_4_tiff(Detection="D3",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.025,0.35),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.08,0.08),
                               ylimG3_1=c(-0.025,0.025),ylimG3_2=c(-0.2,0.5),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.2,0.35),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.7,0.7),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.7,0.7)) 
    
    Fct_Graph_GA_GB_par_2_tiff(Detection="D3",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.2,1),ylimG1_2=c(-1.2,1),
                               ylimG1_3=c(-0.7,0.7),ylimG1_4=c(-0.7,0.7),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.8,1.6),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.7,0.7)) 
    
    # D4  # -----------------

    Fct_Graph_GA_GB_tout_tiff(Detection="D4",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.5,1.5),ylimAFC2=c(-1.5,1.5),
                              ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                              ylimPCoAbray1=c(-0.025,0.025),ylimPCoAbray2=c(-0.15,0.25),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.015,0.015),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-0.8,0.8),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.3,0.3),
                              ylimPCoAchao1=c(-0.025,0.025),ylimPCoAchao2=c(-0.3,0.5),
                              ylimPCoAchao3=c(-0.01,0.01),ylimPCoAchao4=c(-0.05,0.05),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.8,0.8),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.4,0.4))
    
    Fct_Graph_GA_GB_par_4_tiff(Detection="D4",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.15,0.25),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.015,0.015),
                               ylimG3_1=c(-0.025,0.025),ylimG3_2=c(-0.3,0.5),
                               ylimG3_3=c(-0.01,0.01),ylimG3_4=c(-0.05,0.05),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.8,0.8),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.4,0.4)) 
    
    Fct_Graph_GA_GB_par_2_tiff(Detection="D4",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.5,1.5),ylimG1_2=c(-1.5,1.5),
                               ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.8,0.8),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.3,0.3)) 
    
    # D5  # -----------------

    Fct_Graph_GA_GB_tout_tiff(Detection="D5",couleur_noir="couleur",
                              ylimACP1=c(-1,1),ylimACP2=c(-1,1),
                              ylimACP3=c(-1,1),ylimACP4=c(-1,1),
                              ylimAFC1=c(-1.5,1.5),ylimAFC2=c(-1.5,1.5),
                              ylimAFC3=c(-0.8,0.8),ylimAFC4=c(-0.8,0.8),
                              ylimPCoAbray1=c(-0.03,0.03),ylimPCoAbray2=c(-0.2,0.4),
                              ylimPCoAbray3=c(-0.01,0.01),ylimPCoAbray4=c(-0.05,0.05),
                              ylimnMDSbray1=c(-0.4,0.4),ylimnMDSbray2=c(-1,1),
                              ylimnMDSbray3=c(-0.2,0.2),ylimnMDSbray4=c(-0.3,0.3),
                              ylimPCoAchao1=c(-0.03,0.03),ylimPCoAchao2=c(-0.2,0.7),
                              ylimPCoAchao3=c(-0.005,0.005),ylimPCoAchao4=c(-0.1,0.15),
                              ylimnMDSchao1=c(-0.2,0.2),ylimnMDSchao2=c(-0.6,0.6),
                              ylimnMDSchao3=c(-0.1,0.1),ylimnMDSchao4=c(-0.4,0.4))
    
    Fct_Graph_GA_GB_par_4_tiff(Detection="D5",couleur_noir="couleur",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao",
                               ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                               ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                               ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.2,0.4),
                               ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.05,0.05),
                               ylimG3_1=c(-0.03,0.03),ylimG3_2=c(-0.2,0.7),
                               ylimG3_3=c(-0.005,0.005),ylimG3_4=c(-0.1,0.15),
                               ylimG4_1=c(-0.2,0.2),ylimG4_2=c(-0.6,0.6),
                               ylimG4_3=c(-0.1,0.1),ylimG4_4=c(-0.4,0.4)) 
    
    Fct_Graph_GA_GB_par_2_tiff(Detection="D5",couleur_noir="couleur",
                               graph_1="AFC",graph_2="nMDS_bray",
                               ylimG1_1=c(-1.5,1.5),ylimG1_2=c(-1.5,1.5),
                               ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                               ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-1,1),
                               ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.3,0.3)) 
#--------------------------------------------------------------
    
  # les RV entre les matrices de distances des especes
    # D1  # -----------------

    Fct_Graph_RV_Sp_tt_tiff(Detection="D1") 
    Fct_Graph_RV_Sp_par_4_tiff(Detection="D1",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="nMDS_chao",graph_4="PCoA_chao")
    Fct_Graph_RV_Sp_par_2_tiff(Detection="D1",
                               graph_1="AFC",graph_2="nMDS_bray")
    
    # D2  # -----------------

    Fct_Graph_RV_Sp_tt_tiff(Detection="D2") 
    Fct_Graph_RV_Sp_par_4_tiff(Detection="D2",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="nMDS_chao",graph_4="PCoA_chao")
    Fct_Graph_RV_Sp_par_2_tiff(Detection="D2",
                               graph_1="AFC",graph_2="nMDS_bray")  
    
    # D3  # -----------------

    Fct_Graph_RV_Sp_tt_tiff(Detection="D3") 
    Fct_Graph_RV_Sp_par_4_tiff(Detection="D3",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Sp_par_2_tiff(Detection="D3",
                               graph_1="AFC",graph_2="nMDS_bray")  
    
    # D4  # -----------------

    Fct_Graph_RV_Sp_tt_tiff(Detection="D4") 
    Fct_Graph_RV_Sp_par_4_tiff(Detection="D4",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Sp_par_2_tiff(Detection="D4",
                               graph_1="AFC",graph_2="nMDS_bray")
    
    # D5  # -----------------

    Fct_Graph_RV_Sp_tt_tiff(Detection="D5") 
    Fct_Graph_RV_Sp_par_4_tiff(Detection="D5",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Sp_par_2_tiff(Detection="D5",
                               graph_1="AFC",graph_2="nMDS_bray")
#--------------------------------------------------------------
    
  # les RV entre les matrices de distances des sites
    # D1  # -----------------

    Fct_Graph_RV_Site_tt_tiff(Detection="D1") 
    Fct_Graph_RV_Site_par_2_tiff(Detection="D1",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Site_par_2_tiff(Detection="D1",
                               graph_1="AFC",graph_2="nMDS_bray")
    
    # D2  # -----------------

    Fct_Graph_RV_Site_tt_tiff(Detection="D2") 
    Fct_Graph_RV_Site_par_2_tiff(Detection="D2",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Site_par_2_tiff(Detection="D2",
                               graph_1="AFC",graph_2="nMDS_bray")  
    
    # D3  # -----------------

    Fct_Graph_RV_Site_tt_tiff(Detection="D3") 
    Fct_Graph_RV_Site_par_2_tiff(Detection="D3",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Site_par_2_tiff(Detection="D3",
                               graph_1="AFC",graph_2="nMDS_bray")  
    
    # D4  # -----------------

    Fct_Graph_RV_Site_tt_tiff(Detection="D4") 
    Fct_Graph_RV_Site_par_2_tiff(Detection="D4",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Site_par_2_tiff(Detection="D4",
                               graph_1="AFC",graph_2="nMDS_bray")
    
    # D5  # -----------------

    Fct_Graph_RV_Site_tt_tiff(Detection="D5") 
    Fct_Graph_RV_Site_par_2_tiff(Detection="D5",
                               graph_1="ACP",graph_2="PCoA_bray",
                               graph_3="PCoA_chao",graph_4="nMDS_chao")
    Fct_Graph_RV_Site_par_2_tiff(Detection="D5",
                               graph_1="AFC",graph_2="nMDS_bray")
    
    
    
    