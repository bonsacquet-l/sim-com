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
source(file.path("R","Graph_Detect_D2D4_par_2L_tiff.R"))

source(file.path("R","Graph_Detect_D5_par_2L_tiff.R"))

source(file.path("R","Graph_G1_G2_par_2L_tiff.R"))

source(file.path("R","Graph_GA_GB_par_2L_tiff.R"))

source(file.path("R","Graph_RV_Sp_par_2L_tiff.R"))

#-- executer les fonctions graphiques
# les groupes plus ou moins detectables
# D4 -----------------
Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D4",couleur_noir="couleur",
                                  graph_1="PCoA_bray",graph_2="PCoA_chao",
                                  ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.04,0.15),
                                  ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.02,0.025),
                                  ylimG2_1=c(-0.02,0.02),ylimG2_2=c(-0.1,0.35),
                                  ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.09,0.09))

Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D4",couleur_noir="couleur",
                                 graph_1="ACP",graph_2="nMDS_chao",
                                 ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                 ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                                 ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.2,0.6),
                                 ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.5,0.5))

Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D4",couleur_noir="couleur",
                                 graph_1="AFC",graph_2="nMDS_bray",
                                 ylimG1_1=c(-1.2,1.5),ylimG1_2=c(-1.2,1.5),
                                 ylimG1_3=c(-0.8,0.8),ylimG1_4=c(-0.8,0.8),
                                 ylimG2_1=c(-0.4,0.4),ylimG2_2=c(-0.4,0.7),
                                 ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.25,0.3))
# D2 -----------------

Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D2",couleur_noir="couleur",
                                  graph_1="ACP",graph_2="AFC",
                                  ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                  ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                                  ylimG2_1=c(-1.2,1.2),ylimG2_2=c(-1.2,1.2),
                                  ylimG2_3=c(-0.7,0.7),ylimG2_4=c(-0.7,0.7))

Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D2",couleur_noir="couleur",
                                 graph_1="PCoA_bray",graph_2="PCoA_chao",
                                 ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.05,0.05),
                                 ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.03,0.03),
                                 ylimG2_1=c(-0.02,0.02),ylimG2_2=c(-0.05,0.06),
                                 ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.045,0.045))


Fct_Graph_Detect_D2D4_par_2_tiff(Detection="D2",couleur_noir="couleur",
                                 graph_1="nMDS_bray",graph_2="nMDS_chao",
                                 ylimG1_1=c(-0.45,0.45),ylimG1_2=c(-0.5,0.5),
                                 ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.35,0.35),
                                 ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.4,0.4),
                                 ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.4,0.4))
# D5 -----------------

Fct_Graph_Detect_D5_par_2_tiff(Detection="D5",couleur_noir="couleur",
                                graph_1="ACP",graph_2="AFC",
                                ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                                ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                              ylimG2_1=c(-1,1),ylimG2_2=c(-1.2,1.2),
                              ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_Detect_D5_par_2_tiff(Detection="D5",couleur_noir="couleur",
                               graph_1="PCoA_bray",graph_2="PCoA_chao",
                               ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.3,0.4),
                               ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.05,0.05),
                               ylimG2_1=c(-0.015,0.015),ylimG2_2=c(-0.2,0.8),
                               ylimG2_3=c(-0.005,0.005),ylimG2_4=c(-0.1,0.15))

Fct_Graph_Detect_D5_par_2_tiff(Detection="D5",couleur_noir="couleur",
                               graph_1="nMDS_bray",graph_2="nMDS_chao",
                               ylimG1_1=c(-0.3,0.3),ylimG1_2=c(-0.8,1.1),
                               ylimG1_3=c(-0.25,0.25),ylimG1_4=c(-0.25,0.25),
                               ylimG2_1=c(-0.15,0.15),ylimG2_2=c(-0.5,0.6),
                               ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.3,0.35))
#--------------------------------------------------------------

# les groupes 1 et 2
# D1  # -----------------

Fct_Graph_G1_G2_par_2_tiff(Detection="D1",couleur_noir="couleur",
                            graph_1="ACP",graph_2="AFC",
                            ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                            ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                            ylimG2_1=c(-1.2,1.5),ylimG2_2=c(-1.2,1.5),
                            ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_G1_G2_par_2_tiff(Detection="D1",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.1,0.1),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.07,0.07),
                           ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.25,0.25),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.2,0.2)) 

Fct_Graph_G1_G2_par_2_tiff(Detection="D1",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.5,0.5),
                           ylimG1_3=c(-0.25,0.25),ylimG1_4=c(-0.4,0.4),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.8,0.8),
                           ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.6,0.6)) 

# D2  # -----------------

Fct_Graph_G1_G2_par_2_tiff(Detection="D2",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1,1),ylimG2_2=c(-1,1),
                          ylimG2_3=c(-0.6,0.6),ylimG2_4=c(-0.6,0.6))

Fct_Graph_G1_G2_par_2_tiff(Detection="D2",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.025,0.025),ylimG1_2=c(-0.04,0.04),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.018,0.018),
                           ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.045,0.045),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.025,0.025)) 

Fct_Graph_G1_G2_par_2_tiff(Detection="D2",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.4,0.4),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.25,0.25),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.3,0.3),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.2,0.2)) 

# D3  # -----------------

Fct_Graph_G1_G2_par_2_tiff(Detection="D3",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.2,1),ylimG2_2=c(-1.2,1),
                          ylimG2_3=c(-0.7,0.7),ylimG2_4=c(-0.7,0.7))

Fct_Graph_G1_G2_par_2_tiff(Detection="D3",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.025,0.025),ylimG1_2=c(-0.025,0.35),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.08,0.08),
                           ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.2,0.5),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.2,0.35))
                           
Fct_Graph_G1_G2_par_2_tiff(Detection="D3",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.8,1.6),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.7,0.7),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.7,0.7),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.7,0.7)) 

# D4  # -----------------

Fct_Graph_G1_G2_par_2_tiff(Detection="D4",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.5,1.5),ylimG2_2=c(-1.5,1.5),
                          ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_G1_G2_par_2_tiff(Detection="D4",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.025,0.025),ylimG1_2=c(-0.15,0.25),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.015,0.015),
                           ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.3,0.5),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.05,0.05)) 

Fct_Graph_G1_G2_par_2_tiff(Detection="D4",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.8,0.8),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.3,0.3),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.8,0.8),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.4,0.4))

# D5  # -----------------

Fct_Graph_G1_G2_par_2_tiff(Detection="D5",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.5,1.5),ylimG2_2=c(-1.5,1.5),
                          ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_G1_G2_par_2_tiff(Detection="D5",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.2,0.4),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.05,0.05),
                           ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.2,0.7),
                           ylimG2_3=c(-0.005,0.005),ylimG2_4=c(-0.1,0.15))

Fct_Graph_G1_G2_par_2_tiff(Detection="D5",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-1,1),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.3,0.3),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.6,0.6),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.4,0.4))
#--------------------------------------------------------------

# les groupes A et B
# D1  # -----------------

Fct_Graph_GA_GB_par_2_tiff(Detection="D1",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.2,1.5),ylimG2_2=c(-1.2,1.5),
                          ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_GA_GB_par_2_tiff(Detection="D1",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.1,0.1),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.07,0.07),
                           ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.25,0.25),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.2,0.2))

Fct_Graph_GA_GB_par_2_tiff(Detection="D1",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.5,0.5),
                           ylimG1_3=c(-0.25,0.25),ylimG1_4=c(-0.4,0.4),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.8,0.8),
                           ylimG2_3=c(-0.2,0.2),ylimG2_4=c(-0.6,0.6)) 

# D2  # -----------------

Fct_Graph_GA_GB_par_2_tiff(Detection="D2",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1,1),ylimG2_2=c(-1,1),
                          ylimG2_3=c(-0.6,0.6),ylimG2_4=c(-0.6,0.6))

Fct_Graph_GA_GB_par_2_tiff(Detection="D2",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.025,0.025),ylimG1_2=c(-0.04,0.04),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.018,0.018),
                           ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.045,0.045),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.025,0.025)) 

Fct_Graph_GA_GB_par_2_tiff(Detection="D2",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.4,0.4),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.25,0.25),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.3,0.3),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.2,0.2)) 

# D3  # -----------------

Fct_Graph_GA_GB_par_2_tiff(Detection="D3",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.2,1),ylimG2_2=c(-1.2,1),
                          ylimG2_3=c(-0.7,0.7),ylimG2_4=c(-0.7,0.7))

Fct_Graph_GA_GB_par_2_tiff(Detection="D3",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.025,0.025),ylimG1_2=c(-0.025,0.35),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.08,0.08),
                           ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.2,0.5),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.2,0.35)) 

Fct_Graph_GA_GB_par_2_tiff(Detection="D3",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.8,1.6),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.7,0.7),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.7,0.7),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.7,0.7)) 

# D4  # -----------------

Fct_Graph_GA_GB_par_2_tiff(Detection="D4",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.5,1.5),ylimG2_2=c(-1.5,1.5),
                          ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_GA_GB_par_2_tiff(Detection="D4",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.025,0.025),ylimG1_2=c(-0.15,0.25),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.015,0.015),
                           ylimG2_1=c(-0.025,0.025),ylimG2_2=c(-0.3,0.5),
                           ylimG2_3=c(-0.01,0.01),ylimG2_4=c(-0.05,0.05)) 

Fct_Graph_GA_GB_par_2_tiff(Detection="D4",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-0.8,0.8),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.3,0.3),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.8,0.8),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.4,0.4)) 

# D5  # -----------------

Fct_Graph_GA_GB_par_2_tiff(Detection="D5",couleur_noir="couleur",
                          graph_1="ACP",graph_2="AFC",
                          ylimG1_1=c(-1,1),ylimG1_2=c(-1,1),
                          ylimG1_3=c(-1,1),ylimG1_4=c(-1,1),
                          ylimG2_1=c(-1.5,1.5),ylimG2_2=c(-1.5,1.5),
                          ylimG2_3=c(-0.8,0.8),ylimG2_4=c(-0.8,0.8))

Fct_Graph_GA_GB_par_2_tiff(Detection="D5",couleur_noir="couleur",
                           graph_1="PCoA_bray",graph_2="PCoA_chao",
                           ylimG1_1=c(-0.03,0.03),ylimG1_2=c(-0.2,0.4),
                           ylimG1_3=c(-0.01,0.01),ylimG1_4=c(-0.05,0.05),
                           ylimG2_1=c(-0.03,0.03),ylimG2_2=c(-0.2,0.7),
                           ylimG2_3=c(-0.005,0.005),ylimG2_4=c(-0.1,0.15)) 

Fct_Graph_GA_GB_par_2_tiff(Detection="D5",couleur_noir="couleur",
                           graph_1="nMDS_bray",graph_2="nMDS_chao",
                           ylimG1_1=c(-0.4,0.4),ylimG1_2=c(-1,1),
                           ylimG1_3=c(-0.2,0.2),ylimG1_4=c(-0.3,0.3),
                           ylimG2_1=c(-0.2,0.2),ylimG2_2=c(-0.6,0.6),
                           ylimG2_3=c(-0.1,0.1),ylimG2_4=c(-0.4,0.4))

#--------------------------------------------------------------
#--------------------------------------------------------------

# les RV entre les matrices de distances des especes
# D1  # -----------------

Fct_Graph_RV_Sp_par_2_tiff(Detection="D1",
                              graph_1="ACP",graph_2="AFC")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D1",
                              graph_1="PCoA_bray",graph_2="PCoA_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D1",
                              graph_1="nMDS_bray",graph_2="nMDS_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D1",
                           graph_1="AFC",graph_2="nMDS_bray")

# D2  # -----------------

Fct_Graph_RV_Sp_par_2_tiff(Detection="D2",
                              graph_1="ACP",graph_2="AFC")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D2",
                              graph_1="PCoA_bray",graph_2="PCoA_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D2",
                              graph_1="nMDS_bray",graph_2="nMDS_chao") 

# D3  # -----------------

Fct_Graph_RV_Sp_par_2_tiff(Detection="D3",
                              graph_1="ACP",graph_2="AFC")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D3",
                              graph_1="PCoA_bray",graph_2="PCoA_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D3",
                              graph_1="nMDS_bray",graph_2="nMDS_chao")

# D4  # -----------------

Fct_Graph_RV_Sp_par_2_tiff(Detection="D4",
                              graph_1="PCoA_bray",graph_2="PCoA_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D4",
                             graph_1="ACP",graph_2="nMDS_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D4",
                             graph_1="AFC",graph_2="nMDS_bray")

# D5  # -----------------

Fct_Graph_RV_Sp_par_2_tiff(Detection="D5",
                              graph_1="ACP",graph_2="AFC")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D5",
                             graph_1="PCoA_bray",graph_2="PCoA_chao")
Fct_Graph_RV_Sp_par_2_tiff(Detection="D5",
                             graph_1="nMDS_bray",graph_2="nMDS_chao")



