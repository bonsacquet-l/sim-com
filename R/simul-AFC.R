#============================================================#
# fonction pour la simulations d une communaute d especes et #
# son analyse par AFC                                        #
# lionel.bonsacquet                                          #
#============================================================#

Fct_simulAFC<-function(Nsimul=1000, Nplot=100, Nsp=20, minAbon=5, maxAbon=50,
                   minSeG1, maxSeG1, minSeG2, maxSeG2,
                   sp1, sp2, sp11, sp22, DetectMean,VarDetect,
                   Detect_Mean_On_G1Min=500, Detect_Mean_On_G1Max=500, minSlope, maxSlope,
                   fichier)
{
  #---------------------------------------------------------------#
  # pour les sauvegardes des parametres communs aux n simulations #
  #---------------------------------------------------------------#
  #-- les chemins de sauvegardes --#
  saveData<-file.path("Outcome","out-simul","AFC",paste("AFC_Simul_",fichier,".Rdata",sep=""))
  
  #-- initialisation des tableaux de sauvegarde --#
    #- Sauvegarde pour les AFC "on informed data"
    M_InertiaAFC<-matrix(NA,nrow=Nsimul,ncol=1)         # total inertie pour chaque simul
    M_TabEigAFC<-matrix(NA,nrow=Nsimul, ncol=2)			    # les valeurs propre axes 1 et 2 de l'AFC de chaque simulation pour le tableau non degrade
    A_TabCoordAFC<-array(NA, dim=c(Nsp, 2, Nsimul) )   	# tableau 3D contenant les coordonnees de chaque especes axes 1 et 2
    A_TabCoordAFC_Site<-array(NA, dim=c(Nplot, 2, Nsimul) )   	# tableau 3D contenant les coordonnees de chaque site axes 1 et 2
    
    #- Sauvegarde pour les AFC "on naïve data"
    M_InertiaAFC_Naive<-matrix(NA,nrow=Nsimul,ncol=1)
    M_TabEigAFC_Naive<-matrix(NA,nrow=Nsimul, ncol=2)			  # matrice contenant les valeurs propre axes 1 et 2 de l'AFC
    A_TabCoordAFC_Naive<-array(NA, dim=c(Nsp, 2, Nsimul) ) 	# tableau 3D contenant les coordonnees de chaque especes axes 1 et 2
    A_TabCoordAFC_Site_Naive<-array(NA, dim=c(Nplot, 2, Nsimul) ) 	# tableau 3D contenant les coordonnees de chaque site axes 1 et 2
    
    #- Sauvegarde des tableau des abondances simulees
    A_MemAbon<-array(NA, dim=c(Nplot,Nsp,Nsimul) )         # array des abondances "informees" simulees
    A_MemAbon_Naive<-array(NA, dim=c(Nplot,Nsp,Nsimul) )   # array des abondances naive simulees
  
    #- Sauvegarde des donnees especes
    M_MemDetectSp<-matrix(NA,nrow=Nsimul,ncol=Nsp)         # matrice des detections moyennes espece/simulation
    A_MemDetectSp_Plot<-array(NA, dim=c(Nplot,Nsp,Nsimul) ) # array des detection par plot/espece/simulation
  
    #- Sauvegarde des controle des abondances nulles (total et par especes)
    M_NbrSpPerdu<-matrix(0,nrow=Nsimul,ncol=1)     # nombre d'espece perdu entre les donnees informees et donnees naives
    M_NbrSpAbsente<-matrix(0,nrow=Nsimul,ncol=1)  # nombre d'espece absente dans les donnees informees
  
    M_NbrZeroAbon<-matrix(0,nrow=Nsimul,ncol=1)  # Nombre de zero dans la matrice d'abondance
    M_NbrZeroAbon_Naive<-matrix(0,nrow=Nsimul,ncol=1)  # Nombre de zero dans la matrice d'abondance naive
  
  #------------------------------------#
  # repartition des sites sur G1 et G2 # identique a chaque simulation
  #------------------------------------#
  #-- sur G1
  V_Site_G1<-seq(from=0+(1000/(2*Nplot)), to=1000, by=(1000/Nplot))
    
  #-- sur G2
  V_Site_G2<-rep(seq(from=(0+(360/(ceiling(Nplot/10)*2))), to=359, by=(360/(ceiling(Nplot/10)))),10)
  V_Site_G2<-V_Site_G2[seq(1,Nplot)]
    
  #---------------------------------------------------#
  # repartition des optimums des especes sur G1 et G2 # identique a chaque simulation
  #---------------------------------------------------#
  #-- sur G1
  V_OptimumSp_G1<-ceiling(seq(from=(0+1000/(2*Nsp)), to=1000, by=(1000/Nsp)))
    
  #-- sur G2
  V_OptimumSp_G2_1<-ceiling(seq(from=ceiling(0+(180/Nsp)), to=180, by=(2*180/Nsp)))
  V_OptimumSp_G2_2<-ceiling(seq(from=ceiling(180+(180/Nsp)), to=360, by=(2*180/Nsp)))
  V_OptimumSp_G2<-vector()
    
  for (is in 1:(Nsp/2)) {
      V_OptimumSp_G2<-c(V_OptimumSp_G2,V_OptimumSp_G2_1[is],V_OptimumSp_G2_2[is])
  }
    
  V_OptimumSp_G2<-V_OptimumSp_G2[seq(1,Nsp)]
    
  #------------------------------------#
  # debut de la boucle des simulations # abondances et abondances naives ainsi que AFC
  #------------------------------------#
  for (simul in 1:Nsimul)	{			# boucle pour faire les simulations une par une
    print(simul)
    
    #__________abondances informees__________
    #-- tableau pour contenir les abondances
    M_tabprov<-matrix(NA,nrow=Nplot,ncol=Nsp)		# Pour stocker les abondances moyenne
    M_tab<-matrix(NA,nrow=Nplot,ncol=Nsp)		    # Pour les abondances reelles "informees"
    
    #-- tirage aleatoire des caracteristique des especes (chgt a chaque simulation)
    V_Nmax<-runif(Nsp,min=minAbon,max=maxAbon)		# nombre maximum que peut atteindre la moyenne 
                                                  # d'une espece a son optimum
      #- sur G1
      V_seG1<-runif(Nsp,min=minSeG1,max=maxSeG1)	# ecart type de la loi normale que suit l'abondance de l'especes
      
      #- sur G2
      V_seG2<-runif(Nsp,min=minSeG2,max=maxSeG2)		#ecart type de la loi normale que suit l'espece
    
    #-- pour stocker la valeur du coef multiplicateur pour chaque espece
    V_CoefAbon<-c(rep(NA,Nsp))
    
    #-- calcul coef multiplicateur et abondance MOYENNE de chaque espece sur chaque plot
    for (is in 1:Nsp)	{   # boucle espece
      V_CoefAbon[is]<-V_Nmax[is]*(V_seG1[is]*V_seG2[is]*(sqrt(2*3.14))^2)	#calcul du coef multiplicateur grace a
                                                                          # de la densite de la loi normale 
      for (zs in 1:Nplot) {		# boucle plot (densite loi bi-normale)
        M_tabprov[zs,is]<-(1/(V_seG2[is]*V_seG1[is]*(sqrt(2*3.14))^2)*exp(-0.5*(((V_Site_G2[zs]-V_OptimumSp_G2[is])/V_seG2[is])^2+((V_Site_G1[zs]-V_OptimumSp_G1[is])/V_seG1[is])^2))*V_CoefAbon[is])
      }			
    }	
  
    #-- Abondances reelles sur les plots
    for (i in 1:Nplot) {     # boucle sur les plots
      for (z in 1:Nsp) {     # boucle sur les especes
        M_tab[i,z]<-rpois(1,M_tabprov[i,z])			# loi de poisson autour de l'abondance MOYENNE du site
      }
    }

    #--sauvegarde de toute les abondances reelles ("informee") simulees
    A_MemAbon[,,simul]<-as.matrix(M_tab)       # sauvegarde des abondances
    M_NbrZeroAbon[simul,1]<-sum(M_tab[]==0)    # sauvegarde nbr abondance nulle

    #__________AFC sur les donnnees informees__________
    #-- mise en forme des donnees et AFC
    M_tab<-as.data.frame(M_tab)
    resAFC<-dudi.coa(M_tab,scannf = F,nf=2)			# realisation AFC sur le tableau
  
    #-- sauvegarde des resultats de AFC
    M_TabEigAFC[simul,]<-as.matrix(resAFC$eig[1:2]/sum(resAFC$eig))	# inertie expliquee par axe 1 et 2
    A_TabCoordAFC[,,simul]<-as.matrix(resAFC$co[,1:2]) # coordonnees des pt sp dans premier plan de projection				
    A_TabCoordAFC_Site[,,simul]<-as.matrix(resAFC$li[,1:2]) # coordonnees des pt site dans premier plan de projection	
    M_InertiaAFC[simul,]<-sum(resAFC$eig) # inertie totale
  
    #__________Simulation du probeme de detection__________
    #-- matrice de sauvegarde des valeurs des parametres
    M_Detect_Mean_On_G1Sp<-matrix(NA,nrow=Nsp,ncol=1)	# 500 pour tt les especes en lineaire
    M_Slope<-matrix(NA,nrow=Nsp,ncol=1)				# les pentes du gradient de detection pour chaque especes
    M_DetectSp<-matrix(1,nrow=Nsp,ncol=1)	  # les proba de detect de chaque espece, valeur 1 par default
    M_DetectsSp_Plot<-matrix(1,nrow=Nplot,ncol=Nsp)  # les proba des Sp/Plot, valeur 1 par default
  
    #-- Detection moyenne de chaque espece
    if(sp1!=0) {
      for (is in sp1:sp2) {        # pour toute les especes entre sp1 et sp2
        M_DetectSp[is,1]<-rnorm(1,DetectMean,VarDetect)			#remplissage de la matrice (heterogeneite entre especes)
        if(M_DetectSp[is,1]>0.9){M_DetectSp[is,1]<-0.9}			#limite la detectabilite <1
        if(M_DetectSp[is,1]<0.1){M_DetectSp[is,1]<-0.1}			#limite la detectabilite >0
      }
    }

    if(sp11!=0) {
      for (is in sp11:sp22) {   # pour toute les especes entre sp11 et sp22
        M_DetectSp[is,1]<-rnorm(1,DetectMean,VarDetect)			#remplissage de la matrice (heterogeneite entre especes)
        if(M_DetectSp[is,1]>0.9){M_DetectSp[is,1]<-0.9}			#limite la detectabilite <1
        if(M_DetectSp[is,1]<0.1){M_DetectSp[is,1]<-0.1}			#limite la detectabilite >0
      }
    }

    #-- sauvegarde des detection moyenne de chaque simulation
    M_MemDetectSp[simul,]<-t(M_DetectSp) # sauvegarde pour toute les simulation

    #-- localisation de la detection moyenne et pente du gradient
    for (is in 1:Nsp) {			
      M_Slope[is,1]<-runif(1,minSlope,maxSlope)				#facteur Beta du modele lineaire
      M_Detect_Mean_On_G1Sp[is,]<-runif(1,Detect_Mean_On_G1Min,Detect_Mean_On_G1Max)	#500 tt le tps
    }
        
    #-- proba de dtection de chaque espece a chaque plot
    if(sp1!=0) {
      for (is in sp1:sp2) {							
        dmean<-M_DetectSp[is,]		          # detection moyenne
        dmeanlogit<-log(dmean/(1-dmean))		# le logit de la detection moyenne 
        V_pprov<-(dmeanlogit-M_Detect_Mean_On_G1Sp[is]*M_Slope[is])+V_Site_G1*M_Slope[is]
  
        M_DetectsSp_Plot[,is]<-exp(V_pprov)/(1+exp(V_pprov))	# inversion du logit
      }
    }   
        
    if(sp11!=0) {
      for (is in sp11:sp22) {							
        dmean<-M_DetectSp[is,]		          # detection moyenne
        dmeanlogit<-log(dmean/(1-dmean))		# le logit de la detection moyenne 
        V_pprov<-(dmeanlogit-M_Detect_Mean_On_G1Sp[is]*M_Slope[is])+V_Site_G1*M_Slope[is]
  
        M_DetectsSp_Plot[,is]<-exp(V_pprov)/(1+exp(V_pprov))	# inversion du logit
      }
    }   

    #-- sauvegarde des detection sp/plot de chaque simulation
    A_MemDetectSp_Plot[,,simul]<-M_DetectsSp_Plot
        
    #___________Abondance Naives (loi binomial)__________
    #-- pour stocker les abondances naives
    M_tab_Naive<-matrix(NA,nrow=Nplot,ncol=Nsp) 

    #-- calcul des abondances naives  
    for (z in 1:Nsp) {       # sp par sp
      for (i in 1:Nplot) {	 # plot par plot		
        M_tab_Naive[i,z]<-sum(rbinom(M_tab[i,z],1,M_DetectsSp_Plot[i,z]))	 #loi binomial
      }
    }
        
    #-- sauvegarde des abondances naives de toutes les simulations
    A_MemAbon_Naive[,,simul]<-as.matrix(M_tab_Naive)
    M_NbrZeroAbon_Naive[simul,1]<-sum(M_tab_Naive[]==0)  # nbr de zero dans M_tab_Naive
        
    #__________AFC sur les donnnees naives__________      
    #-- AFC 
      M_tab_Naive<-as.data.frame(M_tab_Naive)				       #changement de format
      resAFC_Naive<-dudi.coa(M_tab_Naive,scannf = F,nf=2)	 #realisation de l'AFC

    #-- sauvegarde des resultats de AFC
    M_TabEigAFC_Naive[simul,]<-as.matrix(resAFC_Naive$eig[1:2]/sum(resAFC_Naive$eig))	#remplissage de la table des valeurs propre
    A_TabCoordAFC_Naive[,,simul]<-as.matrix(resAFC_Naive$co[,1:2])			  #remplissage des coordonnees des sp
    A_TabCoordAFC_Site_Naive[,,simul]<-as.matrix(resAFC_Naive$li[,1:2])	  #remplissage des coordonnees des sites
    M_InertiaAFC_Naive[simul,]<-sum(resAFC_Naive$eig)	
    
    #__________Controle especes perdue ou non__________  
    for (esp in 1:Nsp) {
      if(sum(M_tab_Naive[,esp])==0){if(sum(M_tab[,esp])!=0){M_NbrSpPerdu[simul,]<-(M_NbrSpPerdu[simul,]+1)}}
      if(sum(M_tab[,esp])==0){M_NbrSpAbsente[simul,]<-(M_NbrSpAbsente[simul,]+1)}
    }	
  }                 #-- fin de la boucle de chaque simulation
  
  Max_NbrSpPerdu<-max(M_NbrSpPerdu)         # plus grd nbr especes perdues lors des n simul
  Max_NbrSpAbsente<-max(M_NbrSpAbsente)    # lus grd nbr especes absentes lors des n simul
  
  #-----------------#
  # les sauvegardes #
  #-----------------#
  save(Nsimul, Nplot, Nsp,V_Site_G1, V_Site_G2,
       V_OptimumSp_G1, V_OptimumSp_G2,A_MemAbon,
       A_TabCoordAFC, A_TabCoordAFC_Site,
       M_TabEigAFC, M_InertiaAFC,
       M_MemDetectSp,A_MemDetectSp_Plot,
       A_MemAbon_Naive, A_TabCoordAFC_Naive, A_TabCoordAFC_Site_Naive,
       M_TabEigAFC_Naive, M_InertiaAFC_Naive,
       M_NbrSpAbsente,Max_NbrSpPerdu, Max_NbrSpAbsente,
       M_NbrZeroAbon, M_NbrZeroAbon_Naive,
       fichier, saveData,
       list = c("Nsimul", "Nplot", "Nsp","V_Site_G1", "V_Site_G2",
                "V_OptimumSp_G1", "V_OptimumSp_G2","A_MemAbon",
                "A_TabCoordAFC", "A_TabCoordAFC_Site",
                "M_TabEigAFC", "M_InertiaAFC",
                "M_MemDetectSp","A_MemDetectSp_Plot",
                "A_MemAbon_Naive", "A_TabCoordAFC_Naive", "A_TabCoordAFC_Site_Naive",
                "M_TabEigAFC_Naive", "M_InertiaAFC_Naive",
                "M_NbrSpAbsente","Max_NbrSpPerdu", "Max_NbrSpAbsente",
                "M_NbrZeroAbon", "M_NbrZeroAbon_Naive",
                "fichier", "saveData"),
       file = saveData)
 
  #------------------------------------------#
  # les sorties de la fonction de simulation #
  #------------------------------------------#
  List_Res<-list(Nsimul=Nsimul, Nplot=Nplot, Nsp=Nsp,
                 V_Site_G1=V_Site_G1, V_Site_G2=V_Site_G2,
                 V_OptimumSp_G1=V_OptimumSp_G1, V_OptimumSp_G2=V_OptimumSp_G2,
                 A_MemAbon=A_MemAbon, A_TabCoordAFC=A_TabCoordAFC, M_TabEigAFC=M_TabEigAFC,
                 M_InertiaAFC=M_InertiaAFC,
                 M_MemDetectSp=M_MemDetectSp,A_MemDetectSp_Plot=A_MemDetectSp_Plot,
                 A_MemAbon_Naive=A_MemAbon_Naive, A_TabCoordAFC_Naive=A_TabCoordAFC_Naive,
                 M_TabEigAFC_Naive=M_TabEigAFC_Naive, M_InertiaAFC_Naive=M_InertiaAFC_Naive,
                 M_NbrSpAbsente=M_NbrSpAbsente, M_NbrSpPerdu=M_NbrSpPerdu,
                 Max_NbrSpPerdu=Max_NbrSpPerdu, Max_NbrSpAbsente=Max_NbrSpAbsente,
                 M_NbrZeroAbon=M_NbrZeroAbon, M_NbrZeroAbon_Naive=M_NbrZeroAbon_Naive,
                 A_TabCoordAFC_Site=A_TabCoordAFC_Site, A_TabCoordAFC_Site_Naive=A_TabCoordAFC_Site_Naive,
                 fichier=fichier, saveData=saveData)
    
  return(List_Res)
}  

################################################################################
################################################################################