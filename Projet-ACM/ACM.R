# Lecture du jeu de données
	Dataset = read.csv2("Data_for_UCI_named.csv")
	View(Dataset)

# Discretization en 3 classes pour chaque variable.
	new_Dataset = data.frame()
	data_frame  = data.frame()
	result      = data.frame()
	n = 2
	for( col_name in colnames(Dataset))
	{
		new_Dataset["Taux",col_name] = 0
		for(i in 1:nrow(Dataset)+1)
		{
			new_Dataset[i,col_name]=Dataset[i,col_name]
		}
		kmean = kmeans(Dataset[col_name],centers = n, nstart = 5)
		while(kmean$betweenss/kmean$totss<.5)
		{
			n = n + 1
			kmean = kmeans(Dataset[col_name],centers = n, nstart = 5)
		}
		current = paste(col_name,"_Modifié",sep="") 
		new_Dataset["Taux", current] = kmean$betweenss/kmean$totss
		for(i in 1:nrow(Dataset)+1)
		{
			data_frame[i-1,col_name] = paste(col_name,kmean$centers[kmean$cluster[i-1]],sep=":")
			new_Dataset[i,current] = kmean$centers[kmean$cluster[i-1]]
		}
		print(c(col_name,n , kmean$betweenss/kmean$totss))
	}
	write.csv(new_Dataset,"Data_for_UCI_named_New.csv")

# Construction du tableau disjonctif complet
	# Transformation des nombres en facteurs pour construire le tableau disjonctif;
	k=0
	while(k < ncol(data_frame))
	{
	  k=k+1
	  data_frame[,k] = as.factor(data_frame[,k])
	}
	# Construction du tableau disjonctif complet
	Tabl_Disj_Com <- tab.disjonctif(data_frame)
	write.csv(Tabl_Disj_Com,"Tableau_Disjonctif_Complet.csv")

# Calcul de la fréquence de chaque modalité
	Freq_Mod = apply(Tabl_Disj_Com,2,sum)/(nrow(Tabl_Disj_Com))
	print(Freq_Mod)

# Application de l'ACM au tableau disjonctif complet
	library(FactoMineR)
	ACM <- MCA(data_frame, graph=FALSE)
	print(ACM) 

# Les valeurs propres, le pourcentage d’inertie de chaque valeur propre ainsi que le cumul des pourcentages d’inertie.
	Valeur_Propres = ACM$eig
	print(Valeur_Propres)

# Le graphique des valeurs propres
	par(mfrow=c(1,1))
	barplot(Valeur_Propres[,1], main= "Histogramme des valeurs propres", 
		names.arg=rownames(ACM$eig),
		 xlab= "Axes", 
		 ylab= "Pourcentage d'inertie", cex.axis=0.8)

#####  Partie 2: Nuage de Modalités  #####

# Calcul du cos2 des modalités
	Cos2_Modalite = ACM$var$cos2
	print(Cos2_Modalite)

# Distinction des modalités bien, moyennement et faiblement représentées sur le sous espace
	library(corrplot)
	par(mfrow=c(1,1))
	corrplot(Cos2_Modalite[,1:2], is.corr=FALSE)


# Calcul de la contribution des modalités 
	Contribution_Modalite = ACM$var$contrib
	print(Contribution_Modalite)

# Application de la CAH au tableau des contributions des modalités
	write.table(Contribution_Modalite,"Contribution_Modalite.csv", 
					sep=";", 
					col.names=TRUE, 
					dec=',', 
					row.names=TRUE)
	Data_Cont_Mod = read.csv2("Contribution_Modalite.csv",row.names=1)
	View(Data_Cont_Mod)
	CAH <- HCPC(Data_Cont_Mod, graph = FALSE)
	plot(CAH, choice = "tree")

# Tracer le nuage des modalités projeté sur les 2 premiers axes.
	plot(CAH, axes = c(1, 2))

####  Partie 3: Nuage des individus  ####
	
# Calcul du cos2 des individus
	Cos2_Individus = ACM$ind$cos2
	print(Cos2_Individus)

# Calcul de la contribution des modalités 
	Contribution_Individus = ACM$ind$contrib
	print(Contribution_Individus)

# Application de la CAH au tableau des contributions des individus 
	write.table(Contribution_Individus,"Contribution_Individus.csv",
					sep=";",
					col.names=TRUE,
					dec=',', 
					row.names=TRUE)
	Contribution_Individus = read.csv2("Contribution_Individus.csv",row.names=1)
	CAH <- HCPC(Contribution_Individus, graph = FALSE)
	plot(CAH, choice = "tree")


###  Partie 4: Nuage des variables  ###

# Calcul des coefficients de corrélation des variables 
	ACM$var$eta2

# Graphique des coefficients de corrélation 
	library(ade4)
	plot(ACM$var$eta2, boxes = FALSE)
	