
# Lecture du jeu de données
    Dataset = read.csv2("Data_for_UCI_named.csv")
    View(Dataset)
    
# Centrage réduction des données
    # Sauvegarde du jeu de données avec des variables centrée réduites
    write.csv(scale(Dataset, center = T, scale = T), 'Data_for_UCI_named_cr.csv')
    # Lecture du jeu de données avec des variables centrées réduites
    Dataset.center_scale = read.csv2("Data_for_UCI_named_cr.csv")
    View(Dataset.center_scale)
    
# l'ACP
    # Nous ajoutons la bibliothèque PCA qui contient la méthode PCA
    library(FactoMineR)
    ACP = PCA(Dataset.center_scale,ncp=12)
    summary(ACP)
    
#Calcul de l'indice KMO et des MSAi
    # Nous ajoutons la bibliothèque psych qui contient la méthode KMO
    library(psych)
    KMO <- KMO(cor(Dataset)) 
    KMO
    
# Calcul des valeurs propres, des pourcentage et Cumul des pourcentages d’inertie
    # Nous appelons simplement l'attribut eig de la variable ACP
    ACP$eig
    
# Graphique des valeurs propres
    plot(1:nrow(ACP$eig),ACP$eig[,1],type="b",
                                    ylab="Valeurs propres",
                                    xlab="Composantes",
                                    main="Graphique des valeurs propres")
    
# Détermination de la dimension du sous espace en utilisant Inértie_Intra/Inértie_Totale<0.05  
    nombre_eigs = nrow(ACP$eig)
    var_total = var(ACP$eig[,1]) * nombre_eigs
    inertie_expliquee <- rep(0,times=nombre_eigs)
    
    for(k in 1:nombre_eigs)
    {
        inertie_expliquee[k] <- var(ACP$eig[i:nombre_eigs,1])*(nombre_eigs-k)/var_total
    }
    data.frame(inertie_expliquee)

# Calcul de cos2 des variables sur le sous espace
    ACP$var$cos2[,1:12]
    
# Distinction des variables bien représentées, moyennement représentées et faiblement représentées sur le sous espace
    corrplot(ACP$var$cos2[,1:12], is.corr=FALSE)
    
# Contribution des variables dans chaque axe du sous espace
    ACP$var$contrib[,1:12] 
    
# Application de la CAH au tableau des contributions des variables aux axes du sous espace
    Dataset = data.frame(ACP$var$contrib[,1:12])
    CAH = HCPC(Dataset,nb.clust=-1)
    
# Tracer le nuage des variables projeté sur les 2 premiers axes.
    ACP_plot = PCA(Dataset.center_scale)
    
# Calcul du cos2 des individus sur le sous espace
    ACP$ind$cos2[,1:12]

# Distinction des individus bien représentées, moyennement représentées et faiblement représentées sur le sous espace
  fviz_cos2(ACP, choice="ind")

# Contribution des individues dans chaque axe du sous espace
    ACP$ind$contrib[,1:12]
    
# Appliucation de la CAH au tableau des contributions des individues aux axes du sous espace
    Contribution = data.frame(ACP$ind$contrib[,1:12])
    CAH = HCPC(Contribution,nb.clust=-1)








