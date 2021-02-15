
# Lecture du jeu de données
    Dataset = read.csv2("Data_for_UCI_named.csv")
    View(Dataset)

# Centrage réduction des données
    Dataset.center_scale = scale(Dataset, center = T, scale = T)
    # Sauvegarde de variables centrée réduites
    write.csv(Dataset.center_scale,  'Data_for_UCI_named_cr.csv')
    Dataset.center_scale = read.csv2("Data_for_UCI_named_cr.csv")

# Application du k-means au tableau des variables quantitatives centrées et réduites
# Center = k - nombre de groupes demandés
# nstart = 5 - nombre d'essais avec différents individus de départ car les résultats sont dépendants de l’initialisation
    time = 10
    for (k in 1:time){
        kmeans_app <- kmeans(Dataset.center_scale, centers=k, nstart=5)
        # Affichage des résultats
        print(kmeans_app)
    }
    
    # Evaluation de la proportion d'inertie expliquée
        inertie.expliquee <- rep(0,times=time)
        # On commence l'évaluation d'inertie à partir de deux classe car elle est nulle lorsqu'elle existe une seul classe
        for (k in 1:time){
            kmeans_app <- kmeans(Dataset.center_scale, centers=k, nstart=5)
            inertie.expliquee[k] <- kmeans_app$betweenss/kmeans_app$totss
        }
    # Graphique
        plot(1:time,inertie.expliquee,type="b",ylab="% inertie expliquée") 
        inertie.expliquee

library(FactoMineR)

# CAH
    Res<-HCPC(Dataset.center_scale,nb.clust=2)
    Res$data.clust
    Res$desc.var
    Res$desc.ind

# Calcul du taux d'inertie avant et après la consolidation de la CAH.
    Inertie<-Res$call
    Inertie


