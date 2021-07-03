
# Lecture de données
Data<-read.csv2('Data_for_UCI_named.csv')
# Le modèle de régression linéaire multiple avec toute les variables explicatives.
myModel = lm(stab~tau1+tau2+tau3+tau4+p2+p3+p4+g1+g2+g3+g4, data = Data)
# Visualisation de résultat
summary(myModel)

# step () est une fonction de R utilisée pour la sélection des variables explicatives intéressantes
  step(myModel)

# Le nouveau modèle qui contient les variables sélectionnées par la procédure step()
  newModel = lm(stab~tau1+tau2+tau3+tau4+p3+g1+g2+g3+g4, data = Data)
  summary(newModel)

library(leaps)
library(caTools)
library(dplyr)

# Tests de validation
    # Test de normalité
          # Test de shapiro-wlki
          shapiro.test(resid(newModel))
          # Test de KS
          ks.test(resid(newModel),pnorm)
    
    # Test d'homoscédastisité
          #test d'homoscédastisité par la méthode graphique
          par(mfrow=c(2,2)) # init 4 charts in 1 panel
          plot(newModel)
          
          #test d'homoscedastisité par la méthode statistiques
          library(vars)   
          library(het.test)
          model1<-VAR(Data,p=1)
          whites.htest(model1)

    #valeurs aberrantes
          library(outliers)
          grubbs.test(resid(newModel))
          
# Application de la méthode de sélection pas à pas
    # On ajoute la bibliothèque FactoMineR qui contient la procédure RegBest 
    library(FactoMineR)
    # En utilisant la procédure RegBest pour appliquer la méthode pas à pas
    RegBest(y=Data[,12], x = Data[,-12], nbest=1)

    # Tests de validation
          # Nouveau modèle. annulation de trois variables p1, p2 et p3.
          NewModel = lm(stab~tau1+tau2+tau3+tau4+g1+g2+g3+g4, data = Data)
          # Test de normalité
              # Test de shapiro-wlki
                  shapiro.test(resid(NewModel))
              # Test de KS
                  ks.test(resid(NewModel),pnorm)
          
          # Test d'homoscédastisité par la méthode statistiques
              model1<-VAR(Data,p=1)
              whites.htest(model1)
          
          #valeurs aberrantes
              grubbs.test(resid(newModel))
              
    # Calcul du critère AIC du modèle obtenu par cette méthode
        AIC(myModel)
          