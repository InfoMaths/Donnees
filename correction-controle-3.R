library(tidyverse)

# Question 1: Définition des conteneurs
valtrim <- c(2500, 1000, 2300, 2000, 2600, 2100, 1000, 2300, 
             2600, 2000, 1500, 2500, 1900,1000, 2200, 2400)
numtr <- 1:16

# Question 2: Définition de la série chronologique
serie <- ts(valtrim, start=c(2018,1), frequency=4)
serie

# Question 3: le chronogramme
plot(serie, type='o', col='green4')
grid(16)

# Question 4: La création du data.frame
data <- data.frame(numtr, valtrim)
data

# Question 4 : Les courbes des profils
an1 <- serie[1:4]
an2 <- serie[5:8]
an3 <- serie[9:12]
an4 <- serie[13:16]
plot(an1, col=1, lwd=2, type='o', ylim=c(min(valtrim), max(valtrim)), 
     main='Courbes des profils',
     xlab='Trimestres', ylab='valeurs')
lines(an2, col=2 , lwd=2, type='o')
lines(an3, col=3 , lwd=2, type='o')
lines(an4, col=4 , lwd=2, type='o')
grid(4)

# Question 6: Commande et Équation la droite y=-2.206 t + 2012.5
droite <- lm(valtrim ~numtr)
droite
summary(droite) ### commande pour avoir plus d'information sur les coefficients de la droite

## Facultatif : Affichage de la droite sur le chronogramme avec la courbe lissée
plot(data$numtr, data$valtrim, col='red', type='o', lwd=2,
     xlab='Trimestres', ylab='valeurs',
     main='Chronogramme avec le Trend')
grid(16)  
abline(droite, col='blue', lwd=3)
text(4, 1900, "y=-2.206 x +2012.5", col='blue', cex=1.2)

# Question 7 Analyse de la série chronologique
## a) le modèle est multiplicatif car les profils se coupent

## b) Calcul des moyennes mobiles d'ordre 2 et stockage dans data
poids=c(1/4, 1/2, 1/4)
MM_2 <- stats::filter(valtrim, filter=poids, sides =2) ## Calcul des MM_2
data$MM_2 <- MM_2 ## stockage dans la base de données data
data

## c) Détermination du Trend par les MM-2
trend <- lm(data$MM_2~data$numtr)
trend   # le trend est y= -10.6 t +2049.1
summary(trend) ### commande pour avoir plus d'information sur les coefficients de la droite

## d) la pente est négative a= -10.6 la tendance est à la baisse

# Question 8 : Calcul des estimées par le trend
data$vallesti <- -10.6*numtr+2049.1 # calcul des valeurs estimées et stockage dans la base
data

# Question 9: Comparaison des valeurs réelles et des valeurs estimées
## la comparaison est une division des valeurs réelles par les valeurs estimées
data$compare <- round(data$valtrim/data$vallesti,2)      ## valeur à deux décimales et stockage
data
# Question 10: Calcul des coefficients saisonniers et stockage dans la base

## Calcul des coefficients saisonniers
coef_1 <- round((data$compare[1]+data$compare[5]+data$compare[9]+data$compare[13])/4, 2)
coef_2 <- round((data$compare[2]+data$compare[6]+data$compare[10]+data$compare[14])/4, 2)
coef_3 <- round((data$compare[3]+data$compare[7]+data$compare[11]+data$compare[15])/4, 2)
coef_4 <- round((data$compare[4]+data$compare[8]+data$compare[12]+data$compare[16])/4, 2)
cat('les coefficients saisonniers :','\n')
cat(coef_1,' ', coef_2,' ',coef_3,' ', coef_4)
## stockage dans la base
coef_s <- rep(c(coef_1, coef_2, coef_3, coef_4),4) # stockage dans la base
coef_s
data$coef_s <- coef_s
data
# Question 11 : Correction des coefficients saisonniers
## On calcule la moyenne des coefficients saisonnier
moyenne <- mean(data$coef_s)
cat('la moyenne des coefficient saisonniers est :', moyenne,'\n')
cat(' le modèle est multiplicatif')
cat(' Il faut corriger les coefficients saisonniers car leur moyenne est diffirente de 1. ')
# Question Prévision pour le trimestre 2 de l'année 2026
## ce trimestre a pour numéro 34 =16+4*(2025-2021)+2
valeur <- -10.6*34+2049.1
valeur_corrigee <- valeur/coef_2  # car le modèle est multiplicatif
cat("la valeur estimée corrigée pour le trimestre 2 de l' année 2026 est :", valeur_corrigee)
##


## Bonus : Graphique contenant le chronogramme, les MM_2 et le Trend
plot(data$valtrim, col='red', type='o', lwd=2, main="Chronogramme, MM_2 et Trend",
     xlab='Trimestres', ylab='valeurs')
lines(data$MM_2, col='green4', type='o', lwd=3)
abline(a=2049.1, b=-10.6, col='blue', lwd=4)
text(10, 1500, "y=-10.6 x +2049.1", col='blue', cex=2)
grid(10)
legend('bottom',legend=c("chronogramme","MM_2","Trend"), col=c('red','green4','blue'), lwd=2)


## Autre façons de définir les profils avec matplot
profil <- matrix(data$valtrim, nrow=4, byrow=TRUE)
profil
matplot(t(profil), type='o', lwd=2,
        main='Courbes des profils avec matplot',
        xlab='Trimestres', ylab='valeurs', col=rainbow(4))
grid(4)
