library(tidyverse)
# Les données
indices <- c(118.2, 129, 138.9, 157.1, 148.6, 154.5, 163, 184, 163.3, 175.3, 189.1, 217.9)
num <- 1:12

## Définition de la base
data_2 <- data.frame(num,indices)
data_2

# définition de la série
serie_22 <- ts(indices, start=c(1997,1), frequency=4)
serie_22

# le chronogramme
par(mar=c(1,1,1,1))
an_97 <- data_2[1:4]
plot(serie_22, col='red', type='o')
grid()



## Les profils
an_97 <- data_2$indices[1:4]
an_98 <- data_2$indices[5:8]
an_99 <- data_2$indices[9:12]
max <- max(indices)+10
min <- min(indices)-10
cat('min=',min,'    maw=', max)
plot(an_97, col='red', type='o', lwd=2, ylim=c(min, max), 
     xlab='années', ylab='indice', main='les profils')
lines(an_98, col='green', type='o', lwd=2)
lines(an_99, col='blue', type='o', lwd=2)
grid()

# moyenne mobile d'ordre-2 et stockage dans la base
poids=c(0.5,1,0.5)/2
MM_2 <- stats::filter(indices, poids, sides=2)
MM_2
data_2$MM_2 <- round(MM_2,0)
data_2

# la droite d'ajustement linéaire par MOINDRES CARRES
numus <- 2:11
droite <- lm(data_2$MM_2 ~data_2$num)
droite
summary(droite) ## affiche des détails de la droite
esti <- round(droite$coefficients[2]*data_2$num+droite$coefficients[1],1)
droite$coefficients[2]
data_2$estmes <- esti
data_2
comp <- data_2$indices-data_2$estmes
data_2$compa <- round(comp,1)
data_2

## coefficient saisonniers
coef_1 <- (data_2$compa[1]+ data_2$compa[5]+ data_2$compa[9])/3
coef_2 <- (data_2$compa[2]+ data_2$compa[6]+ data_2$compa[10])/3
coef_3 <-(data_2$compa[3]+ data_2$compa[7]+ data_2$compa[11])/3
coef_4 <- (data_2$compa[4]+ data_2$compa[8] + data_2$compa[12])/3
cat('coef_1',coef_1)
cat('coef_2',coef_2)
cat('coef3',coef_3)
cat('coef_4',coef_4)

moyen_saison <-(coef_1 +coef_2 +coef_3 + coef_4)/4
moyen_saison
mean(data_2$compa)

coef <- c(coef_1, coef_2, coef_3, coef_4)
mean(coef)

# stockage des coefficients saisonniers
data_2$coefnc <- round(rep(coef,3),2)
data_2


# Correction des coefficients saisonniers
mimi <- mean(data_2$coefnc)
mimi
data_2$ceof_si_cr <- data_2$coefnc-mimi
data_2
round(mean(data_2$ceof_si_cr),2)

newdat <-data.frame(numu=c(15,25,26,27))
droite <- lm(data_2$MM_2 ~ data_2$num)
####prev <- predict(droite, newdata = newdat)
###prev
pre_3_2000 <- droite$coefficients[2]*15+droite$coefficients[1]-data_2$ceof_si_cr[3]
pre_4_2026 <- droite$coefficients[2]*120+droite$coefficients[1]-data_2$ceof_si_cr[4]
cat("prevision pour le trimestre 3 de l'année 2000:   =>", pre_3_2000)
cat("prevision pour le trimestre 4 de l'année 2026:==>>", pre_4_2026)
############
