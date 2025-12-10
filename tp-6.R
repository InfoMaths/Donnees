library(tidyverse)

df <- read.csv("k:/marbio.csv", sep = ";", header = TRUE)
df


# Question 1) 2) 3) 4) et 5)
# créer la série pour la colonne “Acartia”
acartia_ts <- ts(df$Acartia, start = c(2011, 1), frequency = 4)
acartia_ts
plot(acartia_ts,col='red', main = "Acartia – série trimestrielle depuis 2011",
     xlab = "Année", ylab = "Valeur")
grid(col="lightgray")
reg_Aca <- lm(acartia_ts ~ time(acartia_ts))
abline(reg_Aca, col="blue", lwd=2)
text(2020,800, labels =" y=-15.7 *x + 31932.3", col="blue")
summary(reg_Aca)



# Question 6) Prévission pour 2030
future_time <-2030 + c(0,1/4, 2/4, 3/4)
reg_Aca
future_values <- reg_Aca$coefficients[1] + reg_Aca$coefficients[2] * future_time
future_values

cat("Prévisions pour Acartia en 2030:\n:    ", future_time)
cat(round(future_values,1), "\n")

## Question 7)  les profils de la serie acartia_t
decomp_Aca <- decompose(acartia_ts)
plot(decomp_Aca, col=4)


## Question 8)  les profils de la série acartia_t 
profils <- matrix(acartia_ts, ncol = 4, byrow = TRUE)
profils
matplot(t(profils), type = "b", pch = 20,
        xlab = "Trimestre", ylab = "Acartia",
        main = "Profils trimestriels d'Acartia par année")


## Question 9) Moyennes mobiles d'ordre 4
poids=c(0.5,1,1,1,0.5)/4
MM_4 <- stats::filter(acartia_ts, filter = poids, sides = 2)
plot(acartia_ts, col='red', main = "Acartia avec Moyennes Mobiles d'ordre 4",
     xlab = "Année", ylab = "Valeur")
lines(MM_4, col='blue', lwd=2)
legend("topright", legend = c("Acartia", "Moyenne Mobile d'ordre 4"),
       col = c("red", "blue"), lty = 1, lwd = 2)


# Question 10) Régression linéaire sur les MM d'ordre 4
droite <- lm(MM_4 ~ time(MM_4))
abline(droite, col="green", lwd=2)
text(2020,600, labels =" y=-18.2 *x + 37106.6", col="green")
summary(droite)

# Question 11) data frame de la série chronologique avec toutes les colonnes
df_ts <-data.frame(trimestres= time(acartia_ts),
                   Acartia=as.numeric(acartia_ts), 
                   MM_4=round(as.numeric(MM_4),1) )
 
# Question 12) les Valeurs estimées par le modèle de régression linéaire
df_ts$Val_estim <- round(-18.2 * df_ts$trimestres + 37106.6,1)


# Question 13) Coefficients saisonniers pour Acartia
df_ts$diff <- round(df_ts$Acartia / df_ts$Val_estim,2)

mat <- round(matrix(df_ts$diff, ncol = 4, byrow = TRUE),2)
mat
moyennes_saisonnieres <- round(colMeans(mat, na.rm = TRUE),2)
moyennes_saisonnieres
df_ts$coef_saison <- rep(moyennes_saisonnieres, length.out = nrow(df_ts))

moyenne <- mean(df_ts$coef_saison)
moyenne
df_ts$coef_saison_corr <- round(df_ts$coef_saison / moyenne,2)

## Question 14) Série chronologique multivariée pour toutes les colonnes du data frame  

# question 
# Si vous voulez créer une série “multivariée” (toutes les colonnes), vous pouvez faire :
df_ts <- ts(df[, -1], start = c(2011, 1), frequency = 4)



plot(df_ts, plot.type = "single", col = rainbow(ncol(df_ts)),
     main = "Tous groupes – série trimestrielle depuis 2011",
     xlab = "Année", ylab = "Valeur")
legend("topright", legend = colnames(df_ts), col = rainbow(ncol(df_ts)), lty = 1)
grid(col="lightgray")


# Question 15-bis) Régression linéaire pour toutes les colonnes
par(mfrow=c(3,2))
for (i in 1:ncol(df_ts)) {
  serie <- df_ts[, i]
  plot(serie, main = colnames(df_ts)[i], xlab = "Année", ylab = "Valeur", col='red')
  reg <- lm(serie ~ time(serie))
  abline(reg, col="blue", lwd=2)
  eq <- paste0("y=", round(reg$coefficients[2],1), " *x + ", round(reg$coefficients[1],1))
  text(2020, max(serie, na.rm = TRUE) * 0.9, labels = eq, col="blue")
  print(summary(reg))
}
par(mfrow=c(1,1))

# Question 15) Comparaison des colonnes avec boxplots 
x1 <- df$Acartia
x2 <- df$Oithona
x3 <- df$Nauplii
x4 <- df$Acanthaires
x5 <- df$Cladocerans

boxplot(x1, x2, x3, x4, x5,
        names = c("Acartia", "Calanus", "Centropages", "Euchaeta", "Oithona"),
        main = "Comparaison des groupes zooplanctoniques",
        ylab = "Valeurs",
        col = rainbow(5))
# Ajout des séries individuelles
maxi <- max(c(x1, x2, x3, x4, x5), na.rm = TRUE)
minimo <- min(c(x1, x2, x3, x4, x5), na.rm = TRUE)
ylim <- c(minimo - 0.1 * abs(minimo), maxi + 0.1 * abs(maxi))
plot(1, type="n", xlim=c(0.5,5.5), ylim=ylim,
     xlab="Groupes", ylab="Valeurs",
     main="Séries individuelles superposées")
plot(x1, col='red', type="o", ylim=c(minimo, maxi))
lines(x2, col='blue', type="o")
lines(x3, col='green', type="o")
lines(x4, col='purple', type="o")
lines(x5, col='orange', type="o")
legend("topright", legend = c("Acartia", "Calanus", "Centropages", "Euchaeta", "Oithona"),
       col = c("red", "blue", "green", "purple", "orange"), lty = 1, pch=1)

# Question 16) Etdude de la distribution de Oithona
qqplot(time(df_ts),x2, col='green', main = "Q-Q plot pour Oithona",
       xlab = "Quantiles théoriques", ylab = "Quantiles échantillon")
qqnorm(x2, main = "Q-Q plot pour Oithona")
qqline(x2, col = "red")
shapiro.test(x2)
hist(x2, breaks = 10, col='lightblue', main = "Histogramme pour Oithona",
     xlab = "Valeurs", ylab = "Fréquence")
boxplot(x2, main = "Boxplot pour Oithona", col='lightgreen',
        ylab = "Valeurs")


# Question 17) Corrélation entre Acartia et Oithona
cor_acartia_oithona <- cor(x1, x2, use = "complete.obs")
cor_acartia_oithona
plot(x1, x2, main = "Corrélation entre Acartia et Oithona",
     xlab = "Acartia", ylab = "Oithona", col='purple')
abline(lm(x2 ~ x1), col="blue", lwd=2)
text(max(x1, na.rm = TRUE)*0.7, max(x2, na.rm = TRUE)*0.9,
     labels = paste0("Corrélation = ", round(cor_acartia_oithona,2)), col="blue")

# Question 18) Recherche des valeurs aberrantes dans Oithona
Q1 <- quantile(x2, 0.25, na.rm = TRUE)
Q3 <- quantile(x2, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- which(x2 < lower_bound | x2 > upper_bound)
outliers_values <- x2[outliers]
outliers_values
hist(df$Oithona, breaks = 10, col='lightblue', main = "Histogramme pour Oithona avec valeurs aberrantes",
     xlab = "Valeurs", ylab = "Fréquence")
abline(v=lower_bound, col='red', lwd=6, lty=2)
abline(v=upper_bound, col='red', lwd=2, lty=2)
text(lower_bound, 5, labels = "Limite inférieure", col='red', pos=4)
text(upper_bound, 5, labels = "Limite supérieure", col='red', pos=4)
boxplot(x2, main = "Boxplot pour Oithona avec valeurs aberrantes", col='lightgreen',
        ylab = "Valeurs")
points(rep(1, length(outliers_values)), outliers_values, col='red')
abline(h=lower_bound, col='blue', lwd=6, lty=2)
abline(h=upper_bound, col='blue', lwd=2, lty=2)
text(150, lower_bound, labels = "Limite inférieure", col='red', pos=4)
text(1,4050, upper_bound, labels = "Limite supérieure", col='red', pos=4)

# Fin du TP
