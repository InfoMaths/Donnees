library(tidyverse)
df <- data.frame(ChickWeight)
### La base poids des poussins
## 4 Variables: 
## weight: poids en grammes
## Time: temps en jours
## Chick: identifiant du poussin
## Diet: regime alimentaire (1,2,3,4)
###### 1) Etude préliminaire des données
str(df)
summary(df)

####### 2 on groupe par régime et temps pour avoir le poids moyen
poulet <- df |> group_by(Diet, Time) |> 
  summarise(mean_weight = mean(weight),.groups = 'drop') |> 
  as.data.frame()

####### 3 Tracer les courbes des poids moyens en fonction du temps pour chaque régime
poul1 <- poulet[poulet$Diet == 1, ]
poul2 <- poulet[poulet$Diet == 2, ]
poul3 <- poulet[poulet$Diet == 3, ]
poul4 <- poulet[poulet$Diet == 4, ]


plot(poul1$Time,poul1$mean_weight, type='b', col=2,  xlab="Temps( jours)", ylab="Poids moyen (grammes)", 
     main="Poid moyen en fonction du temps et le Diet", ylim=c(0,400), lwd=2)
lines(poul1$Time, poul2$mean_weight, type='b', col=3, lwd=2)
lines(poul1$Time,poul3$mean_weight, col=4, type="b", lwd=2)
lines(poul1$Time, poul3$mean_weight, col=5, type="b", lwd=2)
legend("topleft", legend=c("Diet 1", "Diet 2", "Diet 3", "Diet 4"),
       col=c(2,3,4,5), lty=1, cex=0.8)
grid(col='black')


####### 4)  Tracer des représentations graphiques du poids des poussins
# Histogramme 
hist(df$weight,braks=10, main="Histogramme Poids des poussins", 
     xlab="poids (grammes)", col="lightblue", border="black")


boxplot(weight ~ Diet, data=df, 
        main="Boxplot poids et régime alimentaire", 
        xlab="Diet", ylab="Poids  (grammes)", 
        col=c("lightgreen", "lightpink", "lightyellow", "lightgray"))

stripchart(weight ~ Diet, data=df, 
           main="Stripchart of Chick Weights by Diet", 
           xlab="Diet", ylab="Weight (grams)", 
           col=c("red", "blue", "green", "purple"), 
           pch=16, method="jitter")




plot(df$Time, df$weight, 
     main="Poids des poussins en fonction du temps", 
     xlab="Temps (Jours)", ylab="Poids (grammes)", 
     col=rainbow(10), pch=16)
legend("topleft", legend=c("Diet 1", "Diet 2", "Diet 3", "Diet 4"),
       col=1:4, pch=16, cex=0.8)

mosaicplot(table(df$Diet, df$Time), 
           main="Mosaique Plot : Diet vs Temps", 
           xlab="Diet", ylab="Temps (Jours)", 
           color=TRUE)

# Disposition en grille 2x2
par(mfrow=c(2,2))
boxplot(weight ~ Diet, data=df, 
        main="Boxplot Poids des poussins par régime alimentaire", 
        xlab="Diet", ylab="Poids (grammes)", 
        col=c("lightgreen", "lightpink", "lightyellow", "lightgray"))

stripchart(weight ~ Diet, data=df, 
           main="Stripchart :Poids et régime alimentaire", 
           xlab="Diet", ylab="Poids (grammes)", 
           col=c("red", "blue", "green", "purple"), 
           pch=16, method="jitter")

plot(df$Time, df$weight, 
     main="Points : Poids en fonction du temps", 
     xlab="Temps (Jours)", ylab="Poids (grammess)", 
     col=rainbow(10), pch=16)
legend("topleft", legend=c("Diet 1", "Diet 2", "Diet 3", "Diet 4"),
       col=1:4, pch=16, cex=0.8)

mosaicplot(table(df$Diet, df$Time), 
           main="Mosaic : Régime vs Temps", 
            xlab="Diet", ylab="temps (Jours)",color=TRUE)

par(mfrow=c(1,1))  # Réinitialiser la disposition           

######5) Etude de la distribution du poids des poussins pour le régime alimentaire 1
poids <- df$weight[df$Diet == 1]
temps <- df$Time[df$Diet == 1]
poids
qqplot(temps,poids, main="Q-Q Plot : Poids des poussins (Diet 1)", col="blue", pch=16)
hist(poids, breaks=10, main="Histogramme : Poids des poussins (Diet 1)", 
     xlab="Poids (grammes)", col="lightblue", border="black")
boxplot(poids, main="Boxplot : Poids des poussins (Diet 1)", ylab="Poids (grammes)", col="lightgreen")
qqnorm(poids, main="Q-Q Plot : Poids des poussins (Diet 1)", col="blue", pch=16)
qqline(poids, col="red", lwd=2)
shapiro.test(poids)           
# Test de normalité de Shapiro-Wilk
# p-value < 0.05 indique que la distribution n'est pas normale

#### Recherche des valeurs abberrantes 

boxplot_stats <- boxplot.stats(poids)
outliers <- boxplot_stats$out
outliers
# Afficher les valeurs aberrantes 
if(length(outliers) > 0) {
  cat("Valeurs aberrantes détectées pour le régime alimentaire 1 :\n")
  print(outliers)
} else {
  cat("Aucune valeur aberrante détectée pour le régime alimentaire 1.\n")
}

# Visualiser les valeurs aberrantes sur le boxplot
boxplot(poids, main="Boxplot : Poids des poussins (Diet 1) avec valeurs aberrantes", 
        ylab="Poids (grammes)", col="lightgreen")
points(rep(1, length(outliers)), outliers, col="red", pch=19)
# Les points rouges indiquent les valeurs aberrantes

## Calcul des fréquences
freq_table <- table(poids)
freq_table
barplot(freq_table, main="Barplot : Fréquences des poids (Diet 1)", 
        xlab="Poids (grammes)", ylab="Fréquence", col="lightblue", border="black")
pie(freq_table, main="Pie Chart : Répartition des poids (Diet 1)",
    col=rainbow(length(freq_table)))    
legend("topright", legend=names(freq_table), fill=rainbow(length(freq_table)), cex=0.8)


## Etude comparative des régimes alimentaires 
poids1 <- df$weight[df$Diet == 1]
poids2 <- df$weight[df$Diet == 2]
poids3 <- df$weight[df$Diet == 3]
poids4 <- df$weight[df$Diet == 4]

fre <- table(df$Diet)
fre
porptions <- prop.table(df$weight)


barplot(fre, main="Barplot : Fréquences des régimes alimentaires",
        xlab="Diet", ylab="Fréquence", col=c("lightgreen",
                                               "lightpink", 
                                               "lightyellow", 
                                               "lightgray"), border="black")
pie(fre, main="Pie Chart : Répartition des régimes alimentaires",
    col=rainbow(length(fre)))
  legend("topright", legend=names(fre), fill=rainbow(length(fre)), cex=0.8)
boxplot(poids1, poids2, poids3, poids4,
        names=c("Diet 1", "Diet 2", "Diet 3", "Diet 4"),
        main="Boxplot : Poids des poussins par régime alimentaire",
        xlab="Diet", ylab="Poids (grammes)",
        col=c("lightgreen", "lightpink", "lightyellow", "lightgray"))  
# Valeurs en pourcentage
total_counts <- sum(fre)
percentages <- round((fre / total_counts) * 100,2)
percentages

categories <- group_by(df, Diet) %>%
  summarise(
    count = n(),
    mean_weight = mean(weight),
    sd_weight = sd(weight)
  )
categories
propo <- prop.table(categories$count)
cat("les pourcentages par régime alimentaire ", round(propo*100,2) )
