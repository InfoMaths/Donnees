library(tidyverse)


## Modèle linéaire univarié avec variable continue
data_Mai <- read.csv2("mais.csv")
data_Mai
plot(x=data_Mai$nitro, data_Mai$yield, col=rainbow(10),
     xlab="Azote", ylab="Rendement",main="rendement par l'azote")
grid(10)

## Ajustement linéaire par la méthode des moindres carrées.
droite <- lm(yield ~nitro, data =data_Mai)
droite
summary(droite)

## intervalle de confiance des coefficients
confint(droite, level =0.95)

## Afficher les coefficient y= nitro * x +intercept
coefficients(droite)

## prédictions pour des doses de 10 à 20
predict(droite)[10:20]
round(predict(droite)[10:20],2)

## Prédiction pour d'autres données
nd <- data.frame(nitro =seq(from =0 , to= 100, by =10))
round(predict(droite, newdata =nd),2)

## Analyse des résidus: La différence entre les valeur réelles et les valeurs estimées
residu_f <- data.frame(nitro =data_Mai$nitro,
                       residu_lm =residuals(droite),
                       residu_calcul =data_Mai$yield - predict(droite))
sample_n(residu_f, 10)

## Représentation graphique des résidus
plot(x= residu_f$nitro, y=residu_f$residu_lm, col=rainbow(10),
     xla='azote', ylab='Résidus', main='Représentation graphique des résidus')
abline(h=0, col='red', lwd= 4)

## Distribution des résidus

qqplot(x= residu_f$nitro, residu_f$residu_lm,   # produit un qqplot de deux jeux de donnée
       col=rainbow(10),xlab='Azote', ylab='Les résidus',
       main=" quantile -quantile des résidus"
       )               
grid()
qqnorm(residu_f$residu_lm, col= 'green') # produit un qqplot pour une loi normale
qqline(residu_f$residu_lm, col='blue', lwd=2)   # trace la droite de Henry
grid()

## Histogramme
hist(residu_f$residu_lm, breaks=10, col=rainbow(10), xlab='residu',
     main="histogrammes des résidus")

## Boîte à moustache horizontale
boxplot(residu_f$residu_lm, col=4, main=' boîte à moustache des résidus')
abline(h=mean(residu_f$residu_lm), col='red', lwd=4)
abline(h=quantile(residu_f$residu_lm, probs= seq(0.25, 0.75, 0.25)), col='blue', lwd=2 )
legend("bottomleft", legend=c("Moyenne","Les quantiles "), col=c('red','blue'), lwd=2)

## Boîte à moustache verticale
boxplot(residu_f$residu_lm, col=4, main=' boîte à moustache des résidus', horizontal =TRUE)
abline(v=mean(residu_f$residu_lm), col='red', lwd=4)
abline(v=quantile(residu_f$residu_lm, probs= seq(0.25, 0.75, 0.25)), col='blue', lwd=2 )
legend("bottomleft", legend=c("Moyenne","Les quantiles "), col=c('red','blue'), lwd=2)

### Régression multiple
# Etude du rendement en fonction de la topologie du terrain
data_Mai$topo <- factor(data_Mai$topo)
levels(data$topo)
plot(data_Mai$topo , data_Mai$yield , col=rainbow(4),
     xlab='topologie du terrain', ylab='le rendement')

## Droite d'ajustement linéaire du rendement en fonction de la topologie
droite_2 <- lm(yield ~topo, data =data_Mai)
droite_2
summary(droite_2)

## Régression Multiple à plusieurs variables
# lat: latitude, long : longitude et bv : teneur en matière organique du sol
droite_3 <- lm( yield ~lat + long +nitro + topo +bv, data =data_Mai)
droite_3
summary(droite_3)


## Il faut centrer les variable
data_Mai_cent <- data_Mai |> mutate_at(c("lat", "long", "nitro", "bv"), scale)
droite_4 <- lm( yield ~lat + long +nitro + topo +bv, data =data_Mai_cent)
droite_4
summary(droite_4)

## Modèle dépendant de l'azote , la topologie et l'interaction entre l'azote et la topologie
droite_4 <- lm(yield ~ nitro + topo + nitro:topo, data = data_Mai_cent)
droite_4
summary(droite_4)

## On peut écrire simplement :lm(yield ~nitre * topo , data = data_Mai_cent)

model.matrix(~nitro *topo , data =data_Mai_cent)
model.matrix(~nitro *topo , data =data_Mai_cent) |>  head(15)

## la parcelle
plot(data_Mai$lat, data_Mai$long, col=rainbow(100))
plot(data_Mai$long, data_Mai$lat, col=rainbow(100))

#####################################################################
library(leaflet)                    ## Lycée nature
leaflet() |> addTiles()
leaflet() |> addTiles() |> addMarkers(lng=- 1.38018 ,lat=46.66517,  popup="Lycée Nature")

lat <- 46.66517
lng <- - 1.38018

leaflet() %>%
  addTiles() %>%                      # fond de carte OSM
  setView(lng = lng, lat = lat, zoom = 12) %>%  # centre la roche
  addMarkers(lng = lng, lat = lat, label = "Lycée Nature", popup="ici le lycée")


lat <- 46.66517
lng <- - 1.38018

leaflet() %>%
  addTiles() %>%                      # fond de carte OSM
  setView(lng = lng, lat = lat, zoom = 12) %>%  # centre la roche
  addMarkers(lng = lng, lat = lat, label = "Lycée Nature", popup="ici le lycée") |> 
  addCircleMarkers(radius=30,col='green',
                   stroke =FALSE,
                   fillOpacity =0.3,
                   lng =lng, lat =lat,
                   label="secteur")
  




######################################################################
library(leaflet)
library(sf)

# Création de l'objet Ligne simple
line_view <- st_sfc(st_linestring(cbind(data_Mai$long, data_Mai$lat)), crs = 4326)
summary(data_Mai$long)
summary(data_Mai$lat)
# Calcul du centre de la ligne pour le centrage de la carte
centre_line <- st_coordinates(st_centroid(line_view))
lng_c <- centre_line[1]
lat_c <- centre_line[2]

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  # 1. Dessine la ligne de connexion (elle se croisera)
  addPolylines(data = line_view, color = "red", weight = 3) %>%
  # 2. Ajoute des marqueurs pour chaque point
  addCircles(
    lng = data_Mai$long,
    lat = data_Mai$lat,
    radius = 5,
    color = "blue",
    fillOpacity = 0.8
  ) %>%
  # Centre la carte sur le centroïde de la ligne
  setView(lng = lng_c, lat = lat_c, zoom = 16)



