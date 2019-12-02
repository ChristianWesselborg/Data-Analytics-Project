####Assignment 6 Christian Wesselborg####

####Update log####
#11-19-19 created file - maps color change - PCA updated - linear model added - preliminary decision tree added
#11-20-19 big day - data resorted to include all variables - maps updated and working, hurray - PCA updated - decition tree reworked - random forrest added
#11-21-19 added potential improvements to the maps function including new data set with polygons - not quite working yet
#11-22-19 updated maping fuctions
#11-29-19 cleaned map code - PCA updated, promising result
####setup####
rm(list = ls())
cittot <- read.csv("C:/Classes/4F19/Data Analytics/Assignment 6/Combined 500 Cities Data.csv")

cities <- read.csv("C:/Classes/4F19/Data Analytics/Data Sets/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2018_release.csv", header = T)
library(ggplot2)
library(tidyverse)

#converts , to blank in the population metric
as.numeric(gsub(",", "", cities$Population2010))
cities$Population2010 <- as.numeric(gsub(",", "", cities$Population2010))

cit <- cities[, c(1, 2, 3, 4, 5, 6, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63)]

intermediate <- read.table(text=gsub('[()]', '', cit$Geolocation), 
                           sep=",", col.names=c('Latitute', 'Longitude'))

cit <- cbind(cit, intermediate)
####plots for poster####
#try new maps function
#potential rework
colnames(cit)[5]
colnames(cit)[5] <- "plctract10"

frame <- data.frame(mappers@data, mappers@polygons)

ggtract <- fortify(mappers, region = "polygons")

ggtract <- fortify(mappers@data, region = mappers@polygons)
typeof(ggtract)

mappers@data$plctract10
city_data <- left_join(cit, ggtract, by = "plctract10")

ggplot() +
  geom_polygon(data = city_data, aes(x = Longitude, y = Latitute))
#base code
ggplot() +
  geom_sf(count, mapping = aes(fill = "grey"))

ggplot() +
  geom_polygon(data = counties, aes(long, lat, group = group), color = "white", fill = "blue") +
  geom_point(aes(x = cittot$Longitude, y = cittot$Latitute)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
#maps again - may return

library(sp)
library(rgdal)

#attempt to combine data based off stack overflow
#very important load data mappers first, then load the following packages in the following order, then fortify works
library(ggplot2)
library(rgeos)
library(maptools)

mappers <- readOGR("C:/Classes/4F19/Data Analytics/Assignment 6/500Cities_Tracts_11082016/500Cities_Tracts_Clip.shp")

mappers@data$id <- rownames(mappers@data)

mappers.fort <- fortify(mappers, region = "id")

library(dplyr)

city_data <- left_join(mappers.fort, mappers@data, by = "id")

#plots
ggplot() +
  geom_polygon(data = mappers[mappers$PlaceName == "New York", ], aes(x = long, y = lat, group = group), fill = mappers[mappers$PlaceName == "New York", ]$PlcTrPop10, color = "white")

ggplot() +
  geom_polygon(data = mappers[mappers$PlaceName == "New York", ], aes(x = long, y = lat, group = group, fill = mappers[mappers$PlaceName == "New York", ]$PlcTrPop10||NA), color = "white")

ggplot() +
  geom_polygon(data = city_data[city_data$PlaceName == "New York", ], aes(x = long, y = lat, group = group, fill = city_data[city_data$PlaceName == "New York", ]$PlcTrPop10))

ggplot() +
  geom_polygon(data = city_data[city_data$PlaceName == "New York", ], aes(x = long, y = lat, group = group))
#Now we get into it

names(cit)[names(cit) == "Place_TractID"] <- "plctract10"

city_data2 <- left_join(city_data, cit, by = "plctract10")

ggplot() +
  geom_polygon(data = city_data2[city_data2$PlaceName.x == "New York", ], aes(x = long, y = lat, group = group, fill = city_data2[city_data2$PlaceName.x == "New York", ]$CANCER_CrudePrev))

ggplot() +
  geom_polygon(data = city_data2[city_data2$PlaceName.x == "New York", ], aes(x = long, y = lat, group = group, fill = city_data2[city_data2$PlaceName.x == "New York", ]$PlcTrPop10))

ggplot() +
  geom_polygon(data = city_data2[city_data2$PlaceName.x == "New York", ], aes(x = long, y = lat, group = group, fill = city_data2[city_data2$PlaceName.x == "New York", ]$CASTHMA_CrudePrev))

ggplot() +
  geom_polygon(data = city_data2, aes(x = long, y = lat, group = group, fill = city_data2$PlcTrPop10))

#more maps - keep for now
library(acs)
api.key.install(key = "76c9a712e6a536ca5be2caa356ccfe91ea408031")

#leaflet - interesting if works
library(leaflet)

map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = mappers, 
              fillColor = ~pal(PrcTrPop10), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,)
map2

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = mappers$PlcTrPop10)

map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = mappers,
              fillColor = ~pal(PlcTrPop10),
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,)
map2


  ####PCA####
library(corpcor)
library(GPArotation)
library(psych)

dim(na.omit(cittot))

cit_mat <- cor(na.omit(cittot[, -c(1, 2)]))
cit_mat

cortest.bartlett(na.omit(cittot[, -c(1, 2)]), n = 330)

kmo = function( data ){
  library(MASS) 
  X <- cor(as.matrix(data)) 
  iX <- ginv(X) 
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a) 
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the correlation matrix. That is the  negative of the partial correlations, partialling out all other variables.
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  # Reporting the conclusion 
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance miserable.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance mediocre.'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance middling.' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance meritorious.' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
  
  ans <- list( overall = kmo,
               report = test,
               individual = MSA,
               AIS = AIS,
               AIR = AIR )
  return(ans)
} 

kmo(na.omit(cittot[, -c(1, 2)]))

det(cit_mat)

det(cit_mat)


pc1 <- principal(na.omit(cittot[, -c(1, 2)], nfactors = 6, rotate = "none"))
pc1
plot(pc1$values, type = "b")

pc2 <- principal(na.omit(cittot[, 3:25], nfactors = 3, rotate = "none"))
pc2

pc3 <- principal(na.omit(cittot[, -c(1, 2, 32, 33, 12, 13, 6, 8, 16, 4 , 14, 10, 17, 21, 5, 29, 19, 26, 18, 25, 23, 31, 7, 20, 28, 9, 22, 11, 15)], nfactors = 3, rotate = "verimax"))
print.psych(pc3, cut = 0.4, sort = TRUE)

biplot(pc3)

#PCA with county data
library(corpcor)
library(GPArotation)
library(psych)

cit_mat <- cor(na.omit(cit[, -c(1, 2, 3, 4, 5, 31, 35, 36, 37)]))
cit_mat

round(cit_mat, 3)

det(cit_mat)

pc1 <- principal(na.omit(cit[, -c(1,2,3,4,5,35,36,37)]), nfactors = 6, rotate = "none")
pc1
plot(pc1$values, type = "b")
abline(1,0)

pc2 <- principal(na.omit(cit[, -c(1,2,3,4,5,35,36,37)]), nfactors = 3, rotate = "varimax")
print.psych(pc2, cut = 0.5, sort = TRUE)

pc3 <- principal(na.omit(cit[, -c(1,2,3,4,5,6,35,36,37)]), nfactors = 3, rotate = "varimax")
print.psych(pc3, cut = 0.5, sort = TRUE)

pc3.1 <- principal(na.omit(cit[, -c(1,2,3,4,5,6,27,30,35,36,37)]), nfactors = 2, rotate = "varimax")
print.psych(pc3.1, cut = 0.66, sort = TRUE)
print.psych(pc3.1, cut = 0, sort = TRUE)
biplot(pc3.1)

#PCA with city total data

pca <- principal(na.omit(cittot[, -c(1,2,32,33)]), nfactors = 6, rotate = "none")
pca
plot(pca$values, type = "b")

pcb <- principal(na.omit(cittot[, -c(1,2,32,33)]), nfactors = 3, rotate = "varimax")
pcb
print.psych(pcb, cut = .5, sort = T)

pcc <- principal(na.omit(cittot[, -c(1,2,32,33)]), nfactors = 2, rotate = "varimax")
print.psych(pcc, cut = .5, sort = T)

pcd <- principal(na.omit(cittot[, -c(1,2,3,6,32,33)]), nfactors = 3, rotate = "varimax")
print.psych(pcd, cut = .5, sort = T)

pce <- principal(na.omit(cittot[, -c(1,2,3,6,24,27,32,33)]), nfactors = 2, rotate = "varimax")
print.psych(pce, cut = .5, sort = T)

pce.1 <- principal(na.omit(cittot[, -c(1,2,3,6,24,27,32,33)]), nfactors = 2, rotate = "varimax")
print.psych(pce.1, cut = .64, sort = T)
biplot(pce.1)

####linear model####

cancer_pop <- lm(cittot$CANCER_CrudePrev ~ log(cittot$Population2010, 10))
summary(cancer_pop)

plot(x = log(cittot$Population2010, 10), y = cittot$CANCER_CrudePrev)
abline(cancer_pop)

models <- lm(cittot$Population2010 ~ cittot[, -c(1,2,3)])
summary(models)

plot(cit$CANCER_CrudePrev ~ cit$ARTHRITIS_CrudePrev)

plot(cit$CANCER_CrudePrev ~ cit$CASTHMA_CrudePrev)

plot(cit$CANCER_CrudePrev ~ cit$TEETHLOST_CrudePrev)

plot(cit$CANCER_CrudePrev ~ cit$MAMMOUSE_CrudePrev)

plot(cit$CANCER_CrudePrev ~ cit$CSMOKING_CrudePrev)

plot(cit$CANCER_CrudePrev ~ cit$STROKE_CrudePrev)

plot(cit$CANCER_CrudePrev ~ cit$OBESITY_CrudePrev)
####decition tree####

library(rpart)
library(rpart.plot)

rpart <- rpart(CANCER_CrudePrev ~ Population2010 + CSMOKING_CrudePrev + SLEEP_CrudePrev, data = cittot)
rpart.plot(rpart, fallen.leaves = T, type = 1)

library(randomForest)

set.seed(40)

cittot_omit <- na.omit(cittot[, -c(1, 2)])

ind <- sample(2, nrow(cittot_omit), replace=TRUE, prob=c(0.7, 0.3))

Train <- cittot_omit[ind == 1, ]
Test <- cittot_omit[ind == 2, ]

forrest <- randomForest(Train$CANCER_CrudePrev ~ ., data = Train, ntree = 500, mtry = 120, importance = T)
forrest
forrest$importance

train_pred <- predict(forrest, Train)
table(train_pred, Train$CANCER_CrudePrev)

mean(train_pred - Train$CANCER_CrudePrev)

test_pred <- predict(forrest, Test)

mean(test_pred - Test$CANCER_CrudePrev)
