####Assignment 6 Christian Wesselborg####

####Update log####
#11-19-19 created file - maps color change - PCA updated - linear model added - preliminary decision tree added
#11-20-19 big day - data resorted to include all variables - maps updated and working, hurray - PCA updated - decition tree reworked - random forrest added

####setup####
rm(list = ls())
cittot <- read.csv("C:/Classes/4F19/Data Analytics/Assignment 6/Combined 500 Cities Data.csv")

####plots for poster####
#try new maps function
library(urbnmapr)
library(tidyverse)

#rounding to get info
cittot_round <- cittot
cittot_round$long <- round(cittot$Longitude, 6)
cittot_round$lat <- round(cittot$Latitute, 6)
city_data <- left_join(cittot_round, counties, by = "lat")

#KNN to predict fips

cittot_round <- cittot
cittot_round$long <- cittot$Longitude
cittot_round$lat <- cittot$Latitute

library("class")

ind <- sample(2, nrow(counties), replace=TRUE, prob=c(0.8, 0.2))

Train <- counties[ind == 1, ]
Test <- counties[ind == 2, ]

KNNpred <- knn(train = Train[, c(1, 2)], test = Test[, c(1, 2)], cl = Train$county_fips, k = 5)
KNNpred

qplot(Test$long, Test$lat)

redundant <- as.factor(Test$county_fips)
table(KNNpred[1:6], redundant[1:6])
mean(KNNpred == redundant)

KNNpred <- knn(train = counties[, c(1, 2)], test = cittot_round[, c(34, 35)], cl = counties$county_fips, k = 5)
KNNpred

cittot_round$county_fips <- KNNpred

#ahhhhh I did it maybe?

city_data <- left_join(cittot_round, counties, by = "county_fips") 

city_data %>%
  ggplot(aes(long.y, lat.y, group = group, fill = log10(Population2010))) +
  geom_polygon(color = "black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "population")

city_data %>%
  ggplot(aes(long.y, lat.y, group = group, fill = CANCER_CrudePrev)) +
  geom_polygon(color = "black") +
  geom_point(aes(long.x, lat.x))
  labs(fill = "cancer")

#I did it for real
city_data <- left_join(cittot_round, count, by = "county_fips") 

count <- get_urbn_map(map = "counties", sf = T)

ggplot() +
  geom_sf(count, mapping = aes(), fill = "grey") +
  geom_sf(city_data, mapping = aes(fill = household_data$CANCER_CrudePrev, geometry = geometry))
  
#base code
ggplot() +
  geom_sf(count, mapping = aes(fill = "grey"))

ggplot() +
  geom_polygon(data = counties, aes(long, lat, group = group), color = "white", fill = "blue") +
  geom_point(aes(x = cittot$Longitude, y = cittot$Latitute)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

#fips map with combined data
library(maps)
citcol <- cittot_round
citcol$coler2 <- 255 * (cittot$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))
map("county", col = "blue", fill = T)
points(x = cittot$Longitude, y = cittot$Latitute, col = rgb(citcol$coler2/255, citcol$coler2/255, 255/255, 1))

data("county.fips")

us.cities
data("world.cities")

data(countyMapEnv)

citcol <- cittot_round

citcol$col <- cittot$CANCER_CrudePrev/max(cittot$CANCER_CrudePrev)
summary(citcol$col)

citcol$coler <- grey(cit$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

citcol$coler2 <- 255 * (cittot$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

map("county", fill=TRUE, col= rgb(0/255, citcol$coler2/255, 255/255, 1))

citcol$coler2 <- 255 * citcol$Population2010/max(citcol$Population2010)

citcol$coler2 <- 255 * log10(cittot$Population2010)/log10(max(cittot$Population2010))

map(database = "", fill=TRUE, col= rgb(citcol$coler2/255, 0/255, 255/255, 1))

data("countyMapEnv")

summary(cittot$Population2010)

sort(cittot$Population2010, index.return = T, decreasing = T)

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

####linear model####

cancer_pop <- lm(cittot$CANCER_CrudePrev ~ log(cittot$Population2010, 10))
summary(cancer_pop)

plot(x = log(cittot$Population2010, 10), y = cittot$CANCER_CrudePrev)
abline(cancer_pop)

models <- lm(cittot$Population2010 ~ cittot[, -c(1,2,3)])
summary(models)

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
