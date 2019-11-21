#setup
rm(list=ls())

cities <- read.csv("C:/Classes/4F19/Data Analytics/Data Sets/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2018_release.csv", header = T)
library(ggplot2)
library(tidyverse)

#converts , to blank in the population metric
as.numeric(gsub(",", "", cities$Population2010))
cities$Population2010 <- as.numeric(gsub(",", "", cities$Population2010))

summary(cities$Population2010)

ggplot(data = cities) +
  geom_boxplot(aes(x = "Population2010", y = Population2010))

cit <- cities[, c(2, 3, 6, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63)]

cit$Geolocation %>% extract(cit$Geolocation, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")

intermediate <- read.table(text=gsub('[()]', '', cit$Geolocation), 
           sep=",", col.names=c('Latitute', 'Longitude'))

cit <- cbind(cit, intermediate)

cit <- cit[, -32]
#Plots
head(cities$ACCESS2_CrudePrev)
summary(cities$ACCESS2_CrudePrev)
ggplot(data = cities) +
  geom_boxplot(aes(x = "Access2", y = cities$ACCESS2_CrudePrev)) +
  geom_boxplot(aes(x = "arthritis", y = cities$ARTHRITIS_CrudePrev))

ggplot(data = cities) +
  geom_boxplot(aes(x = "checkup", y = cities$CHECKUP_CrudePrev)) +
  geom_boxplot(aes(x = "cholesterol", y = cities$CHOLSCREEN_CrudePrev)) +
  geom_histogram(aes(x = cities$CANCER_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$CANCER_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$CASTHMA_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$CSMOKING_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$CHD_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$COPD_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$KIDNEY_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$LPA_CrudePrev))

ggplot(data = cities) +
  geom_histogram(aes(x = cities$SLEEP_CrudePrev))

#classification - defunct

ggplot(data = cities) +
  geom_boxplot(aes(x = "Population2010", y = Population2010))

ggplot(data = cities) +
  geom_histogram(aes(x = Population2010))

cities$cat <- factor(levels = "small", "medium", "large", ordered = T)
small <- cities[cities$Population2010 < 2000,]
medium <- cities[cities$Population2010 > 2000 & cities$Population2010 < 8000,]
large <- cities[cities$Population2010 > 8000, ]

quant <- cut(cities$Population2010, quantile(cities$Population2010, (0:4)/4))
table(quant)

summary(cit$CANCER_CrudePrev ~ cit$PlaceName)

summary(cit[cit$PlaceName == "New York", ]$CANCER_CrudePrev)

#linear models

ggplot() +
  geom_boxplot(aes(x = cit[cit$PlaceName == "New York",]$PlaceName, y = cit[cit$PlaceName == "New York",]$CANCER_CrudePrev))

ggplot() +
  geom_boxplot(aes(x = cit$PlaceName, y = cit$CANCER_CrudePrev))

ggplot() +
  geom_boxplot(aes(x = cit$PlaceName, y = cit$Population2010))

levels(cit$PlaceName)

cancer_pop <- lm(cit$CANCER_CrudePrev ~ cit$Population2010)
summary(cancer_pop)

plot(x = cit$Population2010 ,y = cit$CANCER_CrudePrev)
abline(cancer_pop)

cancer_pop <- lm(cit$CANCER_CrudePrev ~ cit$Population2010 + cit$PlaceName)
summary(cancer_pop)

anova <- aov(cit$CANCER_CrudePrev ~ cit$PlaceName)
summary(anova)

#fips map - defunct
library(maps)

county.fips(cit$PlaceFIPS)
county.fips(cit$PlaceName)

data("county.fips")

citcol <- cit

citcol$col <- cit$CANCER_CrudePrev/max(cit$CANCER_CrudePrev)
summary(citcol$col)

citcol$coler <- grey(cit$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

citcol$coler2 <- 255 * (cit$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

map("county", fill=TRUE, col= rgb(0/255, citcol$coler2/255, 0/255, 1))

#Combining based on city name

levels(cit$PlaceName)

names <- levels(cit$PlaceName)
names

cittot <- cit[1:474, ]

colnames(cittot) <- colnames(cit)
cittot$PlaceName <- names
cittot[, -c(1, 2, 32, 33)] <- 0

combiner <- function(data){
  x <- 0 #population holder
  y <- 0 #value holder
  z <- 0 #county population holder
  
  for(i in 1:nrow(cittot)){ #every city name
    for(j in 1:nrow(cit)){ #every observation in origional
      if(cittot[i, 1] == cit[j, 1]){ #if the place name matches
        cittot[i, 2] <- cit[j, 2]
        z <- as.numeric(cit[j, 3]) #county population
        for(k in 4:(ncol(cittot) - 2)){ #every variable to be calculated less 2 (lat,long)
          y <- as.numeric(cit[j, k]/100 * z) #get the population value of that area for each variable
          
          cittot[i, k] <- cittot[i, k] + y #add to the data so sum can be collected
        }
        cittot[i, 32] <- (cit[j, 32] + cittot[i, 32])/2
        cittot[i, 33] <- (cit[j, 33] + cittot[i, 33])/2
        x <- x + cit[j, 3] #running total population value
      }
      else{
      }
    }
    cittot[i, 3] <- x #finalize population value
    for(k in 4:(ncol(cittot) - 2)){
      cittot[i, k] <- cittot[i, k]/x * 100 #readjust based on total population
    }
    print(i)
    y <- 0
    x <- 0
    assign('cittot',cittot , envir = .GlobalEnv)
  }
}

combiner(cittot)

write.table(cittot, file = "C:/Classes/4F19/Data Analytics/Assignment 6/Combined 500 Cities Data.csv", sep = ",", row.names = F)

#plots with combined data

ggplot() +
  geom_histogram(aes(x = cittot$CANCER_CrudePrev))

ggplot() +
  geom_histogram(aes(x = cittot$Population2010))

ggplot() +
  geom_point(aes(x = cittot[cittot$Population2010 < 1e05, ]$Population2010, y = cittot[cittot$Population2010 < 1e05, ]$CANCER_CrudePrev))

#fips map with combined data
library(maps)
data("county.fips")

citcol <- cittot

citcol$col <- cittot$CANCER_CrudePrev/max(cittot$CANCER_CrudePrev)
summary(citcol$col)

citcol$coler <- grey(cit$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

citcol$coler2 <- 255 * (cittot$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

map("county", fill=TRUE, col= rgb(0/255, citcol$coler2/255, 0/255, 1))

#pca
library(corpcor)
library(GPArotation)
library(psych)

dim(na.omit(cittot))

cit_mat <- cor(na.omit(cittot[, 3:25]))
cit_mat

cortest.bartlett(na.omit(cittot[, 3:25]), n = 368)

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

kmo(na.omit(cittot[, 3:25]))

det(cit_mat)

pc1 <- principal(na.omit(cittot[, 3:25], nfactors = 6, rotate = "none"))
pc1
plot(pc1$values, type = "b")

pc2 <- principal(na.omit(cittot[, 3:25], nfactors = 3, rotate = "none"))

pc3 <- principal(na.omit(cittot[, 3:25], nfactors = 3, rotate = "oblimin"))
print.psych(pc3, cut = 0.4, sort = TRUE)

biplot(pc3)
