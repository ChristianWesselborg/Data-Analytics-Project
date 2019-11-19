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

cit <- cities[, c(2, 3, 6, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49)]

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

#classification

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

#fips map
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

combiner <- function(data){
  x <- 0 #population holder
  y <- 0 #value holder
  y.2 <- 1:(ncol(cittot) - 4) #running total holder
  z <- 0 #couty population holder
  
  for(i in 1:nrow(cittot)){
    for(j in 1:nrow(cit)){
      if(cittot[i, 1] == cit[j, 1]){
        for(k in 4:ncol(cittot)){
          z <- as.numeric(cit[j, 3])
          y <- as.numeric(cit[j, k]/100 * z)
          
          cittot[i, k] <- cit[i, k] + y
        }
        x <- x + cit[j, 3]
      }
      else{
      }
    }
    cittot[i, 3] <- x
    print(x)
    for(k in 4:ncol(cittot)){
      cittot[i, k] <- cittot[i, k]/x * 100
    }
    print(i)
    y <- 0
    x <- 0
    assign('cittot',cittot , envir = .GlobalEnv)
  }
}

lapply(cittot, combiner)

#here it is
assign('', envir = global.env)