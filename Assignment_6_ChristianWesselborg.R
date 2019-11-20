####Assignment 6 Christian Wesselborg####

####Update log####
#11-19-19 created file - maps color change - PCA updated - linear model added - preliminary decision tree added

####setup####

cittot <- read.csv("C:/Classes/4F19/Data Analytics/Assignment 6/Combined 500 Cities Data.csv")

####plots for poster####
#fips map with combined data
library(maps)
data("county.fips")

citcol <- cittot

citcol$col <- cittot$CANCER_CrudePrev/max(cittot$CANCER_CrudePrev)
summary(citcol$col)

citcol$coler <- grey(cit$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

citcol$coler2 <- 255 * (cittot$CANCER_CrudePrev/max(citcol$CANCER_CrudePrev))

map("county", fill=TRUE, col= rgb(0/255, citcol$coler2/255, 255/255, 1))

####PCA####
library(corpcor)
library(GPArotation)
library(psych)

dim(na.omit(cittot))

cit_mat <- cor(na.omit(cittot[, -c(1, 2, 3, 22, 11)]))
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

pc1 <- principal(na.omit(cittot[, -c(1,2,3,22,11)], nfactors = 6, rotate = "none"))
pc1
plot(pc1$values, type = "b")

pc2 <- principal(na.omit(cittot[, 3:25], nfactors = 3, rotate = "none"))

pc3 <- principal(na.omit(cittot[, -c(1,2,3,22,11,12,13,6)], nfactors = 3, rotate = "verimax"))
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

rpart <- rpart(CANCER_CrudePrev ~ ., data = cittot)
rpart.plot(rpart, fallen.leaves = T, type = 1)
