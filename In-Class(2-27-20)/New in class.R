library(ggplot2)
library(tidyverse)
library(caret)
library(class)
install.packages("aod")
library(aod)
data <- read_csv("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv")
str(data)

data2 <- data.frame(matrix(ncol=4,nrow=0))
colnames(data2)<-c("LotArea", "SalePrice", "Fireplaces", "HeatingQC")

LotArea <- c(data$LotArea)
SalePrice <- c(data$SalePrice)
Fireplaces <- c(data$Fireplaces)
HeatingQC <- c(data$HeatingQC)
data2 <- data.frame(LotArea, SalePrice,Fireplaces,HeatingQC)

data2 = data2[which(is.na(data2$Fireplaces)==FALSE),]
data2 = data2[which(is.na(data2$SalePrice)==FALSE),]
data2 = data2[which(is.na(data2$LotArea)==FALSE),]
data2 = data2[which(is.na(data2$HeatingQC)==FALSE),]
data2$HeatingQC = as.numeric(data2$HeatingQC)
#dataFireplacesBinary <- ifelse(data$Fireplaces==0, "0", ifelse(data$Fireplaces>0, "1",NA))
data2$Fireplaces <- factor(data2$Fireplaces)
data2$Fireplaces <- ifelse(data2$Fireplaces==0, 0, ifelse(data$Fireplaces>0, 1,NA))
ran <- sample(1:nrow(data2), 0.5 * nrow(data2)) 
train_data <- data2[ran,]
test_data <- data2[-ran,]

names(test_data)
mylogit <- glm(Fireplaces ~ SalePrice + LotArea + HeatingQC, data = data2, family = "binomial")
summary(mylogit)

#knn_ames <- knnreg(Fireplaces ~ SalePrice + LotArea + HeatingQC, test_data, k = 5)
#str(knn_ames)
pr <- knn(train_data,test_data,cl=train_data$Fireplaces,k=5, use.all=TRUE)
tabl <- table(pr, test_data$Fireplaces)
tabl <- t(as.matrix(tabl))
print(tabl)
final <- barplot(tabl)
final
