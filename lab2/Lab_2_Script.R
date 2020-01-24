library(ggplot2)

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# When header = FALSE, the column names are read in as the first row of data, and the
# columns are named V1, V2, V3, etc. This is the default setting for read.table
names(ameslist)

typeof(ameslist)
GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist )

ameslist <- subset(ameslist, is.na(GarageType)==FALSE)
ameslist <- cbind(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

Ames <- data.frame(cbind(ameslist$Id, ameslist$LotArea, 
                    ameslist$OverallQual, ameslist$OverallCond, ameslist$YearBuilt,
                    ameslist$X1stFlrSF, ameslist$X2ndFlrSF , ameslist$LowQualFinSF,
                    ameslist$GrLivArea, ameslist$FullBath, ameslist$BedroomAbvGr,
                    ameslist$BsmtFullBath, ameslist$BsmtHalfBath, ameslist$GarageCars,
                    ameslist$SalePrice))

names(Ames) <- c("ID", "LotArea", "OverallQual", "OverallCond", "YearBuilt", "X1stFlrSF",
                 "X2ndFlrSF","LowQualFinSF","GrLivArea","FullBath","BedroomAbvGr",  "BsmtFullBath", 
                 "BsmtHalfBath","GarageCars","SalePrice")

pairs(ameslist[,c(5,18,19,20,44:47,50,52,62,81)], pch=19)
cor(ameslist[,c(5,18,19,20,44:47,50,52,62,81)])

# I am surprised by some of correlations with SalePrice. For example: I expected 
# Overall condition to have a significant positive correlation with the Sale Price,
# but this is not the case as they actually have a slight negative correlation. On
# the other hand, I am surprised that the year a house was built has such a high
# positive correlation with Sale Price. Most of the other correlations make some
# intuitive sense. For example Overall Quality has a high positive correlation with
# Sale Price.

coef<-coef(lm(SalePrice ~ GrLivArea, data = Ames))

ggplot(Ames, aes(x = GrLivArea, y = SalePrice))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)

# Biggest Outlier
Ames[which(Ames$GrLivArea> 5000),]

