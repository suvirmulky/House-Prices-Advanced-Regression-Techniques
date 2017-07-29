ames_train <- read.csv(file.choose())
ames_test <- read.csv(file.choose())

View(ames_train)
str(ames_test)

###conbining traing and test data set
df.combined <- rbind(within(ames_train,rm("Id","SalePrice")),within(ames_test,rm("Id")))
dim(df.combined)






summary(ames_train$SalePrice)

library(e1071)  ### skewness is present in e1071
skewness(ames_train$SalePrice)

#### sales price is right skewed.
hist(ames_train$SalePrice)

### log transform of saleprice to improve linearity of data
skewness(log(ames_train$SalePrice))
hist(log(ames_train$SalePrice)) ### data is normally distributed



#### finding out the data type of the independent variables
sapply(ames_train,class)








##################################### Replacing the NA values ###########################

###finding out the number of NAs
na.cols <- which(colSums(is.na(df.combined)) > 0)
sort(colSums(sapply(df.combined[na.cols],is.na)),decreasing=TRUE)

paste('There are', length(na.cols), 'columns with missing values')


# helper function for plotting categoric data for easier data visualization
plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(df.combined[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}

################### PoolQC: Pool quality

table(df.combined$PoolQC)
plot.categoric('PoolQC', df.combined)

## finding out rows where pool area >0 and pool quality is n.a.. for these rows n.a values should be replaced
##by non zero values
df.combined[(df.combined$PoolArea>0)& (is.na(df.combined$PoolQC)),c("PoolArea","PoolQC")]

## finding the avg. pool area of the 3 categories of pool quality
tapply(df.combined$PoolArea,df.combined$PoolQC,mean)

### the assigning the category closest to avg. value of the pool areas in those categories
df.combined[2421,'PoolQC'] <- 'Ex'
df.combined[2504,'PoolQC'] <- 'Ex'
df.combined[2600,'PoolQC'] <- 'Fa'
df.combined$PoolQC <- as.character(df.combined$PoolQC) ##to add none as a factor
df.combined$PoolQC[is.na(df.combined$PoolQC)] <- 'None'
df.combined$PoolQC <- as.factor(df.combined$PoolQC)
summary(df.combined$PoolQC)






############# Garage features 

### GarageYrBlt: Year garage was built

length(which(df.combined$YearBuilt==df.combined$GarageYrBlt)) ##tells us 2216 of the 2919 houses have same year for for GarageYrBlt and YearBuit

### replacing the NA values with the year the house was built
idx <- which(is.na(df.combined$GarageYrBlt))
df.combined$GarageYrBlt[idx] <- df.combined$YearBuilt[idx]

##### checking for 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType'
garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
df.combined[is.na(df.combined$GarageCond),garage.cols]

idx <- which(((df.combined$GarageArea < 370) & (df.combined$GarageArea > 350)) & (df.combined$GarageCars == 1))
names(sapply(df.combined[idx, garage.cols], function(x) sort(table(x), decreasing=TRUE)[1]))

##assigning the most frequent values
df.combined[2127,'GarageQual'] = 'TA'
df.combined[2127, 'GarageFinish'] = 'Unf'
df.combined[2127, 'GarageCond'] = 'TA'

str(df.combined[idx,garage.cols])

df.combined$GarageFinish <- as.character(df.combined$GarageFinish)
df.combined$GarageFinish[is.na(df.combined$GarageFinish)] <- 'None'
df.combined$GarageFinish <- as.factor(df.combined$GarageFinish)

df.combined$GarageCond <- as.character(df.combined$GarageCond)
df.combined$GarageCond[is.na(df.combined$GarageCond)] <- 'None'
df.combined$GarageCond <- as.factor(df.combined$GarageCond)

df.combined$GarageType <- as.character(df.combined$GarageType)
df.combined$GarageType[is.na(df.combined$GarageType)] <- 'None'
df.combined$GarageType <- as.factor(df.combined$GarageType)

df.combined$GarageQual <- as.character(df.combined$GarageQual)
df.combined$GarageQual[is.na(df.combined$GarageQual)] <- 'None'
df.combined$GarageQual <- as.factor(df.combined$GarageQual)

df.combined$GarageArea[2577] <- 0
df.combined$GarageCars[2577] <- 0

##############KitchenQual: Kitchen quality and Electrical: Electrical system

## replacing NA with most frequent value. (only 1 na present for both)

table(df.combined$KitchenQual)
df.combined$KitchenQual[is.na(df.combined$KitchenQual)] = 'TA'
table(df.combined$Electrical)
df.combined$Electrical[is.na(df.combined$Electrical)] = 'SBrkr'



###############Basement features
install.packages("stringr")
library(stringr) ##for str_detect() funtion
### locating the NA rows of all the basement features
bsmt.cols <- names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Bsmt'))]
str(df.combined[is.na(df.combined$BsmtExposure),bsmt.cols])

###no is the most frequent value.
table(df.combined[,"BsmtExposure"])
df.combined[c(949, 1488, 2349), 'BsmtExposure'] = 'No'

## giving the value of None to the other rows
df.combined$BsmtQual <- as.character(df.combined$BsmtQual)
df.combined$BsmtQual[is.na(df.combined$BsmtQual)] <- 'None'
df.combined$BsmtQual <- as.factor(df.combined$BsmtQual)

df.combined$BsmtCond <- as.character(df.combined$BsmtCond)
df.combined$BsmtCond[is.na(df.combined$BsmtCond)] <- 'None'
df.combined$BsmtCond <- as.factor(df.combined$BsmtCond)

df.combined$BsmtExposure <- as.character(df.combined$BsmtExposure)
df.combined$BsmtExposure[is.na(df.combined$BsmtExposure)] <- 'None'
df.combined$BsmtExposure <- as.factor(df.combined$BsmtExposure)

df.combined$BsmtFinType1 <- as.character(df.combined$BsmtFinType1)
df.combined$BsmtFinType1[is.na(df.combined$BsmtFinType1)] <- 'None'
df.combined$BsmtFinType1 <- as.factor(df.combined$BsmtFinType1)

df.combined$BsmtFinType2 <- as.character(df.combined$BsmtFinType2)
df.combined$BsmtFinType2[is.na(df.combined$BsmtFinType2)] <- 'None'
df.combined$BsmtFinType2 <- as.factor(df.combined$BsmtFinType2)

for (col in bsmt.cols){
  if (sapply(df.combined[col], is.numeric) == TRUE){
    df.combined[sapply(df.combined[col], is.na),col] = 0
  }
}

########### Exterior features

table(df.combined$Exterior1st)
table(df.combined$Exterior2nd)

#### since only 1 N.A value for each.. we are replacing them with "other" as  NA is likely due to having an exterior cover that is not listed.
df.combined$Exterior1st <- as.character(df.combined$Exterior1st)
df.combined$Exterior1st[is.na(df.combined$Exterior1st)] <- "Other"
df.combined$Exterior1st <- as.factor(df.combined$Exterior1st)

df.combined$Exterior2nd <- as.character(df.combined$Exterior2nd)
df.combined$Exterior2nd[is.na(df.combined$Exterior2nd)] <- "Other"
df.combined$Exterior2nd <- as.factor(df.combined$Exterior2nd)


########### Sale type

### sale type and sale condition are related to each other

## finding the sale condition for the sale type = N.A 
df.combined[which(is.na(df.combined$SaleType)),"SaleCondition"]

##### finding out the most frequent sale type for sale condition=Normal
table(df.combined$SaleCondition,df.combined$SaleType)

##replacing NA with WD
df.combined$SaleType[is.na(df.combined$SaleType)] = 'WD'


#################Functional

df.combined[which(is.na(df.combined$Functional)),"OverallCond"]

table(df.combined$OverallCond,df.combined$Functional)

df.combined$Functional[2217] = 'Typ'
df.combined$Functional[2474] = 'Maj1'

#####################Utilities
## all are PUB values except for 1
table(df.combined$Utilities)

## the only non PUB value belongs to the training set
which(df.combined$Utilities=="NoSeWa")

## dropping the utilities column.. as it shows no variation

utilities.drop <- "Utilities"
df.combined <- df.combined[,!names(df.combined) %in% c("Utilities") ]

################# MSZoning feature

### MSZoning is realted to MS Sub class

df.combined[which(is.na(df.combined$MSZoning)),c("MSZoning","MSSubClass")]

table(df.combined$MSZoning,df.combined$MSSubClass)

### gving the values of higest fequency appropriately
df.combined$MSZoning[1916] <- "RM"
df.combined$MSZoning[2217] <- "RL"
df.combined$MSZoning[2251] <- "RM"
df.combined$MSZoning[2905] <- "RL"

############# MasVnrType: Masonry veneer type andMasVnrArea: Masonry veneer area in square feet

### checking if the NA values for both are for the same rows in the data set

df.combined[(is.na(df.combined$MasVnrType)) | (is.na(df.combined$MasVnrType)),c("MasVnrType","MasVnrArea")]

### find the avg area for each type
tapply(df.combined$MasVnrArea,df.combined$MasVnrType,mean)

df.combined[2611,"MasVnrType"] <- "BrkCmn"

## asssigning 0 to the remaining areas and none to the remaining types
df.combined$MasVnrArea[is.na(df.combined$MasVnrArea)] <- 0
df.combined$MasVnrType[is.na(df.combined$MasVnrType)] = 'None'




############################ LotFrontage: Linear feet of street connected to property

tapply(df.combined$LotFrontage,df.combined$Neighborhood,median,na.rm=T)

library(dplyr) ### for group_by function 

df.combined['Nbrh.factor'] <- factor(df.combined$Neighborhood, levels = unique(df.combined$Neighborhood))
lot.by.nbrh <- df.combined[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))

(lot.by.nbrh)


idx = which(is.na(df.combined$LotFrontage))

for (i in idx){
  lot.median <- lot.by.nbrh[lot.by.nbrh$Neighborhood == df.combined$Neighborhood[i],'median']
  df.combined[i,'LotFrontage'] <- lot.median[[1]]
}


############ Fence: Fence quality and misc. feature

#We can replace any missing vlues for Fence and MiscFeature with 'None' 
#as they probably don't have this feature with their property.

df.combined$Fence <- as.character(df.combined$Fence)
df.combined$Fence[is.na(df.combined$Fence)] <- "None"
df.combined$Fence <- as.factor(df.combined$Fence)

df.combined$MiscFeature <- as.character(df.combined$MiscFeature)
df.combined$MiscFeature[is.na(df.combined$MiscFeature)] <- "None"
df.combined$MiscFeature <- as.factor(df.combined$MiscFeature)


###########Fireplaces: Number of fireplaces and FireplaceQu: Fireplace quality

table(df.combined$Fireplaces,df.combined$FireplaceQu)
### no such combination is there
which((df.combined$Fireplaces > 0) & (is.na(df.combined$FireplaceQu)))

df.combined$FireplaceQu <- as.character(df.combined$FireplaceQu)
df.combined$FireplaceQu[is.na(df.combined$FireplaceQu)] = 'None'
df.combined$FireplaceQu <- as.factor(df.combined$FireplaceQu)


########## Alley

df.combined$Alley <- as.character(df.combined$Alley)
df.combined$Alley[is.na(df.combined$Alley)] = 'None'
df.combined$Alley <- as.factor(df.combined$Alley)


#################################

paste('There are', sum(sapply(df.combined, is.na)), 'missing values left')




################################ separating numeric and categorical features

num_features <- names(which(sapply(df.combined, is.numeric)))
cat_features <- names(which(sapply(df.combined, is.factor)))

cat_features

df.numeric <- df.combined[num_features]


###############################converting ordinal data into numeric
sapply(df.combined,class)
##splitting into train data
group.df <- df.combined[1:1460,]
group.df$SalePrice <- ames_train$SalePrice
dim(group.df)

install.packages("ggplot2")
library(ggplot2)

install.packages("magrittr")
library(magrittr)

install.packages("scales")
library(scales)

library(dplyr)

group.prices <- function(col) {
  group.table <- group.df[,c(col, 'SalePrice', 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), n = n()) %>%
    arrange(mean.Quality)
  
  print(qplot(x=reorder(group.table[[col]], -group.table[['mean.Price']]), y=group.table[['mean.Price']]) +
          geom_bar(stat='identity', fill='cornflowerblue') +
          theme_minimal() +
          scale_y_continuous(labels = dollar) +
          labs(x=col, y='Mean SalePrice') +
          theme(axis.text.x = element_text(angle = 45)))
  
  return(data.frame(group.table))
}

## functional to compute the mean overall quality for each quality
quality.mean <- function(col) {
  group.table <- df.combined[,c(col, 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.qual = mean(OverallQual)) %>%
    arrange(mean.qual)
  
  return(data.frame(group.table))
}


# function that maps a categoric value to its corresponding numeric value and returns that column to the data frame
map.fcn <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[df.combined[,col]])
  }
  return(df)
}

###Any of the columns with the suffix 'Qual' or 'Cond' denote the quality or condition of that specific feature. 
###Each of these columns have the potential values: TA, Fa, Gd, None, Ex, Po. 
###We'll compute the mean house prices for these unique values to get a better sense of what their abbreviations mean.

qual.cols <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual')


group.prices('FireplaceQu')
group.prices('BsmtQual')
group.prices('KitchenQual')


###From seeing the mean saleprices from a few of the quality and condition features we can infer that the abbreviations mean poor, fair, typical/average, good and excelent. 
###We'll map numeric values from 0-5 to their corresponding categoric values (including 0 for None) and combine that to our dataframe.

##Note: we will set 'None' = 0 for all categories as None signifies that the house does not have that particular quality/condition to rank 
###and regardless of the houses overall quality or sale price we will keep 'None' = 0 for consistency.

qual.list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

df.numeric <- map.fcn(qual.cols, qual.list, df.numeric)



group.prices('BsmtExposure')


bsmt.list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

df.numeric = map.fcn(c('BsmtExposure'), bsmt.list, df.numeric)


group.prices('BsmtFinType1')


# visualization for BsmtFinTyp2 instead of another table
df.combined[,c('BsmtFinType1', 'BsmtFinSF1')] %>%
  group_by(BsmtFinType1) %>%
  summarise(medianArea = median(BsmtFinSF1), counts = n()) %>%
  arrange(medianArea) %>%
  ggplot(aes(x=reorder(BsmtFinType1,-medianArea), y=medianArea)) +
  geom_bar(stat = 'identity', fill='cornflowerblue') +
  labs(x='BsmtFinType2', y='Median of BsmtFinSF2') +
  geom_text(aes(label = sort(medianArea)), vjust = -0.5) +
  scale_y_continuous(limits = c(0,850)) +
  theme_minimal()


##Through investigating the relationships between the basement quality and areas we an see the true order of qualities of each basement to be 
##'None' < 'Unf' < 'LwQ' < 'BLQ' < 'Rec' < 'ALQ' < 'GLQ'.

bsmt.fin.list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
df.numeric <- map.fcn(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list, df.numeric)

group.prices('Functional')


functional.list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4, 'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)

df.numeric['Functional'] <- as.numeric(functional.list[df.combined$Functional])


group.prices('GarageFinish')
garage.fin.list <- c('None' = 0,'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

df.numeric['GarageFinish'] <- as.numeric(garage.fin.list[df.combined$GarageFinish])



group.prices('Fence')
fence.list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 2, 'MnPrv' = 3, 'GdPrv' = 5)

df.numeric['Fence'] <- as.numeric(fence.list[df.combined$Fence])

MSdwelling.list <- c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)

df.numeric['NewerDwelling'] <- as.numeric(MSdwelling.list[as.character(df.combined$MSSubClass)])


######### calculating the correlation between sale price and the categorical variables(which have been converted to ordinal variables)
library(corrplot)

# need the SalePrice column
corr.df <- cbind(df.numeric[1:1460,], ames_train['SalePrice'])

# only using the first 1460 rows - training data
correlations <- cor(corr.df)
# only want the columns that show strong correlations with SalePrice
corr.SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)


###matrix of scatter plots to see what these relationships look like under the hood
###to get a better sense of whats going on.

install.packages("GGally")
library(GGally)


lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 20, alpha = 0.7, color = 'darkseagreen') +
    geom_smooth(method=loess, fill="red", color="red") +
    geom_smooth(method=lm, fill="blue", color="blue") +
    theme_minimal()
  return(plt)
}

#The blue lines in the scatter plots represent a simple linear regression fit while the red lines represent a local polynomial fit. 
#We can see both OverallQual and GrLivArea and TotalBsmtSF follow a linear model, but have some outliers we may want to look into. 
#For instance, there are multiple houses with an overall quality of 10, but have suspisciously low prices. 
#We can see similar behavior in GrLivArea and TotalBsmtSF. GarageCars and GarageArea both follow more of a quadratic fit. 
#It seems that having a 4 car garage does not result in a higher house price and same with an extremely large area.


ggpairs(corr.df, corr.idx[1:6], lower = list(continuous = lm.plt))

ggpairs(corr.df, corr.idx[c(1,7:11)], lower = list(continuous = lm.plt))


##############################################

##########Nominal Variables

#LotShape has 3 values for having an irregular shape and only 1 for regular. 
#We can create a binary column that returns 1 for houses with a regular lot shape and 0 for houses with any of the 3 irregular lot shapes. 
#Using this method of turning a categoric feature into a binary column will ultimately help our data 
#train better through boosted models without using numeric placeholders on nominal data.

plot.categoric('LotShape', df.combined)

df.numeric['RegularLotShape'] <- (df.combined$LotShape == 'Reg') * 1

table(df.numeric$RegularLotShape)
table(df.combined$LotShape)

# Same process is applied to the other nominal variables as well

plot.categoric('LandContour', df.combined)
df.numeric['LandLeveled'] <- (df.combined$LandContour == 'Lvl') * 1

plot.categoric('LandSlope', df.combined)
df.numeric['LandSlopeGentle'] <- (df.combined$LandSlope == 'Gtl') * 1

plot.categoric('Electrical', df.combined)
df.numeric['ElectricalSB'] <- (df.combined$Electrical == 'SBrkr') * 1

plot.categoric('GarageType', df.combined)
df.numeric['GarageDetchd'] <- (df.combined$GarageType == 'Detchd') * 1

plot.categoric('PavedDrive', df.combined)
df.numeric['HasPavedDrive'] <- (df.combined$PavedDrive == 'Y') * 1

df.numeric['HasWoodDeck'] <- (df.combined$WoodDeckSF > 0) * 1

df.numeric['Has2ndFlr'] <- (df.combined$X2ndFlrSF > 0) * 1

df.numeric['HasMasVnr'] <- (df.combined$MasVnrArea > 0) * 1

table(df.combined$WoodDeckSF)


plot.categoric('MiscFeature', df.combined)

#For MiscFeature the only feature with a significant amount of houses having it is Shed. 
#We can one-hot encode houses that have Sheds vs those who do not.

df.numeric['HasShed'] <- (df.combined$MiscFeature == 'Shed') * 1


################# feature engineering 

#Many of the houses recorded the same year for YearBuilt and YearRemodAdd. 
#We can create a new column that records that a house was remodelled 
#if the year it was built is different than the remodel year. This

df.numeric['Remodeled'] <- (df.combined$YearBuilt != df.combined$YearRemodAdd) * 1

#We can also create a column that seperates which houses have been recently remodelled vs those who are not. 
#Houses that have been remodelled after the year they were sold will fall into this category.

df.numeric['RecentRemodel'] <- (df.combined$YearRemodAdd >= df.combined$YrSold) * 1

#There can be potential value to homes who were sold the same year they were built as this could be an indicator
#that these houses were hot in the marke

df.numeric['NewHouse'] <- (df.combined$YearBuilt == df.combined$YrSold) * 1

#What about the houses with area based features equal to 0? Houses with 0 square footage for a columnshows that the house does not have that feature at all. 
#We add a one-hot encoded column for returning 1 for any house with an area greater than 0 
#since this means that the house does have this feature and 0 for those who do not


cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')

for (col in cols.binary){
  df.numeric[str_c('Has',col)] <- (df.combined[,col] != 0) * 1
}


### see how houses sold month wise

ggplot(df.combined, aes(x=MoSold)) +
  geom_bar(fill = 'cornflowerblue') +
  geom_text(aes(label=..count..), stat='count', vjust = -.5) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)

#The largest proportion of houses sold is during the summer months: May, June, July. 
#Let's add a column that seperates the the summer houses from the rest.

df.numeric['HighSeason'] <- (df.combined$MoSold %in% c(5,6,7)) * 1


### some neighbourhoods are more expensive than others

ames_train[,c('Neighborhood','SalePrice')] %>%
  group_by(Neighborhood) %>%
  summarise(median.price = median(SalePrice, na.rm = TRUE)) %>%
  arrange(median.price) %>%
  mutate(nhbr.sorted = factor(Neighborhood, levels=Neighborhood)) %>%
  ggplot(aes(x=nhbr.sorted, y=median.price)) +
  geom_point() +
  geom_text(aes(label = median.price, angle = 45), vjust = 2) +
  theme_minimal() +
  labs(x='Neighborhood', y='Median price') +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45))

library(dplyr)  ### needed for group_by function

#StoneBr, NoRidge, NridgHt have a large gap between them versus the rest of the median prices from any of the other neighborhods. 
#It would be wise of us to check if this is from outliers or if these houses are much pricier as a whole.

other.nbrh <- unique(df.combined$Neighborhood)[!unique(df.combined$Neighborhood) %in% c('StoneBr', 'NoRidge','NridgHt')]

ggplot(ames_train, aes(x=SalePrice, y=GrLivArea, colour=Neighborhood)) +
  geom_point(shape=16, alpha=.8, size=4) +
  scale_color_manual(limits = c(other.nbrh, 'StoneBr', 'NoRidge', 'NridgHt'), values = c(rep('black', length(other.nbrh)), 'indianred',
                                                                                         'cornflowerblue', 'darkseagreen')) +
  theme_minimal() +
  scale_x_continuous(label=dollar)

#lets one-hot encode the more expensive neighborhoods and add that to our dataframe

nbrh.rich <- c('Crawfor', 'Somerst, Timber', 'StoneBr', 'NoRidge', 'NridgeHt')
df.numeric['NbrhRich'] <- (df.combined$Neighborhood %in% nbrh.rich) *1

group.prices('Neighborhood')

nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
              'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
              'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
              'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
              'NridgHt' = 4)

df.numeric['NeighborhoodBin'] <- as.numeric(nbrh.map[df.combined$Neighborhood])

### sale condition

group.prices('SaleCondition')
df.numeric['PartialPlan'] <- (df.combined$SaleCondition == 'Partial') * 1


group.prices('HeatingQC')
heating.list <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)

df.numeric['HeatingScale'] <- as.numeric(heating.list[df.combined$HeatingQC])

area.cols <- c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
               'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
               'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')

df.numeric['TotalArea'] <- as.numeric(rowSums(df.combined[,area.cols]))

df.numeric['AreaInside'] <- as.numeric(df.combined$X1stFlrSF + df.combined$X2ndFlrSF)

#We've seen how strong of an effect the year of a house built has on the house price, 
#therefore, as this dataset collects houses up until 2010 
#we can determine how old a house is and how long ago the house was sold:

df.numeric['Age'] <- as.numeric(2010 - df.combined$YearBuilt)

df.numeric['TimeSinceSold'] <- as.numeric(2010 - df.combined$YrSold)

# how many years since the house was remodelled and sold 
df.numeric['YearSinceRemodel'] <- as.numeric(df.combined$YrSold - df.combined$YearRemodAdd)


#####################################

###Correlation plot with OverallQual
library(corrplot)

corr.OverallQual <- as.matrix(sort(correlations[,'OverallQual'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.OverallQual, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx, corr.idx]), type = 'upper',
         method = 'color', addCoef.col = 'black', tl.cex =.7, cl.cex = .7,
         number.cex = .7)


############ outliers

train.test.df <- rbind(dplyr::select(ames_train,-SalePrice), ames_test)
train.test.df$type <- c(rep('train',1460),rep('test',1459))

ggplot(ames_train, aes(x=GrLivArea)) +
  geom_histogram(fill='lightblue',color='white') +
  theme_minimal()

outlier_values <- boxplot.stats(ames_train$GrLivArea)$out  # outlier values.
boxplot(ames_train$GrLivArea, main="GrLivArea", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values[outlier_values>4000], collapse=", ")), cex=0.6)


ggplot(train.test.df, aes(x=type, y=GrLivArea, fill=type)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(breaks = c("test", "train"), values = c("indianred", "lightblue"))


idx.outliers <- which(ames_train$GrLivArea > 4000)
df.numeric <- df.numeric[!1:nrow(df.numeric) %in% idx.outliers,]
df.combined <- df.combined[!1:nrow(df.combined) %in% idx.outliers,]
dim(df.numeric)

################################### Preprocessing

############### checking for normality of independent variable and standardizing the independent variables

###### normality check:skewness,kurtosis, Kolmogorov-Smirnof test
### log(x+1) is taken for higly skewed values

View(df.numeric)

library(moments)
library(psych)
# linear models assume normality from dependant variables 
# transform any skewed data into normal
skewed <- apply(df.numeric, 2, skewness)
skewed <- skewed[(skewed > 0.8) | (skewed < -0.8)]
skewed
kurtosi <- apply(df.numeric, 2, kurtosis)
kurtosi <- kurtosis[(kurtosis > 3.0) | (kurtosis < -3.0)]
kurtosi

# not very useful in our case
ks.p.val <- NULL
for (i in 1:length(df.numeric)) {
  test.stat <- ks.test(df.numeric[i], rnorm(1000))
  ks.p.val[i] <- test.stat$p.value
}
ks.p.val

for(col in names(skewed)){
  if(0 %in% df.numeric[, col]) {
    df.numeric[,col] <- log(1+df.numeric[,col])
  }
  else {
    df.numeric[,col] <- log(df.numeric[,col])
  }
}

# normalize the data
library(caret)
scaler <- preProcess(df.numeric)
df.numeric <- predict(scaler, df.numeric)



#### For the rest of the categoric features we can one-hot encode each value to get as many splits in the data as possible


# one hot encoding for categorical data
# sparse data performs better for trees/xgboost
dummy <- dummyVars(" ~ ." , data=df.combined[,cat_features])
df.categoric <- data.frame(predict(dummy,newdata=df.combined[,cat_features]))

str(df.combined)

# every 20 years create a new bin
# 7 total bins
# min year is 1871, max year is 2010!
year.map = function(col.combined, col.name) {
  for (i in 1:7) {
    year.seq = seq(1871+(i-1)*20, 1871+i*20-1)
    idx = which(df.combined[,col.combined] %in% year.seq)
    df.categoric[idx,col.name] = i
  }
  return(df.categoric)
}

df.categoric['GarageYrBltBin'] = 0
df.categoric <- year.map('GarageYrBlt', 'GarageYrBltBin')
df.categoric['YearBuiltBin'] = 0
df.categoric <- year.map('YearBuilt','YearBuiltBin')
df.categoric['YearRemodAddBin'] = 0
df.categoric <- year.map('YearRemodAdd', 'YearRemodAddBin')

bin.cols <- c('GarageYrBltBin', 'YearBuiltBin', 'YearRemodAddBin')

for (col in bin.cols) {
  df.categoric <- cbind(df.categoric, model.matrix(~.-1, df.categoric[col]))
}

# lets drop the orginal 'GarageYrBltBin', 'YearBuiltBin', 'YearRemodAddBin' from our dataframe
df.categoric <- df.categoric[,!names(df.categoric) %in% bin.cols]

### combining into a single df
df <- cbind(df.numeric, df.categoric)

str(df)

### distribution of housing prices

install.packages("WVPlots")
library(WVPlots)
y.true <- ames_train$SalePrice[which(!1:1460 %in% idx.outliers)]

qplot(y.true, geom='density') +# +(train, aes(x=SalePrice)) +
  geom_histogram(aes(y=..density..), color='white', 
                 fill='lightblue', alpha=.5, bins = 60) +
  geom_line(aes(y=..density..), color='cornflowerblue', lwd = 1, stat = 'density') + 
  stat_function(fun = dnorm, colour = 'indianred', lwd = 1, args = 
                  list(mean(ames_train$SalePrice), sd(ames_train$SalePrice))) +
  scale_x_continuous(breaks = seq(0,800000,100000), labels = dollar) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate('text', label = paste('skewness =', signif(skewness(ames_train$SalePrice),4)),
           x=500000,y=7.5e-06)


qqnorm(ames_train$SalePrice)
qqline(ames_train$SalePrice)


#We can see from the histogram and the quantile-quantile plot that the distribution of sale prices is right-skewed and does not follow a normal distribution. 
#Lets make a log-transformation and see how our data looks

y_train <- log(y.true+1)

qplot(y_train, geom = 'density') +
  geom_histogram(aes(y=..density..), color = 'white', fill = 'lightblue', alpha = .5, bins = 60) +
  scale_x_continuous(breaks = seq(0,800000,100000), labels = comma) +
  geom_line(aes(y=..density..), color='dodgerblue4', lwd = 1, stat = 'density') + 
  stat_function(fun = dnorm, colour = 'indianred', lwd = 1, args = 
                  list(mean(y_train), sd(y_train))) +
  #scale_x_continuous(breaks = seq(0,800000,100000), labels = dollar) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate('text', label = paste('skewness =', signif(skewness(y_train),4)),
           x=13,y=1) +
  labs(x = 'log(SalePrice + 1)')

qqnorm(y_train)
qqline(y_train)


paste('The dataframe has', dim(df)[1], 'rows and', dim(df)[2], 'columns')
