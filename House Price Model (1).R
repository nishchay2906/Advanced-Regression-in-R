# House Price Modelling

# Importing datasets
training.data<-read.csv(file.choose(), header=TRUE, sep=",")
testing.data<-read.csv(file.choose(), header=TRUE, sep=",")

names(training.data)[44:45] <- c("F_FLSF","S_FLSF")
names(training.data)[70] <- c("TSnPorch")

names(testing.data)[44:45] <- c("F_FLSF","S_FLSF")
names(testing.data)[70] <- c("TSnPorch")



training.data$MSSubClass <- as.character(training.data$MSSubClass)
testing.data$MSSubClass <- as.character(testing.data$MSSubClass)

# Changing YrSold to a character #
training.data$YrSold <- as.character(training.data$YrSold)
testing.data$YrSold <- as.character(testing.data$YrSold)

# Changing MoSold to a character #
training.data$MoSold <- as.character(training.data$MoSold)
testing.data$MoSold <- as.character(testing.data$MoSold)

# TotalSqFt being added #
training.data$TotalSqFt <- training.data$GrLivArea + training.data$TotalBsmtSF + training.data$GarageArea
testing.data$TotalSqFt <- testing.data$GrLivArea + testing.data$TotalBsmtSF + testing.data$GarageArea

# TotalPorch Sf being added #
training.data$TotalPorchSf <- training.data$OpenPorchSF + training.data$EnclosedPorch + training.data$ScreenPorch + training.data$TSnPorch
testing.data$TotalPorchSf <-testing.data$OpenPorchSF +testing.data$EnclosedPorch + testing.data$ScreenPorch + testing.data$TSnPorch

# TotalBaths being added #
training.data$TotalBaths <- training.data$BsmtFullBath + (training.data$BsmtHalfBath*.5) + training.data$FullBath + (training.data$HalfBath*.5)
testing.data$TotalBaths <- testing.data$BsmtFullBath + (testing.data$BsmtHalfBath*.5) + testing.data$FullBath + (testing.data$HalfBath*.5)


# HouseAge being added # 
training.data$HouseAge <- as.numeric(training.data$YrSold) - as.numeric(training.data$YearBuilt)
testing.data$HouseAge <-  as.numeric(testing.data$YrSold) - as.numeric(testing.data$YearBuilt)



#LastModified being added #
training.data$LastModified <- as.numeric(training.data$YrSold) - as.numeric(training.data$YearRemodAdd)
testing.data$LastModified <-  as.numeric(testing.data$YrSold) - as.numeric(testing.data$YearRemodAdd)


# Remodeled being added #
training.data$Remodeled <- ifelse(training.data$YearBuilt == training.data$YearRemodAdd,'No','Yes')
testing.data$Remodeled <- ifelse(testing.data$YearBuilt == testing.data$YearRemodAdd,'No','Yes')

# NewHouse #
training.data$NewHome <- ifelse(as.numeric(training.data$YrSold) - as.numeric(training.data$YearBuilt) <= 15,'Yes','No') 
testing.data$NewHome <- ifelse(as.numeric(testing.data$YrSold) - as.numeric(testing.data$YearBuilt) <= 15,'Yes','No') 

# HasPorch #
training.data$HasPorch <- ifelse(training.data$TotalPorchSf > 0,'Yes','No')
testing.data$HasPorch <- ifelse(testing.data$TotalPorchSf > 0,'Yes','No')

# HasDeck #
training.data$HasDeck <- ifelse(training.data$WoodDeckSF > 0,'Yes','No')
testing.data$HasDeck <- ifelse(testing.data$WoodDeckSF > 0,'Yes','No')



# Pre-processing data
new_training <- subset(training.data, select = -c(Alley, PoolQC, Fence, MiscFeature, BsmtCond, BsmtFinType2, GarageCond))
impute_data <- subset(new_training, select = -c(Street, Utilities, Condition2, RoofMatl, Heating, LandContour))

testing <- subset(testing.data, select = -c(Alley, PoolQC, Fence, MiscFeature, BsmtCond, BsmtFinType2, GarageCond))
testing <- subset(testing, select = -c(Street, Utilities, Condition2, RoofMatl, Heating, LandContour))

impute_data<-impute_data[!(impute_data$Id=="694" | impute_data$Id=="336" | impute_data$Id=="1325" | impute_data$Id=="689" | impute_data$Id=="875"| impute_data$Id=="329" | impute_data$Id=="524" | impute_data$Id=="62" | impute_data$Id=="969"  | impute_data$Id=="1454"),]

impute_data$BsmtQual<-replace(impute_data$BsmtQual, is.na(impute_data$BsmtQual), "None")
impute_data$BsmtExposure <-replace(impute_data$BsmtExposure, is.na(impute_data$BsmtExposure), "None")
impute_data$BsmtFinType1 <-replace(impute_data$BsmtFinType1, is.na(impute_data$BsmtFinType1), "None")
impute_data$FireplaceQu <-replace(impute_data$FireplaceQu, is.na(impute_data$FireplaceQu), "None")
impute_data$GarageType <-replace(impute_data$GarageType, is.na(impute_data$GarageType), "None")
impute_data$GarageFinish <-replace(impute_data$GarageFinish, is.na(impute_data$GarageFinish), "None")
impute_data$GarageQual <-replace(impute_data$GarageQual, is.na(impute_data$GarageQual), "None")

testing$BsmtQual<-replace(testing$BsmtQual, is.na(testing$BsmtQual), "None")
testing$BsmtExposure <-replace(testing$BsmtExposure, is.na(testing$BsmtExposure), "None")
testing$BsmtFinType1 <-replace(testing$BsmtFinType1, is.na(testing$BsmtFinType1), "None")
testing$FireplaceQu <-replace(testing$FireplaceQu, is.na(testing$FireplaceQu), "None")
testing$GarageType <-replace(testing$GarageType, is.na(testing$GarageType), "None")
testing$GarageFinish <-replace(testing$GarageFinish, is.na(testing$GarageFinish), "None")
testing$GarageQual <-replace(testing$GarageQual, is.na(testing$GarageQual), "None")




Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var in 1:ncol(impute_data)) {
  if (class(impute_data[,var]) %in% c("integer", "numeric")) {
    impute_data[is.na(impute_data[,var]),var] <- median(impute_data[,var], na.rm = TRUE)
  } else if (class(impute_data[,var]) %in% c("character")) {
    impute_data[is.na(impute_data[,var]),var] <- Mode(impute_data[,var], na.rm = TRUE)
  }
}

for (var in 1:ncol(testing)) {
  if (class(testing[,var]) %in% c("integer", "numeric")) {
    testing[is.na(testing[,var]),var] <- median(testing[,var], na.rm = TRUE)
  } else if (class(testing[,var]) %in% c("character")) {
    testing[is.na(testing[,var]),var] <- Mode(testing[,var], na.rm = TRUE)
  }
}




# Splitting the training dataset into the training and validation sets
library (caTools)
set.seed(6)
inx <- sample.split(seq_len(nrow(impute_data)), 0.80)
impute_data.training <- impute_data[inx, ]
impute_data.prediction <-  impute_data[!inx, ]
impute_data.predictionX=impute_data.prediction[c(1:67)];
actual.prices=impute_data.prediction[68];

# Creating the y variable and matrix (capital X) of x variables
library(glmnet)
y.training <- log(impute_data.training$SalePrice)
X<-model.matrix(~MSSubClass + MSZoning + LotFrontage + log(LotArea) + LotShape + LotConfig + Neighborhood + BldgType +  HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + (TotalBsmtSF) + HeatingQC + CentralAir + Electrical + F_FLSF + S_FLSF + LowQualFinSF + log(GrLivArea) + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + WoodDeckSF + log(OpenPorchSF) + log(EnclosedPorch) + log(TSnPorch) + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold + SaleCondition + MSSubClass*F_FLSF + MSSubClass*KitchenAbvGr + log(LotArea)*(TotalBsmtSF) + log(LotArea)*F_FLSF + log(LotArea)*log(GrLivArea) + log(LotArea)*Fireplaces + OverallQual*YearBuilt + OverallQual*YearRemodAdd + OverallQual*(TotalBsmtSF)  + OverallQual*F_FLSF   + OverallQual*log(GrLivArea)   + OverallQual*FullBath   + OverallQual*TotRmsAbvGrd   + OverallQual*Fireplaces   + OverallQual*GarageCars  + OverallQual*GarageArea  + OverallCond*YearBuilt + YearBuilt*YearRemodAdd  + YearBuilt*(TotalBsmtSF)  + YearBuilt*F_FLSF  + YearBuilt*FullBath  + YearBuilt*GarageCars  + YearBuilt*GarageArea  + YearBuilt*log(EnclosedPorch) + YearRemodAdd*(TotalBsmtSF)  + YearRemodAdd*log(GrLivArea)  + YearRemodAdd*FullBath  + YearRemodAdd*GarageCars  + YearRemodAdd*GarageArea + (TotalBsmtSF)*F_FLSF  + (TotalBsmtSF)*log(GrLivArea)  + (TotalBsmtSF)*FullBath  + (TotalBsmtSF)*TotRmsAbvGrd  + (TotalBsmtSF)*Fireplaces  + (TotalBsmtSF)*GarageCars  + (TotalBsmtSF)*GarageArea + F_FLSF*log(GrLivArea)  + F_FLSF*TotRmsAbvGrd  + F_FLSF*Fireplaces  + F_FLSF*GarageCars  + F_FLSF*GarageArea  + F_FLSF*FullBath + log(GrLivArea)*FullBath  + log(GrLivArea)*TotRmsAbvGrd  + log(GrLivArea)*Fireplaces  + log(GrLivArea)*GarageCars  + log(GrLivArea)*GarageArea  + log(GrLivArea)*WoodDeckSF  + log(GrLivArea)*log(OpenPorchSF) + KitchenAbvGr*TotRmsAbvGrd + FullBath*TotRmsAbvGrd  + FullBath*GarageCars  + FullBath*GarageArea  + FullBath*log(OpenPorchSF) + TotRmsAbvGrd*Fireplaces  + TotRmsAbvGrd*GarageCars  + TotRmsAbvGrd*GarageArea + Fireplaces*GarageCars + Fireplaces*GarageArea + GarageCars*GarageArea + GarageQual+ LandSlope +Condition1 + Functional + PavedDrive + SaleType+log(TotalSqFt)+log(TotalPorchSf)+log(TotalBaths)+HouseAge+LastModified+Remodeled+NewHome+HasPorch+HasDeck,impute_data)[,-1]
X<-cbind(impute_data$Id,X)

# spliting X into training and validation as before
X.training<-X[inx,]
X.testing<-X[!inx,]



#LASSO (alpha=1) ------------------------------------------------------------------------------------------------------
lasso.fit<-glmnet(x = X.training, y = y.training, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#Selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 1) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.008,0.015)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the validation set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
lasso.testing.MSE <- mean((lasso.testing- impute_data.prediction[,68] )^2) #calculate and display MSE in the testing set
lasso.testing.MAPE <- mean(abs(lasso.testing-impute_data.prediction[,68])/impute_data.prediction[,68]*100) # MAPE: mean absolute percentage error 

#ridge (alpha=0) ----------------------------------------------------------------------------------------------------
ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval.ridge <-  cv.glmnet(x = X.training, y = y.training, alpha = 0)
plot(crossval.ridge)
penalty.ridge <- crossval.ridge$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
ridge.testing.MSE <- mean((ridge.testing- impute_data.prediction[,68] )^2) #calculate and display MSE  in the testing set
ridge.testing.MAPE <-mean(abs(ridge.testing-impute_data.prediction[,68])/impute_data.prediction[,68]*100)  # MAPE: mean absolute percentage error 

# MODEL SELECTION: comparing the prediction error in the testing set
lasso.testing.MSE # LASSO 
ridge.testing.MSE # Ridge
# MSE results
# > lasso.testing.MSE # LASSO 
# [1] 377580941
# > ridge.testing.MSE # Ridge
# [1] 356002699
# ridge is better, so use it for prediction

# Predicting the test set results *******************************************************************************
testing$SalePrice <- NA
finaldata <- rbind(impute_data, testing)
impute_data.training <- finaldata[1:1450, ]
impute_data.prediction <-  finaldata[1451:2909, ]

library(glmnet)
#create the y variable and matrix (capital X) of x variables
y.training <- log(impute_data.training$SalePrice)
X.training.total<-model.matrix(~MSSubClass + MSZoning + LotFrontage + log(LotArea) + LotShape + LotConfig + Neighborhood + BldgType +  HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + (TotalBsmtSF) + HeatingQC + CentralAir + Electrical + F_FLSF + S_FLSF + LowQualFinSF + log(GrLivArea) + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + WoodDeckSF + log(OpenPorchSF) + log(EnclosedPorch) + log(TSnPorch) + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold + SaleCondition + MSSubClass*F_FLSF + MSSubClass*KitchenAbvGr + log(LotArea)*(TotalBsmtSF) + log(LotArea)*F_FLSF + log(LotArea)*log(GrLivArea) + log(LotArea)*Fireplaces + OverallQual*YearBuilt + OverallQual*YearRemodAdd + OverallQual*(TotalBsmtSF)  + OverallQual*F_FLSF   + OverallQual*log(GrLivArea)   + OverallQual*FullBath   + OverallQual*TotRmsAbvGrd   + OverallQual*Fireplaces   + OverallQual*GarageCars  + OverallQual*GarageArea  + OverallCond*YearBuilt + YearBuilt*YearRemodAdd  + YearBuilt*(TotalBsmtSF)  + YearBuilt*F_FLSF  + YearBuilt*FullBath  + YearBuilt*GarageCars  + YearBuilt*GarageArea  + YearBuilt*log(EnclosedPorch) + YearRemodAdd*(TotalBsmtSF)  + YearRemodAdd*log(GrLivArea)  + YearRemodAdd*FullBath  + YearRemodAdd*GarageCars  + YearRemodAdd*GarageArea + (TotalBsmtSF)*F_FLSF  + (TotalBsmtSF)*log(GrLivArea)  + (TotalBsmtSF)*FullBath  + (TotalBsmtSF)*TotRmsAbvGrd  + (TotalBsmtSF)*Fireplaces  + (TotalBsmtSF)*GarageCars  + (TotalBsmtSF)*GarageArea + F_FLSF*log(GrLivArea)  + F_FLSF*TotRmsAbvGrd  + F_FLSF*Fireplaces  + F_FLSF*GarageCars  + F_FLSF*GarageArea  + F_FLSF*FullBath + log(GrLivArea)*FullBath  + log(GrLivArea)*TotRmsAbvGrd  + log(GrLivArea)*Fireplaces  + log(GrLivArea)*GarageCars  + log(GrLivArea)*GarageArea  + log(GrLivArea)*WoodDeckSF  + log(GrLivArea)*log(OpenPorchSF) + KitchenAbvGr*TotRmsAbvGrd + FullBath*TotRmsAbvGrd  + FullBath*GarageCars  + FullBath*GarageArea  + FullBath*log(OpenPorchSF) + TotRmsAbvGrd*Fireplaces  + TotRmsAbvGrd*GarageCars  + TotRmsAbvGrd*GarageArea + Fireplaces*GarageCars + Fireplaces*GarageArea + GarageCars*GarageArea + GarageQual+ LandSlope +Condition1 + Functional + PavedDrive + SaleType+log(TotalSqFt)+log(TotalPorchSf)+log(TotalBaths)+HouseAge+LastModified+Remodeled+NewHome+HasPorch+HasDeck, finaldata)[,-1]
X.training.total<-cbind(finaldata$Id,X.training.total)
X.training <- X.training.total[1:1450, ]
X.prediction <-  X.training.total[1451:2909, ]

#LASSO (alpha=1) ---------------------------------------------------------------------------------------------
lasso.fit<-glmnet(x = X.training, y = y.training, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#Selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 1) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.008,0.015)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
#write.csv(lasso.testing, file = "Predicted House Prices lasso.csv")
write.csv(data.frame(Id=testing$Id,SalePrice=lasso.testing),"lasso_prediction.csv",row.names = F)

#ridge (alpha=0) ----------------------------------------------------------------------------------------------------
ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval.ridge <-  cv.glmnet(x = X.training, y = y.training, alpha = 0)
plot(crossval.ridge)
penalty.ridge <- crossval.ridge$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

# predicting the performance on the testing set
ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.prediction))
#write.csv(ridge.testing, file = "Predicted House Prices ridge.csv")
write.csv(data.frame(Id=testing$Id, SalePrice= ridge.testing),"ridge_testing.csv",row.names = F)
