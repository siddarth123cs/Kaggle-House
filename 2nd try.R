
library(glmnet)
library(caret)
library(MASS)
library(vegan)
library(data.table)
library(doParallel)
library(DMwR)
library(dummies)
registerDoParallel(8)
rm(list=ls(all=T))
setwd("C:\\Users\\SIDDARTH\\Desktop\\kaggle house")
housetrain=read.csv("train.csv",stringsAsFactors = F)
housetest=read.csv("test (2).csv",stringsAsFactors = F)
housetrain=housetrain[,setdiff(names(housetrain),c("Id","PoolQC","BsmtFinSF2","X3SsnPorch","TotRmsAbvGrd"))]#,"MiscFeature","Utilities","PoolArea","Street","Condition2","LowQualFinSF"))]
housetest=housetest[,setdiff(names(housetest),c("Id","PoolQC","BsmtFinSF2","X3SsnPorch","TotRmsAbvGrd"))]#,"MiscFeature","Utilities","PoolArea","Street","Condition2","LowQualFinSF"))]
house=rbind(housetrain[,-76],housetest)
library(car)
len=(1:length(housetrain$SalePrice))
x=plot(density(1/(housetrain$SalePrice)))
colSums(is.na(house))
str(house)
house$GarageYrBlt=ifelse((house$GarageYrBlt)==2207,2007,house$GarageYrBlt)
summary(house)
house$MasVnrArea=ifelse(is.na(house$MasVnrArea)==T,0,house$MasVnrArea)
house$Alley[is.na(house$Alley)]=rep("notthere",sum(is.na(house$Alley)))
house$MasVnrType[is.na(house$MasVnrType)]="notthere"
house$BsmtQual[is.na(house$BsmtQual)]="notthere"
house$BsmtCond[is.na(house$BsmtCond)]="notthere"
house$BsmtExposure[is.na(house$BsmtExposure)]="notthere"
house$BsmtFinType1[is.na(house$BsmtFinType1)]="notthere"
house$BsmtFinType2[is.na(house$BsmtFinType2)]="notthere"
house$FireplaceQu[is.na(house$FireplaceQu)]="notthere"
house$GarageType[is.na(house$GarageType)]="notthere"
house$GarageYrBlt[is.na(house$GarageYrBlt)]=0
house$GarageArea[is.na(house$GarageArea)]=0
house$Functional[is.na(house$Functional)]="notthere"
house$BsmtHalfBath[is.na(house$BsmtHalfBath)]=0
house$BsmtFullBath[is.na(house$BsmtFullBath)]=0
house$GarageCars[is.na(house$GarageCars)]=0
house$TotalBsmtSF[is.na(house$TotalBsmtSF)]=0
house$BsmtUnfSF[is.na(house$BsmtUnfSF)]=0
house$BsmtFinSF1[is.na(house$BsmtFinSF1)]=0
house$BsmtFinSF2[is.na(house$BsmtFinSF2)]=0
house$GarageFinish[is.na(house$GarageFinish)]="notthere"
house$GarageQual[is.na(house$GarageQual)]="notthere"
house$GarageCond[is.na(house$GarageCond)]="notthere"
house$MiscFeature[is.na(house$MiscFeature)]="notthere"
house$PoolArea[is.na(house$PoolArea)]=0
house$Fence[is.na(house$Fence)]="notthere"
house$MiscFeature[is.na(house$MiscFeature)]="notthere"
house$age=house$YrSold - house$YearBuilt
house$adage=house$YearRemodAdd - house$YearBuilt
house$adsold=house$YearRemodAdd - house$YrSold
house$totarea=house$TotalBsmtSF+house$X1stFlrSF+house$X2ndFlrSF
str(data)
num_attr<-c("LotFrontage","LotArea","YearBuilt","MasVnrArea","BsmtFinSF1","OverallQual","YearRemodAdd","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","GarageYrBlt","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","ScreenPorch","totarea","PoolArea","LowQualFinSF","YrSold","age","adage","adsold")
cat_attr<-setdiff(x=names(house),y=num_attr)
cat_data<-data.frame(sapply(house[,cat_attr],as.factor))
#sapply(cat_attr,function(x)table(finaldata2[,x]))
#sapply(cat_attr,function(x)table(finaltest[,x]))
num_data<-data.frame(sapply(house[,num_attr],as.numeric))
data = cbind(num_data, cat_data)
data=data[,setdiff(names(data),c("YrSold"))]

data$GrLivArea=ifelse(data$GrLivArea==0,0,log(data$GrLivArea))
data$TotalBsmtSF=ifelse(data$TotalBsmtSF==0,0,log(data$TotalBsmtSF))
data$X1stFlrSF=ifelse(data$X1stFlrSF==0,0,log(data$X1stFlrSF))
data$X2ndFlrSF=ifelse(data$X2ndFlrSF==0,0,log(data$X2ndFlrSF))
#data$age=ifelse(data$age==0|1,0,1/(log(sqrt((data$age)^2))))
data$MasVnrArea=ifelse(data$MasVnrArea==0,0,log(data$MasVnrArea))
colSums(is.na(data))
imp=data[,c(1,34,35)]
colSums(is.na(imp))
library(DMwR)
imp=knnImputation(imp,k=15)
data[,c(1,34,35)]=imp[,c(1,2,3)]
houseimp=centralImputation(data)
library(dplyr)
d_cor <- as.matrix(cor(data.matrix(houseimp)))
d_cor_melt <- arrange(melt(d_cor), -abs(value))
dplyr::filter(d_cor_melt, value > .)
cor(houseimp$LotFrontage)
finaldata=houseimp[1:1460,]
finaltest=houseimp[1461:2919,]
finaldata$price=log(housetrain[,76])
str(finaldata)
set.seed(123)
p=plot(finaldata$price,finaldata$totarea)
out=identify(x=finaldata$price,finaldata$totarea)
p=plot(finaldata$price,finaldata$age)
finaldata=finaldata[-out,]
plot(finaldata$price,(finaldata$YrSold - finaldata$YearRemodAdd))
cor(finaldata$price,as.numeric(finaldata$LotFrontage))
str(finaldata)
library(glmnet)
x=data.matrix(finaldata[,setdiff(names(finaldata),"price")])
fit.lasso.cv <- cv.glmnet(x,finaldata$price, type.measure="mse", alpha=0.96, 
                          family="gaussian",nfolds=20,parallel=TRUE)
finaltest=data.matrix(finaltest)
pred_Test = predict(fit.lasso.cv,finaltest,s=fit.lasso.cv$lambda.min)
pred_Test=exp(pred_Test)
lasso_train=predict(fit.lasso.cv,data.matrix(finaldata[,setdiff(names(finaldata),"price")]),s=fit.lasso.cv$lambda.min)
write.csv(x=pred_Test,file = "try.csv")



#stacking
train_Pred_All_Models = data.frame(lass = (lasso_train),
                                   xgb = (xg_train),
                                   random = (random_train))
train_Pred_All_Models = data.frame(sapply(train_Pred_All_Models, 
                                          as.numeric))
train_Pred_All_Models = cbind(train_Pred_All_Models, price = (finaldata$price))
x=data.matrix(train_Pred_All_Models[,setdiff(names(train_Pred_All_Models),"price")])
ensemble_Model = cv.glmnet(x,train_Pred_All_Models$price, type.measure="mse", alpha=0, 
                           family="gaussian",nfolds=20,parallel=TRUE)
ensemble_Model2=randomForest(price~.,train_Pred_All_Models,ntree=51)
test_Pred_All_Models = data.frame(lass = pred_Test,
                                  xgb = pred_Test2,
                                  random = pred_Test3) 
test_Pred_All_Models = data.frame(sapply(test_Pred_All_Models, as.numeric))
test_Pred_All_Models=data.matrix(test_Pred_All_Models)
ensemble_pred = predict(ensemble_Model,test_Pred_All_Models,s=ensemble_Model$lambda.min)
pred_Test4=exp(ensemble_pred)
write.csv(x=pred_Test4,file = "try.csv")

  