#1.
CH_test = read.csv("ChiHomes(test).csv")
CH = read.csv("ChiHomes(train).csv")
CH_test = CH_test[,-3]
CH = CH[,-3]

# THIS IS WRONG BUT IM KEEPING IT SO I CAN UNDERSTAND WHAT I WAS MISUNDERSTANDING
#CH_test$ListPrice = log(CH_test$ListPrice)
#CH$ListPrice = log(CH$ListPrice)

#tree1 = rpart(Price~.,data=diam.train)
#prp(tree1,type=3,main="Regression Tree for Diamond Prices")
#plot(predict(tree1),diam.train$Price) #Bad - really doesn't predict price

PredAcc = function(y,ypred){
  RMSEP = sqrt(mean((y-ypred)^2))
  MAE = mean(abs(y-ypred))
  MAPE = mean(abs(y-ypred)/y)*100
  cat("RMSEP\n")
  cat("===============\n")
  cat(RMSEP,"\n\n")
  cat("MAE\n")
  cat("===============\n")
  cat(MAE,"\n\n")
  cat("MAPE\n")
  cat("===============\n")
  cat(MAPE,"\n\n")
  return(data.frame(RMSEP=RMSEP,MAE=MAE,MAPE=MAPE))
}

CH.rpart = rpart(log(ListPrice)~.,data=CH)
plot(CH.rpart)
text(CH.rpart)
prp(CH.rpart, type=4,digits=4)

ypredlog = predict(CH.rpart,newdata=CH_test)
ypred.test = exp(ypredlog)
PredAcc(CH_test$ListPrice,ypred.test)

# The Prediction accuracy is 26.6%

#2.
rpart.logsscv = function(fit,data,p=.667,B=100,
                      cp=fit$control$cp,minsplit=fit$control$minsplit) {
  MSE = rep(0,B)
  MAE = rep(0,B)
  MAPE = rep(0,B)
  y = exp(fit$y)
  n = nrow(data)
  ss <- floor(n*p)
  for (i in 1:B) {
    sam = sample(1:n,ss,replace=F)
    fit2 = rpart(formula(fit),data=data[sam,],cp=cp,minsplit=minsplit)
    ynew = exp(predict(fit2,newdata=data[-sam,]))
    MSE[i] = mean((y[-sam]-ynew)^2)
    MAE[i] = mean(abs(y[-sam]-ynew))
    MAPE[i] = mean((abs(y[-sam]-ynew)/y[-sam]))*100
  }
  RMSEP = sqrt(mean(MSE))
  MAEP = mean(MAE)
  MAPEP = mean(MAPE)
  cat("RMSEP\n")
  cat("===============\n")
  cat(RMSEP,"\n\n")
  cat("MAEP\n")
  cat("===============\n")
  cat(MAEP,"\n\n")
  cat("MAPEP\n")
  cat("===============\n")
  cat(MAPEP,"\n\n")
  temp = data.frame(MSEP=MSE,MAEP=MAE,MAPEP=MAPE)
  return(temp)
}

results = rpart.logsscv(CH.rpart,data=CH,cp=.015,minsplit=19)
plotcp(CH.rpart)

#3.

Diamonds = read.csv("Diamonds.csv")
library(rpart)
library(rpart.plot)
fit = rpart(log(Price)~.,data=Diamonds)

tree.vary = function(fit,data) {
  n = nrow(data)
  sam = sample(1:n,floor(n*.5),replace=F)
  temp = rpart(formula(fit),data=data[sam,])
  prp(temp,type=4,digits=3)
}


tree.vary(fit,data=Diamonds)

# Each tree is different! Yay