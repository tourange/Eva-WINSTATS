#
# R-Code for Handout 5
#
#
misclass = function(fit,y) {
  temp <- table(fit,y)
  cat("Table of Misclassification\n")
  cat("(row = predicted, col = actual)\n")
  print(temp)
  cat("\n\n")
  numcor <- sum(diag(temp))
  numinc <- length(y) - numcor
  mcr <- numinc/length(y)
  cat(paste("Misclassification Rate = ",format(mcr,digits=3)))
  cat("\n")
}
#
# Read in Breast Cancer diagnosis .csv file.
#
BreastDiag = read.table(file.choose(),header=T,sep=",")
names(BreastDiag)
head(BreastDiag,5)

str(BreastDiag)
par(mfrow=c(3,2))
plot(Radius~Diagnosis,data=BreastDiag,col="red")
plot(Texture~Diagnosis,data=BreastDiag,col="red")
plot(FracDim~Diagnosis,data=BreastDiag,col="red")
plot(Symmetry~Diagnosis,data=BreastDiag,col="red")
plot(Compactness~Diagnosis,data=BreastDiag,col="red")
plot(Concavity~Diagnosis,data=BreastDiag,col="red")
par(mfrow=c(1,1))

sam = sample(1:569,floor(569*.6666),replace=F)

#Should do train valid test set, but just going to do this for now (go back and forth bw train valid [tuning], then try test)
BCtrain = BreastDiag[sam,]
BCtest = BreastDiag[-sam,]

dim(BCtrain)
dim(BCtest)

bc.rpart = rpart(Diagnosis~.,data=BCtrain)
plot(bc.rpart)
text(bc.rpart)

yfit = predict(bc.rpart,type="class")
misclass(yfit,BCtrain$Diagnosis)

# This command may not work.
post(bc.rpart) #can't view, try below
prp(bc.rpart)

yprob = predict(bc.rpart)
head(yprob,5)
summary(bc.rpart)

bc.rpart2 = rpart(Diagnosis~.,data=BCtrain,cp=.0001,minsplit=4)
yfit = predict(bc.rpart2)
yfit = predict(bc.rpart2,type="class")
misclass(yfit,BCtrain$Diagnosis)

prp(bc.rpart2,type=4,extra=3,cex=.7)

#test
ypred = predict(bc.rpart,newdata=BCtest,type="class")
misclass(ypred,BCtest$Diagnosis)

ypred2 = predict(bc.rpart2,newdata=BCtest,type="class")
misclass(ypred2,BCtest$Diagnosis)

crpart.sscv = function(fit,y,data,B=25,p=.333) {
  n = length(y)
  cv <- rep(0,B)
  for (i in 1:B) {
    ss <- floor(n*p)
    sam <- sample(1:n,ss)
    temp <- data[-sam,]
    fit2 <- rpart(formula(fit),data=temp,parms=fit$parms,control=fit$control)
    ynew <- predict(fit2,newdata=data[sam,],type="class")
    tab <- table(y[sam],ynew)
    mc <- ss - sum(diag(tab))
    cv[i] <- mc/ss
  }
  cv
}

bc.rpart3 = rpart(Diagnosis~.,data=BCtrain,cp=.00001,minsplit=5)
results = crpart.sscv(bc.rpart3,BCtrain$Diagnosis,data=BCtrain,B=200)
summary(results)

plotcp(bc.rpart3)

bc.rpart4 = rpart(Diagnosis~.,data=BCtrain,cp=.019,minsplit=5)
results = crpart.sscv(bc.rpart4,BCtrain$Diagnosis,data=BCtrain,B=200)
summary(results)

ypred = predict(bc.rpart4,newdata=BCtest,type="class")
misclass(ypred,BCtest$Diagnosis)

#library(ipred)
#bagging
bc.bag = bagging(Diagnosis~.,data=BCtrain,nbagg=100,coob=T,
                 control=rpart.control(cp=.019,minsplit=5,xval=0))
bc.bag

ypred = predict(bc.bag,newdata=BCtest,type="class")
misclass(ypred,BCtest$Diagnosis)

library(randomForest)
bc.rf = randomForest(Diagnosis~.,data=BCtrain)
bc.rf
#importance of variables
varImpPlot(bc.rf) #focus on wconpts, wperi, warea, wrad, COncavePts

#Predict test cases without any fine tuning
ypred = predict(bc.rf, newdata=BCtest,type="class")
misclass(ypred,BCtest$Diagnosis)

#boosting
library(ada)
# no tuning here - there are some tuning parameters also
bc.boost = ada(Diagnosis~.,data=BCtrain)
bc.boost
summary(bc.boost)

ypred = predict(bc.boost,newdata=BCtest)
attributes(ypred)
misclass(ypred,BCtest$Diagnosis)

#
# Task - Classifying Olive Oils - read in the file OliveOils.csv
#

#can't boost here because boosting in ada package for binary problems only (ada binary, adabag nonbinary)
OliveOils = read.table(file.choose(),header=T,sep=",") 
names(OliveOils)
dim(OliveOils)
train = sample(1:572,floor(.6666*572),replace=F)
Olive.train = OliveOils[train,]
Olive.test = OliveOils[-train,]
dim(Olive.train)
dim(Olive.test)
#
# You will have to replace "other settings you choose" with something!!
#
#1
oo.rpart = rpart(Area.name~.,data=Olive.train)
prp(oo.rpart)
yfit = predict(oo.rpart,type="class")
misclass(yfit,Olive.test$Area.name)
results = crpart.sscv(oo.rpart,Olive.train$Area.name,data=Olive.train,B=200)
summary(results)
plotcp(oo.rpart)

oo.rpart2 = rpart(Area.name~.,data=Olive.train,cp=0.0001, minsplit=3)
results = crpart.sscv(oo.rpart2,Olive.train$Area.name,data=Olive.train,B=200)
summary(results)
yfit = predict(oo.rpart2,newdata= Olive.test, type="class")
misclass(yfit,Olive.test$Area.name)

prp(oo.rpart2)
table(Olive.train$Area.name)
#makes table of actual vs pred.
fitted = predict(oo.rpart2,newdata=Olive.train)
fitted2 = predict(oo.rpart2,newdata=Olive.test,type="class") #predicted region, not probability distribution in that terminal node
  # gives you nine number for each area (prediction is highest probability)
misclass(fitted2,Olive.test$Area.name)
summary(oo.rpart2)

#2.
olive.bag = bagging(Area.name~.,data=Olive.train, nbagg=100,coob=T,
                    control=rpart.control(cp=.019,minsplit=5,xval=0))
ypred = predict(olive.bag,newdata=Olive.test,type="class")
attributes(ypred)
# just extracting class argument
#ypred$error
#misclass(ypred$class,Olive.test$Area.name)
misclass(ypred,Olive.test$Area.name)

#3.
oo.rf = randomForest(Area.name~.,data=Olive.train)
oo.rf
#importance of variables
varImpPlot(oo.rf) #focus on oleic, linoleic, palmitoleic

#Predict test cases without any fine tuning
ypred = predict(oo.rf, newdata=Olive.test,type="class")
misclass(ypred,Olive.test$Area.name)


#4.
#
# Again you have some tunning parameters to fill in.
#oo.rpart2 = rpart(Area.name~.,data=Olive.train,cp=0.0001, minsplit=3)
library(adabag)
olive.boost = boosting(Area.name~.,data=Olive.train,mfinal=100,
                       control=rpart.control(cp=0.0001, minsplit=3))
ypred = predict(olive.boost,newdata=Olive.test)
#attributes(ypred)
misclass(ypred$class,Olive.test$Area.name)	

#
# Task 2 - Mushrooms - read in the file Mushrooms.csv --- should be able to get area down to 0 (just used rpart)
#
Mushrooms = read.table(file.choose(),header=T,sep=",")
dim(Mushrooms) 
names(Mushrooms)
summary(Mushrooms)
str(Mushrooms)
Mushrooms2 = Mushrooms[,-c(12,17)]
train = sample(1:nrow(Mushrooms2),floor(nrow(Mushrooms)*0.5),replace=F)
Mush.train = Mushrooms2[train,]
Mush.test = Mushrooms2[-train,]
mush.tree = rpart(Poisonous~.,data=Mush.train)
summary(mush.tree)
misclass(Mush.train$Poisonous,predict(mush.tree,type="class"))
ypred = predict(mush.tree,newdata=Mush.test,type="class")
misclass(Mush.test$Poisonous,ypred)

#Brant's solution
results = rpart(Poisonous~.,data=Mush.train,control=rpart.control(cp=0,minbucket=1))
ypred = predict(results,newdata=Mush.test,type="class")
misclass(Mush.test$Poisonous,ypred)

# random forest

mush.rf = randomForest(Poisonous~.,data=Mush.train)
mush.rf
#importance of variables
varImpPlot(mush.rf)

#Predict test cases without any fine tuning
ypred = predict(mush.rf, newdata=Mush.test,type="class")
misclass(ypred,Mush.test$Poisonous)







