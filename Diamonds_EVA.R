Diamonds = read.table(file.choose(), header=T,sep=",")
Diamonds$Price = log(Diamonds$Price)
Diamonds.train=Diamonds[Diamonds$Test<2,]
Diamonds.test = Diamonds[Diamonds$Test==2,]

Diamonds.train = Diamonds.train[,-10]

#lm1 = lm(Price~Carats+Cut+Color+Clarity+TDdiff+TDratio,data=Diamonds.train)
lm1 = lm(Price~.,data=Diamonds.train)
summary(lm1)
plot(lm1)

lm2 = lm(Price~.-Table-Depth, data=Diamonds.train)
summary(lm2)
plot(lm2)

lm3 = lm(Price~Carats*Clarity+Cut+Color+TDratio,data=Diamonds.train)
summary(lm3)

lm4 = lm(Price~Carats*Clarity+Color+Cut,data=Diamonds.train)
summary(lm4)

anova(lm4)
inverseResponsePlot(lm4)

lm5 = lm(Price~Carats*Clarity+Clarity*Color+Cut,data=Diamonds.train)
summary(lm5)

plot(Diamonds.train$Clarity,Diamonds.train$Price)

par(mfrow=c(2,2))
lm.poo2 = lm(Price~poly(Carats,3)+Clarity+Color+Cut+TDratio,data=Diamonds.train)
plot(lm.poo2)

#Final model
lm.poo3 = lm(Price~poly(Carats,3)+Cut*Color+Cut*Clarity+Clarity*Color+TDratio, data=Diamonds.train)
summary(lm.poo3)

ypredlog = predict(lm.poo3, newdata = Diamonds.test)
ypred = exp(ypredlog)
yactual = exp(Diamonds.test$Price)
plot(yactual, ypred, xlab="Actual Price", ylab="Predicted Price")

PredAcc(yactual, ypred)
