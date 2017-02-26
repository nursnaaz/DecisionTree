# setting the working directory
rm(list=ls(all=TRUE))

setwd("~/Desktop/GNQ3/20170205_Batch25_CSE7405c_DecisionTreesActivity")
#read.table-reads a file in table format and creates a data frame on it.
univ = read.table('UnivBank.csv', header=T,sep=',',
                  col.names=c('ID','age','exp','inc',
                              'zip','family','ccavg',
                              'edu','mortgage','loan',
                              'securities','cd',
                              'online','cc'))

# removing the id, experience and Zip, 
# experience is correlated to age
univ=univ[,-c(1,3,5)]
univ1 = univ
univ2= univ

str(univ)
univ$family=as.factor(univ$family)
univ$edu=as.factor(univ$edu)
univ$mortgage=as.factor(univ$mortgage)
univ$loan=as.factor(univ$loan)
univ$securities=as.factor(univ$securities)
univ$cd=as.factor(univ$cd)
univ$online=as.factor(univ$online)
univ$cc=as.factor(univ$cc)

univ1 = as.data.frame(apply(univ1,2,function(x){as.factor(x)}))

#univ2 = apply(univ2,2,function(x){as.factor(x)})
#str(univ2)
#rm(univ2)
#convert mortgage as numeric
univ$mortgage=as.numeric(univ$mortgage)
str(univ)
set.seed(123)
rows = seq(1, nrow(univ), 1)
trainRows = sample(rows, nrow(univ) * .6)
testRows=rows[-(trainRows)]


train = univ[trainRows,] 
test=univ[testRows,] 


rm(univ,evalRows,remainingRows,rows,testRows,trainRows)

library(rpart)

# Regression Trees using CART 
dtCart = rpart(inc ~., data=train, method="anova")    
plot(dtCart, main="Decision Tree for Income", margin=0.15, uniform=TRUE,)
text(dtCart, use.n=T)

predCartTrain = predict(dtCart, newdata=train, type="vector")
predCartTest = predict(dtCart, newdata=test, type="vector")
predCartEval = predict(dtCart, newdata=eval, type="vector")

library(DMwR)
regr.eval(train[,"inc"], predCartTrain, train.y = train[,"inc"])
regr.eval(test[,"inc"], predCartTest, train.y = train[,"inc"])
regr.eval(eval[,"inc"], predCartEval, train.y = train[,"inc"])


dtCart=rpart(inc ~.,data=train,method="anova", cp=0.001)

dtCart=rpart(inc ~.,data=train,method="anova", cp=0.002)

dtCart=rpart(inc ~.,data=train,method="anova", cp=0.005)



# Classification Trees using CART 
dtCart=rpart(loan~., data=train, method="class")    
plot(dtCart,main="Classification Tree for loan Class",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

a = table(train$loan, predict(dtCart, newdata=train, type="class"))
(a[2,2])/(a[2,1]+a[2,2])*100

# Classification Trees using C5.0 
library(C50)
dtC50 = C5.0(loan ~ ., data = train, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)


a=table(train$loan, predict(dtC50, newdata=train, type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
a=table(test$loan, predict(dtC50, newdata=test, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100


cat("Recall in Training", rcTrain, '\n',
    "Recall in Testing", rcTest
    )

