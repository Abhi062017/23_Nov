#C5.0 classification on UCI's 'balance-scale' dataset
getwd()
data <- read.csv('balance-scale.data', header = F, stringsAsFactors = F)
str(data)
summary(data)
unique(data$V1)
data$V1 <- as.factor(data$V1) #casting the response variable into factor

#randomize for stratified split
random.data <- data[order(runif(nrow(data))),]
View(random.data)

#split
nrow(random.data)
0.8*625
train <- random.data[1:500,]
test <- random.data[501:625,]
#***************************************************************************

#C5.0 model building
install.packages('C50', dependencies = T)
library(C50)
model <- C5.0(train[,-1], train[,1])
summary(model)
plot(model)

#prediction
model.pred <- predict(model, test)
model.pred
table(test[,1], model.pred) #confusion matrix of sorts
sum(diag(table(test[,1], model.pred)))/sum(table(test[,1], model.pred)) #accuracy
#*****************************************************************************

#Tuning:
#Tune Model by introducing 'trials' (iterations)
model.tuned <- C5.0(train[,-1], train[,1], trials = 25)
model.tuned
plot(model.tuned)

#prediciton
model.tuned.pred <- predict(model.tuned, test)
model.tuned.pred
table(values = test[,1],model.tuned.pred) #confusion matrix of sorts
a <- table(values = test[,1],model.tuned.pred)
sum(diag(a))/sum(a) #accuracy (better than the one from previous model)
