#rpart classification on UCI's glass dataset
getwd()

#read file
glass <- read.csv('glass.csv', header = F, stringsAsFactors = F)
str(glass)
glass <- glass[,-1]
unique(glass$V11)
glass$V11 <- as.factor(glass$V11) #casting the response to factor
summary(glass)

#normalizing the dataset
normalize <- function(y){(y-min(y))/(max(y)-min(y))}
normalized.glass <- as.data.frame(lapply(glass[,-10], normalize))#excluding response variable
View(normalized.glass)

#cbind the response variable
normalized.glass <- cbind(normalized.glass, V11 = glass$V11) #changing colname on the fly
summary(normalized.glass)

#randomize for stratified split
random.glass <- normalized.glass[order(runif(nrow(normalized.glass))),]
View(random.glass)

#80/20 split
nrow(random.glass)
0.8*214
train <- random.glass[1:171,]
test <- random.glass[172:214,]
#******************************************************************************

#Building a rpart model
library(rpart)
model <- rpart(V11 ~ ., data = train, method = 'class')
print(model) #not visual enuff, invoke 'rpart.plot' instead.
summary(model)

#plotting the rpart model
install.packages("rpart.plot", dependencies = T)
library(rpart.plot)
rpart.plot(model)

#prediction
pred <- predict(model, test, type = 'class')
pred
table(test[,10], pred)
accuracy<-table(test[,10], pred)
sum(diag(accuracy))/sum(accuracy) #accuracy=79%
