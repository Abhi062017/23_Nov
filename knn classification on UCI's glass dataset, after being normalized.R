#knn classification on UCI's glass dataset
getwd()

#read the dataset
glass <- read.csv('glass.csv', header = F, stringsAsFactors = F)
str(glass) #V1 is ID no., which is unnecessary, V11 is response variable
glass <- glass[,-1] #ridding of the unnecessary variable
glass$V11 <- as.factor(glass$V11) #casting the response as factors
unique(glass$V11)
View(glass)
sum(is.na(glass)) #No NA's
summary(glass) #notice the wide range in variables, hence need to normalize

#normalizing variables
normalize <- function(y) {(y-min(y))/(max(y)- min(y))}
normalized.glass <- as.data.frame(lapply(glass[,-10], normalize)) #10 is response variable
summary(normalized.glass)
View(normalized.glass)

#cbind the response variable back to the normalized dataset
normalized.glass <- cbind(normalized.glass, glass$V11)
View(normalized.glass)
summary(normalized.glass)
colnames(normalized.glass)[10] <- 'V11' #changing the variable name back.

#randomize to enable stratified split
tail(normalized.glass)
random.glass <- normalized.glass[order(runif(nrow(normalized.glass))),]
View(random.glass)
summary(random.glass)
summary(normalized.glass)

#splitting 80/20
nrow(random.glass) #214 rows
0.8*214 #80% dataset
train <- random.glass[1:171,]
test <- random.glass[172:214,]

train.labels <- train[,10] #class labels for knn model
test.labels <- test[,10]

#Building a knn Model
library(class)
model <- knn(train = train, test = test, cl = train.labels, k = sqrt(nrow(random.glass)))
summary(model)
table(test.values=test[,10], model.prediction=model)

#checking accuracy
accuracy.data <- table(test.values=test[,10], model.prediction=model)
sum(diag(accuracy.data))/sum(accuracy.data) #accuracy = 97%
