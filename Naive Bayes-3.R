library("caret")
library("klaR")

allData <- read.table("pima-indians-diabetes.txt", FALSE, ",")
dim(allData)
head(allData)

numOfRows = nrow(allData)
trainRows = floor(numOfRows*0.8)
testRows = numOfRows - trainRows

trainData = allData[1:trainRows, ]
testData = allData[(trainRows+1):numOfRows,]

x = trainData[,-9]
y = trainData$V9
y <- as.factor(y)

model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#predict(model$finalModel,x)
matrix <- table(predict(model$finalModel,testData[,1:8])$class,testData$V9)

sum <- 0
diagonalSum <- 0

for(i in 1:2)
{
  for(j in 1:2)
  {
    if(i == j)
    {
      diagonalSum <- diagonalSum + matrix[i, j]
    }
    sum <- sum + matrix[i,j]
  }
}

accuracy <- diagonalSum / sum
print(accuracy)


