library("klaR")

allData <- read.table("pima-indians-diabetes.txt", FALSE, ",")

numOfRows = nrow(allData)
trainRows = floor(numOfRows*0.8)
testRows = numOfRows - trainRows

trainData = allData[1:trainRows, ]
testData = allData[(trainRows+1):numOfRows,]

x = trainData[,-9]
y = trainData$V9
y <- as.factor(y)

model <- svmlight(x, y, pathsvm="svm_light")
matrix <- table(predict(model, testData)$class, testData$V9)

print(matrix)

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
