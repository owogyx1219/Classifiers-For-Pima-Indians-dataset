allData <- read.table("pima-indians-diabetes.txt", FALSE, ",")

numOfRows = nrow(allData)
trainRows = floor(numOfRows*0.8)
testRows = numOfRows - trainRows

allData[, 3][allData[, 3] == 0] <- NA
allData[, 4][allData[, 4] == 0] <- NA
allData[, 6][allData[, 6] == 0] <- NA
allData[, 8][allData[, 8] == 0] <- NA

data = allData[1:trainRows, ]
testData = allData[(trainRows+1):numOfRows, ]


meanTable = mean(data[[1]])
varianceTable = sd(data[[1]])

num_features <- ncol(data) - 1

class0Data <- subset(data, data$V9==0)
class1Data <- subset(data, data$V9==1)

for (i in 1:num_features){
  if(i==1)
  {
    mean_first = mean(class0Data[[i]], na.rm = TRUE)
    variance_first = sd(class0Data[[i]], na.rm = TRUE)
    conprobTable_Class0 = c(mean_first, variance_first) 
  }
  else
  {
    mean =  mean(class0Data[[i]], na.rm = TRUE)
    variance = sd(class0Data[[i]], na.rm = TRUE)
    newRow = c(mean, variance)
    conprobTable_Class0 <- rbind(conprobTable_Class0, newRow)
  }
}

for (i in 1:num_features){
  if(i==1)
  {
    mean_first = mean(class1Data[[i]], na.rm = TRUE)
    variance_first = sd(class1Data[[i]], na.rm = TRUE)
    conprobTable_Class1 = c(mean_first, variance_first) 
  }
  else
  {
    mean =  mean(class1Data[[i]], na.rm = TRUE)
    variance = sd(class1Data[[i]], na.rm = TRUE)
    newRow = c(mean, variance)
    conprobTable_Class1 <- rbind(conprobTable_Class1, newRow)
  }
}

prior_class0 <- nrow(class0Data) / (nrow(class0Data) + nrow(class1Data))
prior_class1 <- nrow(class1Data) / (nrow(class0Data) + nrow(class1Data))
numOfRightData <- 0
numOfTotalData <- testRows
numofValidData <- 0

for (rowIndex in 1:numOfTotalData){
  
  probSum_class0 <- 0.0
  probSum_class1 <- 0.0
  
  if(!is.na(testData[rowIndex, 2]) & !is.na(testData[rowIndex, 4]) & !is.na(testData[rowIndex, 6]) & !is.na(testData[rowIndex, 8]))
  {
    numofValidData <- numofValidData + 1
    
    for(featureIndex in 1:num_features){
      probSum_class0 = probSum_class0 + log2(dnorm(testData[rowIndex, featureIndex], conprobTable_Class0[featureIndex, 1], conprobTable_Class0[featureIndex, 2]))
      probSum_class1 = probSum_class1 + log2(dnorm(testData[rowIndex, featureIndex], conprobTable_Class1[featureIndex, 1], conprobTable_Class1[featureIndex, 2]))
    }
    
    probSum_class0 = probSum_class0 + log2(prior_class0)
    probSum_class1 = probSum_class1 + log2(prior_class1)
    
    if(probSum_class0 > probSum_class1)
    {
      if(testData[rowIndex, 9] == 0)
      {
        numOfRightData <- numOfRightData + 1
      }
    }
    else
    {
      if(testData[rowIndex, 9] == 1)
      {
        numOfRightData <- numOfRightData + 1
      }
    }
  }
  
}


print("Result of Accuracy is" )
print(numOfRightData/numofValidData)
