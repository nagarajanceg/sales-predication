setwd('/Users/nagaraj/Documents/study/codes/R/proj/project 2/')
pdInfo <- read.csv("product_distribution_training_set.txt",header = FALSE,sep = "\t")
library('Matrix')
library("forecast")
library("tseries")
daySum <- colSums(pdInfo[,-1])

year <- 365.25
givenDays <- 118
toPredict <- 29
freq = year/(givenDays/toPredict)

#Determine the best model to predict time series
EvaluteModel <- function(frameData, start , end, pos, trend, regFactor){
  timeSeries <- ts(ts(data.frame(frameData[c(start:end),pos])), frequency = 1)
  # Extrenal regressor
  regressor <- fourier(ts(ts(data.frame(frameData[c(start:end), pos])),frequency =freq ),K=4)
  #neural network with size
  neuralResult <- nnetar(timeSeries,size = 29, xreg = regressor)
  #arima Model with auto order selection
  arimaResult <- auto.arima(timeSeries,xreg = regressor, seasonal = trend, allowdrift=TRUE)
  
  if(regFactor){
    regressor1 <- fourier(ts(ts(data.frame(frameData[c(start:end), pos])),frequency =freq ),K=4,h=29)
    arimaFit <- forecast(arimaResult, xreg= regressor1, h=29)
    neuralFit <- forecast(neuralResult, xreg= regressor1, h=29)
    # plot(neuralFit, main="forecast plot")
  }else{
    # Forecast future values using ARIMA and Neural
    arimaFit <- forecast(arimaResult, xreg= regressor, h=29) 
    neuralFit <-forecast(neuralResult, xreg= regressor, h=29)   
  }
  arimaAccuarcy <- accuracy(arimaFit)
  neuralAccuarcy <- accuracy(neuralFit)
  #choosing the best accuracy model
  if(arimaAccuarcy[1,2] > neuralAccuarcy[1,2]){
    return(neuralFit)
  }else{
    return(arimaFit)
  }
}
abmatrix <- data.matrix(daySum, rownames.force = NA)
#Choosing a model
modelList<- EvaluteModel(abmatrix,1,nrow(abmatrix),1,TRUE,FALSE)

# plotting the graph
#plot(modelList)

#calculating mean value for the chosen 
modelParameterEstimate <- function(predictedModel, condition){
  value <- predictedModel$mean
  if(condition){
    value[value < 0] <- predictedModel$upper[,1]  
  }
  value[value < 0] <- 0
  value <- as.numeric(round(value))
  value
}
# plot(overAllPrediction)
pervalue <- modelParameterEstimate(modelList, TRUE)
cmatrix <- matrix(c(0,pervalue),1,119)
resultMatrix <- matrix(0,30,101)
tcmatrix <- t(cmatrix) 
resultMatrix[,1] <- tcmatrix[c(1:30),]


prdocutMatrix <- t(data.matrix(pdInfo,rownames.force = NA))
resultMatrix[1,-(1:1)] <- prdocutMatrix[1,]

# Model selection and prediction for 100 key products
for(i in 1:100){
  fitted <- EvaluteModel(prdocutMatrix,2,nrow(prdocutMatrix),i,FALSE,TRUE)
  pervalue <- modelParameterEstimate(fitted,FALSE)
  prevalueMatrix <- as.matrix(pervalue)
  resultMatrix[-(1:1),1+i] <- prevalueMatrix[,1]
}
outMatrix <- t(resultMatrix)
op <- outMatrix
write.table(op,file="output.txt",sep = "\t",quote = F,row.names = F,col.names = F)
