setwd("C:/Users/hp/documents")
inputData <- read.table("adult.data", sep = ",", header = FALSE)
getwd()
View(inputData)
str(inputData)
colnames(inputData) <- c("age", "workclass", "fnlwgt", 
                        "education", "educationnum", 
                        "maritalstatus", "occupation",
                        "relationship", "race", "sex", 
                        "capitalgain", "capitalloss", 
                        "hoursperweek", "nativecountry", "income")
str(inputData, vec.len = 2, strict.width = "no", width = 30)
levels_factors <- function(mydata) {
  col_names <- names(mydata)
  for (i in 1:length(col_names)) {
    if (is.factor(mydata[, col_names[i]])) {
      message(noquote(paste("Covariate ", "*", 
                            col_names[i], "*", 
                            " with factor levels:", 
                            sep = "")))
      print(levels(mydata[, col_names[i]]))
    }
  }
}

levels_factors(inputData)
###CLEANING THE DATA##
inputData <- read.table("adult.data",
                       sep = ",", 
                       header = FALSE, 
                       na.strings = " ?")


colnames(inputData) <- c("age", "workclass", "fnlwgt", "education", 
                        "educationnum", "maritalstatus", "occupation",
                        "relationship", "race", "sex", "capitalgain", 
                        "capitalloss", "hoursperweek", "nativecountry", "income")
inputData <- na.omit(inputData)
row.names(inputData) <- 1:nrow(inputData)
View(inputData)
str(inputData)
inputData$ABOVE50K <- NA
inputData$ABOVE50K<-ifelse(as.factor(inputData$income) ==" >50K",1,0)
as.factor(inputData$ABOVE50K)
table(inputData$ABOVE50K)
head(inputData)
######CHECK FOR CLASS BIAS#####
table(inputData$ABOVE50K)
#####Create Training Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 
######COMPUTE INFORMATIONAL VALUE
library(blob)
library(smbinning)
library(sqldf)
library(Formula)
library(mvtnorm)
library(libcoin)
library(grid)
library(partykit)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
for(factor_var in factor_vars){
  inputData[[factor_var]] <- WOE(X=inputData[, factor_var], Y=inputData$ABOVE50K)
}
head(inputData)
# segregate continuous and factor variables
factor_vars <- c ("workclass", "education", "maritalstatus", "occupation", "relationship", "race", "sex", "nativecountry")
continuous_vars <- c("age", "fnlwgt","educationnum", "hoursperweek", "capitalgain", "capitalloss")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df
str(inputData)
##inputData$ABOVE50K <- factor(inputData$ABOVE50K)
str(inputData)
###building the model##
logitMod <- glm(ABOVE50K ~ relationship + age + capitalgain + occupation + educationnum, data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores
library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
optCutOff

   summary(logitMod)
library(VIF)
vif(logitMod)
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)
plotROC(testData$ABOVE50K, predicted)
Concordance(testData$ABOVE50K, predicted)
sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)
confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)
