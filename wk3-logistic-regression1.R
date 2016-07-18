library(caTools)

quality <- read.csv("datasets/quality.csv")
str(quality)
table(quality$PoorCare)

## split data into training and testing sets
set.seed(88)
split <- sample.split(quality$PoorCare,SplitRatio = 0.75)
split ## true or false values identifying if should be in training set or not

qualityTrain <- subset(quality,split==TRUE)
qualityTest <- subset(quality,split==FALSE)
nrow(qualityTrain)
nrow(qualityTest)

## build logistic regression model
qualitylog <- glm(PoorCare ~ OfficeVisits+Narcotics,data=qualityTrain,family=binomial)
summary(qualitylog) ## output from value: estimates are positive and asterisks indicate significance
## make prediction
predictTrain = predict(qualitylog,type="response")
summary(predictTrain)
## check predictions vs true outcomes
tapply(predictTrain,qualityTrain$PoorCare,mean)
## interpretation:
## - for all true poor care cases (1), we predict ave. probability of ~0.44
## - for all true good care cases (0), we predict ave. probability of ~0.19
## - good because we're predicting higher probabily for actual poor care cases


## threshold examples using confusion tables
