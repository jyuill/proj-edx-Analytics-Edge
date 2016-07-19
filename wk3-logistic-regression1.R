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

### Threshold
## set threshold over which prediction is determined
## if no preference use t=0.5
## threshold examples using confusion tables
## sensitivity = TP / (TP + FN) True positive rate
## specificity = TN / (TN + FP) True negative rate

table(qualityTrain$PoorCare,predictTrain>0.5)
## sensitivity - True pos: (based on numbers in confusion matrix)
10/25
## specificity - True neg:
70/74
## conclusion: much more accurate at identifying negatives, not so good at positives
## try again with higher threshold
table(qualityTrain$PoorCare,predictTrain>0.7)
## sensitivity - True pos rate:
8/25
## specificity - True neg rate:
73/74
## conclusion: even better at identifying negatives, worse with positives
## try again with lower threshold
table(qualityTrain$PoorCare,predictTrain>0.2)
## sensitivity - True pos rate:
16/25
## specificity -True neg rate:
54/74
## conclusion: still leans toward negatives, but more balanced

### ROC CURVES to determine where to set threshold, based on priorities
library(ROCR)
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
## chart above shows combinations of True positive rate and False positive rate
## at different thresholds - but doesn't tell you threshold values
plot(ROCRperf,colorize=TRUE)
## chart above associates colors in line with different threshold values
plot(ROCRperf,colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
## using chart above can determine threshold to use, depending on
## preferences as decision-maker