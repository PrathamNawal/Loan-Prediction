set.seed(101)
a <- as.data.frame(train$Loan_Status)
train[,1:13] <- data1[1:614,]
test[,1:13] <- data1[615:981,]
train <- cbind(train,a)
train = train[,c(-13,-14)]
b <- as.data.frame(rain$Loan_Status)
names(b) <- c("Loan_Status")
train <- cbind(train,b)
names(train[c(13,14)]) <- c("Total_Income","Loan_Status")
class(train$Loan_Status)
train$Loan_Status = as.factor(train$Loan_Status)
#Modelling

#Decision-Tree
library(C50)
modelDT = C5.0(`Loan_Status`~ `Gender`+`Married`+`Dependents`+`Education`+`Self_Employed`+`ApplicantIncome`+`CoapplicantIncome`+`LoanAmount`+`Loan_Amount_Term`+`Credit_History`+`Property_Area`, data = train)
summary(modelDT)

#Prediction
predictDT <- as.data.frame(predict(modelDT,test,type = "class"))

#Submission
sample_submissionDT <- cbind(test$Loan_ID,predictDT)
names(sample_submissionDT) <- c("Loan_ID","Loan_Status")

#Saving the results
write.csv(sample_submissionDT,"sample_submissionDT.csv",row.names = FALSE)

#Results - 77.0834%

#Naive-Bayes
library(e1071)
modelNB <- naiveBayes(`Loan_Status`~ `Gender`+`Married`+`Dependents`+`Education`+`Self_Employed`+`ApplicantIncome`+`CoapplicantIncome`+`LoanAmount`+`Loan_Amount_Term`+`Credit_History`+`Property_Area`,train)

#Prediction
predictNB <- as.data.frame(predict(modelNB,test))

#Submission
sample_submissionNB <- cbind(test$Loan_ID,predictNB)
names(sample_submissionNB) <- c("Loan_ID","Loan_Status")

#Saving the results
write.csv(sample_submissionNB,"sample_submissionNB.csv",row.names = FALSE)

#Results - 76.389%

#SVM
library(e1071)
modelSVM  <- svm(`Loan_Status`~ `Gender`+`Married`+`Dependents`+`Education`+`Self_Employed`+`ApplicantIncome`+`CoapplicantIncome`+`LoanAmount`+`Loan_Amount_Term`+`Credit_History`+`Property_Area`, data = train, kernel = "radial", gamma = 0.001, cost = 10) 
summary(modelSVM)
tuned <- tune.svm(`Loan_Status`~ `Gender`+`Married`+`Dependents`+`Education`+`Self_Employed`+`ApplicantIncome`+`CoapplicantIncome`+`LoanAmount`+`Loan_Amount_Term`+`Credit_History`+`Property_Area`, data = train, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)

#Prediction
predictSVM <- as.data.frame(predict(modelSVM,test))

#Submission
sample_submissionSVM <- cbind(test$Loan_ID,predictSVM)
names(sample_submissionSVM) <- c("Loan_ID","Loan_Status")

#Saving the results
write.csv(sample_submissionSVM,"sample_submissionSVM.csv",row.names = FALSE)

#Results- 79.1667%

#knn
library(knn)
modelKNN <- knn(`Loan_Status` ~ `Gender`+`Married`+`Dependents`+`Education`+`Self_Employed`+`ApplicantIncome`+`CoapplicantIncome`+`LoanAmount`+`Loan_Amount_Term`+`Credit_History`+`Property_Area`,train)

#Random-Forest
library(randomForest)
modelRF <- randomForest(`Loan_Status` ~ `Gender`+`Married`+`Dependents`+`Education`+`Self_Employed`+`ApplicantIncome`+`CoapplicantIncome`+`LoanAmount`+`Loan_Amount_Term`+`Credit_History`+`Property_Area`,train)
