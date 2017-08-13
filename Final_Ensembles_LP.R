#Ensembling

#Defining the training controls for multiple models
library(caret)
library(quantreg)
library(e1071)
fitControl <- trainControl(method = "cv",number = 5,savePredictions = 'final',classProbs = T)

#Defining the predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome","CoapplicantIncome")
outcomeName<-'Loan_Status'

#Training the random forest model
model_rf<-train(train[,predictors],train[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)

#Predicting using random forest model
test$pred_rf<-predict(object = model_rf,test[,predictors])

#Submission
sample_submission_rf <- as.data.frame(test$Loan_ID)
sample_submission_rf <- cbind(sample_submission_avg,test$pred_rf)
names(sample_submission_rf) <- c("Loan_ID","Loan_Status")

#Saving the submission
write.csv(sample_submission_rf,"sample_submission_rf.csv",row.names = FALSE)
#Results - 79.1667%

#Training the knn model
model_knn<-train(train[,predictors],train[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Predicting using knn model
test$pred_knn<-predict(object = model_knn,test[,predictors])

#Submission
sample_submission_knn <- as.data.frame(test$Loan_ID)
sample_submission_knn <- cbind(sample_submission_knn,test$pred_knn)
names(sample_submission_knn) <- c("Loan_ID","Loan_Status")

#Saving the submission
write.csv(sample_submission_knn,"sample_submission_knn.csv",row.names = FALSE)

#Results - 64.66%

#Training the Logistic regression model
model_lr<-train(train[,predictors],train[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#Predicting using lr model
test$pred_lr<-predict(object = model_lr,test[,predictors])

#Submission
sample_submission_lr <- as.data.frame(test$Loan_ID)
sample_submission_lr <- cbind(sample_submission_lr,test$pred_lr)
names(sample_submission_lr) <- c("Loan_ID","Loan_Status")

#Saving the submission
write.csv(sample_submission_lr,"sample_submission_lr.csv",row.names = FALSE)
#Results - 79.1667%
#Averaging
#Predicting the probabilities
test$pred_rf_prob<-predict(object = model_rf,test[,predictors],type='prob')
test$pred_knn_prob<-predict(object = model_knn,test[,predictors],type='prob')
test$pred_lr_prob<-predict(object = model_lr,test[,predictors],type='prob')

#Taking average of predictions
test$pred_avg<-(test$pred_rf_prob$Y+test$pred_knn_prob$Y+test$pred_lr_prob$Y)/3

#Splitting into binary classes at 0.5
test$pred_avg<-as.factor(ifelse(test$pred_avg>0.5,'Y','N'))

#Submission
sample_submission_avg <- as.data.frame(test$Loan_ID)
sample_submission_avg <- cbind(sample_submission_avg,test$pred_avg)
names(sample_submission_avg) <- c("Loan_ID","Loan_Status")

#Saving the submission
write.csv(sample_submission_avg,"sample_submission_avg.csv",row.names = FALSE)
#Results - 79.1667%

#Majority Voting
#The majority vote
test$pred_majority<-as.factor(ifelse(test$pred_rf=='Y' & test$pred_knn=='Y','Y',ifelse(test$pred_rf=='Y' & test$pred_lr=='Y','Y',ifelse(test$pred_knn=='Y' & test$pred_lr=='Y','Y','N'))))

#Submission
sample_submission_maj <- as.data.frame(test$Loan_ID)
sample_submission_maj <- cbind(sample_submission_avg,test$pred_majority)
names(sample_submission_maj) <- c("Loan_ID","Loan_Status")

#Saving the submission
write.csv(sample_submission_maj,"sample_submission_maj.csv",row.names = FALSE)
#Results - 79.1667%

#Weighted Average
#Taking weighted average of predictions
test$pred_weighted_avg<-(test$pred_rf_prob$Y*0.25)+(test$pred_knn_prob$Y*0.25)+(test$pred_lr_prob$Y*0.5)

#Splitting into binary classes at 0.5
test$pred_weighted_avg<-as.factor(ifelse(test$pred_weighted_avg>0.5,'Y','N'))

#Submission
sample_submission_weg <- as.data.frame(test$Loan_ID)
sample_submission_weg <- cbind(sample_submission_weg,test$pred_weighted_avg)
names(sample_submission_weg) <- c("Loan_ID","Loan_Status")

#Saving the submission
write.csv(sample_submission_weg,"sample_submission_weg.csv",row.names = FALSE)
#Results - 79.1667%