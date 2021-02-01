Bank_data=read.csv(choose.files(),header=T)
dim(Bank_data)
View(Bank_data)
# missing entries per column
colSums(is.na(Bank_data)) 
summary(Bank_data)
str(Bank_data)

#column manipulation
Bank_data$Customer.ID=NULL
Bank_data$Credit_Amount=as.numeric(Bank_data$Credit_Amount)
Bank_data$Gender=as.factor(Bank_data$Gender)
Bank_data$Academic_Qualification=as.factor(Bank_data$Academic_Qualification)
Bank_data$Marital=as.factor(Bank_data$Marital)
Bank_data$Repayment_Status_Jan=as.factor(Bank_data$Repayment_Status_Jan)
Bank_data$Repayment_Status_Feb=as.factor(Bank_data$Repayment_Status_Feb)
Bank_data$Repayment_Status_March=as.factor(Bank_data$Repayment_Status_March)
Bank_data$Repayment_Status_April=as.factor(Bank_data$Repayment_Status_April)
Bank_data$Repayment_Status_May=as.factor(Bank_data$Repayment_Status_May)
Bank_data$Repayment_Status_June=as.factor(Bank_data$Repayment_Status_June)
Bank_data$Default_Payment=as.factor(Bank_data$Default_Payment)


#divide data in training and testing (hold out : 75 (train),25(test))
require(caret)
set.seed(500)
x=createDataPartition(Bank_data$Gender,p=0.75,list=F)
Bank_train=Bank_data[x,]
Bank_test=Bank_data[-x,]
dim(Bank_train)
dim(Bank_test)


#Regression
Bank_model=glm(Default_Payment ~.,data=Bank_train,family="binomial")
summary(Bank_model)

#fetch only significant vars
null_bankmodel=glm(Default_Payment ~1,data=Bank_train,family="binomial")

step(null_bankmodel,direction="forward",scope =list(lower=null_bankmodel,upper=Bank_model))

step_bankmodel=glm(formula = Default_Payment ~ Repayment_Status_Jan + Repayment_Status_March + 
                     Credit_Amount + Repayment_Status_May + Previous_Payment_Jan + 
                     Marital + Academic_Qualification + Repayment_Status_June + 
                     Previous_Payment_Feb + Gender + Repayment_Status_April + 
                     Repayment_Status_Feb + Previous_Payment_May + Feb_Bill_Amount + 
                     Previous_Payment_April + Jan_Bill_Amount + Age_Years, family = "binomial", 
                   data = Bank_train)
summary(step_bankmodel)

#find probabilities
Bank_train$pred_prob=predict(step_bankmodel,Bank_train,type="response")
head(Bank_train)

#cutoff identification
require(ROCR)
pred_var=prediction(Bank_train$pred_prob,Bank_train$Default_Payment)
perf_var=performance(pred_var,"auc")
perf_var1=performance(pred_var,"tpr","tnr")
plot(perf_var1,colorize=T,print.cutoffs.at=seq(0.1,by=0.1)) 


Bank_train$pred_Default_Payment=ifelse(Bank_train$pred_prob>0.2,"1","0")
View(Bank_train)

table(pred=Bank_train$pred_Default_Payment,actual=Bank_train$Default_Payment)

#overall accuracy:(14254+2962)/nrow(Bank_train)


#find probabilities on test data
Bank_test$pred_prob_test=predict(step_bankmodel,Bank_test,type="response")
pred_var_test=prediction(Bank_test$pred_prob_test,Bank_test$Default_Payment)
perf_var1_test=performance(pred_var_test,"tpr","tnr")
plot(perf_var1_test,colorize=T,print.cutoffs.at=seq(0.1,by=0.1)) 


Bank_test$pred_Default_Payment=ifelse(Bank_test$pred_prob_test>0.2,"1","0")
table(pred=Bank_test$pred_Default_Payment,actual=Bank_test$Default_Payment)

Bank_test1=Bank_test
Bank_test = select(Bank_test1,-(ends_with("prob_test")))
View(Bank_test)

write.csv(Bank_train,file="Bankcredittraindata_pred.csv")
write.csv(Bank_test,file="Bankcredittestdata_pred.csv")
