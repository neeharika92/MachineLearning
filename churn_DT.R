churn_data=read.csv(choose.files(),header = T)
View(churn_data)
str(churn_data)
churn_data = subset(churn_data, select = -c(customerID))

set.seed(500)
cd=sample(2,nrow(churn_data),replace=T,prob=c(0.6,0.4))
train_churn=churn_data[cd==1,]
test_churn=churn_data[cd==2,]
dim(train_churn)
dim(test_churn)


churn_tree_model=ctree(Churn~.,data=train_churn,controls = ctree_control(mincriterion = 0.9,minsplit = 500))
plot(churn_tree_model)
c=predict(churn_tree_model,train_churn)
tab=table(predicted=c,actual=train_churn$Churn)
tab
1-sum(diag(tab))/sum(tab)

install.packages("randomForest")
require(randomForest)
set.seed(500)
rf_model=randomForest(Churn~.,data=train_churn)
print(rf_model)

require(caret)
p1=predict(rf_model,train_churn)
confusionMatrix(p1,train_churn$Churn)


p2=predict(rf_model,test_churn)
confusionMatrix(p2,test_churn$Churn)

plot(rf_model)

t=tuneRF(train_churn[,-20],train_churn[,20],stepFactor = 0.5,plot=T,ntreeTry = 500,trace=T,improve = 0.05)

rf_model=randomForest(Churn~.,data=train_churn,ntree=500,mtry=8,importance=T,proximity=T)
print(rf_model)


