wine_data=read.csv(choose.files(),header=T)
View(wine_data)

colnames(wine_data)=c("Type","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavonoids",
                      "Nonflavanoid.phenols","Proanthocyanins","Color.intensity","Hue","Dilution","Proline")

str(wine_data)
wine_data$Type=as.factor(wine_data$Type)
wine_data$Magnesium=as.numeric(wine_data$Magnesium)
wine_data$Proline=as.numeric(wine_data$Proline)

library(psych)
pairs.panels(wine_data[1:14],gap=0,bg=c("red","green","blue")[wine_data$Type],pch=21)


set.seed(500)
wdata=sample(2,nrow(wine_data),replace=T,prob=c(0.6,0.4))
train_wine_data=wine_data[wdata==1,]
test_wine_data=wine_data[wdata==2,]
dim(train_wine_data)
dim(test_wine_data)

require(MASS)
wine_lda_model=lda(Type~.,train_wine_data)
wine_lda_model

wpredict=predict(wine_lda_model,train_wine_data)
ldahist(data=wpredict$x[,1],g=train_wine_data$Type)
ldahist(data=wpredict$x[,2],g=train_wine_data$Type)


require(klaR)
partimat(Type ~ .,data=train_wine_data,method="lda")


wp1=predict(wine_lda_model,train_wine_data)$class
wtab=table(predicted=wp1,actual=train_wine_data$Type)
wtab
sum(diag(wtab))/sum(wtab)

wine_train_new1 = predict(wine_lda_model,train_wine_data)
wine_train_data_final = cbind(train_wine_data, wine_train_new1)
View(wine_train_data_final)

write.csv(wine_train_data_final,file="wine_train_preddata.csv")



wp2=predict(wine_lda_model,test_wine_data)$class
wtab2=table(predicted=wp2,actual=test_wine_data$Type)
wtab2
sum(diag(wtab2))/sum(wtab2)



wine_test_new1=predict(wine_lda_model,test_wine_data)
wine_test_data_final = cbind(test_wine_data, wine_test_new1)
View(wine_test_data_final)
wine_test_data_final=subset(wine_test_data_final,select=-c(posterior.1,posterior.2,posterior.3,x.LD1,x.LD2))
names(wine_test_data_final)[names(wine_test_data_final) == "class"] = "Predicted_Type"


write.csv(wine_test_data_final,file="wine_test_preddata.csv")


