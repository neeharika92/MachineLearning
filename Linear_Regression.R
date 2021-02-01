#Load all packages required
packages = c("caret", "dplyr", "reshape2", "dummies", "VIM", "scales", "psych")
load_packages = lapply(packages, require, character.only = TRUE)
rm(packages)

setwd("G:\\Techmate_MG\\Imarticus\\Linear Regression") # give path for your working directory

train_tj = read.csv("loan_data_train.csv", stringsAsFactors = FALSE)
test_tj = read.csv("loan_data_test.csv", stringsAsFactors = FALSE)

str(train_tj)
str(test_tj)

#check if the dimensions are correct or not
ncol(test_tj) == ncol(train_tj)
dim(test_tj) [2] == dim(train_tj) [2]

#removing the extra column, we will predict this for test file
train_tj = select(train_tj,-"Interest.Rate")

#add extra column to both the data sets so it will be easy to split after combining
train_tj$data = "Train"
test_tj$data = "Test"

myfile = rbind(train_tj,test_tj)

#removing unwanted columns
myfile = select(myfile,-"ID")

#replacing unwanted characters
myfile$Debt.To.Income.Ratio = gsub("%","",myfile$Debt.To.Income.Ratio)
myfile$Employment.Length = gsub("years","",myfile$Employment.Length)
myfile$Employment.Length = gsub("year","",myfile$Employment.Length)
myfile$Employment.Length = gsub("<","",myfile$Employment.Length)
myfile$Employment.Length = gsub(" ","",myfile$Employment.Length)
myfile$Employment.Length = gsub("n/a","NA",myfile$Employment.Length)
myfile$Employment.Length = gsub("\\+","",myfile$Employment.Length)
myfile$Loan.Length =gsub(" months","",myfile$Loan.Length)
myfile$State = gsub(".","CA",myfile$State, fixed = TRUE)

#changing the data types
myfile$Amount.Requested = as.numeric(myfile$Amount.Requested)
myfile$Amount.Funded.By.Investors = as.numeric(myfile$Amount.Funded.By.Investors)
myfile$Loan.Length = as.numeric(myfile$Loan.Length)
myfile$Debt.To.Income.Ratio = as.numeric(myfile$Debt.To.Income.Ratio)
myfile$Monthly.Income = as.numeric(myfile$Monthly.Income)
myfile$Open.CREDIT.Lines = as.numeric(myfile$Open.CREDIT.Lines)
myfile$Revolving.CREDIT.Balance = as.numeric(myfile$Revolving.CREDIT.Balance)
myfile$Employment.Length = as.numeric(myfile$Employment.Length)

myfile$Loan.Purpose =as.factor(myfile$Loan.Purpose)
myfile$State =as.factor(myfile$State)
myfile$Home.Ownership =as.factor(myfile$Home.Ownership)
myfile$data =as.factor(myfile$data)

#Spliting the Fico range column as Min & Max 
temp = colsplit(myfile$FICO.Range,pattern = "-", names = c("Min","Max"))
myfile = cbind(myfile,temp)
myfile$AvgFico = ((myfile$Min + myfile$Max)/2)
myfile = select(myfile,-c(FICO.Range,Min,Max))
rm(temp)

#K-Nearest Neighbour (kNN)
table(is.na(myfile)) #You get True & False
table(myfile$State) #before kNN (CA is 433)

myfile = kNN(myfile,variable = c(colnames(myfile)),k=7)

table(is.na(myfile)) #You get only False which means NAs are predicted
table(myfile$State) #after kNN (CA is 434)

View(myfile)

#deleting additional columns that are created
myfile = select(myfile,-(ends_with("_imp"))) 

#dealing with outliers
boxplot(myfile, main = "outliers")
boxplot(myfile$Amount.Requested, main = "Amount.Requested")
boxplot(myfile$Amount.Funded.By.Investors, main = "Amount.Funded.By.Investors")
boxplot(myfile$Loan.Length, main = "Loan.Length")
boxplot(myfile$Monthly.Income, main = "Monthly.Income")
boxplot(myfile$Debt.To.Income.Ratio, main = "Debt.To.Income.Ratio")
boxplot(myfile$Open.CREDIT.Lines, main = "Open.CREDIT.Lines")
boxplot(myfile$Revolving.CREDIT.Balance, main = "Revolving.CREDIT.Balance")
boxplot(myfile$Inquiries.in.the.Last.6.Months, main = "Inquiries.in.the.Last.6.Months")
boxplot(myfile$Employment.Length, main = "Employment.Length")
boxplot(myfile$AvgFico, main = "AvgFico")

myfile$Amount.Requested = squish(myfile$Amount.Requested, quantile(myfile$Amount.Requested, c(.05, .95)))
myfile$Amount.Funded.By.Investors = squish(myfile$Amount.Funded.By.Investors, quantile(myfile$Amount.Funded.By.Investors, c(.05, .95)))
myfile$Monthly.Income = squish(myfile$Monthly.Income, quantile(myfile$Monthly.Income, c(.05, .95)))
myfile$Open.CREDIT.Lines = squish(myfile$Open.CREDIT.Lines, quantile(myfile$Open.CREDIT.Lines, c(.05, .95)))
myfile$Revolving.CREDIT.Balance = squish(myfile$Revolving.CREDIT.Balance, quantile(myfile$Revolving.CREDIT.Balance, c(.05, .95)))
myfile$Inquiries.in.the.Last.6.Months = squish(myfile$Inquiries.in.the.Last.6.Months, quantile(myfile$Inquiries.in.the.Last.6.Months, c(.05, .95)))
myfile$AvgFico = squish(myfile$AvgFico, quantile(myfile$AvgFico, c(.05, .95)))

#Center and Scale the value so that they do not influence much
preprocvalues = preProcess(myfile, method = c("center","scale"))
myfile = predict(preprocvalues, myfile)
View(myfile)

#Creating Dummy Variables
myfile_dummies = dummyVars(~., data = myfile, levelsOnly = TRUE)
myfile = predict(myfile_dummies, myfile)
myfile = as.data.frame(myfile)
rm(myfile_dummies)
View(myfile)

#Split Data into 2 files and add interest rate column in Train file
test_file1 = filter(myfile, Test == 1)
test_file1 = select(test_file1, -c("Test","Train"))

train_file1 =filter(myfile, Train == 1)
train_file1 = select(train_file1, -c("Test","Train"))

train_file_raw = read.csv("loan_data_train.csv", stringsAsFactors = FALSE)
train_file_raw$Interest.Rate = as.numeric(gsub("%","", train_file_raw$Interest.Rate))
Interest.Rate = train_file_raw$Interest.Rate
train_file2 = cbind(train_file1,Interest.Rate)

#correlation and multicollinearity
require(ggplot2)
require(GGally)
colnames(train_file2)
ggpairs(data=train_file2, columns=c(1:3,70:76), title="Loan data")
correlation_matrix = round(cor(train_file2[,c(1:3,70:76)]),2)
View(correlation_matrix)

#Amount funded by investors and amount requested has high correlation so remove one
test_file1.1 = subset(test_file1, select = -(Amount.Funded.By.Investors))
train_file1.1 = subset(train_file2, select = -(Amount.Funded.By.Investors))
View(train_file1.1)

#now check correlation again
ggpairs(data=train_file1.1, columns=c(1:2,70:75), title="Loan data")
correlation_matrix<-round(cor(train_file1.1[,c(1:2,70:75)]),2)
View(correlation_matrix)

model=lm(Interest.Rate~., data=train_file1.1[,c(2,3,69,71,72,73,74,75)])
require(car)
vif(model)

#All Good now lets run linear regression
linearMod = lm(Interest.Rate ~ ., data=train_file1.1)
summary(linearMod)

#Lets use the significant data only for prediction
colnames(train_file1.1)
train_file2.2 = train_file1.1[,c(1,2,11,12,69,70,72,74,75)]
View(train_file2.2)

linearMod2 = lm(Interest.Rate ~ ., data=train_file2.2)
summary(linearMod2)
coef(linearMod2)

#Lets Forecast now
pred_model = predict(linearMod2, test_file1.1) 
pred_model = (round((as.data.frame(pred_model)),2))

#combine the original test file with pred_model having predicted interest rates
nrow(pred_model)
nrow(test_file1.1)
test_final = read.csv("loan_data_test.csv", stringsAsFactors = FALSE)
test_final = cbind(test_final,pred_model)
names(test_final)[names(test_final) == "pred_model"] = "Predicted_Interest_Rate"
test_final$Predicted_Interest_Rate = paste(test_final$Predicted_Interest_Rate,"%")
View(test_final)

#write in CSV if required
write.csv(test_final, file = "load_data_predicted.csv", row.names = F)