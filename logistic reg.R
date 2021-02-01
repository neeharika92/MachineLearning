setwd(choose.dir())
require (dplyr)
require(tidyr)
rg_train= read.csv('rg_train.csv',stringsAsFactors = F)
rg_test= read.csv('rg_test.csv',stringsAsFactors = F)
dim(rg_test)
dim(rg_train)
names(rg_test)
names(rg_train)
summary(rg_test)
View(rg_test)
View(rg_train)
require(caret)
require(reshape2)
require(dummies)
require(VIM)
require(scales)
require(psych)
setdiff(names(rg_train),names(rg_test))
unique(rg_train$Revenue.Grid)
rg_test$Revenue.Grid = "3"
dim(rg_train)[2]==dim(rg_test)[2]
table(colnames(rg_train) == colnames(rg_test))


all_file = rbind(rg_train,rg_test)
View(all_file)
all_file =select(all_file,-"REF_NO")
all_file$children = gsub("Zero","0",all_file$children)
unique(all_file$children)
str(all_file)
all_file$children = gsub("\\+","",all_file$children)
all_file$age_band = gsub("Unknown","NA",all_file$age_band)
table((all_file$age_band))
all_file$age_band = gsub("\\+","-75",all_file$age_band)
temp = colsplit(all_file$age_band,pattern = "-", names = c("age_min","age_max"))
View(temp)
all_file = cbind(all_file,temp)
str(all_file$age_min)
all_file$Avg_age = ((all_file$age_min + all_file$age_max)/2)
rm(temp)
all_file= select(all_file, -c("age_min","age_max"))
all_file= select(all_file, -"age_band")



all_file$occupation = gsub("Unknown","NA",all_file$occupation)
all_file$occupation_partner = gsub("Unknown","NA",all_file$occupation_partner)
table(all_file$family_income)
all_file$family_income = gsub("Unknown","NA",all_file$family_income)
all_file$family_income = gsub(", >=","-",all_file$family_income)
all_file$family_income = gsub(">=","",all_file$family_income)
all_file$family_income = gsub("<","",all_file$family_income)
all_file$family_income = gsub(",","",all_file$family_income)
temp = colsplit(all_file$family_income,pattern = "-", names = c("inc_max","inc_min"))
View(temp)
all_file = cbind(all_file,temp)
str(all_file$inc_max)
all_file$inc_min[is.na(all_file$inc_min)] = all_file$inc_max[is.na(all_file$inc_min)]
all_file$inc_max[is.na(all_file$inc_max)] = all_file$inc_min[is.na(all_file$inc_max)]
all_file$Avg_fam_income = ((all_file$inc_min + all_file$inc_max)/2)
rm(temp)
all_file= select(all_file, -c("inc_min","inc_max")) 
unique(all_file$home_status)
str(all_file)
summary(all_file)
summarise(all_file)
table(all_file$self_employed)


allfile_test=all_file
allfile_test=as.data.frame(lapply(allfile_test, function(x){gsub("Unknown",NA,x)}))
allfile_test=as.data.frame(lapply(allfile_test, function(x){gsub("NA",NA,x)}))

View(allfile_test)  
  
  #str(allfile)

allfile_test$children=as.integer(allfile_test$children)
allfile_test$status=as.factor(allfile_test$status)
allfile_test$occupation=as.factor(allfile_test$occupation)
allfile_test$occupation_partner=as.factor(allfile_test$occupation_partner)
allfile_test$year_last_moved=as.integer(allfile_test$year_last_moved)
allfile_test$home_status=as.factor(allfile_test$home_status)
allfile_test$TVarea=as.factor(allfile_test$TVarea)
allfile_test$post_area=as.factor(allfile_test$post_area)
allfile_test$region=as.factor(allfile_test$region)
allfile_test$self_employed=as.factor(allfile_test$self_employed)
allfile_test$self_employed_partner=as.factor(allfile_test$self_employed_partner)
allfile_test$year_last_moved=as.factor(allfile_test$year_last_moved)
str(allfile_test)


table(is.na(allfile_test))


allfile_test = kNN(allfile_test,variable = c(colnames(allfile_test)),k=5)
allfile_test = select(allfile_test, -(ends_with("_imp")))
