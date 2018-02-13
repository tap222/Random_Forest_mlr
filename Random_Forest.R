rm(list =ls());gc()

#Setting of working directory
path <- "/home/tapas/Bhaskar/"
setwd(path)

#load libraries and data
library(data.table) #To read the text file and show them in table format
library(mlr) #used for imputing missing values and modelling

#For reading text file headers should be set by runing command seperately
#set variable names
setcol <- c("age",
            "workclass",
            "fnlwgt",
            "education",
            "education-num",
            "marital-status",
            "occupation",
            "relationship",
            "race",
            "sex",
            "capital-gain",
            "capital-loss",
            "hours-per-week",
            "native-country",
            "target")

#load data
train <- read.table("adultdata.txt",header = F,sep = ",",col.names = setcol,na.strings = c(" ?"),stringsAsFactors = F)

#After we've loaded the data set, first we'll set the data class to data.table. 
#data.table is the most powerful R package made for faster data manipulation.
setDT(train)


#Data Sanity/Data dimension and structure 
dim(train) #32561 X 15
str(train)


#check missing values
table(is.na(train)) #total number of missing values in train
sapply(train, function(x) sum(is.na(x))) #columnwise missing value in train

#check target variable
#binary in nature check if data is imbalanced
train[,.N/nrow(train),target]
#    target   V1
#1:  <=50K 0.7591904
#2:   >50K 0.2408096

#remove leading whitespace
library(stringr)
char_col <- colnames(train)[sapply(train,is.character)]
for(i in char_col)
  set(train,j=i,value = str_trim(train[[i]],side = "left"))
#Here all the white space is replaced with the '-'both in train and test in the character column
#To resolve the dicrepancies due to whitespace

#set all character variables as factor
fact_col <- colnames(train)[sapply(train,is.character)]

for(i in fact_col)
  set(train,j=i,value = factor(train[[i]]))
#Here converting all the character into factor variables

#impute missing values
imp1 <- impute(obj = as.data.frame(train),target = "target",classes = list(integer=imputeMedian(), factor=imputeMode()))
train <- setDT(imp1$data)
#Here imputing missing value with median if column is continous in nature else imputing with mode
#if column is categorical in nature

#checking whether missing value in test and train after imputation
sum(is.na(train)) 

----------------------------------#Modelling#------------------------------------------

#Call libraries
library(caret)
library(randomForest)

#Partition dataset, 70:30
trainInd  <-  createDataPartition(train$target, p = 0.7, list = F)
trainData <-  train[trainInd,]
Valid  <-  train[-trainInd,]

#Generate Random Forest learning trees
model_rf <- randomForest(target~.,data=trainData,ntree=100)
pred_rf<-predict(model_rf,Valid[,-15])
head(pred_rf)

#Determine accuracy
table(pred_rf,Valid$target)
sum(pred_rf == Valid$target)/nrow(Valid)

#Which variable is most important
importance(model_rf)
varImpPlot(model_rf)

