library(Funclustering)
library(fda)
library(dplyr)
library(MASS)
library(randomForest)

rm(list=ls())

setwd("C:/Users/changxi/Dropbox/Parkinson Not Shared/Rscript")
source("../Rscript/xchang_commonly_used_function.R")

tapdata = read.csv("../Data/tap.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
#attach(tapdata)

mpowerdata = read.csv("../Data/mpower.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
demoinfo = c(2, 3, 5, 19, 20, 30, 87, 96:107, 110, 111, 114)
demodata = mpowerdata[, demoinfo]
demodata = demodata[!is.na(mpowerdata_red$professional.diagnosis), ]

## write.csv(mpowerdata_red, file = "../Data/mpower_demodata.csv")
## a0 = read.csv("../Data/mpower_demodata.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")

tapdemo = left_join(tapdata, demodata, by = c('healthCode'))

# write.csv(tapdemo, file = "../Data/tapdemo.csv", row.names = FALSE) ## manually removed the first 2 columns in Excel 
# tapdemo = read.csv("../Data/tapdemo.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")


tapdemo = tapdemo %>% filter(!is.na(professional.diagnosis)) %>% 
            filter(!is.na(age)) %>% 
            filter(!is.na(are.caretaker)) %>%
            filter(!is.na(education)) %>%
            filter(!is.na(employment)) %>%
            filter(!is.na(gender)) %>%
            filter(!is.na(maritalStatus)) %>%
            filter(!is.na(smartphone)) %>%
            filter(!is.na(smoked))

# diagornot = tapdemo$professional.diagnosis
tapdata = tapdemo %>% dplyr::select(-c(healthCode, recordId, createdOn, years.smoking))

load(file = "../Data/tapdata_deeplearning.RData")
save(tapdata, tapdemo,  file = "../Data/tapdata_deeplearning.RData")

set.seed(999)
train.percent = 0.7
train.ind = sample(nrow(tapdata), nrow(tapdata)*train.percent)

data.train = tapdata[train.ind,]
data.test = tapdata[-train.ind,]

data.train2 = data.train[,c(1:43, 50)] ## only use tapping data, no demographic info
data.test2 = data.test[,c(1:43, 50)]

whichtraindata = data.train2
whichtestdata = data.test2

#Fit Random Forest Model
rf = randomForest(professional.diagnosis ~ .,  ntree = 100 ,data = data.train)
rf2 = randomForest(professional.diagnosis ~ .,  ntree = 100 , mtry = 6, data = whichtraindata, importance = TRUE)

#### Performance Evaluation ####
whichrf = rf3

# plot importance plot 
varImpPlot(whichrf)

# Predict on train set 
predTrain <- predict(whichrf, whichtraindata, type = "class")
# Predict on train set 
predTest <- predict(whichrf, whichtestdata, type = "class")


cutoff = 0.5

mystat(predTrain > cutoff, data.train$professional.diagnosis)
mystat(predTest > cutoff, data.test$professional.diagnosis)



####
m1 <- glm.nb(tapdemo$professional.diagnosis~tapdemo$professional.diagnosis)


for (i in 1: 66){
  cat(i, sum(is.na(tapdemo[,i])), "\n")
}