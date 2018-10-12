library(Funclustering)
library(fda)
library(dplyr)
library(MASS)
library(randomForest)

## healthCode is patient's ID
## recordId is the activity ID 

rm(list=ls())

setwd("C:/Users/changxi/Dropbox/Parkinson Not Shared/Rscript")
source("../Rscript/xchang_commonly_used_function.R")

tapdata = read.csv("../Data/tap.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
#attach(tapdata)

mpowerdata = read.csv("../Data/mpower.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
code_profdiag_data = data.frame(healthCode = mpowerdata$healthCode, professional.diagnosis = mpowerdata$professional.diagnosis)
code_profdiag_data = code_profdiag_data %>% filter(!is.na(professional.diagnosis))

tapdata = left_join(tapdata, code_profdiag_data, by = c('healthCode'))
tapdata2 = tapdata %>% filter(!is.na(professional.diagnosis))
# write.csv(tapdata2, file = "../Data/tapdata2_remove_na_prof_diag.csv", row.names = FALSE) 
tapdata2 = tapdata2[,-c(1,2)]

healthCodeID = unique(tapdata2$healthCode) ### total of 6414 patients. This is also how th subjects are sorted. 

### extract the exact time stamp for each subject. 
getTime = function(patientID){
  output = filter(tapdata2, healthCode == patientID)$createdOn/1000
}
timestamp = lapply(healthCodeID, getTime) ## saves all the time stamp (in terms of seconds) for all activties for each subject. ## saved in a "list" format. 

timestamp.unlist = unlist(timestamp) 
time.count = sapply(timestamp, length) ### saves the number of activities for each subject. 


### calculate the time difference between each activity and the very first activity done by the same subject. Time measured in seconds. 
getTimeScaled = function(patientID){
  itime = filter(tapdata2, healthCode == patientID)$createdOn/1000
  output = itime - itime[1]
}
timestamp_scaled = lapply(healthCodeID, getTimeScaled) ## saves all the time stamp (in terms of seconds) for all activties for each subject. ## saved in a "list" format. 


set.seed(999)
train.percent = 0.7
train.ind.byperson = sample(healthCodeID, length(healthCodeID)*train.percent)
test.ind.byperson =  healthCodeID [!healthCodeID %in% train.ind.byperson]
  
data.train.byperson = tapdata2 %>% filter(healthCode%in% train.ind.byperson)
data.test.byperson = tapdata2 %>% filter(healthCode%in% test.ind.byperson)






save(train.ind.byperson, test.ind.byperson, data.train.byperson, data.test.byperson, tapdata2, timestamp, timestamp_scaled, time.count, file = "../Data/tapdata_dataforLSTM_byperson.RData")
#load(file = "../Data/tapdata_dataforLSTM_byperson.RData")





