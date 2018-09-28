library(Funclustering)
library(fda)
library(dplyr)

rm(list=ls())

tempdata = read.csv("../Data/tap.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
attach(tempdata)
summary(createdOn)

mpowerdata = read.csv("../Data/mpower.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")


tempdata = arrange(tempdata, desc(healthCode))
healthCode.unique = unique(tempdata$healthCode)
n = length(healthCode.unique)
healthCode.num = 1:n

codedata = data.frame(I(healthCode.unique), healthCode.num)
names(codedata)[1] = "healthCode" ### identifier for people 

tempdata2 = left_join(tempdata, codedata, by = c('healthCode'))
tempdata = tempdata2
names(tempdata)[49] = "PatientID"

### diagonosis data ####
diagdata = mpowerdata[,c(2, 96)]
codedata = left_join(codedata, diagdata, by = c('healthCode'))
diagornot = codedata[,3] ### this has missing values 


### calculate the time lags (in seconds) between two consecutive activities
extracttime = function(patientID){
  output = c(0, diff(filter(tempdata, PatientID == patientID)$createdOn/1000))
}
timestamp = lapply(1:n, extracttime)

### UseNumber = number of times used this app for each subject
countnumber = function(patientID){
  output = nrow(filter(tempdata, PatientID == patientID))
}
UseNumber = lapply(1:n, countnumber)
UseNumber = unlist(UseNumber)

timestamp.unlist = unlist(timestamp)


### calculate the time lags (in days) between two consecutive activities
extracttimeday = function(patientID){
  itime = (filter(tempdata, PatientID == patientID)$createdOn/1000)
  output = floor((itime - itime[1])/86400)+1
}
timestampday = lapply(1:n, extracttimeday)


timestampday.unlist = unlist(timestampday)


### calculate the number of unique days data we have. 
### may only wanna consider subjects with at least 3 unique days of data
### data obtained on the same day are considered replicates, and will be replaced using their averages. 
### 1775 subjects used the app on at least 3 unique days 
### 1252 subjects used the app on at least 4 unique days 


unique.day.count = sapply(sapply(timestampday, unique), length)
# sum(unique.day.count  >= 2)
# [1] 2994
# sum(unique.day.count  >= 3)
# [1] 1775
# sum(unique.day.count  >= 4)
# [1] 1252

# id3 are the patientID who has with at least 3 unique days of data
id3 = which(unique.day.count >= 3)

# id4 are the patientID who has with at least 4 unique days of data
id4 = which(unique.day.count >= 4)

idsubset = id4

cal.average.data = function(a, alist, datalist){
  selectindex = which(alist == a)
  m = length(selectindex)
  sum(rep(1/m, m) * datalist[selectindex])
}

cal.average.data.v = Vectorize(cal.average.data, vectorize.args = "a")


dayanddata = function(tempid, datacolumnname){
  tempday = timestampday[[tempid]]
  rawdata = unlist(filter(tempdata, PatientID == tempid) %>% select(datacolumnname))
  
  uniqueday = unique(tempday)
  
  output = matrix(0, ncol = length(uniqueday), nrow = 2)
  output[1,] = uniqueday
  output[2,] = cal.average.data.v(uniqueday, tempday, rawdata)
  output
}


### Specify data ###

averaged.data = lapply(idsubset, dayanddata, datacolumnname = "meanTapInter")
#averaged.data[[1]]

datamtx = matrix(NA, ncol = length(idsubset), nrow = 184)

for (i in 1:length(id4)){
  datamtx[averaged.data[[i]][1,],i] =  averaged.data[[i]][2,]
}

datamtx.filled = datamtx

fillfun = function(alist){
  for (i in which(is.na(alist))){
    alist[i] = alist[i-1]
  }
  alist
}

datamtx = apply(datamtx, 2, fillfun)

K = 2
t= 1:184
sampleindex = sample(1:length(idsubset), 200) 
datamtx.data = (as.matrix(datamtx[t,sampleindex]))
## norder = 4: cubic b-spline
splines <- create.bspline.basis(rangeval=c(1, max(t)), nbasis = 6, norder=4); 
datamtx.fdata = Data2fd(datamtx.data, argvals = t, basisobj=splines);
res.fd = funclust(datamtx.fdata, K)
#summary(res.fd)
plotfd(datamtx.fdata,col=res.fd$cls)

diagtruth = diagornot[id4]

cor(res.fd$cls, diagtruth[sampleindex], use = "complete") ### diagornot has missing values 


setwd("C:/Users/changxi/Dropbox/Parkinson Not Shared/Rscript")
load(file = "../Data/CleanedData.RData")
cleaneddata = save(n, tempdata, codedata, diagornot, timestampday, id3, id4, dayanddata,  file = "../Data/CleanedData.RData")





