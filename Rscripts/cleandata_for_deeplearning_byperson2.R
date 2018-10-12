
load(file = "../Data/tapdata_dataforLSTM_byperson.RData")

mytap <- arrange(tapdata2, healthCode, createdOn)
mytap$createdOn = mytap$createdOn/1000
mytap2 <- dplyr::select(mytap, meanTapInter:corXY) 
mytap2 = scale(mytap2, scale = TRUE) ## scale each column

mytap2 = cbind(mytap$createdOn, mytap2)

#mytap2 = cbind(mytap$healthCode, mytap$createdOn, mytap2)
mytap2 = as.data.frame(mytap2)
names(mytap2)[1] = c("createdOn")

mytap2$healthCode = mytap$healthCode

y <- dplyr::select(tapdata2, healthCode, professional.diagnosis)
y <- unique(y)
y <- arrange(y, healthCode)

x = list()
for (i in 1:nrow(y)){
  temp = dplyr::select(filter(mytap2, healthCode==y$healthCode[i]), createdOn:corXY)
  temp$createdOn_scaled = temp$createdOn - temp$createdOn[1] 
  temp <- temp %>% dplyr::select(createdOn, createdOn_scaled, everything())
  x[[y$healthCode[i]]] = temp
}

healthCode = y$healthCode

set.seed(999)
train.percent = 0.7
train.healthCode = sample(healthCode, length(healthCode)*train.percent)
test.healthCode =  healthCode[!healthCode %in% train.healthCode]

data.train = x[train.healthCode]
data.test = x[test.healthCode]

save(x, y, mytap2, healthCode, train.healthCode, test.healthCode, data.train, data.test, file = "../Data/tapdata_dataforLSTM_byperson2.RData")
