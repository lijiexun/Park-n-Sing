


normalize = function(amtx){
  temp = sqrt(apply(amtx, 1, sum))
  temp[temp==0] = 10^(-20)  
  amtx2 = Diagonal(nrow(amtx), x = 1/temp)
  amtx.std = amtx2 %*% Matrix(amtx, sparse=T) %*% amtx2
  amtx.std
}

mystat = function(mypredict, true){ ## calculate precision, recall and fmeasure
  precision <- sum(mypredict & true) / sum(mypredict)
  recall <- sum(mypredict & true) / sum(true)
  Fmeasure <- 2 * precision * recall / (precision + recall)
  round(c(precision, recall, Fmeasure), 4)
}