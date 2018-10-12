library(keras)
library(dplyr)

rm(list=ls())

setwd("C:/Workspace/Park-n-Sing/Rscripts")

cat('Loading data...\n')

load('../Data/tapdata_dataforLSTM_byperson2.RData')

data.train[[1]]
data.test[[1]]

y_train <- y[y$healthCode %in% train.healthCode,]$professional.diagnosis
y_test <- y[y$healthCode %in% test.healthCode,]$professional.diagnosis


list2ary = function(input.list){  #input a list of lists
  rows.cols <- dim(input.list[[1]])
  sheets <- length(input.list)
  output.ary <- array(unlist(input.list), dim = c(rows.cols, sheets))
  colnames(output.ary) <- colnames(input.list[[1]])
  row.names(output.ary) <- row.names(input.list[[1]])
  return(output.ary)    # output as a 3-D array
}


#padding
maxlen <- 10

df <- data.train
for (i in 1:length(df)){
  df[[i]] <- df[[i]] %>% head(maxlen)
  while (nrow(df[[i]])<maxlen) {
    df[[i]][nrow(df[[i]])+1,] <- 0
  } 
}
x_train <- list2ary(df)
dim(x_train) <- c(length(df), maxlen, 44)

df <- data.test
for (i in 1:length(df)){
  df[[i]] <- df[[i]] %>% head(maxlen)
  while (nrow(df[[i]])<maxlen) {
    df[[i]][nrow(df[[i]])+1,] <- 0
  } 
}
x_test <- list2ary(df)
dim(x_test) <- c(length(df), maxlen, 44)

cat('x_train shape:', dim(x_train), '\n')
cat('x_test shape:', dim(x_test), '\n')

# Response variables for training and testing sets
y_train <- ifelse(y_train == TRUE, 1, 0)
y_test  <- ifelse(y_test == TRUE, 1, 0)

#max_features <- 20000
batch_size <- 32

cat('Build model...\n')
model <- keras_model_sequential()
model %>%
  #layer_embedding(input_dim = max_features, output_dim = 128) %>% 
  layer_lstm(units = 128, input_shape = c(maxlen, 44), dropout = 0.2, recurrent_dropout = 0.2) %>% 
  #layer_dense(units = 32, activation = 'relu', input_shape = ncol(x_train)) %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 32, activation = 'relu') %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model

# Try using different optimizers and different optimizer configs
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Create a validation set
x_val <- x_train[1:500,,]
x_train_part <- x_train[501:nrow(x_train),,]
y_val <- y_train[1:500]
y_train_part <- y_train[501:length(y_train)]

cat('Train...\n')
model %>% fit(
  x_train_part, y_train_part,
  batch_size = batch_size,
  epochs = 40,
  validation_data = list(x_val, y_val)
)

scores <- model %>% evaluate(
  x_test, y_test,
  batch_size = batch_size
)

cat('Test score:', scores[[1]])
cat('Test accuracy', scores[[2]])

