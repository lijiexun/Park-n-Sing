library(keras)
library(dplyr)

rm(list=ls())

setwd("C:/Workspace/Park-n-Sing/Rscripts")

cat('Loading data...\n')

load('../Data/tapdata_dataforLSTM.RData')

glimpse(whichtestdata)
glimpse(whichtraindata)

x_train <- select(whichtraindata, meanTapInter:corXY)
x_test <- select(whichtestdata, meanTapInter:corXY)
y_train <- whichtraindata$professional.diagnosis
y_test <- whichtestdata$professional.diagnosis

cat(length(x_train), 'train sequences\n')
cat(length(x_test), 'test sequences\n')

x_train <- scale(x_train)
x_test <- scale(x_test)

# Cut texts after this number of words (among top max_features most common words)
#maxlen <- 80  
#cat('Pad sequences (samples x time)\n')
#x_train <- pad_sequences(x_train, maxlen = maxlen)
#x_test <- pad_sequences(x_test, maxlen = maxlen)

x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

cat('x_train shape:', dim(x_train), '\n')
cat('x_test shape:', dim(x_test), '\n')

dim(x_train) <- c(nrow(x_train),1,ncol(x_train))  # re-dimension
dim(x_test) <- c(nrow(x_test),1,ncol(x_test))  # re-dimension

# Response variables for training and testing sets
y_train <- ifelse(y_train == TRUE, 1, 0)
y_test  <- ifelse(y_test == TRUE, 1, 0)

max_features <- 20000
batch_size <- 32

cat('Build model...\n')
model <- keras_model_sequential()
model %>%
  #layer_embedding(input_dim = max_features, output_dim = 128) %>% 
  layer_lstm(units = 64, input_shape = c(1, 43), dropout = 0.2, recurrent_dropout = 0.2) %>% 
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

cat('Train...\n')
model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = 20,
  validation_data = list(x_test, y_test)
)

scores <- model %>% evaluate(
  x_test, y_test,
  batch_size = batch_size
)

cat('Test score:', scores[[1]])
cat('Test accuracy', scores[[2]])

