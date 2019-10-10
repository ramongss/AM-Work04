rm(list = ls())

# Load Packages
library(EBImage)
library(keras)

set.seed(1234)

# Read Images
# setwd("C:/Users/Usuario/Dropbox/Trabalhos_AM_20192/Work04")
setwd("C:/Users/ramon/Dropbox/PUCPR/Doutorado/2º Trimestre/Aprendizado de Máquina/Trabalhos_AM_20192/Work04")

BaseDir    <- getwd()
DataDir    <- paste(BaseDir, "Data", sep="/")
AnimalDir  <- paste(DataDir, "Animals", sep="/")
ObjectsDir <- paste(DataDir, "Objects", sep="/")

setwd(AnimalDir) #AnimalDir or ObjectsDir

pics <- c('b1.jpg', 'b2.jpg', 'b3.jpg', 'b4.jpg', 'b5.jpg', 'b6.jpg',
          'c1.jpg', 'c2.jpg', 'c3.jpg', 'c4.jpg', 'c5.jpg', 'c6.jpg',
          'p1.jpg', 'p2.jpg', 'p3.jpg', 'p4.jpg', 'p5.jpg', 'p6.jpg')
mypic <- list()
for (i in 1:18) {mypic[[i]] <- readImage(pics[i])}

# Explore
print(mypic[[1]])
display(mypic[[8]])
summary(mypic[[1]])
hist(mypic[[2]])
str(mypic)

# Resize
for (i in 1:18) {mypic[[i]] <- resize(mypic[[i]], 28, 28)}

# Reshape
for (i in 1:18) {mypic[[i]] <- array_reshape(mypic[[i]], c(28,28,3))}

# Row Bind
trainx <- NULL
for (i in 1:5)   {trainx <- rbind(trainx, mypic[[i]])}
for (i in 7:11)  {trainx <- rbind(trainx, mypic[[i]])}
for (i in 13:17) {trainx <- rbind(trainx, mypic[[i]])}
str(trainx)
testx <-rbind(mypic[[6]], mypic[[12]],mypic[[18]])
trainy <- c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2)
testy <- c(0,1,2)

# One Hot Encoding
trainLabels <-to_categorical(trainy)
testLabels <- to_categorical(testy)

# Model
model <- keras_model_sequential()
model %>%
         layer_dense(units = 256, activation = 'sigmoid', input_shape = c(2352)) %>%
         layer_dense(units = 128, activation = 'sigmoid') %>%
         layer_dense(units = 3, activation = 'softmax')
summary(model)

# Compile
model %>%
         compile(loss = 'categorical_crossentropy',
                 optimizer = optimizer_rmsprop(),
                 metrics = c('accuracy'))

# Fit Model
history <- model %>%
         fit(trainx,
             trainLabels,
             epochs = 30,
             batch_size = 32,
             validation_split = 0.2)

# Evaluation & Prediction - train data
model %>% evaluate(trainx,trainLabels)
pred <- model %>% predict_classes(trainx)
table(Predicted = pred, Actual = trainy)
prob <- model %>% predict_proba(trainx)
cbind(prob, Prected = pred, Actual= trainy)

# Evaluation & Prediction - test data
model %>% evaluate(testx, testLabels)
pred <- model %>% predict_classes(testx)
table(Predicted = pred, Actual = testy)
prob <- model %>% predict_proba(testx)
cbind(prob, Predicted_class = pred, Actual = testy)
