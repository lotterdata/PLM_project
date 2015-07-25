library(dplyr)
library(caret)

all <- read.csv("pml_training.csv") %>%
    dplyr::select(
        roll_belt,
        pitch_belt,
        yaw_belt,
        total_accel_belt,
        gyros_belt_x,
        gyros_belt_y,
        gyros_belt_z,
        accel_belt_x,
        accel_belt_y,
        accel_belt_z,
        magnet_belt_x,
        magnet_belt_y,
        magnet_belt_z,
        roll_arm,
        pitch_arm,
        yaw_arm,
        total_accel_arm,
        gyros_arm_x,
        gyros_arm_y,
        gyros_arm_z,
        accel_arm_x,
        accel_arm_y,
        accel_arm_z,
        magnet_arm_x,
        magnet_arm_y,
        magnet_arm_z,
        roll_dumbbell,
        pitch_dumbbell,
        yaw_dumbbell,
        total_accel_dumbbell,
        gyros_dumbbell_x,
        gyros_dumbbell_y,
        gyros_dumbbell_z,
        accel_dumbbell_x,
        accel_dumbbell_y,
        accel_dumbbell_z,
        magnet_dumbbell_x,
        magnet_dumbbell_y,
        magnet_dumbbell_z,
        roll_forearm,
        pitch_forearm,
        yaw_forearm,
        total_accel_forearm,
        gyros_forearm_x,
        gyros_forearm_y,
        gyros_forearm_z,
        accel_forearm_x,
        accel_forearm_y,
        accel_forearm_z,
        magnet_forearm_x,
        magnet_forearm_y,
        magnet_forearm_z,
        classe
    )

set.seed(135724)

intrain <- createDataPartition(all$classe,p=0.6)[[1]]

train <- all[intrain,]
pretest <- all[-intrain,]

intest <- createDataPartition(pretest$classe,p=0.5)[[1]]
test <- pretest[intest,]
validation <- pretest[-intest,]

library(klaR)

lda.index <-  0
qda.index <- 0
lda.accuracy <- 0
qda.accuracy <- 0
lda.best.model <- NULL
qda.best.model <- NULL

for(i in 52:52){
    preProc <- preProcess(train[,-53],method="pca",pcaComp=i)
    train.pc <- predict(preProc,train[,-53])
    train.pc$classe <- train$classe
    test.pc <- predict(preProc,test[,-53])
    test.pc$classe <- test$classe    
    
    model <- train(classe ~ ., data = train.pc,method="lda")
    trial <- predict(model,test.pc)
    if(sum(trial==test.pc$classe) > lda.accuracy){
        lda.index <- i
        lda.accuracy <- sum(trial==test.pc$classe)
        lda.best.model <- model
    } 
    
    model <- train(classe ~ ., data = train.pc,method="qda")
    trial <- predict(model,test.pc) 
    if(sum(trial==test.pc$classe) > qda.accuracy){
        qda.index <- i
        qda.accuracy <- sum(trial==test.pc$classe)
        qda.best.model <- model
    } 
}

preProc <- preProcess(train[,-53],method="pca")
train.pc <- predict(preProc,train[,-53])
train.pc$classe <- train$classe
test.pc <- predict(preProc,test[,-53])
test.pc$classe <- test$classe
nb.model <- train(classe ~ ., data = train.pc,method="nb")
predict.nb <- predict(nb.model,test.pc)
trial <- predict(nb.model,test.pc)
nb.accuracy <- sum(trial==test.pc$classe)

preProc <- preProcess(train[,-53],method="pca",pcaComp=52)
test.pc <- predict(preProc,test[,-53])
predict.lda <- predict(lda.best.model,test.pc)
predict.qda <- predict(qda.best.model,test.pc)

dfPred <- data.frame(predict.lda,
                     predict.qda,
                     predict.nb,
                     classe = test$classe)

combMod <- train(classe~.,method="rf",data=dfPred)

combPred <- predict(combMod,dfPred)
