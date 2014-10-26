##Following lines provide the run down of commands executed using R 3.1.1 ( 64 Bit Version) on Windows 7
## this corresponds the course project assignment for predicting exercise class preedition
## this part of Preactical machine Learning class by 
## by dr.Jeff Leek, Dr.Roger D. Peng, Dr.Brian Caffo

  ## Read test data and traning data
  fitnessData <- read.csv("pml-training.csv")
  fitnessTest <- read.csv("pml-testing.csv")
  library(caret)
  set.seed(975)
  ## Study the Data for both fitnessTest and fitnessData and make sure NA columns in test are not are !!
  ## take out any NA columns
  fitnessData <- fitnessData[colSums(is.na(fitnessData)) == 0]
  testing1 <- fitnessTest[colSums(is.na(fitnessTest)) == 0]
  ## divide the fitnessData into traning and testing data sets
  inTrain = createDataPartition(fitnessData1$classe, p = 0.5)[[1]]
  training = fitnessData1[ inTrain,]
  intesting = fitnessData1[-inTrain,]
  summary(training)
  ## observe for paterns and interdependence between predictors 
  isRoll <- (substr(names(training),1,4) == "roll")
  isBelt <- (substr(names(training),1,4) == "belt")
  isYaw <- (substr(names(training),1,3) == "yaw")
  isPitch <- (substr(names(training),1,5) == "pitch")
  ##
  featurePlot(x=training[,names(training)[isRoll]], y= training$classe, plot="pairs")
  featurePlot(x=training[,names(training)[isBelt]], y= training$classe, plot="pairs") 
  feature plot is easy way to identify patterns and interdependence
  ## columns to exclude based upon the feature plot
  colExclude <- c(1:7)
  ## finally only 53 columns remain with 52 predictors
  training1 <- training[,-colExclude]
  ## try rpart training model first with all predictors
  modFit1 <- train(training1$classe~.,method="rpart",data=training1)
  ## print accuracy of model, you will not be happy to see the result
  print(modFit1)
  ## first predict on training data itself
  pred1 <- predict(modFit1,newdata=training1)
  ## just to complete this compute prediction and compare with training set classe
  tab1 <- table( training1$classe,pred1)
  ## print tab1 to see the hits and misses
  print(tab1)
  ## miss classification rate
  1-sum(diag(tab1))/sum(tab1)
  
  ##################Next try Random Forest with all predictors included
  modFit4 <- train(training1$classe ~., data=training1, method="rf",trControl=trainControl(method="oob",repeats=10))
  modFit4
  summary(modFit4)
  
  ### estimate the prediction 
  pred5 <- predict(modFit4,newdata=training)
  tab5 <- table(training$classe,pred5)
  ## and the accuracy and miss classification rate
  1-sum(diag(tab5))/sum(tab5)
  pred5 <- predict(modFit4,newdata=training1)
  tab5 <- table(training1$classe,pred5)
  1-sum(diag(tab5))/sum(tab5)
  ## now let us testing on the other half of the data we partiioned for testing cross validation of the model
  pred4 <- predict(modFit4,newdata=intesting)
  tab4 <- table(intesting$classe,pred4)
  tab4
  ## cross validate the error we had predicted
  1-sum(diag(tab4))/sum(tab4)
  ## let us see if we can reduce the number of predictors
  ## for that we will look at relative importance of error
  myResult <- rfcv(training1[,-53], training1[,53], cv.fold=5, scale="log", step=0.5,mtry=function(p) max(1, floor(sqrt(p))), recursive=FALSE)
  with(myResult, plot(n.var, error.cv, log="x", type="o", lwd=2))
    
  rf1 <- randomForest(classe ~ ., data=training1, ntree=500,
                      keep.forest=FALSE, importance=TRUE)
  importance(rf1)
  ## based upon above we try with 30 importnat predictors
  
  modFit30 <- train(classe ~ roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell, data=training1, method="rf",trControl=trainControl(method="oob",repeats=5))
 
  pred301 <- predict(modFit30,newdata=training1)
  ## find the estimated error
  tab301 <- table(training1$classe,pred301)
  1-sum(diag(tab301))/sum(tab301)
  ## cross validate with testing data
  pred302 <- predict(modFit30,newdata=intesting)
  tab302 <- table(intesting$classe,pred302)
  1-sum(diag(tab302))/sum(tab302)
  
 
  #### now we are ready for testing on the real data i.e submission project
  predTest30 <- predict(modFit30,newdata=testing1)
  predTest <- predict(modFit4,newdata=testing1)
  ### in both case you will see the answer to be
  ## [1] B A B A A E D B A A B C B A E E A B B B