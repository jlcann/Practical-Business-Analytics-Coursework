
train_MLP_Model <- function(train, outputField, hiddenNeurons, numEpochs, test){
  
  #Remove the class field from the training data and convert to a matrix as 
  #required by the model.
  trainingData <- as.matrix(train[-(which(names(train)==outputField))])
  
  
  #Create a list containing the expected outputs for the classifier 
  #and convert them to categorical, also required by the model.
  expectedOutput <- to_categorical(train[,(which(names(train)==outputField))])

  
  #Create the model classifier, ready for layers and parameters to be added.
  model_Classifier <- keras_model_sequential()
  
  #Create and add layers to the model. Must used keras pipeline operator '%>%'
  model_Classifier %>%
    #First layer must have input dimensions of the dataset, so number of columns
    layer_dense(input_shape = ncol(trainingData), units = ncol(trainingData), activation = "relu") %>%
    #Dropout layer, this helps to prevent over-fitting the model.
    layer_dropout(0.2) %>%
    #As this isn't the first layer, we use the argument passed into the function 'hidden neurons'
    layer_dense(units = hiddenNeurons, activation = "relu") %>%
    layer_dropout(0.2) %>%
    #Output layer, units must be equal to unique classes, softmax is universally used for classification problems
    #as the activation function.
    layer_dense(units = 2, activation = "softmax")
  
  #Print model summary
  summary(model_Classifier)

  
  #Must now add a loss function and an optimiser to the model
  model_Classifier %>%
    compile(
      loss = "categorical_crossentropy",
      optimizer = "adagrad",
      metrics = "accuracy"
    )
  
  #Fit the model
  model_Classifier %>%
    fit(
      x = trainingData,
      y = expectedOutput,
      epochs = numEpochs,
      batch_size = 5,
      validation_split = 0.2
    )
  print("HELLO HELLO HELLO HELLO")
  test_MLP_Model(test,outputField,model_Classifier)

  
} 


test_MLP_Model<-function(testData,outputField,mlp_model){
  print("HELLO HELLO HELLO HELLO")
  test <- as.matrix(testData[-(which(names(testData)==outputField))])
  expectedTestOutput <- testData[,(which(names(testData)==outputField))]
  print("HELLO HELLO HELLO HELLO")
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredicted<-predict(mlp_model,test)
  
  results <- determineThreshold(testPredicted[,2],expectedTestOutput)
  
  return(results)
  
}


#NEED TO FINISH THIS FUNCTION
determineThreshold<-function(testPredicted, expectedTestOutput){
  for (threshold in seq(0,1,by=0.01)){
    result<-evaluateModel(testPredicted, expectedTestOutput,threshold)
  }
}

evaluateModel<-function(testPredicted,expectedTestOutput,threshold){
  
  class<-ifelse(testPredicted<threshold,0,1)
  
  results<-calculateConfusionMatrix(expectedTestOutput,class)
  
  return(results)
}

calculateConfusionMatrix<-function(expectedTestOutput,class){
  
  confusion<-table(factor(class,levels=0:1),factor(expectedTestOutput,levels=0:1))
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
}

NcalcMeasures<-function(TP,FN,FP,TN){
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}

