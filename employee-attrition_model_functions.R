
train_MLP_Model <- function(train, outputField, hiddenNeurons, numEpochs){
  
  #Remove the class field from the training data and convert to a matrix as 
  #required by the model.
  trainingData <- as.matrix(train[-(which(names(train)==outputField))])
  
  #Create a list containing the expected outputs for the classifier 
  #and convert them to categorical, also required by the model.
  expectedOutput <- keras::to_categorical(train[,(which(names(train)==outputField))])
  
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
      optimizer = "adam",
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
    
} 