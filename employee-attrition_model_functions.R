#*
#* List of functions in this file:
#*
#* train_MLP_Model() 
#* 
#* test_MLP_Model()  
#* 
#* calculateThreshold()----->Function taken from Prof. Nick Ryman-Tubb
#*                            lab session 4.
#*
#* evaluateModel()---------->Function taken from Prof. Nick Ryman-Tubb
#*                           lab session 4.
#* 
#* 
#* calculateConfusion()----->Function taken from Prof. Nick Ryman-Tubb
#*                           lab session 4.
#* 
#* 
#* NcalcMeasures()---------->Function taken from Prof. Nick Ryman-Tubb
#*                          lab session 4.
#* 
#* 
#* plotConfusionMatrix() 
#* 
#* 
#* getTreeClassifications() 
#* 
#* 
#* getTreeRules() 
#* 
#* 
#* areaUnderCurve()------->By Miron Kursa https://mbq.me
#                         See https://stackoverflow.com/questions/4903092/calculate-auc-in-r
#* 
#* 
#* plotThresholdGraph()--->Function taken from Prof. Nick Ryman-Tubb
#*                        lab session 4.
#* 
#* 
#* createDT()
#* 
#*  
#* createAndEvaluateDT()
#* 
#* 
#* createForest()
#* 
#* 
#* createAndEvaluateForest()
#* 
#* 
#* getTreeMetricts() 
#* 
#* 
#* kFoldModel() 





# ************************************************
# train_MLP_Model()
#
# This function trains a multilayer perceptron Model on the desired Dataset
# It's output list is the result of multiple other functions evaluating the model
# and processing its results.
#
# INPUT : train - data.frame - Train Dataset
#         test - data.frame - Test Dataset
#         outputField - String - Name of predicted field
#         i - integer - The current fold of the model - or an arbitrary int to identify holdout. IN our case, 999.
#         load - Boolean - Whether to load a saved data run the algorithm again
#         plot - Boolean - Whether to plot the model outputs
#
# OUTPUT : results - List - A list containing the accuracy measurements of the model.
#
# ************************************************
train_MLP_Model <- function(train, test, outputField, i, load, plot = TRUE){
  
  
  if (!load) {
    
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
      layer_dropout(NN_DROPOUT) %>%
      #Another ReLU layer, this time using the amount of neurons defined in the NN_HIDDEN_RELU constant.
      layer_dense(input_shape = ncol(trainingData), units = NN_HIDDEN_RELU, activation = "relu") %>%
      #A second dropout layer to help prevent overfit.
      layer_dropout(NN_DROPOUT) %>%
      #A sigmoid layer with only two neurons.
      layer_dense(units = NN_HIDDEN_SIGMOID, activation = "sigmoid") %>%
      #And finally the output layer which will give us our classification probabilities
      layer_dense(units = 2, activation = "softmax")
    
    #Print model summary
    summary(model_Classifier)
    
    
    #Must now add a loss function and an optimizer to the model
    model_Classifier %>%
      compile(
        loss = "binary_crossentropy",
        optimizer = NN_OPTIMISER,
        metrics = "accuracy"
      )
    
    #Fit the model
    model_Classifier %>%
      fit(
        x = trainingData,
        y = expectedOutput,
        epochs = NN_EPOCHS,
        batch_size = NN_BATCH_SIZE,
        validation_split = 0.2
      )
    
    #This will save the model once it has been fit on the training data.
    model_Classifier %>% save_model_tf(paste0("MLP_Model_", i))
  }
  
  else {
    #This will load a model ready to be tested on any test sets. 
    model_Classifier <- load_model_tf(paste0("MLP_Model_", i))
  }
  
  #Assign the results of the model tested on the test dataset.
  results <- test_MLP_Model(test,outputField,model_Classifier,plot)
  
  
  if (plot) {
    
    #This block of code will plot the accuracy statistics into the viewer, and also plot the confusion matrix in the plots.
    plotConfusionMatrix(results, "MLP Model Confusion Matrix")
    metricsView <- as.data.frame(as.matrix(results))
    colnames(metricsView) <- "MLP with Holdout Measures"
    print(formattable::formattable(metricsView))
  }
  
  #return the stats of the tested model.
  return(results)
  
  
} 

# ************************************************
# test_MLP_Model()
#
# This function tests the model created and trained in the train_MLP__Model() function.
# 
#
# INPUT : testData - Data.Frame - Testing Dataset
#         outputField - String - Name of the field to be predicted
#         mlp_Model - keras_model_sequential object - The model trained on the train dataset
#         plot - Boolean - whether to plot the output graphs 
#
# OUTPUT : results - List - A list containing the accuracy measurements of the model.
#
# ************************************************

test_MLP_Model<-function(testData,outputField,mlp_model,plot){
  
  title = "MLP Model"
  #Remove the class field from the training data and convert to a matrix as 
  #required by the model.
  test <- as.matrix(testData[-(which(names(testData)==outputField))])
  
  #Create a vector containing the classes for each of the test rows which we expect the model to predict.
  expectedTestOutput <- testData[,(which(names(testData)==outputField))]
  
  #Test the model on the testset matrix and assign the results to a variable.
  #returns a two field matrix containing the confidence of each class.
  testPredicted<-predict(mlp_model,test)
  
  #Return the results model with the best threshold determined.
  #TestPredicted Col 2 is Yes to Attrition Yes, meaning person will leave job.
  results <- calculateThreshold(testPredicted[,2],expectedTestOutput,title,plot)
  
  return(results)
  
}


# ************************************************
# calculateThreshold() :
#
# For the range of thresholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Determine "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - predicted   - probability of being class 1
#         :   vector double  - expected    - dataset to evaluate
#         :   string         - title            - chart title
#         :   boolean        - plot             - TRUE=output charts
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
# ************************************************
calculateThreshold<-function(predicted,
                             expected,
                             title="",
                             plot=TRUE){
  toPlot<-data.frame()
  
  
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-evaluateModel(predicted,
                           expected,
                           threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  # Youdan = sensitivty + specificity -1
  #        = TPR + (1-FPR) -1
  
  
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  
  # 121020NRT - max Youdan
  # use which.max() to return a single index to the higest value in the vector
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  
  # 121020NRT - Euclidean distance to "perfect" classifier (smallest the best)
  # use which.min() to return a single index to the lowest value in the vector
  
  minEuclidean<<-toPlot$x[which.min(toPlot$distance)]
  
  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  
  results$threshold<-myThreshold
  results$AUC<-areaUnderCurve(score=predicted,bool=expected) # Estimate the AUC
  # Plot threshold graph  
  if (plot==TRUE){
    plotThresholdGraph(toPlot, maxYoudan, minEuclidean,
                       auc = results$AUC, title)
  } # endof if plotting
  
  return(evaluateModel(predicted, expected, minEuclidean))
} #endof calculateThreshold()

# ************************************************
# evaluateModel()
#
# Determine the best threshold in the range [0,1] for calculating a confusion matrix.
# Best threshold is determined based on the smallest Euclidean Distance.
#
# INPUT : testPredicted - Vector - Predicted class from the tested model
#         expectedTestOutput - Vector -  The expected class for each of the predicted values
#         threshold - double - The threshold value of the model
#
# OUTPUT : results - List - A list containing the accuracy measurements of the model.
#
# ************************************************

evaluateModel<-function(testPredicted,expectedTestOutput,threshold){
  
  #For each of the predicted classes in the testPredicted vector, if the value is
  #smaller than the threshold, class = 0, otherwise class = 1.
  class<-ifelse(testPredicted<threshold,0,1)
  
  #Calculate a confusionMatrix using calculateConfusion() using the classes determined 
  # in the line above, and the expected classes.
  results<-calculateConfusion(expectedTestOutput,class,threshold)
  
  #Return the list containing the accuracy measurements of the confusion matrix.
  return(results)
}

# ************************************************
# calculate Confusion() :
#
# Calculate a confusion matrix for 2-class classifier - Yes/1 and No/0
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#        double - threshold      - The threshold value of the model
#
# OUTPUT: A list with the  entries from NcalcMeasures()
# ************************************************

calculateConfusion<-function(expectedClass,predictedClass,threshold){
  
  #Assign the resulting table into our 'confusion'variable 
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # The following bit places the information from the table above in our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  TH<-as.double(threshold)
  
  return(NcalcMeasures(TP,FN,FP,TN,TH))
  
} #endof calculateConfusion()


# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FN        - double - False Negative records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        TH        - double - Threshold value
# ************************************************

NcalcMeasures<-function(TP,FN,FP,TN,TH){
  
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
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)),
                  "Theshold"= TH
  )
  return(retList)
}

# ************************************************
# plotConfusionMatrix() :
#
# Plot the confusion matrix as well as accuracy details 
# INPUT: vector - measures  - Measures with the confusion matrix details
#        vector - title     - title of the confusion matrix
#
# OUTPUT: A plotted confusion matrix as well as the accuracy details
# ************************************************
plotConfusionMatrix <- function(measures, title) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(title, cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='Green')
  text(195, 435, 'Leaves', cex=1.2)
  rect(250, 430, 340, 370, col='Red')
  text(295, 435, 'Stays', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='Red')
  rect(250, 305, 340, 365, col='Green')
  text(140, 400, 'Leaves', cex=1.2, srt=90)
  text(140, 335, 'Stays', cex=1.2, srt=90)
  
  # Confusion Matrix Details
  text(195, 400, measures$TP, cex=1.6, font=2, col='white')
  text(195, 335, measures$FN, cex=1.6, font=2, col='white')
  text(295, 400, measures$FP, cex=1.6, font=2, col='white')
  text(295, 335, measures$TN, cex=1.6, font=2, col='white')
  
  
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Model Details", xaxt='n', yaxt='n')
  text(10, 85, "Accuracy", cex=1.2, font=2)
  text(10, 70, round(as.numeric(measures$accuracy), 3), cex=1.2)
  text(30, 85, "TPR", cex=1.2, font=2)
  text(30, 70, round(as.numeric(measures$TPR), 3), cex=1.2)
  text(50, 85, "Precision", cex=1.2, font=2)
  text(50, 70,round(measures$TP/(measures$TP+measures$FP),3), cex=1.2)
  text(70, 85, "Recall", cex=1.2, font=2)
  text(70, 70, round(measures$TP/(measures$TP+measures$FN),3), cex=1.2)
  text(90, 85, "Threshold", cex=1.2, font=2)
  text(90, 70, round(as.numeric(measures$Theshold), 3), cex=1.2)
  
  # Details for the correctly and wrongly predicted results
  text(30, 35, "Correctly Predicted", cex=1.5, font=2)
  text(30, 20, measures$TP + measures$TN, cex=1.4)
  text(70, 35, "Wrongly Predicted", cex=1.5, font=2)
  text(70, 20, measures$FN + measures$FP, cex=1.4)
}  

# ************************************************
# getTreeClassifications() :
#
# Run the predict function on a decision tree to generate the predicted classes
#
# INPUT
#         :   object         - tree                          - the trained decision tree
#         :   Data Frame     - testDataset                   - the test dataset used to make predictions on
#         :   string         - predictorField                - the name of the predictor field in the dataset
#
# OUTPUT  
#         :   double vector  - predictedClassProbabilities   - a vector consisting of all of the classifications generated for the given dataset
#
# ************************************************
getTreeClassifications <- function(tree, testDataset, predictorField){
  # Use the input fields to generate outputs from the tree
  inputs <- testDataset[-(which(names(testDataset) == predictorField))]
  
  # We use type=prob here so that we can later find the ideal threshold for the classifier
  predictedClassProbabilities <- predict(tree, inputs, type = "prob")
  
  return(predictedClassProbabilities)
} #endof getTreeClassifications()

# ************************************************
# getTreeRules()
#
# REQUIRES tidyrules
#
# INPUT: 
#      :Object  - tree  - trained decision tree
#      :boolean - print - If true, prints the rules out to the viewer
#
# OUTPUT:
#       :  data frame   -  rules   - the rules for the decision tree
# ************************************************
getTreeRules<-function(tree, print = F){
  #library(tidyrules) is already imported in the main script file
  
  # extract rules into a data frame using tidyRules and make text format more friendly
  rules <- as.data.frame(tidyRules(tree))
  for (i in 1:nrow(rules)) {
    rules[i, 2] <- str_replace_all(rules[i, 2], "%in%", "is")
    rules[i, 2] <- gsub("c(", "", rules[i, 2], fixed = T)
    rules[i, 2] <- gsub(")", "", rules[i, 2], fixed = T)
    rules[i, 2] <- str_replace_all(rules[i, 2], ",", " OR")
    rules[i, 2] <- str_replace_all(rules[i, 2], "&", "AND")
  }
  
  # Use more descriptive column names and drop undesired columns
  colnames(rules) <- c("Rule Number", "Rule", "Classification", "Occurences in Dataset", "Confidence")
  rules <- rules[, 1:5]
  
  if (print) {
    print(formattable::formattable(rules))
  }
  
  return(rules)
}

# ************************************************
# areaUnderCurve() :
#
# Calculate the Area Under Curve (AUC) for ROC
#
# INPUT   :   vector double     - score            - probability of being class 1
#             vector double     - bool             - Expected class of 0 or 1
#
# OUTPUT  :   double   - AUC
#
# ************************************************
# By Miron Kursa https://mbq.me
# See https://stackoverflow.com/questions/4903092/calculate-auc-in-r
areaUnderCurve <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}


# ************************************************
#  plotThresholdGraph() :
#
# Use data generated by calculateThreshold() to plot threshold graph
#
# INPUT       :   data frame  - toPlot
#             :   double      - maxYoudan
#             :   double      - minEuclidean
#             :   double      - auc
#             :   string      - title
#             :   boolean     - plot
#
# ************************************************
plotThresholdGraph <- function(toPlot,
                               maxYoudan,
                               minEuclidean,
                               auc,
                               title = "",
                               plot = TRUE) {
  
  # Sensitivity (TPR)
  plot(toPlot$x,toPlot$tpr,
       xlim=c(0, 1), ylim=c(0, 100),
       type="l",lwd=3, col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main=paste("Threshold Perfomance Classifier Model",title))
  
  # Plot the specificity (1-FPR)
  lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
  
  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  
  if (!is.na(crosspoint)){
    if ((crosspoint<1) & (crosspoint>0))
      abline(v=crosspoint,col="red",lty=3,lwd=2)
  }
  
  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
  
  # Plot the min distance, as might be more (311019NRT check it is within range)
  if ((minEuclidean<1) & (minEuclidean>0))
    abline(v=minEuclidean,col="green",lty=3,lwd=2)
  
  # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
  lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
  
  if ((maxYoudan<1) & (maxYoudan>0))
    abline(v=maxYoudan,col="purple",lty=3,lwd=2)
  
  legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))
  
  # ROC graph
  
  sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
  specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
  #auc<-areaUnderCurve(score=predicted,bool=expected) 
  
  # Set origin point for plotting
  toPlot<-rbind(toPlot,data.frame(x=0,fpr=0,tpr=0, youdan=0,distance=0))
  
  plot(100-toPlot$fpr,toPlot$tpr,type="l",lwd=3, col="black",
       main=paste("ROC:",title),
       xlab="Specificity (1-FPR) %",
       ylab="Sensitivity (TPR) %",
       xlim=c(100,0),
       ylim=c(0,100)
  )
  
  axis(1, seq(0.0,100,10))
  axis(2, seq(0.0,100,10))
  
  #Add crosshairs to the graph
  abline(h=sensitivityROC,col="red",lty=3,lwd=2)
  abline(v=specificityROC,col="red",lty=3,lwd=2)
  
  annotate<-paste("Threshold: ",round(minEuclidean,digits=4L),
                  "\nTPR: ",round(sensitivityROC,digits=2L),
                  "%\n1-FPR: ",round(specificityROC,digits=2L),
                  "%\nAUC: ",round(auc,digits=2L),sep="")
  
  text(x=specificityROC, y=sensitivityROC, adj = c(-0.2,1.2),cex=1, col="red",annotate)
  
} # endof plotThresholdGraph()

# ************************************************
# createDT() :
#
# Creates a C5 Decision Tree from training data
#
# INPUT   :
#             Data Frame     - train                 - train dataset
#             Data Frame     - test                  - test dataset
#             charatcter     - predictorField        - the name of the predictor field in the dataset
#             character      - title                 - the title of the decision tree
#             boolean        - plot                  - if true, also plots tree rules
#
# OUTPUT
#         :   object     - tree    -  a new trained decision tree
#
# ************************************************
createDT <- function(train, test, predictorField, title = "Importance for Decision Tree", plot = F) {
  # Need to produce a data frame from the predictor fields and a vector for the output
  outputClassIndex <- which(names(train) == predictorField)
  inputs <- train[, -outputClassIndex]
  output <- train[, outputClassIndex]
  
  tree <- C50::C5.0(x=inputs, y=factor(output), rules=T, trials=1)
  
  if (plot){    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Weight"
    importance<-importance[order(importance$Weight,decreasing=TRUE),,drop=FALSE]    
    print(formattable::formattable(importance))    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=title)    
  }
  
  return(tree)
} #endof createDT()

# ************************************************
# createAndEvaluateDT() :
#
# Creates and evaluates a C5 Decision Tree from training data
#
# INPUT   :
#             Data Frame     - train                 - train dataset
#             Data Frame     - test                  - test dataset
#             character     - predictorField        - the name of the predictor field in the dataset
#             integer        - i                     - the fold for the cross validation
#             character      - title                 - the title for the model
#             character      - classLabelChar        - the class label
#             character      - filename              - the file to load or dave the model from.
#             boolean        - showInViewer          - if true, show the tree rules in the viewer pane
#             boolean        - plot                  - if true, also plots tree rules
#
# OUTPUT
#         :   object     - tree    -  a new trained decision tree
#
# ************************************************
createAndEvaluateDT <- function(train, test, predictorField, i, load, title = "Importance for Decision Tree", classLabelChar = NULL, fileName = "_dt_", showInViewer = F, plot = F) {
  
  if (!load) {
    
    tree <- createDT(train, test, predictorField, title, plot=plot)
    
    if (!dir.exists("Tree_Models")) {
      dir.create("Tree_Models")
    }
    
    saveRDS(tree, paste0("Tree_Models/", fileName, i, ".rds"))
  } else {
    tree <- readRDS(paste0("Tree_Models/", fileName, i, ".rds"))
  }
  
  
  if (plot){    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]    
    print(formattable::formattable(importance))    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=title)    
  }
  
  treeClassifications <- getTreeClassifications(tree, test, predictorField)
  treeMetrics <- getTreeMetrics(treeClassifications, test, predictorField, classLabelChar = classLabelChar)
  
  if (showInViewer) {
    metricsView <- as.data.frame(as.matrix(treeMetrics))
    colnames(metricsView) <- paste0("Decision Tree Holdout Measures (", fileName, ")")
    print(formattable::formattable(metricsView))
    plotConfusionMatrix(measures = treeMetrics, title = "Decision Tree Holdout Measures")
  }
  
  # Only print out the rules for the tree generated from the first fold
  treeRules <-getTreeRules(tree,ifelse(i == 1, T, F))
  
  return(treeMetrics)
} #endof createAndEvaluateDT()


# ************************************************
# createForest() :
#
# Creates a Random Forest on a dataset
#
# INPUT   :
#         :   Data Frame     - train              - train dataset
#             Data Frame     - test               - test dataset
#             character      - predictorField     - the field we are predicting 
#             integer        - forestSize         - the size of the forest to create
#             character      - title              - title for the forest
#             boolean        - plot               - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
createForest<-function(train,test,predictorField,forestSize,title = "Importance for Random Forest",plot=TRUE) {
  
  # Need to produce a data frame from the predictor fields and a vector for the output
  outputClassIndex <- which(names(train) == predictorField)
  inputs <- train[-outputClassIndex]
  output <- train[, outputClassIndex]
  
  rf<-randomForest::randomForest(inputs,
                                 factor(output),
                                 ntree=forestSize,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(inputs)))
  
  if (plot){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Weight"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=title)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(rf)
}

# ************************************************
# createAndEvaluateForest() :
#
# Creates Random Forest on a dataset and evaluates it
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
createAndEvaluateForest<-function(train,test,predictorField,i,load,forestSize,title = "Importance for Random Forest",showInViewer=F,plot=TRUE){
  
  if (!load) {
    
    rf <- createForest(train, test, predictorField, forestSize, title, plot=plot)
    
    if (!dir.exists("Tree_Models")) {
      dir.create("Tree_Models")
    }
    
    saveRDS(rf, paste0("Tree_Models/_rf_", i, ".rds"))
  } else {
    rf <- readRDS(paste0("Tree_Models/_rf_", i, ".rds"))
  }
  
  treeClassifications <- getTreeClassifications(rf, test, predictorField)
  treeMetrics <- getTreeMetrics(treeClassifications, test, predictorField)
  
  if (showInViewer) {
    metricsView <- as.data.frame(as.matrix(treeMetrics))
    colnames(metricsView) <- "Random Forest Holdout Measures"
    print(formattable::formattable(metricsView))
  }
  
  return(treeMetrics)
} #endof createAndEvaluateForest()

# ************************************************
# getTreeMetrics() :
#
# Generates metrics for a decision tree
#
# INPUT   :
#             Data Frame     - treeClassifications       - classifications made by a decision tree
#             Data Frame     - testDataset               - test dataset used to generate the classifications
#             character      - predictorField            - the name of the predictor field in the dataset 
#             double         - classLabelDouble          - the class label for the predictor field if a double (default=1)
#             character      - classLabelChar            - the class label for the predictor field if a character (default=NULL)
#
# OUTPUT
#         :   Data Frame     - metrics                   - a range of performance metrics for the tree on the test dataset
#
# ************************************************
getTreeMetrics <- function(treeClassifications, testDataset, predictorField, classLabelDouble = 1, classLabelChar = NULL) {
  # Need to find the position of the (positive) column label
  if (!is.null(classLabelChar)) {
    classIndex <- which(colnames(treeClassifications) == classLabelChar)
  } else {
    classIndex <- which(as.numeric(colnames(treeClassifications)) == classLabelDouble)
  }
  
  # Get the probabilities for classifying the (positive) outcome
  predictedProbabilities <- treeClassifications[, classIndex]
  
  # Use the expected values from the actual test set to compare with the predictions
  expectedResults <- testDataset[, which(colnames(testDataset) == predictorField)]
  
  # If the class label was of type character, we need to convert the positive class to a '1', and everything else to '0'
  if (!is.null(classLabelChar)) {
    resultAsDouble <- vector()
    for (i in 1:length(expectedResults)) {
      if (expectedResults[i] == classLabelChar) {
        resultAsDouble <- append(resultAsDouble, 1)
      } else {
        resultAsDouble <- append(resultAsDouble, 0)
      }
    }
    expectedResults <- resultAsDouble
  }
  
  
  metrics <- calculateThreshold(predictedProbabilities, expectedResults,"Tree Model")
  
  return(metrics)
}


# ************************************************
# kFoldModel() :
#
# 
#
# INPUT   :    FUN - Function Name (Model)
#              dataset - dataset contained in data.frame
#              outputField - output field we are prediciting
#           
#
# OUTPUT :     returns the confusion matrix details. 
# ************************************************


kFoldModel <- function(FUN,dataset,outputField,...){
  
  results <- data.frame()
  
  #Iterate from 1 to number of folds and train / test a model for each.
  for (i in 1:K_FOLDS) {
    
    #Create the training set consisting of K-1 of the folds,
    #and the testing set of 1 of the folds.
    separatedData<-kFoldTrainingSplit(dataset,i)
    
    #Call the model function passed in the argument with the testing and training data
    #as well as the value of i so the model can be saved and loaded later.
    modelMeasures<-FUN(train=separatedData$train,
                       test=separatedData$test,outputField,i,...)
    
    #Bind the results list to the result data frame
    results <- rbind(results, modelMeasures)
    
  }
  
  #Average the results from all K models.
  resultMeans<<-colMeans(results)
  #Change columns 1 through 4, so TP, FN, TN, FP to an integer
  resultMeans[1:4]<-as.integer(resultMeans[1:4])
  
  
  #Depending on what model was called into the function
  #We want to produce the model accuracy statistics and also 
  #Plot a confusion matrix.
  #The code below does this.
  if (deparse(substitute(FUN)) == "train_MLP_Model"){
    plotConfusionMatrix(as.list(resultMeans), "MLP Model Stratified Cross Validation Confusion Matrix")
    confRes <- as.data.frame(as.matrix(resultMeans))
    colnames(confRes) <- "MLP Stratified Cross Validation Measures"
    print(formattable::formattable(round(confRes, 2)))
  }
  if (deparse(substitute(FUN)) == "createAndEvaluateDT") {
    plotConfusionMatrix(as.list(resultMeans), "Decision Tree Stratified Cross Validation Confusion Matrix")
    confRes <- as.data.frame(as.matrix(resultMeans))
    colnames(confRes) <- "Decision Tree Stratified Cross Validation Measures"
    print(formattable::formattable(round(confRes, 2)))
  }
  if (deparse(substitute(FUN)) == "createAndEvaluateForest"){
    plotConfusionMatrix(as.list(resultMeans), "Forest Stratified Cross Validation Confusion Matrix")
    confRes <- as.data.frame(as.matrix(resultMeans))
    colnames(confRes) <- "Random Forest Stratified Cross Validation Measures"
    print(formattable::formattable(round(confRes, 2))) 
  }
  
  #Return the average results as a list.
  return(as.list(resultMeans))
}

