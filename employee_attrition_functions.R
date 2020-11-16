# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

# ************************************************
# basicStatistics()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
basicStatistics<-function(dataset,...){
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}



# ************************************************
# NPREPROCESSING_setInitialFieldType() :
#
# Set  each field for NUMERIC or SYNBOLIC
#
# INPUT:
#        String - name - name of the field to manually set
#        String - type - manual type
#
# OUTPUT : None
# ************************************************
# ************************************************
# FieldTypes() :
#
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
FieldTypes<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}

# ****************
# oneHotEncoding() :
#   Pre-processing method to convert appropriate 
#   categorical fields into binary representation
#
# INPUT       :   dataframe - dataset           - dataset to one hot encode
#                 vector    - fieldsForEncoding -  
#
# OUTPUT      :   Encoded fields
# ****************
oneHotEncoding<-function(dataset,fieldsForEncoding){
  # Combine input fields for encoding
  stringToFormulate <- substring(paste(" + ", fieldsForEncoding, sep = "", collapse = ""), 4)
  
  OHEFormula <- as.formula(paste("~",stringToFormulate))
  
  # One hot encode fields listed in function
  dmy <- dummyVars(OHEFormula, data = dataset)
  trsf<- data.frame(predict(dmy, newdata = dataset))
  
  # Combine the encoded fields back to the originalDataset
  encodedDataset <- cbind(dataset,trsf)
  
  # Remove original fields that have been hot encoded
  newData<- encodedDataset %>% select(-c(fieldsForEncoding))
  # Return new dataset
  return(newData)
}


# ************************************************
# normalise() :
#   Normalise fields between 1 and 0
#
# INPUT       :   Fields to normalise
#
# OUTPUT      :   Normalised fields between 1 and 0
# ************************************************
normalise <- function(values) {
  return ((values - min(values)) / (max(values) - min(values)))
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
# DECISION TREE CONVERT DT RULES TO ASCII FORMATTED RULES
#
# <anticedent 1> AND <anticedent 2> ...
# Each anticedent is: [field][comparision][value]
#
# INPUT: Object - tree - Trained tree
#
# OUTPUT: data frame of rules, class and anticedents
# ************************************************
DecisionTreeRules<-function(tree){
  #library(stringr) already imported in the main script file
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  dataFrameWithRules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  dataFrameWithRules<-setNames(dataFrameWithRules,c("Rule","Class","Anti"))
  
  numberofrules<-tree$size
  
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
    warning("More than one tree have been found. Extracting and displaying rules for just the first one")
  }
  
  totalAnticedents<-0
  for (ruleNumber in 1:numberofrules){
    start<-regexpr("\\*\\*",x)[1]+2
    end<-regexpr("->",x)[1]-3
    onerule<-substr(x,start,end) #Single rule, anticedents seperated by '**'
    onerule<-gsub("\\*\\*"," AND ",onerule) #Rule now has "AND" between anticedents
    #onerule<-convertNormalisedDTRuleToRealWorld(onerule)
    NumAnticedents<-str_count(onerule,"AND")+1
    totalAnticedents=totalAnticedents+NumAnticedents
    classpos<-regexpr("class ",x)+6
    classID<-as.numeric(substr(x,classpos,classpos))  #This has the class of the rule, i.e. {0,1}
    dataFrameWithRules$Rule[ruleNumber]<-onerule
    dataFrameWithRules$Class[ruleNumber]<-ifelse(classID==0,"LEAVE","STAY") # Convert class to label
    dataFrameWithRules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  print(formattable::formattable(dataFrameWithRules))
  return(dataFrameWithRules)
  
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
areaUnderCurve <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}


# ************************************************
# calculateMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        attrYes   - double - precision for "Yes" (values are 1) measure
#        attrNo    - double - precision for "No" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
# ************************************************
calculateMeasures<-function(TP,FN,FP,TN){
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "attrNo"=  100.0*(TP/(TP+FP)),
                  "attrYes"= 100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}


# ************************************************
# calculateConfusion() :
#
# Calculate a confusion matrix for 2-class classifier - Yes/1 and No/0
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the  entries from calculateMeasures()
# ************************************************
# calculateConfusion<-function(expectedClass,predictedClass,threshold){
calculateConfusion<-function(expectedClass,predictedClass){
  
  #Assign the resulting table into our 'confusion'variable 
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # The following bit places the information from the table above in our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  # TH<-as.double(threshold)
  
  # return(calculateMeasures(TP,FN,FP,TN,TH))
  return(calculateMeasures(TP,FN,FP,TN)) 
  
} #endof calculateConfusion()


# ************************************************
# calculate TreeMetrics() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
calculateTreeMetrics<-function(predicted,expected,threshold) {
  
  predictedClass<-ifelse(predicted<threshold,0,1)
 
  results<-calculateConfusion(expectedClass=expected,
                              predictedClass=predicted)
  
  return(results)
} #endof calculate TreeMetrics()

# ************************************************
#  plot ThresholdGraph() :
#
# Use data generated by calculate Threshold() to plot threshold graph
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
  
} # endof plot ThresholdGraph()






# ************************************************
# calculate Threshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Determine "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - predicted   - probability of being class 1
#         :   vector double  - expected    - dataset to evaluate
#         :   boolean        - plot             - TRUE=output charts
#         :   string         - title            - chart title
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
#                        - Threshold at min Euclidean distance
#                        - AUC - area under the ROC curve
#                        - Predicted class probability
# ************************************************
calculateThreshold<-function(predicted,
                             expected,
                             plot=TRUE,
                             title=""){
  toPlot<-data.frame()
  
  
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-calculateTreeMetrics(predicted=predicted,
                                  expected=expected,
                                  threshold=threshold)
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
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]
  
  #myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  
  
  #   # TODO: Don't call this function from here, call it from within getTreeMetrics. Just return the threshold from this function
  #   #Use the "best" distance threshold to evaluate classifier
  #   results<-calculateTreeMetrics(test_predicted=predicted,
  #                                test_expected=expected,
  #                                threshold=myThreshold)
  
  results$threshold<-myThreshold
  results$AUC<-areaUnderCurve(score=predicted,bool=expected) # Estimate the AUC
  # Plot threshold graph  
  if (plot==TRUE){
    plotThresholdGraph(toPlot, maxYoudan, minEuclidean,
                       auc = results$AUC, title)
  } # endof if plotting
  
  return(results$threshold)
} #endof calculate Threshold()

# ************************************************
# createDT() :
#
# Creates A C5 Decision Tree from training data
#
# INPUT   :
#             Data Frame     - train                 - train dataset
#             charatcter     - predictorField        - the name of the predictor field in the dataset
#             boolean        - plot                  - if true, also plots tree rules
#
# OUTPUT
#         :   Data Frame     - measures    -  the performance metrics of the tree
#
# ************************************************
createDT <- function(train, predictorField, plot = F) {
  # Need to produce a data frame from the predictor fields and a vector for the output
  outputClassIndex <- which(names(train) == predictorField)
  inputs <- train[-outputClassIndex]
  output <- train[, outputClassIndex]
  
  tree<-C50::C5.0(x=inputs, y=factor(output), rules=T, trials=1)
  
  
  if (plot){    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]    
    print(formattable::formattable(importance))    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main="Basic C5.0")    
  }
  
  return(tree)
} #endof createDT()

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
  
  # TODO: Calculate the threshold, then produce the other metrics and return the dataframe to metrics
  # threshold <- calculateThreshold(predictedProbabilities, expectedResults)
  # metrics <- calculateTreeMetrics(predictedProbabilities, expectedResults, threshold)
  # return(metrics)
    measures <- calculateThreshold(predictedProbabilities, expectedResults, title="")
    return(measures)
}


# ************************************************
# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
# ************************************************
# Uses histogram
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      scaled_column<-normalise(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      barplot(bins, main=paste(graphTitle,field_types[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")
      #Bar chart helps visulisation. Type of field is the chart name
      
      
    } #endif numeric types
  } #endof for
  return(field_types)
}



# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}


# ************************************************
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){
  
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}



# ************************************************
# stratifyDataset() :
#
# Separate the classes in the dataset (AttritionYes = 1, and = 0)
#
# For each of the classes, calculate the amount of records that 
# will appear in each of the folds.
# Give each of these groups of records their own fold id.
#
# Combine the two classes back together into a single data.frame and randomize.
#
# Data is now ready to be used for the kFoldTrainingSplit() and kFoldModel()
# functions.
# 
# Return the combined data.frame.
#
#
# INPUT   : dataset - data.frame - a normalised dataset ready for stratification.
#         : output - string - String containing the classes to predict (AttritionYes).
#         : folds - Integer - Number of folds to be used in the Stratified Cross Validation.
#
#
# OUTPUT  : stratifiedData - data.frame - the stratified dataset ready for Cross Validation.
# ************************************************


stratifyDataset <- function(dataset, output, folds){
  
  #Create a variable containing all the unique classes in the column of our output variable 
  uniqueClasses <- unique(dataset[,output])
  
  #Create a variable containing all the row positions where class = uniqueClasses[1], in our case. 1 = Yes, (leaving job)
  rowPositions <- which(dataset[,output]==uniqueClasses[1])
  
  #Create two data frames, one containing all rows in which employees are leaving, and the other in which the employees are staying
  leavingYes<- dataset[rowPositions,]
  leavingNo<- dataset[-rowPositions,]
  
  #Print the totals for each class
  print(paste("Number of people leaving: ", nrow(leavingYes)))
  print(paste("Number of people staying: ", nrow(leavingNo)))
  
  #Create a list for each of the data frames, which contains the fold sequence (1,2,3,4...) up to the amount of rows per data frame.
  #If there are 200 rows, and 10 folds, the sequence of 1 to 10(Folds), will repeat 200/10 times, so 20 times.
  foldSequenceYes <- rep(seq(1:folds),ceiling(nrow(leavingYes)/folds))
  foldSequenceNo  <- rep(seq(1:folds),ceiling(nrow(leavingNo)/folds))
  
  #Creates a new column and appends it to each of the data frames. The columns content is of each of the sequence lists above into it. 
  leavingYes$foldIds <- foldSequenceYes[1:nrow(leavingYes)]
  leavingNo$foldIds  <- foldSequenceNo[1:nrow(leavingNo)]
  
  #Bind the two data frames back together, now each of them have their assigned folds.
  stratifiedData<-rbind(leavingYes, leavingNo)
  
  #Randomise the new data frame before returning it.
  stratifiedData <- stratifiedData[sample(nrow(stratifiedData)),]
  
  #Returns the combined and randomised data frame.
  return(stratifiedData)
  
} 

# ************************************************
# kFoldTrainingSplit() :
#
# Separates a stratified dataset into training and testing data.frames
# based on the desired fold
#
# INPUT   : dataset - data.frame - Stratified dataset.
#         : fold - integer - FoldIds number for the test set
#
# OUTPUT  : separatedData - List - list containing two data.frames, test and train
#
#
#*************************************************

kFoldTrainingSplit <- function(dataset, fold){
  
  #Create a data.frame containing all of the rows with FoldIds == fold.
  testSet <- subset(dataset, subset = foldIds==fold)
  
  #Create a data.frame contraining the rest of the rows, with FoldIds != fold.
  trainingSet <- subset(dataset, subset = foldIds!=fold)
  
  #Merge the two data.frames into a single list, ready to be returned.
  separatedData <- list(test=testSet, train=trainingSet)
  
  #Return the separated data in list form ready for modelling. 
  return(separatedData)
  
}


# ************************************************
# kFoldModel() :
#
# 
#
# INPUT   : dataset - dataset contained in data.frame
#           FUN - Function Name (Model)
#
# OUTPUT : None
# ************************************************

#NEEDS TO BE FINISHED - NEED TO KNOW MORE ABOUT PLOT AND MEASURE RESULTS FIRST.
kFoldModel <- function(dataset,FUN,...){
  
  results <- data.frame()
  
  for (i in 1:K_FOLDS) {
    
    separatedData<-kFoldTrainingSplit(dataset,i)
    
    modelMeasures<-FUN(train=separatedData$train,
                       test=separatedData$test)
    
  }
  
}