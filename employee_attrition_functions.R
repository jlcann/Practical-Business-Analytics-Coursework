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
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myTree        - tree
#         :   Data Frame     - testDataset - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   int            - classLabel   - lable given to the positive (TRUE) class
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
 # if (plot==TRUE)
 #  NprintMeasures(results=measures,title=title)
  
  return(measures)
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
NDT5RuleOutput<-function(tree){
  #library(stringr)
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  df_of_rules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  df_of_rules<-setNames(df_of_rules,c("Rule","Class","Anti"))
  
  numberofrules<-tree$size
  # 271019 allow for multiple trees (i.e. boosted)
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
    warning("Prof Nick says: More than one tree found. Extracting rules for just the first")
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
    df_of_rules$Rule[ruleNumber]<-onerule
    df_of_rules$Class[ruleNumber]<-ifelse(classID==0,"BAD","GOOD") # Convert class to label
    df_of_rules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  return(df_of_rules)
}

# ************************************************
# Nauroc() :
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
auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}


# ************************************************
# NcalcMeasures() :
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
#        pgood     - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
#
# 080819NRT added TNR measure
# ************************************************
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


# ************************************************
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the  entries from NcalcMeasures()
#
# 070819NRT convert values to doubles to avoid integers overflowing
# Updated to the following definition of the confusion matrix
#
# A good loan is indicated when $Status=1 and bad when $Status=0

#                    ACTUAL
#               ------------------
# PREDICTED     GOOD=1   |  BAD=0
#               ------------------
#     GOOD=1      TP     |    FP
#               ==================
#     BAD=0       FN     |    TN
#
#
# ************************************************
NcalcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # This "converts" the above into our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
} #endof NcalcConfusion()


# ************************************************
# NEvaluateClassifier() :
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
NEvaluateClassifier<-function(test_predicted,test_expected,threshold) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  
  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)
  
  return(results)
} #endof NEvaluateClassifier()

# ************************************************
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - test_predicted   - probability of being class 1
#         :   vector double  - test_expected    - dataset to evaluate
#         :   boolean        - plot             - TRUE=output charts
#         :   string         - title            - chart title
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
#                        - Threshold at min Euclidean distance
#                        - AUC - area under the ROC curve
#                        - Predicted class probability
#
# 241019NRT - added plot flag and title for charts
# 311019NRT - added axis bound checks in abline plots
# 191020NRT - Updated to use own ROC plot & calculate AUC
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
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
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
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
    
    # ************************************************
    # 121020NRT ROC graph
    
    sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
    specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
    auc<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
    
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
    
  } # endof if plotting
  
  # Select the threshold - I have choosen distance
  
  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  results$threshold<-myThreshold
  results$AUC<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
  
  return(results)
} #endof myPerformancePlot()

# ************************************************
# simpleDT() :
#
# Create C5 Decision Tree on the raw dataset
# A decision tree may not need the dataset to be pre-processed
#
# INPUT   :
#             Data Frame     - train       - original train dataset
#             Data Frame     - test        - original test dataset
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
simpleDT<-function(train,test,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  tree<-C50::C5.0(x=train_inputs,
                  y=factor(train_expected),
                  rules=TRUE,
                  trials=1)
  
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title="Original Dataset. DT C5.0")
  
  #281019NRT addfed this Function to output the tree as rules to a file
  if (plot==TRUE){
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main="Basic C5.0")
    
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
  }
  
  return(measures)
} #endof simpleDT()




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