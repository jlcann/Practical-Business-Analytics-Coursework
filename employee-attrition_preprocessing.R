#*
#*List of functions in this file:
#*
#*preprocessing()
#*
#*
#*oneHotEncoding()
#*
#*
#*normalise()
#*
#*NPREPROCESSING_discreetNumeric()--> Function taken from Prof. Nick Ryman-Tubb
#*                                    lab session 4.   
#*
#*NPREPROCESSING_outlier()----------> Function taken from Prof. Nick Ryman-Tubb
#*                                    lab session 4.
#*
#*NplotOutliers()-------------------> Function taken from Prof. Nick Ryman-Tubb
#*                                    lab session 4.
#*                                    
#*stratifyDataset()
#*
#*
#*discretiseFields()
#*
#*createHoldoutDataset()
#*
#*
#*checkforOutliers()

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()



# ************************************************
# preprocessing() :
# data preprocessing function
#
# INPUT       :   dataframe - originalDataset - the original dataset 
#
# OUTPUT      :   dataframe - normalisedDataset - dataset set to be used for the ML models
# ************************************************
preprocessing <- function(originalDataset, field_types){
  
  print("Starting Data Pre-Processing Stage")
  
  field_types<-FieldTypes(originalDataset)
  
  # Check for NA fields in original dataset
  print("Checking fields for missing data. Number of missing data in each field:")
  print(sapply(originalDataset,function(x) sum(is.na(x))))
  
  # Determine if NUMERIC fields are DISCREET or ORDINAL
  field_Types_Discreet_Ordinal<- NPREPROCESSING_discreetNumeric(originalDataset,field_types,DISCREET_BINS)
  discreet_fields <- names(originalDataset)[field_Types_Discreet_Ordinal=="DISCREET"]
  
  results<-data.frame(field=names(originalDataset),initial=field_types,types1=field_Types_Discreet_Ordinal)
  print(formattable::formattable(results))
  
  # Discreet subset 
  discreetDataset<-originalDataset[,which(field_Types_Discreet_Ordinal==TYPE_DISCREET)]
  
  # Ordinals subset
  ordinals<-originalDataset[,which(field_Types_Discreet_Ordinal==TYPE_ORDINAL)]
  
  fields_for_discreet <- c("Age", "DailyRate", "DistanceFromHome", "MonthlyIncome", "HourlyRate", 
                           "MonthlyRate")
  
  # Create Bins and complete dataset before normalisation
  ordinalBinsDataset <- discretiseFields(ordinals, fields_for_discreet)
  
  # Symbolic subset
  symbolicDataset<-originalDataset[,which(field_types==TYPE_SYMBOLIC)]
  
  # One hot encode the following fields: Gender, OverTime, Attrition, MaritalStatus
  fieldsForEncoding <- c("Gender", "OverTime", "Attrition", "MaritalStatus", "BusinessTravel", "Department", "JobRole", "EducationField")
  oneHotDataset <- oneHotEncoding(dataset = symbolicDataset, fieldsForEncoding =fieldsForEncoding)
  
  # Create factors for ordered categorical fields
  newSymbolicDataset <- oneHotDataset %>% mutate(across(c(Over18),
                                                        ~as.numeric(as.factor(.))))
  
  # Combine symbolic and numeric datasets
  dataBeforeNormalisation<-cbind(ordinalBinsDataset,discreetDataset,newSymbolicDataset)
  
  # Test if any ordinals are outliers and replace with mean values
  #ordinalsDataset <- NPREPROCESSING_outlier(ordinals = ordinalBinsDataset, OUTLIER_CONFIDENCE)
  checkForOutliers(dataBeforeNormalisation)
  
  if(all(sapply(dataBeforeNormalisation, is.numeric ))){
    print("All Fields Are Numeric")
  }
  
  # remove fields that have zero variance
  toRemove <- nearZeroVar(dataBeforeNormalisation, freqCut = FREQCUT) 
  removedCols <- colnames(dataBeforeNormalisation)[toRemove]
  print(paste("Removing the following columns as all values are the same"))
  print(removedCols)
  dataBeforeNormalisation <- dataBeforeNormalisation[, -toRemove]
  
  # Calculate correlation matrix
  corMatrix <- cor(dataBeforeNormalisation)
  
  # find attributes that are highly corrected
  highlyCorrelated <- findCorrelation(corMatrix, CUTOFF)
  
  #names of highly correlated fields
  highlyCorCol <- colnames(dataBeforeNormalisation)[highlyCorrelated]
  print("Removing the following columns due to high correlation")
  print(highlyCorCol)
  
  # remove highly correlated fields
  dataForNormalisation <- dataBeforeNormalisation[, -which(colnames(dataBeforeNormalisation) %in% highlyCorCol)]
  dim(dataForNormalisation)
  
  # normalise dataset using function above
  normalisedDataset <- as.data.frame(lapply(dataForNormalisation,normalise))
  
  # Remove employee number from normalised dataset
  normalisedDataset<-subset(normalisedDataset, select = -EmployeeNumber)
  
  # Transforms all inputs to a logistic model mapped against AttritionYes
  logisticModelTransformAllInputs<-glm(AttritionYes~.,family=binomial(link='logit'),data=normalisedDataset)
  
  # Use caret library to determine scaled "importance"
  importance<-as.data.frame(caret::varImp(logisticModelTransformAllInputs, scale = TRUE))
  
  leastImportance <- rownames(importance %>% filter(Overall < 0.1))
  
  print("Removing the following fields as they have an importance rating of less than 0.1")
  print(leastImportance)
  
  normalisedDataset <- select(normalisedDataset, -leastImportance)
  
  # Plot the % importance ordered from lowest to highest
  barplot(t(importance[order(importance$Overall),,drop=FALSE]), las = 2, border = 0, cex.names = 0.8)
  
  randomisedDataset <- normalisedDataset[sample(nrow(normalisedDataset)),]
  
  print("Data Pre-Processing Completed - Dataset Ready For ML Models")
  
  return(randomisedDataset)
}


# ****************
# oneHotEncoding() :
#   Pre-processing method to convert appropriate 
#   categorical fields into binary representation
#
# INPUT       :   dataframe - dataset           - dataset to one hot encode
#                 vector    - fieldsForEncoding - fields to be one hot encoded
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
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,bins_cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      
      scaled_column<-normalise(dataset[,field])
      
      
      #Generate the "bins_cutoff" points for each of 10 bins
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
      
      #If the number of bins with less than 1% of the values is greater than the bins_cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>bins_cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      
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
  
  #Merge the two data.frames  into a single list, ready to be returned.
  #Remove the foldIds field from each of them to not skew the models. 
  separatedData <- list(test=subset(testSet, select = -foldIds), train = subset(trainingSet, select = -foldIds))
  
  #Return the separated data in list form ready for modelling. 
  return(separatedData)
  
}

# ************************************************
# discretiseFields() :
#
# Function for creating bins for the dataset
#
# INPUT   : dataset - data.frame - dataset to be used
#         : fields  - vector     - vector of fields to be binned from the dataset
#
# OUTPUT  : dataset - data.frame - dataset with the binned data 
#
#*************************************************

discretiseFields <-function(dataset, fields){
  
  for (i in fields){
    dataset[,i] <-as.numeric(bin(dataset[,i], nbins=10, labels=c(1:10), method = "content"))
  }
  
  return(dataset)
  
}

# ************************************************
# createHoldoutDataset() :
#
# Function for creating the holdout data
#
# INPUT   : dataset - data.frame - dataset to be used
#         : holdout - integer    - value to be used as the holdout value 
#
# OUTPUT  : list - list of datasets with the training and testing datasets  
#
#*************************************************

createHoldoutDataset <-function(dataset, holdout){
  trainingSampleSize <- round(nrow(dataset))*(HOLDOUT/100)
  
  #Create the training Set
  trainingSet <- dataset[1:trainingSampleSize,]
  
  #Create the test Set
  testSet <- dataset[-(1:trainingSampleSize),]
  
  return(list(training = trainingSet, test = testSet))
}

# ************************************************
# checkForOutliers() :
#
# Function to check the dataset for outliers
#
# INPUT   : dataset - data.frame - dataset to be used
#
# OUTPUT  : NA
#
#*************************************************

checkForOutliers <- function(dataset){
  sum <- 0
  for (i in colnames(dataset)){
    boxplots = boxplot(dataset, plot = FALSE)$i
    
    which(dataset %in% boxplots)
    #print(paste("There are", length(which(dataset %in% boxplots)), "Outliers for the field", i))
    sum = sum + length(which(dataset %in% boxplots))
    
    # cant figure out how to change to mean instead of removing
    dataset[ !(dataset %in% boxplots) ]
    
  }
  
  print(paste("There Are ", sum, " Outliers In The Dataset"))
}
