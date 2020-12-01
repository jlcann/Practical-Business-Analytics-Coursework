#*
#* List of functions in this file:
#*
#*
#* basicStatistics()--> Function taken from Prof. Nick Ryman-Tubb
#*                      lab session 4.
#* 
#* FieldTypes()-------> Function taken from Prof. Nick Ryman-Tubb
#*                      lab session 4.
#* 
#* boxplotAll()
#* 
#* 
#* attritionBars()
#* 
#* 
#* explorationPlots()
#* 
#* 
#* dataExploration()
#*













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
# FieldTypes() :
#
# Test each field for NUMERIC or SYMBOLIC
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
# boxplotAll() :
#   Exploration method to plot a boxplot for each 
#   field to visualize data and see outliers
#
# INPUT       :   dataframe - dataset           - dataset to boxplot
#
# OUTPUT      :   boxplot for each field in dataset
# ****************
boxplotAll <-function(dataset){
  # Iterate through dataset
  for(field in 1:(ncol(dataset))){
    
    # Create a boxplot for each field in dataset
    boxplot(dataset[field],
            main = paste("Boxplot for: ", names(dataset[field])),
            xlab = "Units",
            ylab = names(dataset[field]),
            col = "#FF3F3F",
            border = "black",
            horizontal = TRUE
    )
  }
}


# ****************
# attritionBars() :
#   Exploration method to plot a barplot of the top 5 
#   important field against Attrition
#
# INPUT       :   dataframe - dataset           - dataset to barplot
#                 field - field                 - field to plot against
#
# OUTPUT      :   barplot for each field in dataset
# ****************
attritionBars <- function(dataset, field){ 
  
  # Iterate through dataset
  for (i in colnames(dataset)){
    
    # Check if i in dataset is equal to the field being plotted against
    if (i == field){
      # If equal to field then skip
      next
    } else {
      # Plot boxplot for each field in dataset
      graph <- dataset %>%
        ggplot(aes_string(x = i, group = field)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
                 stat="count", 
                 alpha = 0.7) +
        geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
                  stat= "count", 
                  vjust = -0.4) +
        labs(y = "Percentage %", fill= i) +
        facet_grid(as.formula(paste("~", field))) +
        theme_minimal()+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face="bold", size=16)) + 
        ggtitle(paste("Change of percentage on ", i, " when Attrition is No or Yes"))
      
      # Print the boxplot
      print(graph)
    }
  }
  
}


# Main function for plotting graphs and plots for data exploration
explorationPlots <-function(originalDataset, field_types) {
  
  print("Running Data Exploration Function Plots")
  
  #Print statistics of originalDataSet into the viewer.
  basicStatistics(originalDataset)
  
  # Barplot for Attrition count
  # Plots the total number of Attrition being Yes or No
  barplotAttrition <- originalDataset %>% 
    group_by(Attrition) %>%
    tally() %>%
    ggplot(aes(x = Attrition, y = n,fill=Attrition)) +
    geom_bar(stat = "identity") +
    theme_minimal()+
    scale_fill_manual(values=c("#FF3F3F", "#9EFF95"))+
    labs(x="Attrition", y="Count of Attrition")+
    ggtitle("Attrition Count")+
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=16))+
    geom_text(aes(label = n), vjust = -0.4, position = position_dodge(0.7))
  
  # Print the Attrition barplot
  print(barplotAttrition) 
  
  
  # Barplot job role against Attrition
  # Plots the ratio of attrition for each job role
  jobRoleAttrition <- originalDataset %>%
    ggplot(mapping = aes(x = JobRole)) +
    scale_fill_manual(values=c("#FF3F3F", "#9EFF95")) +
    geom_histogram(aes(fill = Attrition), stat = "count") +
    stat_count(aes(y=..count..,label=..count.., group = Attrition),geom="text",vjust=-1) +
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face="bold", size=16)) +
    labs(x="Job Role", y="Attrition count")+
    ggtitle("Attrition in regards to Job Role")
  
  # Print the Job Role against Attrition barplot
  print(jobRoleAttrition)
  
  
  # Barplot Monthly income against Attrition
  # Plots the ratio of attrition for each monthly income bin
  monthlyIncomeAttrition<-originalDataset %>%
    ggplot(mapping = aes(x = MonthlyIncome)) + 
    scale_fill_manual(values=c("#FF3F3F", "#9EFF95"))+
    geom_histogram(aes(fill = Attrition), binwidth = 2000, boundary = 0)+
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face="bold", size=16))+
    labs(x="Monthly Income ($)", y="Attrition count")+
    scale_x_continuous(breaks = seq(from = round(min(originalDataset$MonthlyIncome), digits = -10),
                                    to = round(max(originalDataset$MonthlyIncome), digits = -1), by = 2000))+
    ggtitle("Attrition in regards to Monthly Income")
  
  # Print the Monthly Income against Attrition barplot
  print(monthlyIncomeAttrition)
  
  # Boxplot all numeric fields for general exploration of dataset
  boxplotAllFields<-boxplotAll(originalDataset[,which(field_types==TYPE_NUMERIC)])
  
  
  # Pearson correlation plot for all numeric fields
  # Visualise the correlation between each field in the dataset
  print(ggcorr(originalDataset[,which(field_types==TYPE_NUMERIC)], method = c("everything", "pearson"))+
          ggplot2::labs(title = "Correlation plot for all numeric fields in dataset") +
          theme(plot.title = element_text(hjust = 0.5, face="bold", size=16)))
  
  
  # Boxplot Monthly Income for each Job Level
  boxplotIncomeJob <- boxplot(MonthlyIncome~JobLevel,
                              data=originalDataset,
                              main="Boxplots for Monthly Income against Job Level",
                              xlab="Job Level",
                              ylab="Monthly Income ($)",
                              col="#FF3F3F",
                              border="black"
  )
  
  # Boxplot Monthly Income for Gender
  boxplotIncomeGender <- boxplot(MonthlyIncome~Gender,
                                 data=originalDataset,
                                 main="Boxplots for Monthly Income against Gender",
                                 xlab="Gender",
                                 ylab="Monthly Income ($)",
                                 col="#9EFF95",
                                 border="black"
  )
  
  # Boxplot Monthly Income for OverTime Yes and No
  boxplotIncomeOverTime <- boxplot(MonthlyIncome~OverTime,
                                   data=originalDataset,
                                   main="Boxplots for Monthly Income against Over Time",
                                   xlab="OverTime",
                                   ylab="Monthly Income ($)",
                                   col="#FF3F3F",
                                   border="black"
  )
  
  # Boxplot Monthly Income for each Job Role
  boxplotIncomeJob <- boxplot(MonthlyIncome~JobRole,
                              data=originalDataset,
                              main="Boxplots for Monthly Income against Job Role",
                              xlab="Job Role",
                              ylab="Monthly Income ($)",
                              col="#FF3F3F",
                              border="black"
  )
  
  
  # Plot scatterplot for Monthly Income against Total Working Years (experience)
  scatterIncomeExperience <- ggplot(originalDataset, aes(x=TotalWorkingYears, y=MonthlyIncome)) +
    geom_point(size=2, shape=1) +
    labs(x="Total Working Years", y="Monthly Income ($)")+
    geom_smooth(method=lm,color="#FF3F3F")
  
  # Plot scatterplot for Monthly Income against Years At Company (loyalty)
  scatterIncomeLoyalty <- ggplot(originalDataset, aes(x=YearsAtCompany, y=MonthlyIncome)) +
    geom_point(size=2, shape=1) +
    labs(x="Total years at the Company", y="Monthly Income ($)")+
    geom_smooth(method=lm,color="#9EFF95")
  
  # Join the scatterplots on one page for Monthly Income based on experience and loyalty for comparison
  doubleScatterPlot <- ggarrange(scatterIncomeExperience, scatterIncomeLoyalty,
                                 labels = c("Experience", "Loyalty"),
                                 ncol = 2, nrow = 1)
  
  # Print the scatterplots for experience and loyalty
  print(doubleScatterPlot)
  
  
  # Density hex plot of Age against MonthlyIncome
  # Visualise the average colleague age and income
  densityAgeIncome<-ggplot(originalDataset, aes(x=Age, y=MonthlyIncome) ) +
    labs(x="Age in years", y="Monthly Income ($)")+
    geom_hex(bins = 8) +
    scale_fill_continuous(type = "viridis") +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=16)) +
    ggtitle("Density plot for Age against Monthly Income")
  
  # Print density plot of age against monthly income
  print(densityAgeIncome)
  
  
  # Dataset of the 5 most impactful fields on Attrition
  # based on analysis from pre-processing
  highImportanceDataset <- subset(originalDataset, select=c(JobInvolvement, JobSatisfaction, NumCompaniesWorked,
                                                            EnvironmentSatisfaction, OverTime, Attrition))
  
  
  # Bar plots for the high importance fields against attrition
  barplotImportantFields <- attritionBars(highImportanceDataset, "Attrition")
  
  print("End of Data Exploration")
  
}


dataExploration <- function(dataset){
  field_types<-FieldTypes(dataset)
  allPlots <- explorationPlots(dataset, field_types)
}
