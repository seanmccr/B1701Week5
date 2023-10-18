# B1701 Week 5 | Talent Identification in R | 11.10.23

# ------ Install & Load Packages ------ 

install.packages("corrplot")
install.packages("fmsb")
install.packages("summarytools")
install.packages("rpart.plot")
library(summarytools)
library(tidyverse)
library(moments)
library(rpart.plot)
library(corrplot)
library(fmsb)


# ----- Loading Data -----
CyclingData <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 5/Week5TalentIDFiles/CyclingTI.csv")

# ----- Cleaning Data -----
str(CyclingData)

# Changing the date of birth variable type
CyclingData$Date_of_Birth <- as.character.Date(CyclingData$Date_of_Birth)

# Identifying 'NA' inputs and the sum of 'NA' inputs per variable
MissingData <- is.na(CyclingData)
print(MissingData)
ColMissData <- colSums(MissingData)
print(ColMissData)

# Code to identify duplicated data
which(duplicated(CyclingData))

# Filtering the data
FilteredCyclingData <- CyclingData %>%
  filter(Gender == "Male" & Age >= 15 & Age <= 17)

# ----- Exploring the Data -----

# Code to select the numeric variables in our dataset
NumericVars <- FilteredCyclingData %>%
  select_if(is.numeric)
# Code to provide a summary for the numeric variables
for (var in names(NumericVars)) {
  VarSummary <- summary(NumericVars[[var]],na.rm=TRUE)
  # Code to print the summary statistics
  cat("Variable:", var, "\n")
  print(VarSummary)
  cat("\n")
}

# Use these commands to tidy up the environment and value sections
rm(MissingData)
rm(VarSummary)
rm(var)
# WE HAVE 15 VARIABLES IN 'NumericVars' BECAUSE IT ONLY INCLUDES NUMERICAL VARIABLES!

# ----- Creating Distributions -----

# Create a histogram to visualize the distribution and outliers of our numerical data
NumericVars <- NumericVars %>% select(-1,-4,-9,-10,-11,-12,-13,-14,-15)
  
for (var in names(NumericVars)) {  
  p_value <- (shapiro.test(FilteredCyclingData[[var]]))$p.value
  skew <- skewness(FilteredCyclingData[[var]])
  kurt <- kurtosis(FilteredCyclingData[[var]])
  Shaplabel <- paste("Shapiro-Wilk p-value: ", format(p_value, digits = 4))
  Skewlabel <- paste("Skewness: ", format(skew, digits = 4))
  Kurtlabel <- paste("Kurtosis: ", format(kurt, digits = 4))
  # Histogram code (use aes_string as our variables are treated as strings, rather than there names)
  histo <- ggplot(FilteredCyclingData, aes_string(x = var)) +
    geom_histogram() +
    ggtitle(paste("Histogram of", var)) +
    labs(subtitle = paste(Shaplabel, "\n", Skewlabel, "\n", Kurtlabel, "\n"))
  
  print(histo)
  

}

# WHEN GRAPHS ARE CREATED, THEY ARE DIFFERENT; HAVE I NOT CLEANED THE DATA PROPERLY?

# ----- Correlation -----

# Code to create a correlation matrix from our 'NumericVars' dataframe
CorMatx <- cor(NumericVars, use = "complete.obs")
CorMatx
# Code to create a correlation plot using the correlation matrix created
corrplot(CorMatx, method = "color")

# ----- Creating New Variables -----
# Code to create new variables using existing variables. ADD TO ORIGINAL DATASET!
# This adds the new variables to the original dataset, meaning we can easily find it
# in future practice.
FilteredCyclingData <- FilteredCyclingData %>%
  mutate(
    PeakPowerKG = peak_power/weight,
    FTP_KG = ftp/weight)

# ----- Creating a New Correlation Plot -----

# Creating the dataframe with the selected numeric variables
NumericVars2 <- FilteredCyclingData %>%
  select_if(is.numeric)
# Code to provide a summary of our numeric variables
for (var in names(NumericVars2)) {
  VarSummary2 <- summary(NumericVars2[[var]], na.rm = TRUE)
  # Code to print summary of our numeric variables
  cat("Variable", var, "\n")
  print(VarSummary2)
  cat("\n")
  }
# Code to select the variables we want in our histogram (also includes outliers and distribution)
NumericVars2 <- NumericVars2 %>% select(-1,-2,-4,-5,-6,-9,-10,-11,-12,-13,-14,-15)

for (var in names(NumericVars2)) {  
  p_value <- (shapiro.test(FilteredCyclingData[[var]]))$p.value
  skew <- skewness(FilteredCyclingData[[var]])
  kurt <- kurtosis(FilteredCyclingData[[var]])
  Shaplabel <- paste("Shapiro-Wilk p-value: ", format(p_value, digits = 4))
  Skewlabel <- paste("Skewness: ", format(skew, digits = 4))
  Kurtlabel <- paste("Kurtosis: ", format(kurt, digits = 4))
  # Histogram coding: use aes_string so that the variable is treated as its name and not the string
  histo <- ggplot(FilteredCyclingData, aes_string(x = var)) +
    geom_histogram() +
    ggtitle(paste("Histogram of", var)) +
    labs(subtitle = paste(Shaplabel, "\n", Skewlabel, "\n", Kurtlabel, "\n"))
  
  print(histo)
  
}
# Code to create the correlation matrix
CorMatx2 <- cor(NumericVars2, use = "complete.obs")
CorMatx2
# Code to create the correlation plot
corrplot(CorMatx2, method = "color")

# Creating a Scatter plot using our new Power variables
# Scatterplot No.1
PkPwPlot <- ggplot(FilteredCyclingData, aes(peak_power, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot)
# Scatterplot No.2
PkPwPlot2 <- ggplot(FilteredCyclingData, aes(PeakPowerKG, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot2)
# Scatterplot No.3
PkPwPlot3 <- ggplot(FilteredCyclingData, aes(ftp, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot3)
# Scatterplot No.4
PkPwPlot4 <- ggplot(FilteredCyclingData, aes(FTP_KG, weight))+
  geom_point() + stat_smooth(method = "lm",
                             formula = y ~ x,
                             geom = "smooth")
print(PkPwPlot4)

# ----- Descriptive Analysis -----
# Code for creating the descriptive table
library(summarytools)
DescTable <- descr(FilteredCyclingData[, c(4:19)])
print(DescTable)

# ----- Identifying Top Performing Riders -----
# Code to filter the cyclists who are over the given variable scores. This can be 
# used for setting benchmarks
TopCyclist <- FilteredCyclingData %>%
  filter(FTP_KG > 5.25 & vo2_max > 67.57)
# Code for ranking the cyclists in order by FTP_KG, then by Vo2 Max
TopCyclist <- TopCyclist %>%
  arrange(FTP_KG, vo2_max, decreasing = FALSE) %>%
  mutate(ranking = rank(desc(FTP_KG)))
print(TopCyclist)

# Define the columns to normalize
columns_to_normalize <- c("Age","vo2_max","lactate_threshold","years_experience","FTP_KG")

# Compute the maximum values for the specified columns
max_values <- apply(FilteredCyclingData[,columns_to_normalize], 2, max,na.rm=TRUE)

# Normalize the specified columns (column values / max_values)
normalized_columns <- sweep(TopCyclist[, columns_to_normalize], 2, max_values, "/")
colnames(normalized_columns) <- paste('Norm', colnames(normalized_columns), sep = '_')

# Add the normalized columns back to the TopCyclist data frame
TopCyclist <- cbind(TopCyclist, normalized_columns)

# Filter for specific rider or riders, in this case riders 1 and 10
Riders <- TopCyclist %>%
  filter(ranking==1 | ranking ==10)

# Creating a vector with the variables we wish to include
radar_data <- Riders[,c("Norm_years_experience", "Norm_Age", "Norm_vo2_max", "Norm_FTP_KG", "Norm_lactate_threshold", "Cyclist_ID")]

# Code to define the information included in the radar chart
radar_data <- data.frame(years_experience = c(1, 0, radar_data$Norm_years_experience),
                         Age = c(1, 0, radar_data$Norm_Age),
                         Vo2_max = c(1, 0, radar_data$Norm_vo2_max),
                         FTP_Kg = c(1, 0, radar_data$Norm_FTP_Kg),
                         Lactate_threshold = c(1,0, radar_data$Norm_lactate_threshold),
                         row.names=c("max","min",as.character(radar_data$Cyclist_ID)))

# Create the radar chart
radar_plot <- radarchart(
  radar_data)
# Code for determining the radar chart properties
legend(-3,0,
       legend=c("P1","P10"),
       pch=c(15, 16),
       col=c("red","black"),
       lty=c(2, 4))

# ----- Predictive Analysis -----

# Code to read in the .csv file from our filepath
CyclingDataPredictive <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 5/Week5TalentIDFiles/CyclingTI_predictive.csv")
# Code to filter the data so we only have male cyclists
CyclingDataPredictive <- CyclingDataPredictive %>%
  filter(Gender == "Male")
# Code to create two variables 'PeakPowerKG' and 'FTP_KG', and adding it to the dataset
CyclingDataPredictive <- CyclingDataPredictive %>%
  mutate("PeakPowerKG" = peak_power/weight,
         "FTP_KG" = ftp/weight)
# Code to sort the data by Cyclist_ID
# CAN I NOT JUST CLICK CYCLIST_ID ON THE DATASET OR IS THERE A RESAON WE NEED THE CODE IN TOO?
CyclingDataPredictive <- with(CyclingDataPredictive, CyclingDataPredictive[order(Cyclist_ID), ])

# Code to identify the most important predictor variable for the tour; in this case, it is VO2 Max
winspredictiontour <- lm(grand_tour_wins ~ FTP_KG + vo2_max + PeakPowerKG + lactate_threshold + anxiety + height + weight+ self_efficacy + attention + technical_skill, data = CyclingDataPredictive)
summary(winspredictiontour)
# Code to identify the most important predictor variable for the sprint; in this case, it is PeakPowerKG
winspredictionsprint <- lm(sprint_wins ~ FTP_KG + vo2_max + lactate_threshold + PeakPowerKG + anxiety + height + weight+ self_efficacy + attention + technical_skill, data = CyclingDataPredictive)
summary(winspredictionsprint)
# Code to identify the most important predictor variable for the classic; in this case, it is FTP_KG
winspredictionclassic <- lm(classic_wins ~ FTP_KG + vo2_max + lactate_threshold + PeakPowerKG + anxiety + height + weight+ self_efficacy + attention + technical_skill, data = CyclingDataPredictive)
summary(winspredictionclassic)

# The code below creates a function called "create_train_test" which will help us split our data in 2 in the next few steps.
CreateTrainTest <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

# Lets create a criteria to indicate top sprinters versus those that are not. We will use more than 5 wins as a criteria for being a sprinter.
CyclingDataPredictive <- CyclingDataPredictive %>%
  mutate(Sprinter = ifelse(sprint_wins>5,1,0))

CyclingDataPredictive2 <- subset(CyclingDataPredictive, select = c(PeakPowerKG, FTP_KG, weight, Sprinter, attention, anxiety))
# Code to split our data into training and testing data, a 20% test - 80% training split
data_train <- create_train_test(CyclingDataPredictive2, 0.8, train = TRUE)
data_test <- create_train_test(CyclingDataPredictive2, 0.8, train = FALSE)
# Code to check the probability of a performer becoming a sprinter from both test and training data sets
prop.table(table(data_test$Sprinter))
prop.table(table(data_train$Sprinter))
# Code to create our decision tree using the KPIs we identified earlier using our regression analysis
fit <- rpart(Sprinter~., data = data_train, method = 'class')
rpart.plot(fit)
# Code to check the test accuracy on our test sample
AccuracyTune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$Sprinter, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

AccuracyTune(fit)
# Code to create our decision tree on the junior dataset
FilteredCyclingData$predictions <- predict(fit, newdata = FilteredCyclingData, type = "class")
