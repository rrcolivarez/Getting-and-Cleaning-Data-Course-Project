library(dplyr)

# 1 Merges the training and the test sets to create one data set.

  #Reading files
  x_train <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/train/subject_train.txt")

  #Reading test data sets
  x_test <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/test/subject_test.txt")

  #Reading features  
  features <- read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/features.txt")
  
  #Reading activitylabels
  activityLabels = read.table("/Users/raulrodrigoolivarezjr./Library/CloudStorage/GoogleDrive-raul.olivarez@globe.com.ph/My Drive/DSP Files/Case Studies/Coursera/3 Data Cleaning/Week 4/GettingandCleaningDataCourseProject/UCI HAR Dataset/activity_labels.txt")
  
  #Adding column names for train and test data
  colnames(x_train) <- features[,2]
  colnames(y_train) <- "activityID"
  colnames(subject_train) <- "subjectID"
  
  colnames(x_test) <- features[,2]
  colnames(y_test) <- "activityID"
  colnames(subject_test) <- "subjectID"
  
  colnames(activityLabels) <- c("activityID", "activityType")
  
  #Merging datasets using cbind and rbind
  alltrain <- cbind(y_train, subject_train, x_train)
  alltest <- cbind(y_test, subject_test, x_test)
  finaldataset <- rbind(alltrain, alltest)
  
  
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
  
    #Defining the ID mean and std using grep and logical OR
  colNames <- colnames(finaldataset)
  mean_and_std <- (grepl("activityID", colNames) |
                     grepl("subjectID", colNames) |
                     grepl("mean..", colNames) |
                     grepl("std...", colNames)
  )
  
  #Mean and Standard Deviation Subset
  mean_and_std_set <- finaldataset[ , mean_and_std == TRUE]
  
# 3 Uses descriptive activity names to name the activities in the data set
  
  activity_names_set <- merge(mean_and_std_set, activityLabels,
                                by = "activityID",
                                all.x = TRUE)
  
#4 Appropriately labels the data set with descriptive variable names.
  #already labeled on #1 and #2 codes
  
#5  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  # Second, independent tidy data set
  tidySet <- suppressWarnings(aggregate(activity_names_set, by = list(activity_names_set$activityID,   activity_names_set$subjectID), mean))
  tidySet <- tidySet[order(tidySet$subjectID, tidySet$activityID), ]
  
  
  # to text file
  write.table(tidySet, "tidySet.txt", row.names = FALSE)