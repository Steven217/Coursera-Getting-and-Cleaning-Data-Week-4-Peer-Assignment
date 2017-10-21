library(data.table)
library(dplyr)

# *************************** LOAD DATA *********************

# Load test data
TestData <- data.frame(fread("UCI HAR Dataset/test/x_test.txt"))
TestActivityLabels <- data.frame(fread("UCI HAR Dataset/test/Y_test.txt", col.names = c("ActivityLabelNumber")))
TestSubjects <- data.frame(fread("UCI HAR Dataset/test/subject_test.txt", col.names = c("SubjectID")))

# Load train data
TrainData <- data.frame(fread("UCI HAR Dataset/train/x_train.txt"))
TrainActivityLabels <- data.frame(fread("UCI HAR Dataset/train/Y_train.txt", col.names = c("ActivityLabelNumber")))
TrainSubjects <- data.frame(fread("UCI HAR Dataset/train/subject_train.txt", col.names = c("SubjectID")))
    
# Load feature description
Features <- data.frame(fread("UCI HAR Dataset/features.txt", col.names = c("FeatureNumber", "FeatureNames")))

# Load activity labels
ActivityLabels <- data.frame(fread("UCI HAR Dataset/activity_labels.txt", col.names = c("ActivityLabelNumber", "ActivityLabel")))


# ********************* Step 1: Merge the training and test sets **************************


# Merge the data, activity and subject test and train files. 
DataTotal <- rbind(TestData, TrainData)
ActivityLabelsTotal <- rbind(TestActivityLabels, TrainActivityLabels)
SubjectsTotal <- rbind(TestSubjects, TrainSubjects)

# Final merging to one datafile is done at the end of step 4


# ******************* Step 2: Extract only mean and standard deviation and add variable names **********************


# Extract only columns with mean en std values from DataTotal

ColumnNumbersMeanAndStdev <- grep("mean\\(\\)|std\\(\\)", Features$FeatureNames)
DataTotalSubset <- DataTotal[, ColumnNumbersMeanAndStdev]

# Add colnames to DataTotal
colnames(DataTotalSubset) <- Features[ColumnNumbersMeanAndStdev, 2]
colnames(DataTotalSubset) <- gsub("mean\\(\\)-|mean\\(\\)", "Mean", colnames(DataTotalSubset))
colnames(DataTotalSubset) <- gsub("std\\(\\)-|std\\(\\)", "Std", colnames(DataTotalSubset))


# ************************** Step 3: Use descriptive activity names **************************


# Add activity labels descriptions to numbers 
ActivityLabelsTotal$ActivityLabel <- factor(ActivityLabelsTotal$ActivityLabelNumber, labels = as.character(ActivityLabels$ActivityLabel))


# **************** Step 4: Add descriptive variable names and final merge to get one dataset *********************


# Already done in the beginning of the script  when the data is loaded and in step 2 for the mean and stdev columns

# Merge all datafiles: Add subject and activity labels to dataset and removing ActivityLabelNumbercolumn(colnumber 2)
DataTotalSubset <- cbind(SubjectsTotal, ActivityLabelsTotal, DataTotalSubset)
DataTotalSubset <- DataTotalSubset[, -2]

# Save the data to a .txt file
write.table(DataTotalSubset, "MergedAndCleanedData.txt", row.name = FALSE)


# ************ Step 5: tidy data set with average of each variable for each activity and each subject *****************


# Group by Subject and Activity, then calculate the mean for all other variables 
TidyDataSet <- DataTotalSubset %>%
    group_by(SubjectID,
             ActivityLabel) %>%
    summarise_all(funs(mean))

# Save the data to a .txt file
write.table(TidyDataSet, "TidyDataSet.txt", row.name = FALSE)
