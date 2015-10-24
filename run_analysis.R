# This script 
# (1) Merges the training and test sets to create one data set;
# (2) Extracts only the measurements corresponding to the mean and standard deviation 
#     for each feature; 
# (3) Gives an appropriately descriptive name to each activity in the data set;
# (4) Appropriately labels the data set with descriptive variable names; 
# (5) Derives a second, independent tidy data set with the average of each variable 
#     for each activity and each subject.

# Load packages required for script
library(dplyr)
library(data.table)
library(Hmisc)

# Read training and test datasets into R as data frames.
test_set <- read.table("UCI_HAR_Dataset/test/X_test.txt", quote = "")
test_labels <- read.table("UCI_HAR_Dataset/test/y_test.txt", quote = "")
test_subjects <- read.table("UCI_HAR_Dataset/test/subject_test.txt", quote = "")
train_set <- read.table("UCI_HAR_Dataset/train/X_train.txt", quote = "")
train_labels <- read.table("UCI_HAR_Dataset/train/y_train.txt", quote = "")
train_subjects <- read.table("UCI_HAR_Dataset/train/subject_train.txt", quote = "")

# (1) Merge the training and test sets to create one data set.
# First merge the subjects, activity labels, and features by columns into one dataset 
# for both test and train datasets separately, then combine test and train by rows.
dataset_combined <- rbind(cbind(test_subjects, test_labels, test_set), 
                          cbind(train_subjects, train_labels, train_set))

# (4) Name columns (variables) according to the README text file and features text 
# file included with the data. 
features <- read.table("UCI_HAR_Dataset/features.txt", quote = "")
feature_names <- as.character(features[,2])
new_col_names <- c("Subject", "Activity", feature_names)
dataset_complete <- setnames(dataset_combined, new_col_names)

# (2) Extract the mean and standard deviation variables (columns) for each 
# measurement. First need to remove duplicate column names for select to work.
data_comp_unique <- dataset_complete[,!duplicated(colnames(dataset_complete))]
data_tidy1 <- select(data_comp_unique, Activity, Subject, contains("-mean"), 
      contains("std"))

# (3) Give an appropriately descriptive name to each activity in the data set
data_tidy1$Activity <- cut(data_tidy1$Activity,6,labels=c("Walking",
      "Walking_Up","Walking_Down", "Sitting","Standing","Lying_Down"))

# (5) Derives a second, independent tidy data set with the average of each variable 
#     for each activity and each subject.
data_tidy2 <- data_tidy1 %>% group_by(Activity, Subject) %>% 
      summarise_each(funs(mean))
