# Getting and Cleaning Data Course Project

This repo contains all files and scripts required for the Project for the Coursera Getting and Cleaning Data class.
This includes all of the components of the tidy data - tidy data, codebook, and recipe - with the exception of the raw data itself, downloaded from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The submitted tidy data file can be read into R with read.table(header=TRUE).

This README describes the recipe the run_analysis.R script used to create the tidy data from the raw data as indicated in the course project instructions. 

(1) The packages required for the script are loaded using library() (assumes they are already installed using install.packages).

(2) The downloaded raw data in the training and test datasets are read into R as data frames using read.table. Note this            includes separate files for the smartphone sensor signals (X_test and X_train), the labels describing the subjects' activity     (y_test and y_train), and the individual or "subject" corresponding to each observation or row of the sensor signal data frame.

(3) To merge the test and train data frames together, first the subject and activity columns are added to the respective test or     train data using cbind(), then the test and train datasets are combined with rbind(). The cbind() function was nested           within the rbind() to ensure the correct row order before combining the test and train data.

(4) Before extracting the mean and std variables, it is necessary to add descriptive names to the variables (columns) first, as     the combined data columns are labeled V1, V2, etc. IMO the most accurate descriptive names come from the data source itself,     and with the exception of the Subject and Activity columns are compiled in order in the features.txt included with the          dataset (the UCI README explains this in more detail). This file is read into R as a data frame containing the feature names     in the 2nd column. The feature names are subsetted from the data frame, converted to characters using as.character(),           combined with Subject and Activity in a character vector, then used to label the data with setnames().

(5) Now that each column or variable has a descriptive name, the mean and standard deviation are extracted for all rows             (measurements) using select() to pull out columns with "-mean" and "std" in the description. Note that this excludes a small     subset of the columns that have Mean in the description, and my rationale for doing so is described below. Also, some of the     columns have duplicated names, and these had to be removed for select() to operate. I did this by subsetting unique columns     from the dataset using !duplicated(colnames(data)).
   >According to the features_info.text file included with the UCI data, there are two different types of mean calculations, one    calculated directly from the sensor signal, the other from a signal window sample used on the angle() variable. For this        assignment I included only the values calculated from the sensor signal.

(6) The descriptive names corresponding to integers 1-6 in the Activity column are found in the activity_labels.txt file            included with the UCI data set. I used cut() and a character vector of slightly improved activity names (in order, i.e. the     first name in the vector = activity #1 etc.) to substitute names for the integers in the Activity column. 

(7) The second, independent tidy data set was cast in the "wide" format that meets tidy data principles, as each column is a        separate variable, and each observation is in a single row. The downstream analysis might warrant melting or pivoting the data, but in the absence of that knowledge additional data manipulation could be unnecessary or even interfere with analysis. To generate a tidy data set with the average of each variable for each activity and each subject, the diplyr function group_by is used to group the data by Activity and Subject, followed by summarise_each to generate the mean for the groups.
