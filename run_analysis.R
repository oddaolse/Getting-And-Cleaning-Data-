#
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(dplyr)

setwd("C:\\Users\\oddat_000\\OneDrive\\2014\\Coursera\\getdata-007")

######################
#
# STEP ONE - MERGE DATA
##
######################

###
# Labes for training and test
###
#The labels for the Y_ , 6 rows, 2 variables
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")


#The labels for X_, 561 rows - read it into a factor (enumerated type)
features <- read.table("./UCI HAR Dataset/features.txt")[,2]


###
# Read the datasets
###


###
# FIRST - training data
###


#has 561 variables
X_train = read.csv("UCI HAR Dataset\\train\\X_train.txt", sep="", header=FALSE) 
#Identifies the activity - has 1 variable
Y_train = read.csv("UCI HAR Dataset\\train\\Y_train.txt", sep="", header=FALSE) 
#Identified the subject - has 1 variable
Subj_train = read.csv("UCI HAR Dataset\\train\\subject_train.txt", sep="", header=FALSE) 


###
# Put on better column names
###

names(X_train) = features
names(Y_train) = c("Activity_id")
names(Subj_train) = "Subject_id"

names(activity_labels) = c("Activity_id", "Activity_name")

###
# Keep only mean and std
###

#array with TRUE and FALSE 
MeanAndStdFeatures <- grepl("mean|std", features)
X_train = X_train[,MeanAndStdFeatures]

###
# Combine the tree dataframes into one with cbind
###
AllTrainingData <- cbind(Subj_train, Y_train, X_train)


###
# SECOND - test data
###

#has 561 variables
X_test = read.csv("UCI HAR Dataset\\test\\X_test.txt", sep="", header=FALSE) 
#Identifies the activity - has 1 variable
Y_test = read.csv("UCI HAR Dataset\\test\\Y_test.txt", sep="", header=FALSE) 
#Identified the subject - has 1 variable
Subj_test = read.csv("UCI HAR Dataset\\test\\subject_test.txt", sep="", header=FALSE) 


###
# Put on better column names
###

names(X_test) = features
names(Y_test) = c("Activity_id")
names(Subj_test) = "Subject_id"


######################
#
# STEP TWO - Keep only mean and std
##
######################

###
# 
###
X_test = X_test[,MeanAndStdFeatures]

###
# Combine the tree dataframes into one with cbind
###
AllTestData <- cbind(Subj_test, Y_test, X_test)

###
# Concatenate the testdata and the trainingdata
###
AllData = rbind(AllTestData, AllTrainingData)


###
# Create a syntactic primary key for each observation - to preserver the sorting order.
# Probably not necessary but nice to have
###
AllData$Observation_id <- 1:nrow(AllData) 


#Join the Data with the activity lables
AllData <- merge(x = AllData, y = activity_labels, by = "Activity_id", all = TRUE)

#Order them back
AllData = arrange(AllData, Observation_id)

#######################
#
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
#######################
# Mean
tidy = aggregate(AllData, by=list(activity = AllData$Activity_name, subject=AllData$Subject_id), mean)
tidy = select(tidy, -Observation_id, -Activity_name, -Subject_id, -Activity_id)
write.table(tidy, "tidy.txt", sep="\t")


