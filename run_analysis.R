##### Assignment: Getting and Cleaning Data Course Project #####

## SET WORKING DIRECTORY ##
setwd("/Users/agislaso/Documents/R/Coursera")

## DOWNLOAD AND UNZIP ##
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile='uciml.zip', method='curl')
unzip('uciml.zip')

## IMPORT AND COMBINE TRAINING DATA ##
train <- read.table("UCI HAR Dataset/train/X_train.txt")
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- tolower(features[,2])
names(train) <- as.character(features[,2])
trainactivity <- read.table("UCI HAR Dataset/train/y_train.txt")
names(trainactivity)[1] <- "activity_code"
trainsubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
names(trainsubject)[1] <- "subject_code"
complete_train <- cbind(trainactivity, trainsubject, train)

##IMPORT AND COMBINE TEST DATA ##
test <- read.table("UCI HAR Dataset/test/X_test.txt")
names(test) <- as.character(features[,2])
testactivity <- read.table("UCI HAR Dataset/test/y_test.txt")
names(testactivity)[1] <- "activity_code"
testsubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(testsubject)[1] <- "subject_code"
complete_test <- cbind(testactivity, testsubject, test)

## COMBINE TEST AND TRAIN DATA ##
combined <- rbind(complete_train, complete_test)

## REMOVE UNWANTED COLUMNS ##
desired_cols <- c("activity_code", "subject_code",
                  grep(".*-mean\\(\\).*|.*-std\\(\\).*", names(test), value=TRUE)
)
combined <- combined[,desired_cols]

## IMPORT ACTIVITY DESCRIPTORS ##
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("activity_code", "activity_descriptor")
combined <- merge(combined, activity_labels, by = "activity_code", all.x=TRUE)

## CREATE CROSSTABULATION BY ACTIVITY & SUBJECT ##
crosstab <- NULL
for (i in 3:68) {
  tmp <- as.data.frame(tapply(combined[,i], list(combined$activity_descriptor, combined$subject_code), mean))
  tmp <- cbind(activity_descriptor = rownames(tmp), tmp)
  rownames(tmp) <- NULL
  metric <- names(combined)[i]
  tmp <- cbind(metric = rep(metric, nrow(tmp)), tmp)
  crosstab <- rbind(tmp, crosstab)
}

write.table(crosstab, "tidy.txt", row.names = FALSE)