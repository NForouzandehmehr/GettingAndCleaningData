library(dplyr)
library(tidyr)

# Loading datasets
xtraining <- read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
xtest <- read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
Ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names=c("activityNum"))
Ytest <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names=c("activityNum"))
stest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subjectID"))
strain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subjectID"))
featureNames  <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors =FALSE)[[2]]
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activityNum", "activity"))

#Merging datasets
colnames(xdataset) <- featureNames
xdataset <- rbind(xtraining, xtest)
activityData <-  merge(rbind(Ytrain, Ytest), activityLabels, by="activityNum", sort=FALSE)
stdOrMeanCols <- featureNames[grepl("mean\\()|std\\()", featureNames)]
allData <- xdataset[,stdOrMeanCols]
 allData["activity"]=activityData["activity"]
allData["subjectId"] <- rbind(strain, stest)

#Cleaning final dataset
  cleanupVariableName <- function(name) {
    # Better variable names
    name <- sub("^f", "meanFrequency", name)
    name <- sub("^t", "meanTime", name)
    name <- sub("Acc", "Acceleration", name)
    name <- sub("Mag", "Magnitude", name)
    name <- sub("Gyro", "Gyroscope", name)
    name <- sub("-mean\\(\\)", "Mean", name)
    name <- sub("-std\\(\\)", "StandardDeviation", name)
    name
}
allData2 <- gather(allData, variable, value, -activity, -subjectId)
 allData3 <- mutate(allData2,variable=cleanupVariableName(variable))
 allData4 <- group_by(allData3, activity, subjectId, variable)
 allData5 <-  summarise(allData4, value=mean(value))
   TidyData <- arrange(allData5, activity, subjectId, variable)

write.table(TidyData, "UCI HAR Dataset/tidy UCR HAR summary.txt", row.names=FALSE)
 
 
