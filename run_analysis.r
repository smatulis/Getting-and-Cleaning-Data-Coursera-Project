library(data.table)
library(dplyr)

# Part 1 ------------------------------------------------------------------
#Merges the training and the test sets to create one data set


#read feature and activity labels
features <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)

#Read Training Sets
subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
X_Train <- read.table("train/x_train.txt", header = FALSE)
Y_Train <- read.table("train/y_train.txt", header = FALSE)

#Read Test Sets
subjectTest <- read.table("test/subject_test.txt", header = FALSE)
X_Test <- read.table("test/x_test.txt", header = FALSE)
Y_Test <- read.table("test/y_test.txt", header = FALSE)


#Combine Training and Test Sets
SubjectCombine <- rbind(subjectTrain, subjectTest)

X_Combine <- rbind(X_Train, X_Test)

Y_Combine <- rbind(Y_Train, Y_Test)

colnames(X_Combine) <- t(features[2])
colnames(Y_Combine) <- "Activity"
colnames(SubjectCombine) <- "Subject"


FullData <- cbind(X_Combine,Y_Combine,SubjectCombine)


# Part 2 ------------------------------------------------------------------
#Extracts only the measurements on the mean and standard deviation for each measurement.


M_SD_Cols <- grep(".*Mean.*|.*Std.*", names(FullData), ignore.case=TRUE)
requiredColumns <- c(M_SD_Cols, 562, 563)
DataSample <- FullData[,requiredColumns]

# Part 3 ------------------------------------------------------------------
# Uses descriptive activity names to name the activities in the data set

# Need to update the Activity names
DataSample$Activity <- as.character(DataSample$Activity)
for (i in 1:6){
  DataSample$Activity[DataSample$Activity == i] <- as.character(activityLabels[i,2])
}
#need to encode as factor
DataSample$Activity <- as.factor(DataSample$Activity)


# Part 4 ------------------------------------------------------------------
#Appropriately labels the data set with descriptive variable names.

#check variable names
names(DataSample)

#check variable names update variable names
names(DataSample)<-gsub("Acc", "Accelerometer", names(DataSample))
names(DataSample)<-gsub("Gyro", "Gyroscope", names(DataSample))
names(DataSample)<-gsub("BodyBody", "Body", names(DataSample))
names(DataSample)<-gsub("Mag", "Magnitude", names(DataSample))
names(DataSample)<-gsub("^t", "Time", names(DataSample))
names(DataSample)<-gsub("^f", "Frequency", names(DataSample))
names(DataSample)<-gsub("tBody", "TimeBody", names(DataSample))
names(DataSample)<-gsub("-mean()", "Mean", names(DataSample), ignore.case = TRUE)
names(DataSample)<-gsub("-std()", "STD", names(DataSample), ignore.case = TRUE)
names(DataSample)<-gsub("-freq()", "Frequency", names(DataSample), ignore.case = TRUE)

#check updated names
names(DataSample)


# Part 5 ------------------------------------------------------------------
#From the data set in part 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Need to encode as factor
DataSample$Subject <- as.factor(DataSample$Subject)
DataSample <- data.table(DataSample)

#create Dataset and output to Tidy.txt
tidyData <- aggregate(. ~Subject + Activity, DataSample, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)