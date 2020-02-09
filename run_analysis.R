#read training data
subj_tr <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))
x_tr <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
y_tr <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
#read test data
Subj_test <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))
x_test <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
y_test <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
# read features
features <- read.table(file.path("UCI HAR Dataset", "features.txt"), as.is = TRUE)
# read activity labels
activities <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
colnames(activities) <- c("actId", "actLabel")
#Merges the training and the test sets to create one data set.
tr_data <- cbind(subj_tr,x_tr,y_tr)
test_data <- cbind(Subj_test,x_test,y_test)
mdata <- rbind(tr_data,test_data)
head(mdata)
colnames(mdata) <- c("subject", features[, 2], "activity")
#Extracts only the measurements on the mean and standard deviation for each measurement.
extr_only <- grepl("subject|activity|mean|std", colnames(mdata))
mdata <- mdata[, extr_only]
#Uses descriptive activity names to name the activities in the data set
mdata$activity <- factor(mdata$activity, levels = activities[, 1], labels = activities[, 2])
#Appropriately labels the data set with descriptive variable names.
colnames(mdata)
cols <- gsub("std","StandardDeviation",colnames(mdata))
cols <- gsub("Gyro","Gyroscope",cols)
cols <- gsub("Acc","Accelerometer",cols)
cols <- gsub("Mag","Magnitude",cols)
cols <- gsub("Freq","Frequency",cols)
cols <- gsub("^f","frequency",cols)
cols <- gsub("^t","time",cols)
cols <- gsub("BodyBody","Body",cols)
cols <- gsub("[\\(\\)-]", "", cols)
colnames(mdata) <- cols
colnames(mdata)
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mdata_average <- mdata %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))
write.table(mdata_average, "tidy_data.txt", row.names = FALSE, quote = FALSE)

