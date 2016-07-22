# Create one R script called class3_project.R that does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#load R packages and raw data into R
library(data.table)
library(reshape2)
activityLable <- read.table("/Users/YanYang/code/UCI HAR Dataset/activity_labels.txt")
activityLable[, 2] <- tolower(activityLable[, 2])
features <- read.table("/Users/YanYang/code/UCI HAR Dataset/features.txt")
x_test <- read.table("/Users/YanYang/code/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("/Users/YanYang/code/UCI HAR Dataset/test/Y_test.txt")
x_train <- read.table("/Users/YanYang/code/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("/Users/YanYang/code/UCI HAR Dataset/train/Y_train.txt")


#clean data in set "test"
subject_test <- read.table("/Users/YanYang/code/UCI HAR Dataset/test/subject_test.txt")
names(x_test) <- features[, 2]
names(y_test) <- c("activity_label")
names(subject_test) <- c("subject")
extract_test <- grepl("mean|std", names(x_test))
mean_std_test <- x_test[, extract_test]
set <- rep("test", times = 2947)
test <- cbind(subject_test, y_test, set, mean_std_test)


#clean data in set "train"
subject_train <- read.table("/Users/YanYang/code/UCI HAR Dataset/train/subject_train.txt")
names(x_train) <- features[, 2]
names(y_train) <- c("activity_label")
names(subject_train) <- c("subject")
extract_train <- grepl("mean|std", names(x_train))
mean_std_train <- x_train[, extract_train]
set <- rep("train", times = 7352)
train <- cbind(subject_train, y_train, set, mean_std_train)


# merge "test" and "train"
data <- rbind(test, train)
library(dplyr)
data <- tbl_df(data)
data <- arrange(data, subject, activity_label)


# change the acitivity labels into descriptive activity names
result <- data.frame()
for(i in 1:6){
        col <- select(data, activity_label)
        row <- filter(data, col == i)
        row[, 2] <- rep(activityLable[i, 2], nrow(row))
        result <- rbind(result, row)
}
write.table(result, "/Users/YanYang/code/UCI HAR Dataset/tidy_data.txt", row.names = FALSE)
write.csv(result, "/Users/YanYang/code/UCI HAR Dataset/tidy_data.csv")


# create another dataset with the average of each variable for each activity and each subject
variable <- select(data, -set)
each_activity <- data.frame()
for(i in 1:30){
        for(j in 1:6){
                colM <- colMeans(filter(variable, subject == i & activity_label == j)[, 3:81])
                each_activity <- rbind(each_activity, colM)
        }
}
colname <- features[extract_test, 2]
names(each_activity) <- colname
activity <- rep(activityLable[, 2], 30)
subject_number <- rep(1:30, each = 6)
summary <- cbind(subject_number, activity, each_activity)
write.table(summary, "/Users/YanYang/code/UCI HAR Dataset/summary.txt", row.names = FALSE)
write.csv(summary, "/Users/YanYang/code/UCI HAR Dataset/summary.csv")



#test the result
result1 <- read.table("/Users/YanYang/code/UCI HAR Dataset/tidy_data.txt")
result2 <- read.table("/Users/YanYang/code/UCI HAR Dataset/summary.txt")

