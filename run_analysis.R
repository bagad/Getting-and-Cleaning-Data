
##run_analysis.R perform following functions:

##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names.
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## package required to use as.data.table
install.packages("data.table")
## package required to use melt functionality
install.packages("reshape2")

require("data.table")
require("reshape2")

## Read source files in table format and creates a data frame for train dataset 
x_train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train_data <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train_data <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## Read files in table format and creates a data frame for test dataset 
x_test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test_data <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test_data <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Read activity labels and data columns from provided source dataset
features_label <- read.table("./UCI HAR Dataset/features.txt")[,2]
names(x_test_data) = features_label
names(x_train_data) = features_label

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
y_test_data[,2] = activity_labels[y_test_data[,1]]
names(y_test_data) <- c("Activity_ID", "Activity_Label")
y_train_data[,2] = activity_labels[y_train_data[,1]]
names(y_train_data) <- c("Activity_ID", "Activity_Label")
names(subject_test_data) = "Subject"
names(subject_train_data) <- "Subject"



final_train_dataset <- cbind(as.data.table(subject_train_data), y_train_data, x_train_data)
final_test_dataset <- cbind(as.data.table(subject_test_data), y_test_data, x_test_data)
final_merged_data <- rbind(final_train_dataset, final_test_dataset)


## Extract required measurements on the mean and standard deviation for each measurement.
target_features <- grepl("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]", features_label)
final_x_test = x_test_data[,target_features]
final_x_train = x_train_data[,target_features]

## Merge final data set for mean and standard only
train_data <- cbind(as.data.table(subject_train_data), y_train_data, final_x_train)
test_data <- cbind(as.data.table(subject_test_data), y_test_data, final_x_test)
final_data <- rbind(test_data, train_data)

##label data set
labels_for_id   <- c("Subject", "Activity_ID", "Activity_Label")
labels_data <- setdiff(colnames(final_data), labels_for_id)
melt_dataset   <- melt(final_data, id = labels_for_id, measure.vars = labels_data)

# Apply mean function to dataset using dcast function to calculate average
tidy_dataset <- dcast(melt_dataset, Subject + Activity_Label ~ variable, mean)

## write final tidy data set
write.table(tidy_dataset, file = "./tidy_dataset.txt", row.name=FALSE)
