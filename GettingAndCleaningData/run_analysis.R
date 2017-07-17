## 0. Set up the environment
# 0.1: Import libraries
library(dplyr)
library(stringr)
library(memisc)
library(foreach)
# 0.2: Set working directory
setwd("/Users/Daniel/Desktop/Getting_and_Cleaning_Data/peer_assignment/")
# 0.3: Define file paths for training/training data as well as 
# general descriptors (i.e. activity labels and features)
base_dir <- "./UCI HAR Dataset"
training_dir <- paste(base_dir, "/train", sep="")
test_dir <- paste(base_dir, "/test", sep="")

train_path <- paste(training_dir, "/X_train.txt", sep = "")
train_label_path <- paste(training_dir, "/y_train.txt", sep = "")
train_subject_path <- paste(training_dir, "/subject_train.txt", sep = "")
test_path <- paste(test_dir, "/X_test.txt", sep = "")
test_label_path <- paste(test_dir, "/y_test.txt", sep = "")
test_subject_path <- paste(test_dir, "/subject_test.txt", sep = "")

features_path <- paste(base_dir, "/features.txt", sep="") 
activity_labels_path <- paste(base_dir, "/activity_labels.txt", sep="")

## 1. Merge the training and the test sets to create one data set.
# 1.1: Read in general descriptors.
features <- read.table(features_path, header = FALSE, stringsAsFactors = FALSE)
activity_labels <- read.table(activity_labels_path, header = FALSE, stringsAsFactors = FALSE)
# 1.2: Read in training data
train_subject <- read.table(train_subject_path, header = FALSE, stringsAsFactors = FALSE)
train_label <- read.table(train_label_path, header = FALSE, stringsAsFactors = FALSE)
train_raw <- tbl_df(read.table(train_path, header = FALSE, stringsAsFactors = FALSE, col.names = features_raw[,2]))
  ## 4. Appropriately label the data set with descriptive variable names.
# 1.3: Read in test data
test_subject <- read.table(test_subject_path, header = FALSE, stringsAsFactors = FALSE)
test_label <- read.table(test_label_path, header = FALSE, stringsAsFactors = FALSE)
test_raw <- tbl_df(read.table(test_path, header = FALSE, stringsAsFactors = FALSE, col.names = features_raw[,2]))
  ## 4. Appropriately label the data set with descriptive variable names.
# 1.4: Tidy the training data
train_clean <- 
  train_raw %>% 
  mutate(subject = train_subject[,1]) %>% 
  mutate(activity = train_label[,1]) %>% 
  mutate(activity = activity_labels_raw[activity, 2])
    ## 3. Use descriptive activity names to name the activities in the dataset
# 1.5: Tidy the test data
test_clean <-
  test_raw %>% 
  mutate(subject = test_subject[,1]) %>% 
  mutate(activity = test_label[,1]) %>% 
  mutate(activity = activity_labels_raw[activity, 2])
    ## 3. Use descriptive activity names to name the activities in the dataset
# 1.6: Merge training/test datasets
merged_data <- bind_rows(train_clean, test_clean)

## 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 2.1: Construct a regexs for extracting mean & std. (as well as other identifying info
# like "class" and "subject")
pattern <- "mean\\.|std\\.|subject|activity"
matching_cols <- str_detect(colnames(merged_data), pattern)
# 2.2: Extract the target columns 
mean_and_std <- merged_data[matching_cols]
# 2.3: Clean out "BodyBody" mistakes
pattern <- "BodyBody"
matching_cols <- str_detect(colnames(mean_and_std), pattern)
cols <- colnames(mean_and_std[matching_cols])
repl_cols <-  str_replace(cols, pattern, "Body")
colnames(mean_and_std) <- c(colnames(mean_and_std)[1:60], repl_cols, colnames(mean_and_std)[67:68])

## 5. Create a second, independent tidy data set with the 
## average of each variable for each activity and each subject. 
# 5.1: Create the tidy dataset
summary <-  
  mean_and_std %>% 
  group_by(activity, subject) %>% 
  select_("activity", "subject", "tBodyAcc.mean...X:fBodyGyroJerkMag.std..") %>% 
  summarize_all(mean)
# 5.2: Write the tidy dataset to a text file
write.table(summary, row.names = FALSE, file="tidy_summary.txt")

## EXTRA: Format Codebook
codebook <- as.data.set(summary)  
codebook <- within(codebook, {
  wording(activity) <- "Activity Name"
  labels(activity) <- c(
    "1"         =  "WALKING",
    "2"         =  "WALKING_UPSTAIRS",
    "3"   =  "WALKING_DOWNSTAIRS",
    "4"          =  "SITTING",
    "5"      =  "STANDING",
    "6"      = "LAYING")
  wording(subject) <- "Subject ID"
  annotation(subject)["Note"] <- "Each subject is given a unique identifier from 1 to 30"
  
  wording(tBodyAcc.mean...X) <- "Mean body accel. signals in X-dir (in hz)"
  wording(tBodyAcc.std...X) <- "Standard deviation body accel. signals in X-dir (in hz)"
  wording(tBodyAcc.mean...Y) <- "Mean body accel. signals in Y-dir (in hz)"
  wording(tBodyAcc.std...Y) <- "Standard deviation body accel. signals in Y-dir (in hz)"
  wording(tBodyAcc.mean...Z) <- "Mean body accel. signals in Z-dir (in hz)"
  wording(tBodyAcc.std...Z) <- "Standard deviation body accel. signals in Z-dir (in hz)"
  wording(tBodyAccMag.mean..) <- "Mean magnitude of body accel. signals (in hz)"
  wording(tBodyAccMag.std..) <- "Standard deviation of magnitude of body accel. signals (in hz)"
  
  wording(tGravityAcc.mean...X) <- "Mean gravity accel. signals in X-dir (in hz)"
  wording(tGravityAcc.std...X) <- "Standard deviation gravity accel. signals in X-dir (in hz)"
  wording(tGravityAcc.mean...Y) <- "Mean gravity accel. signals in Y-dir (in hz)"
  wording(tGravityAcc.std...Y) <- "Standard deviation gravity accel. signals in Y-dir (in hz)"
  wording(tGravityAcc.mean...Z) <- "Mean gravity accel. signals in Z-dir (in hz)"
  wording(tGravityAcc.std...Z) <- "Standard deviation gravity accel. signals in Z-dir (in hz)"
  wording(tGravityAccMag.mean..) <- "Mean magnitude of gravity accel. signals (in hz)"
  wording(tGravityAccMag.std..) <- "Standard deviation of magnitude of gravity accel. signals (in hz)"
  
  wording(tBodyAccJerk.mean...X) <- "Mean body accel. Jerk signals in X-dir (in hz)"
  wording(tBodyAccJerk.std...X) <- "Standard deviation body accel. Jerk signals in X-dir (in hz)"
  wording(tBodyAccJerk.mean...Y) <- "Mean body accel. Jerk signals in Y-dir (in hz)"
  wording(tBodyAccJerk.std...Y) <- "Standard deviation body accel. Jerk signals in Y-dir (in hz)"
  wording(tBodyAccJerk.mean...Z) <- "Mean body accel. Jerk signals in Z-dir (in hz)"
  wording(tBodyAccJerk.std...Z) <- "Standard deviation body accel. Jerk signals in Z-dir (in hz)"
  wording(tBodyAccJerkMag.mean..) <- "Mean magnitude of gravity accel. Jerk signals (in hz)"
  wording(tBodyAccJerkMag.std..) <- "Standard deviation of magnitude of gravity accel. Jerk signals (in hz)"
  
  wording(tBodyGyro.mean...X) <- "Mean body gyro signals in X-dir (in hz)"
  wording(tBodyGyro.std...X) <- "Standard deviation body gyro signals in X-dir (in hz)"
  wording(tBodyGyro.mean...Y) <- "Mean body gyro signals in Y-dir (in hz)"
  wording(tBodyGyro.std...Y) <- "Standard deviation body gyro signals in Y-dir (in hz)"
  wording(tBodyGyro.mean...Z) <- "Mean body gyro signals in Z-dir (in hz)"
  wording(tBodyGyro.std...Z) <- "Standard deviation body gyro signals in Z-dir (in hz)"
  wording(tBodyGyroMag.mean..) <- "Mean magnitude of body gyro signals (in hz)"
  wording(tBodyGyroMag.std..) <- "Standard deviation of magnitude of body gyro signals (in hz)"
  
  wording(tBodyGyroJerk.mean...X) <- "Mean body gyro Jerk signals in X-dir (in hz)"
  wording(tBodyGyroJerk.std...X) <- "Standard deviation body gyro Jerk signals in X-dir (in hz)"
  wording(tBodyGyroJerk.mean...Y) <- "Mean body gyro Jerk signals in Y-dir (in hz)"
  wording(tBodyGyroJerk.std...Y) <- "Standard deviation body gyro Jerk signals in Y-dir (in hz)"
  wording(tBodyGyroJerk.mean...Z) <- "Mean body gyro Jerk signals in Z-dir (in hz)"
  wording(tBodyGyroJerk.std...Z) <- "Standard deviation body gyro Jerk signals in Z-dir (in hz)"
  wording(tBodyGyroJerkMag.mean..) <- "Mean magnitude of body gyro Jerk signals (in hz)"
  wording(tBodyGyroJerkMag.std..) <- "Standard deviation of magnitude of body gyro Jerk signals (in hz)"
  
  wording(fBodyAcc.mean...X) <- "FFT of tBodyAcc.mean...X (in hz)"
  wording(fBodyAcc.std...X) <- "FFT of tBodyAcc.std...X (in hz)"
  wording(fBodyAcc.mean...Y) <- "FFT of tBodyAcc.mean...Y (in hz)"
  wording(fBodyAcc.std...Y) <- "FFT of tBodyAcc.std...Y (in hz)"
  wording(fBodyAcc.mean...Z) <- "FFT of tBodyAcc.mean...Z (in hz)"
  wording(fBodyAcc.std...Z) <- "FFT of tBodyAcc.std...Z (in hz)"
  wording(fBodyAccMag.mean..) <- "FFT of tBodyAccMag.mean.. (in hz)"
  wording(fBodyAccMag.std..) <- "FFT of tBodyAccMag.std.. (in hz)" 
  
  wording(fBodyAccJerk.mean...X) <- "FFT of tBodyAccJerk.mean...X (in hz)"
  wording(fBodyAccJerk.std...X) <- "FFT of tBodyAccJerk.std...X (in hz)"
  wording(fBodyAccJerk.mean...Y) <- "FFT of tBodyAccJerk.mean...Y (in hz)"
  wording(fBodyAccJerk.std...Y) <- "FFT of tBodyAccJerk.std...Y (in hz)"
  wording(fBodyAccJerk.mean...Z) <- "FFT of tBodyAccJerk.mean...Z (in hz)"
  wording(fBodyAccJerk.std...Z) <- "FFT of tBodyAccJerk.std...Z (in hz)"
  wording(fBodyAccJerkMag.mean..) <- "FFT of tBodyAccJerkMag.mean.. (in hz)"
  wording(fBodyAccJerkMag.std..) <- "FFT of tBodyAccJerkMag.std.. (in hz)" 
  
  wording(fBodyGyro.mean...X) <- "FFT of tBodyGyro.mean...X (in hz)"
  wording(fBodyGyro.std...X) <- "FFT of tBodyGyro.std...X (in hz)"
  wording(fBodyGyro.mean...Y) <- "FFT of tBodyGyro.mean...Y (in hz)"
  wording(fBodyGyro.std...Y) <- "FFT of tBodyGyro.std...Y (in hz)"
  wording(fBodyGyro.mean...Z) <- "FFT of tBodyGyro.mean...Z (in hz)"
  wording(fBodyGyro.std...Z) <- "FFT of tBodyGyro.std...Z (in hz)"
  wording(fBodyGyroMag.mean..) <- "FFT of tBodyGyroMag.mag.. (in hz)"
  wording(fBodyGyroMag.std..) <- "FFT of tBodyGyroMag.std.. (in hz)" 
  
  wording(fBodyGyroJerkMag.mean..) <- "FFT of tBodyGyroJerkMag.mean.. (in hz)"
  wording(fBodyGyroJerkMag.std..) <- "FFT of tBodyGyroJerkMag.std.. (in hz)" 

})
Write(codebook(codebook), "./codebook.pdf")







