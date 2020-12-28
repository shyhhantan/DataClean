# Getting and Cleaning Data Course Project
# Peer-graded Assignment

# Here are the data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  

# You should create one R script called run_analysis.R that does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Downloading files, check where they are located, rename for use in R:

# Check working directory
getwd()

# download the zipped files, unzip them into R readable folders:
zippedfile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zippedfile, destfile = "./data/Dataset.zip", method= "curl")
list.files("./data")
unzip(zipfile ="./data/Dataset.zip", exdir = "./data")

# Rename folders by removing spaces
recursive_replace_lowercase<-function(path=".", replace=" ", with="_",
                                      lowercase=TRUE) {
        # this is the base case
        filelist<-list.files(path, full.names=TRUE)
        if (lowercase) {
                for(filename in filelist)
                        file.rename(filename,gsub(replace,with,tolower(filename)))
        } else {
                for(filename in filelist)
                        file.rename(filename,gsub(replace,with,filename))
        }
        # and this is the recursive part
        dirlist<-list.dirs(path, full.names=TRUE, recursive=FALSE)
        if(length(dirlist)) {
                for(dirname in dirlist)
                        recursive_replace_lowercase(dirname, replace=replace, with=with,
                                                    lowercase=lowercase)
        }
}
recursive_replace_lowercase()

# Get a list of files & directories in the uci_har_dataset directory:
list.files("./data/uci_har_dataset/")
# [1] "activity_labels.txt" "features_info.txt"   "features.txt"        "readme.txt"         
# [5] "test"                "train"    

## Read in the activity_labels.txt and features.txt" files.
# Read the activity_labels.txt file as activity_labels and name the columns:
activity_labels <- read.table("./data/uci_har_dataset/activity_labels.txt")
colnames(activity_labels, do.NULL = FALSE)
colnames(activity_labels) <- c("activity_label","activity")
head(activity_labels)

# Read the features.txt" file and mae as features and name the columns:
features<- read.table("./data/uci_har_dataset/features.txt")
colnames(features, do.NULL = FALSE)
colnames(features) <- c("feature_no","feature")
str(features)

# Read in the training set files and merge the files by using cbind
# Get a list of files in the train directory:
list.files("./data/uci_har_dataset/train")

# [1] "inertial_signals"  "subject_train.txt" "x_train.txt"      
# [4] "y_train.txt"      

# Read in the subject_train.txt file as train_subjects and label the column name as "subject":
train_subjects<- read.table("./data/uci_har_dataset/train/subject_train.txt")
colnames(train_subjects, do.NULL = FALSE)
colnames(train_subjects) <- c("subject")
head(train_subjects)

# Read in the y_train.txt file as train_label and label the column name as "activity_label":
train_label<- read.table("./data/uci_har_dataset/train/y_train.txt")
colnames(train_label, do.NULL = FALSE)
colnames(train_label) <- c("activity_label")
head(train_label)

# Read in the x_train.txt file as train_set, 
# name the columnns by using the 2nd column in the "features" file:
train_set <- read.table("./data/uci_har_dataset/train/x_train.txt")
train_set_colnames <- features[,2]
colnames(train_set, do.NULL = FALSE)
colnames(train_set) <- train_set_colnames
str(train_set)

# merge the train_subjects, train_label and train_set data by using cbind:
traindf <- cbind(train_subjects,train_label,train_set)

#check the features of the merged traindf dataframe
head(traindf)
dim(traindf)
str(traindf)

## Read in the test set files and merge the files by using cbind
# Get a list of files in the test directory:
list.files("./data/uci_har_dataset/test")
# [1] "inertial_signals" "subject_test.txt" "x_test.txt"       "y_test.txt"      

# Read in the subject_test.txt file as train_subjects and label the column name as "subject":
test_subjects<- read.table("./data/uci_har_dataset/test/subject_test.txt")
colnames(test_subjects, do.NULL = FALSE)
colnames(test_subjects) <- c("subject")
head(test_subjects)

# Read in the test label file and name the column as "activity_label"
test_label<- read.table("./data/uci_har_dataset/test/y_test.txt")
colnames(test_label, do.NULL = FALSE)
colnames(test_label) <- c("activity_label")
head(test_label)

# Read in the x_train.txt file as test_set
test_set <- read.table("./data/uci_har_dataset/test/x_test.txt")
test_set_colnames <- features[,2]
colnames(test_set, do.NULL = FALSE)
colnames(test_set) <- test_set_colnames
str(test_set)

# Merge the test_subjects, test_label and test_set data by using cbind:
testdf <- cbind(test_subjects, test_label, test_set)

#check the features of the merged testdf dataframe
head(testdf)
dim(testdf)
str(testdf)

## Merge the test and training sets by using rbind
mergedf <- rbind(traindf, testdf)

#check the features of the merged dataframe mergedf
head(mergedf)
dim(mergedf)
str(mergedf)



# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Load the dplyr package
library(dplyr)

# Select the variables containing mean and standard deviation using yje select function:
meanstddf <- select(mergedf, 1:3, contains("mean"), contains("std"))

#check the features of the new dataframe:
str(meanstddf)
dim(meanstddf)
head(meanstddf)

# 3. Uses descriptive activity names to name the activities in the data set

# Use "activity_labels" data read in at the beginning.
# Use "left-join" to merge the meanstddf and activity_labels dataframes 
# by "activity_label", and rearrange the columns:
meanstddf2 <- left_join(meanstddf,activity_labels, "activity_label")

#check the features of the new meanstddf2 dataframe:
meanstddf2
str(meanstddf2)
dim(meanstddf2)

# Replace "activity_label" with the "activity" column 
# by moving it to the second column, assign dataframe to meanstddf3:
meanstddf3 <- meanstddf2[c(1, 89, 3:88)]
str(meanstddf3)
dim(meanstddf3)

# 4.Appropriately labels the data set with descriptive variable names.

# Load the stringr package
library(stringr)

# Replace the shortened words with actual words, t with Time, f and -freq() with Frequency,
# -mean() with Mean, -std() with STD:

names(meanstddf3) <- gsub("Acc", "Accelerometer",names(meanstddf3)) %>% 
        str_replace_all("Gyro", "Gyroscope") %>% 
        str_replace_all("BodyBody", "Body") %>%
        str_replace_all("Mag", "Magnitude") %>% 
        str_replace_all("^t", "Time") %>% 
        str_replace_all("^f", "Frequency") %>% 
        str_replace_all("tBody", "TimeBody") %>% 
        str_replace_all("-mean()", "Mean") %>% 
        str_replace_all("-std\\(\\)", "STD") %>% 
        str_replace_all("-freq\\(\\)", "Frequency") %>% 
        str_replace_all("angle", "Angle") %>% 
        str_replace_all("gravity", "Gravity") %>%
        print
#check the features of the new meanstddf3 dataframe:
str(meanstddf3)
dim(meanstddf3)

# 5.From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# Use dplyr to group by subject and activity
mean_activity<- meanstddf3 %>% 
        group_by(subject,activity) %>% 
        # summarise accross the variables that are numeric by mean, 
        # prepend the label "mean_" before each column
        summarise(across(where(is.numeric), ~ mean(.x), .names = "mean_{.col}")) %>% 
        print

# create the tidy data set
write.csv(meanstddf3, "mean_activity.csv")
View(mean_activity)
