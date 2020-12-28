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

## Downloading files
# Check working directory
# downloading files, 
# check where they are located, 
# rename for use in R:

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

## Read in the activity_labels.txt and features.txt" files.

# Read the features.txt" file and mae as features and name the columns:
features<- read.table("./data/uci_har_dataset/features.txt")

# Read the activity_labels.txt file as activity_labels and name the columns:
activity_labels <- read.table("./data/uci_har_dataset/activity_labels.txt")
colnames(activity_labels) <- c("activity_label","activity")

## Read in the training set files and merge the files by using cbind

# Read in the trainig set files
train_subjects<- read.table("./data/uci_har_dataset/train/subject_train.txt")
train_label<- read.table("./data/uci_har_dataset/train/y_train.txt")
train_set <- read.table("./data/uci_har_dataset/train/x_train.txt")

# Merge the train_subjects, train_label and train_set data by using cbind:
traindf <- cbind(train_subjects,train_label,train_set)

## Read in the test set files and merge the files by using cbind
test_subjects<- read.table("./data/uci_har_dataset/test/subject_test.txt")
test_label<- read.table("./data/uci_har_dataset/test/y_test.txt")
test_set <- read.table("./data/uci_har_dataset/test/x_test.txt")

# Merge the test_subjects, test_label and test_set data by using cbind:
testdf <- cbind(test_subjects, test_label, test_set)

## Merge the test and training sets by using rbind
mergedf <- rbind(traindf, testdf)

# Name the columns of the dataframe, using the variables description in the features file:
train_set_colnames <- features[,2]
colnames(mergedf) <- c("subject", "activity_label", train_set_colnames)
str(mergedf)
head(mergedf)[,1:5]


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Load the dplyr package
library(dplyr)

# Select the variables containing mean and standard deviation using the "select" function:
meandf <- select(mergedf, 1:2, matches("[Mm]ean"), contains("std"))
dim(meandf)
head(meandf)[,1:5]

# 3. Uses descriptive activity names to name the activities in the data set
# Use "left-join" to merge the meandf and activity_labels dataframes joining by "activity_label":
meandf2 <- left_join(meandf,activity_labels, "activity_label")
dim(meandf2)
head(meandf2)[,85:89]

# Rearrange the columns: moving "activity_label" to column 2 and assign dataframe to meandf3:
meandf3 <- meandf2[c(1, 89, 3:88)]
head(meandf3)[,1:5]
dim(meandf3)

# 4.Appropriately labels the data set with descriptive variable names.
# Load the stringr package

library(stringr)

# Replace abbreviations in variable names with actual words: BodyBody, with Body; 
# Acc with Accelerometer; Gyro with Gyroscope; Mag with Magnitude; t with Time; 
# f and -freq() with Frequency; tBody with TimeBody; angle with Angle;
# gravity with Gravity, -mean() with Mean, and -std() with STD:

names(meandf3) <- gsub("Acc", "Accelerometer",names(meandf3)) %>% 
        str_replace_all("Gyro", "Gyroscope") %>% 
        str_replace_all("BodyBody", "Body") %>%
        str_replace_all("Mag", "Magnitude") %>% 
        str_replace_all("^t", "Time") %>% 
        str_replace_all("^f", "Frequency") %>% 
        str_replace_all("tBody", "TimeBody") %>%
        str_replace_all("\\-", "") %>%
        str_replace_all("\\`", "") %>%
        str_replace_all("\\(\\)", "") %>%
        str_replace_all("[Mm]ean", "Mean") %>% 
        str_replace_all("std", "STD") %>% 
        str_replace_all("freq", "Frequency") %>% 
        str_replace_all("angle", "Angle") %>% 
        str_replace_all("gravity", "Gravity") %>%
        print


#check the features of the new meandf3 dataframe:
dim(meandf3)
str(meandf3)
head(meandf3)[,1:5]

# 5.From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# Use dplyr to group by subject and activity
mean_activity<- meandf3 %>% 
        group_by(subject,activity) %>% 
        # summarise accross the variables that are numeric by mean, 
        # prepend the label "mean_" before each column
        summarise(across(where(is.numeric), ~ mean(.x), .names = "mean_{.col}")) %>% 
        print

# create the tidy data set as a .txt file
write.table(mean_activity, "mean_activity.txt",row.name=FALSE)
View(mean_activity)
