# Check if packages are installed, if not, install them    
if (!require(lubridate)) install.packages("lubridate") 
if (!require(data.table)) install.packages("data.table")
if (!require(dplyr)) install.packages("dplyr")

library(lubridate)    
library(data.table)
library(dplyr)

# get data from URL and unzip it...
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "data.zip"))
unzip(zipfile = "data.zip")

#Read all the information from dataset
trainX<-read.table("./UCI HAR Dataset/train/X_train.txt")
testX<-read.table("./UCI HAR Dataset/test/X_test.txt")
trainY<-read.table("./UCI HAR Dataset/train/y_train.txt")
testY<-read.table("./UCI HAR Dataset/test/y_test.txt")
Subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
Subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
activity_lables<-read.table("./UCI HAR Dataset/activity_labels.txt")
features<-read.table("./UCI HAR Dataset/features.txt")

# 1- Merges the training and the test sets to create one data set
juntos<-rbind(trainX,testX)

#2- Extracts only the measurements on the mean and standard deviation for each measurement. 
#2.1 - Change column names
colnames(juntos) <- c(as.character(features[,2]))

#2.2 - Find the positions of the columns that contain 'mean()' and 'std()'
mean_sd_posicoes <- grep("mean\\(\\)|std\\(\\)", colnames(juntos))

#2.3 -  Select the columns corresponding to the found positions
mean_sd <- juntos[, mean_sd_posicoes]

# 3- Uses descriptive activity names to name the activities in the data set
#3.1 -Merges the training and the test of ativity sets to create one data set 
juntosY<-rbind(trainY,testY)
#3.2 -Merges the  ativity and means and SD sets 
juntosAtividades<-cbind(juntosY,mean_sd)
#3.3 - Change activity column name 
colnames(juntosAtividades)[1] <- "Activity"

# 4 - Appropriately labels the data set with descriptive variable names. 

#4.1 - Converts the elements of the second column of the  activity_labels into characters.
activity_lables[,2]<-as.character(activity_lables[,2])

#4.2 - Replace the number of activities by is description
for (i in 1:length(juntosAtividades[, 1])) {
  juntosAtividades[i, 1] <- activity_lables[juntosAtividades[i, 1], 2]
}

# 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# 5.1- Merges the subject training and the test sets 
Subject<-rbind(Subject_train,Subject_test)
# 5.2- Merges the subject - Activities - Means and SD 
Todos<-cbind(Subject,juntosAtividades)
# 5.3- Rename the column Subject
colnames(Todos)[1] <- "Subject"
# 5.4- Create an aggregation by Subject and activity and calculate the mean for each column
Tidy <- Todos %>%
  group_by(Subject, Activity) %>%
  summarise_all(mean)
#write.table(Todos, file = "Todos.txt", row.names = FALSE)

# 5.5- Export the final table
write.table(Tidy, file = "FinalData.txt", row.names = FALSE)
