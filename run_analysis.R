library(dplyr)
#Download the Archive if it doesnt exist
filename<-"edaweek4_assignment.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL,filename)
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


##Read data test data and do some Cleaning
Test<-read.table("UCI HAR Dataset/test/X_test.txt")
#Read the activity file
Test.activity<-read.table("UCI HAR Dataset/test/y_test.txt")
#Read participant id
Test.id<-read.table("UCI HAR Dataset/test/subject_test.txt")
##Read Train data
Train<-read.table("UCI HAR Dataset/train/X_train.txt")
#Read the activity file
Train.activity<-read.table("UCI HAR Dataset/train/y_train.txt")
#Read participant id
Train.id<-read.table("UCI HAR Dataset/train/subject_train.txt")

##Read variable names
cnames<-read.table(("UCI HAR Dataset/features.txt"))

##rename the variables in in Test and Train data sets appropriately
names(Test)<-make.names(cnames$V2)
names(Train)<-make.names(cnames$V2)

##Add IDs to each observation (Row) to show, from which participant it came from
Test<-data.frame(Subject = Test.id$V1,Test)
Train<-data.frame(Subject = Train.id$V1,Train)
##Add Activity id Variable,
Test$Activity.No<-Test.activity$V1
Train$Activity.No<-Train.activity$V1
##Create a variable to identify the data sets,
Test$Test.Train<-rep("Test",nrow(Test))
Train$Test.Train<-rep("Train",nrow(Train))

##Step 1 Join Test and Train data sets to form one
Full.data<-rbind(Test,Train)
## Step 2 
CleanData <- Full.data %>% select(Subject, Activity.No, contains("mean"), contains("std"))
#Step 3
Activity.Description<-with(CleanData,ifelse(Activity.No==1,"WALKING",ifelse(Activity.No==2,"WALKING_UPSTAIRS",ifelse(Activity.No==3,"WALKING_DOWNSTAIRS",ifelse(Activity.No==4,"SITTING",ifelse(Activity.No==5,"STANDING","LAYING"))))))
##Add the description Variable
CleanData$Activity.Description<-Activity.Description
#Step 4
names(CleanData)<-gsub("angle", "Angle", names(CleanData))
names(CleanData)<-gsub("tBody", "TimeBody", names(CleanData))
names(CleanData)<-gsub("Acc", "Accelerometer", names(CleanData))
names(CleanData)<-gsub("Mag", "Magnitude", names(CleanData))
names(CleanData)<-gsub("BodyBody", "Body", names(CleanData))
names(CleanData)<-gsub("^t", "Time", names(CleanData))
names(CleanData)<-gsub("^f", "Frequency", names(CleanData))
names(CleanData)<-gsub("Gyro", "Gyroscope", names(CleanData))
names(CleanData)<-gsub("-mean()", "Mean", names(CleanData), ignore.case = TRUE)
names(CleanData)<-gsub("-std()", "STD", names(CleanData), ignore.case = TRUE)
names(CleanData)<-gsub("-freq()", "Frequency", names(CleanData), ignore.case = TRUE)
names(CleanData)<-gsub("gravity", "Gravity", names(CleanData))
 
#Step 5
TidyData <- CleanData%>%group_by(Subject, Activity.Description) %>%  summarise_all(funs(mean))

#Save as a csv file for uploading
write.csv(TidyData,file = "TidyData.csv",row.names = FALSE)
