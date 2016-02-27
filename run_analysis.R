## This script will perform the following steps on the UCI HAR Dataset downloaded 
## from 

## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
##

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Use descriptive activity names to name the activities in the data set
## 4. Appropriately label the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## For ease of this project, the assumption is data is downloaded from the above link
## and unzipped. Unzipped files are in folder UCI HAR Dataset 

## Read data related to features and activity types from files       
features     = read.table('./UCI HAR Dataset/features.txt',header=FALSE)
activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)

## Read data reated to training  
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain       = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)

# Read data related to test
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest       = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest       = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)


## Assigin column names to the above data set

## assigning column names to training data set
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"


## assigning column names to test data set
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "activityId"

## Merge and create the final data set 

## training data 
trainingFinal = cbind(yTrain,subjectTrain,xTrain)

## test data
testFinal = cbind(yTest,subjectTest,xTest)


## Merge training and test data set to get final merged data set
mergedData = rbind(trainingFinal,testFinal)



## Create a vector for the column names from the merged Data, which will be used
## to select the mean() & stddev() columns
colNames  = colnames(mergedData)

# 2. To extract only the mean and standard deviation for each measurement. 

ncolmn <- ncol(mergedData)
colmn <- c(1,2) ## Colmun are activity ID, subject ID

## To identify all column where mean and standard deviation is

for ( i in 3:ncolmn) {
        
        if (grepl("mean\\()",colNames[i])) {
                colmn <- c(colmn,i)
        }
        if (grepl("std\\()",colNames[i])) {
                colmn <- c(colmn,i)
        }
}

## To retrieve only the mean and standard deviation column
mergedData = mergedData[,colmn]

# 3. Use descriptive activity names to name the activities in the data set


# Merge the mergedData set with the acitivityType table to include descriptive activity names
mergedData = merge(mergedData,activityType,by='activityId',all.x=TRUE)


# Updating the colNames vector to include the new column names after merge
colNames  = colnames(mergedData)


# 4. Appropriately label the data set with descriptive activity names. 


# To clean variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}


# Reassigning the new descriptive column names to the finalData set
colnames(mergedData) = colNames


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 


# Create a new table, finalDataset without the activityType column
finalDataset  = mergedData[,names(mergedData) != 'activityType']

# Summarizing the finalDataset to include mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataset[,names(finalDataset) != c('activityId','subjectId')],by=list(activityId=finalDataset$activityId,subjectId = finalDataset$subjectId),mean)


# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)



# Export the tidyData set 
write.table(tidyData, './UCI HAR Dataset/tidyData.txt',row.names=FALSE,sep='\t')


