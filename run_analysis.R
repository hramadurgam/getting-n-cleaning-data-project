run_analysys <- function() 
{
      
      library(reshape2)
  
      ## Set URL, download and unzip the file name:
      fileName <- "getDataSet.zip"
      fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      
      if (!file.exists(fileName))
      {
        download.file(fileURL, fileName, method="curl")
      }  
      if (!file.exists("UCI HAR Dataset")) 
      { 
        unzip(fileName) 
      }
      
      # Get activity labels
      
      activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
      activityLabels[,2] <- as.character(activityLabels[,2])
      
      # Get features and extract only the data on mean and standard deviation
      featureList <- read.table("UCI HAR Dataset/features.txt")
      featureList[,2] <- as.character(featureList[,2])
      featuresNeeded <- grep(".*mean.*|.*std.*", featureList[,2])
      featuresNeeded.names <- featureList[featuresNeeded,2]
      featuresNeeded.names = gsub('-mean', 'Mean', featuresNeeded.names)
      featuresNeeded.names = gsub('-std', 'Std', featuresNeeded.names)
      featuresNeeded.names <- gsub('[-()]', '', featuresNeeded.names)
      
      
      # Load test data
      
      testX <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresNeeded]
      testY <- read.table("UCI HAR Dataset/test/Y_test.txt")
      testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
      testData <- cbind(testSubject, testY, testX)
      
      # Load train data
      trainX <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresNeeded]
      trainY <- read.table("UCI HAR Dataset/train/Y_train.txt")
      trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
      trainData <- cbind(trainSubject, trainY, trainX)
      
      
      
      # combine datasets and labels
      aggrData <- rbind(trainData, testData)
      colnames(aggrData) <- c("subject", "activity", featuresNeeded.names)
      
      
      
      aggrData$activity <- factor(aggrData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
      aggrData$subject <- as.factor(aggrData$subject)
      
      aggrData.melted <- melt(aggrData, id = c("subject", "activity"))
      aggrData.mean <- dcast(aggrData.melted, subject + activity ~ variable, mean)
      
      
      #Write the tidy data into a file
      
      write.table(aggrData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
      

}