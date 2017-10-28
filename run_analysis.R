
## Function has zip folder and URL, unzip file name as input parameters 
## url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## folder <- "FUCI HAR Dataset.zip"
## UCI_HAR_Fldr <- "UCI HAR Dataset"

## Check if Zip file is already downloaded in WD

run_analysis <- function(folder, url, UCI_HAR_Fldr){

  if (!require("reshape2")){
    install.packages("reshape2")
  }
  
  
  require("reshape2")
  
  
  if (file.exists(UCI_HAR_Fldr)){
    ## Unzipped folder already exists in the WD
    print("Unzipped folder already exists")
  }
  else{
      print("checking for zip file in WD")
      if (file.exists(folder)){
        ## zip file already exists
        print("Zip file already present in WD")
      }
      else{
        print("downloading zip file in WD")
        download.file(url, destfile = "FUCI HAR Dataset.zip" )
      } 
      print("unzipping zip file")
      unzip(folder) 
  }

  ## reading test and train data text files
  subject_train <- read.table(paste0("./", UCI_HAR_Fldr, "/train/subject_train.txt"))
  X_train <- read.table(paste0("./", UCI_HAR_Fldr, "/train/X_train.txt"))
  Y_train <- read.table(paste0("./", UCI_HAR_Fldr, "/train/Y_train.txt"))
  subject_test <- read.table(paste0("./", UCI_HAR_Fldr, "/test/subject_test.txt"))
  X_test <- read.table(paste0("./", UCI_HAR_Fldr, "/test/X_test.txt"))
  Y_test <- read.table(paste0("./", UCI_HAR_Fldr, "/test/Y_test.txt"))
  activity_labels <- read.table(paste0("./", UCI_HAR_Fldr, "/activity_labels.txt"))[,2]
  features_labels <- read.table(paste0("./", UCI_HAR_Fldr, "/features.txt"))[,2]
  
  ## append X_train and X_test, Y_train & Y_test, subject_train & subject_test
  
  
  all_features <- rbind(X_train, X_test)
  activity_id <- rbind(Y_train, Y_test)
  subject <- rbind(subject_train, subject_test)

  ## now add another column to activity containing activity labels
  ## adds descriptive activity names to name the activities in the data set
  activity_id[,2] <- activity_labels[activity_id[,1]]
  activity <- activity_id[,2]
  activity <- as.data.frame(activity)
  
  ## now filter out non std & mean columns from features
  filter_feature <- grepl("std|mean", features_labels)
  ## now use filter feature as the column# which are needed to be extracted from all_features
  features <- all_features[, filter_feature]
  
  ## add colnames to filtered out features
  names(features) <- features_labels[filter_feature]
  
  ## now add colnames to activity
  names(activity) <- "activity_labels"
  
  ## now add colnames to Subject
  
  names(subject) <- "subject"
  
  ## combile features with subject and activity
  mean_std_features_sub_act <- cbind(features, activity, subject)
  
 ## From the data set mean_std_desc_subj_act_final, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  melt_data <- melt(mean_std_features_sub_act, id = c("subject", "activity_labels"))
  
  ## Apply mean to other variables using Subject and then activity as grouping criteria
  tidy_data <- dcast(melt_data, subject + activity_labels ~ variable, mean)
  
  ## write to tidy_data.txt to WD
  
  write.table(tidy_data, file = "./tidy_data.txt", row.names = FALSE)
  
}
