run_analysis.R <- function() {
        
        if(file.exists("./cache/my_data.RData")) { 
                load("./cache/my_data.RData")
        } else {
                # read.csv, do whatever pre-processing
                # connect to a remote db ...  
                xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
                ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
                subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
                xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
                ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
                subtest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
                features <- read.table("./UCI HAR Dataset/features.txt")
                actlabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
                save(xtest, ytest, subtest, xtrain, ytrain, subtrain, features, actlabels, file = "./cache/my_data.RData")
        }
        
        #1. Merges the training and the test sets to create one data set.
        
        #dim(xtrain), dim(ytrain), dim(subtrain)
        #7352 561, 7352 1, 7352 1
        #dim(xtest), dim(ytest), dim(subtest)
        #2947 561, 2947 1, 2947 1
        #dim(features), dim(actlabels)
        #561 2, 6 2
        
        data <- rbind(xtrain, xtest)
        dim(data)    
        
        featuresV <- as.vector(features$V2)
        for (i in 1:561){
                colnames(data)[i] <- featuresV[i]
        }    
        
        sub <- rbind(subtrain, subtest)
        act1 <- rbind(ytrain, ytest)
        
        data1 <- cbind(data, sub, act1)
        head(data1)
        
        #2. Extracts only the measurements on the mean and standard deviation for each measurement. 
        count <- vector()
        for (i in 1:563){
                if (grepl("mean",colnames(data)[i]) == TRUE || grepl("std",colnames(data)[i]) == TRUE){
                        count[i] <- i
                } 
        }    
        
        count <- count[!is.na(count)]
        data2a <- data1[,count]
        data2 <- cbind(data2a, sub, act1)
        head(data2)
        
        #3. Uses descriptive activity names to name the activities in the data set
        act3 <- act1
        for (i in 1:10299){
                if (act3[i,1] == 1){
                        act3[i,1] <- as.vector(subset(actlabels, V1 == 1)[,2])
                } else if (act3[i,1] == 2) {
                        act3[i,1] <- as.vector(subset(actlabels, V1 == 2)[,2])
                } else if (act3[i,1] == 3) {
                        act3[i,1] <- as.vector(subset(actlabels, V1 == 3)[,2])
                } else if (act3[i,1] == 4) {
                        act3[i,1] <- as.vector(subset(actlabels, V1 == 4)[,2])
                } else if (act3[i,1] == 5) {
                        act3[i,1] <- as.vector(subset(actlabels, V1 == 5)[,2])
                } else if (act3[i,1] == 6) {
                        act3[i,1] <- as.vector(subset(actlabels, V1 == 6)[,2])
                }
        }   
        
        data3 <- cbind(data2a, sub, act3)
        head(data3)
        
        #4. Appropriately labels the data set with descriptive variable names. 
        
        data4 <- data2a
        sub4 <- sub
        colnames(sub4) <- c("Subject")
        act4 <- act3
        colnames(act4) <- c("Activity")
        
        data4 <- cbind(data2a, sub4, act4)
        head(data4)
        
        #5. From the data set in step 4, creates a second, independent tidy data set with the average of 
        #?each variable for each activity and each subject.
        
        if(suppressWarnings(!require("dplyr"))) {
                install.packages("dplyr")
        }
        
        library("dplyr")
        
        activitygroup <- group_by(data4, Activity)
        subjectgroup <- group_by(data4, Subject)
        data5a <- summarise_each(activitygroup, funs(mean))
        data5b <- summarise_each(subjectgroup, funs(mean))
        
        data5c <- cbind(data5a, data5b)
        data5c[,162] <- NULL
        subject <- rep(1:30)
        
        data5 <- cbind(subject, data5c)
        data5
        
        write.table(data5, row.name = FALSE)
}

