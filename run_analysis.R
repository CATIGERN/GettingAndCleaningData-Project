run_analysis <- function(){
    
    ## The current working directory has been set to
    ## the UCI HAR Dataset folder using setwd() function
    
    require(dplyr)
    
    ##Loading the 3 tables from the train folder
    xtrain <- read.table("./train/X_train.txt")
    ytrain <- read.table("./train/y_train.txt")
    subjtrain <- read.table("./train/subject_train.txt")
    
    ## Actions vector relating to the subjtrain
    act <- read.table("./activity_labels.txt")
    
    ## Contains descriptive names of the actions
    ## corresponding to the ytrain dataset.
    
    actiontrain <- c()
    
    for(i in seq_along(ytrain[, 1])){
        actiontrain <- c(actiontrain, as.character(act[ytrain[i, 1], 2]))
    }
    
    ##Combining the x, y and the persons tables
    train <- cbind(subjtrain, ytrain)
    train <- cbind(train, actiontrain)
    train <- cbind(train, xtrain)
    
    ##Loading the 3 tables from the test folder
    xtest <- read.table("./test/X_test.txt")
    ytest <- read.table("./test/y_test.txt")
    subjtest <- read.table("./test/subject_test.txt")
    
    ## Contains descriptive names of the actions
    ## corresponding to the ytrain dataset.
    
    actiontest <- c()
    
    for(i in seq_along(ytest[, 1])){
        actiontest <- c(actiontest, as.character(act[ytest[i, 1], 2]))
    }
    
    ##Combining the x, y and the persons tables
    test <- cbind(subjtest, ytest)
    test <- cbind(test, actiontest)
    test <- cbind(test, xtest)
    
    ## Reading the names of the features variables
    features <- read.table("./features.txt")
    
    ## Changing names of the data to be those according to feature names
    ## Please note that Step 3 gets done over here.
    cnames <- c("SubjPerson", "ActivityNo", "ActivityName", as.vector(features[, 2]))
    colnames(train) <- cnames
    colnames(test) <- cnames
    
    ## Merging the train and test data
    ## This completes step 1
    exp_data <- rbind(train, test)
    
    ## Matching the mean()
    mean_indices <- grep("mean()", names(exp_data), ignore.case = FALSE, value = FALSE, fixed = TRUE)
    
    ## Matching the std()
    std_indices <- grep("std()", names(exp_data), ignore.case = FALSE, value = FALSE, fixed = TRUE)
    
    step2data <- exp_data[1:3]
    step2names <- names(exp_data)[1:3]
    ## Now we combine all the mean() columns
    
    for(i in seq_along(mean_indices)){
        step2data <- cbind(step2data, exp_data[,mean_indices[i]])
        step2names <- c(step2names, names(exp_data[mean_indices[i]]))
    }
    
    ## Now we combine all the std() columns
    
    for(i in seq_along(std_indices)){
        step2data <- cbind(step2data, exp_data[,std_indices[i]])
        step2names <- c(step2names, names(exp_data[std_indices[i]]))
    }
    
    colnames(step2data) <- step2names
    
    ## The above step completed step 2 data.
    
    ## Note that the columns were already named according to the features.txt
    ## file and hence this also completes the 3rd step.
    
    ## Now replacing texts with meaningful names
    newnames <- names(step2data)
    
    newnames <- sub("t", "Time", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames <- sub("f", "Frequency", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames <- sub("Acc", "Accelerometer", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames <- sub("Gyro", "Gyroscope", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames <- sub("Mag", "Magnitude", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames <- sub("graviTimey", "gravity", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames <- sub("sTimed", "std", newnames, ignore.case = FALSE, fixed = TRUE)
    newnames[2] <- "ActivityNo"
    newnames[3] <- "ActivityName"
    
    ## The variables have been replaced by meaningful names.
    ## The next assignment to the column names concludes step 4.
    colnames(step2data) <- newnames
    
    ## Now towards step 5
    ## I am basically splitting the data according to the person
    ## and then according to the activity and calculating the mean
    results = data.frame()
    
    for(i in unique(step2data[,1])){
        temp = subset(step2data, step2data[,1] == i)
        for(j in unique(temp[,2])){
            ind = subset(temp, temp[,2] == j)
            means <- lapply(ind[4:69], mean)
            results <- rbind(results, data.frame(ind[1, 1:3], means))
        }
    }
    
    results
    
}