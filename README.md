Initially I loaded the X_train, y_train and subject_train datasets

Then I also loaded the actions data set consisting of the 6 words
Walking, Walking Upstairs,.....Laying.

I then mapped the y_train dataset to the corresponding action into a vector actiontrain.

I then used cbind() to combine all of these datasets.

I combined it in the following manner.
train <- cbind(subjtrain, ytrain)
train <- cbind(train, actiontrain)
train <- cbind(train, xtrain)

So now, my first column is the person performing the task
The second column is the activity no that he is performing.
The third column is the name of the activity that he is performing.
And then follow 561 columns of measurements.

I followed the same procedure for the test dataset.

I then read the features dataset.

I then assigned the column names to both the datasets using these commands

cnames <- c("SubjPerson", "ActivityNo", "ActivityName", as.vector(features[, 2]))
colnames(train) <- cnames
colnames(test) <- cnames

Note that now, Step 3 has been achieved as the column names have been renamed from V1
to something meaningful.

I finally used the rbind(train, test) function to combine both the data sets.

For step 2, I needed all the columns that had the word std() and mean() in them

So, I simply used the grep command to find out the indices of the columns having these words in them.
And then I made another data frame that combined these columns, using the following code. I also had to rename the 
column names and the new data frame didn't preserve the column name of the previous data frame.

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

After this, step 1, 2 and 3 are completed.
Note I completed step 3 earlier when I applied the features dataset to individual column names.

Now for step 4, I wanted to replace t with time, f with frequency and so on.
So I made a vector containing the names of the data set created in step 2.
I would then make changes to these names and reassign them as the names to the data set
created in step 2.

I used the sub() function to replace a part of text in each name.

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

 I renamed "graviTimey" and "sTimed" because when I applied "t" -> "Time", it even replaced the t in gravity and std
 Same holds for activityno and activityname.

 I finally applied this newnames as column names for the data frame created in step 2
 ## The variables have been replaced by meaningful names.
    ## The next assignment to the column names concludes step 4.
    colnames(step2data) <- newnames

Now for step 5, I am not a particular master with summarize function, so instead I followed this principle of:
subset, apply and combine.

So what I did was that I subsetted the original data into a new data that consisted of activities for only person 1.
Then in that dataset I further subsetted it according to the activity that person 1 was doing.
Now I made a new data frame and I kept on using rbind() to combine these small subsets.

This is my code that I used.

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


    Hope I provided a clear explanation of my steps, I am sorry, but I am not very good with R, so I tend to use for loops sometimes.
    Thanks for reading it.
