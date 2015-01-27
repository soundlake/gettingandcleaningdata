# 0. Define some utilities
mload <- function(filename) {
    fp <- file(filename, 'r')
    df <- read.table(fp)
    close(fp)
    df
}



# 1. Merges the training and the test sets to create one data set.
mstep1 <- function() {
    integrate.data <- function(type) {
        path.root <- 'UCI HAR Dataset'

        # load names for the dataset
        path.nameset <- paste(path.root, 'features.txt', sep = '/')
        nameset <- mload(path.nameset)
        nameset <- as.character(nameset[[2]])

        # load the main data
        name.dataset <- paste('X_', type, '.txt', sep = '')
        path.dataset <- paste(path.root, type, name.dataset, sep = '/')
        dataset <- mload(path.dataset)

        # label the main data
        names(dataset) <- nameset

        # load the subject data
        name.subject <- paste('subject_', type, '.txt', sep = '')
        path.subject <- paste(path.root, type, name.subject, sep = '/')
        subject <- mload(path.subject)
        names(subject) <- 'lSubject'

        # load the activity data
        name.activity <- paste('y_', type, '.txt', sep = '')
        path.activity <- paste(path.root, type, name.activity, sep = '/')
        activity <- mload(path.activity)
        names(activity) <- 'lActivity'

        # combind subject and activity to main dataset
        # and return
        cbind(dataset, subject, activity)
    }

    # load each data
    trainset <- integrate.data('train')
    testset <- integrate.data('test')

    # combine and return
    rbind(trainset, testset)
}
mdata <- mstep1()



# 2. Extracts only the measurements on the mean and standard deviation
#    for each measurement.
mstep2 <- function(mdata) {
    index <- grep('mean|std|^l', names(mdata))
    mdata[,index]
}
mdata <- mstep2(mdata)



# 3. Uses descriptive activity names to name the activities in the data set.
mstep3 <- function(mdata) {
    # load the activity names
    activitiyNames <- mload('UCI HAR Dataset/activity_labels.txt')
    activitiyNames <- activitiyNames[[2]]

    # integer to labeled factor
    mdata$lActivity <- factor(mdata$lActivity, labels = activitiyNames)
    
    mdata
}
mdata <- mstep3(mdata)



# 4. Appropriately labels the data set with descriptive variable names.



# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.