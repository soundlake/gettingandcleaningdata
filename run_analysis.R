# 0. Define some utilities
mload <- function(filename) {
    fp <- file(filename, 'r')
    df <- read.table(fp)
    close(fp)
    df
}
# 1. Merges the training and the test sets to create one data set.
mstep1 <- function() {
    # load names for the dataset
    nameset <- mload('UCI HAR Dataset/features.txt')
    nameset <- as.character(nameset[[2]])
    # load training data
    trainset <- mload('UCI HAR Dataset/train/X_train.txt')
    # load test data
    testset <- mload('UCI HAR Dataset/test/X_test.txt')
    # set names on two dataset
    names(trainset) <- nameset
    names(testset) <- nameset
    rbind(trainset, testset)
}
mdata <- mstep1()
# 2. Extracts only the measurements on the mean and standard deviation
#    for each measurement.
mstep2 <- function(mdata) {
    index <- grep('mean|std', names(mdata))
    mdata[,index]
}
mdata <- mstep2(mdata)
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.