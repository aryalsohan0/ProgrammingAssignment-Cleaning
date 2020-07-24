
# 1. Merging train and test data after reading data

temp_trainX <- read.table("./train/X_train.txt")
temp_testX <- read.table("./test/X_test.txt")

X <- rbind(temp_trainX, temp_testX)

temp_trainy <- read.table("./train/y_train.txt")
temp_testy <- read.table("./test/y_test.txt")

Y <- rbind(temp_trainy, temp_testy)

temp_trainsub <- read.table("./train/subject_train.txt")
temp_testsub <- read.table("./test/subject_test.txt")

subject <- rbind(temp_trainsub, temp_testsub)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement

features <- read.table("features.txt")

features_index <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

# subsetting measurements on mean and sd

X <- X[, features_index]

names(X) <- features[features_index, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))


# 3. Uses descriptive activity names to name the activities in the data set

activity <- read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
Y[, 1] = activity[Y[ , 1], 2]
names(Y) <- "activity"


# 4. Appropriately labels the data set with descriptive variable names

names(subject) <- "subject"
clean <- cbind(subject, Y, X)
write.table(clean, "clean_and_tidy_data.txt")



# 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects <- unique(subject)[, 1]
numSubjects <- length(unique(subject)[, 1])
numActivities <- length(activity[, 1])
numColumns <- dim(clean)[2]
result <- clean[1:(numSubjects*numActivities), ]


row <- 1
for (s in 1:numSubjects){
        for (a in 1:numActivities){
                result[row, 1] <- uniqueSubjects[s]
                result[row, 2] <- activity[a, 2]
                temp <- clean[clean$subject == s & clean$activity == activity[a, 2], ]
                result[row, 3:numColumns] <- colMeans(temp[, 3:numColumns])
                row <- row + 1
        }
}


write.table(result, "data_set_with_the_averages.txt")



