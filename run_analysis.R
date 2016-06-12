##Step1. Merges train data and test data
traindata<-read.table("./train/x_train.txt")
traindatay<-read.table("./train/y_train.txt")
traindatasub<-read.table("./train/subject_train.txt")

texdata<-read.table("./test/x_test.txt")
textdatay<-read.table("./test/y_test.txt")
textdatasub<-read.table("./test/subject_test.txt")

mergedataxy<-rbind(traindata,texdata)
mergedatay<-rbind(traindatay,textdatay)
mergedatasubject<-rbind(traindatasub,textdatasub)


##Step2.Extracts only the measurements on the mean and standard deviation for each measurement.
features<-read.table("./features.txt")
featuresmeanstd<-grep("mean\\(\\)|std\\(\\)",features[,2])
mergedataxy <- mergedataxy[, featuresmeanstd]
names(mergedataxy)<-gsub("\\(\\)","",features[featuresmeanstd,2])
names(mergedataxy)<-gsub("-", "", names(mergedataxy))


##Step3. Uses descriptive activity names to name the activities in the data set 
activity<-read.table("./activity_labels.txt")
activity[,2]<-gsub("_","",activity$V2)
activityLabel <- activity[mergedatay[, 1], 2]
mergedatay[, 1] <- activityLabel
names(mergedatay) <- "Activity"


##Step4.Appropriately labels the data set with descriptive variable names.
names(mergedatasubject)<-"Subject"
dataset1<-cbind(mergedatasubject,mergedatay,mergedataxy)
write.table(dataset1,"merged_data.txt")

##Creates a second, independent tidy data set with the average of each variable for 
##each activity and each subject.
subjectmean<-aggregate(dataset1[,3:68],list(dataset1$Subject),mean)
subjectactivitymean<-aggregate(dataset1[,3:68],by=list(dataset1$Subject,dataset1$Activity),mean)
names(subjectactivitymean)[names(subjectactivitymean)=="Group.1"]="Subject"
names(subjectactivitymean)[names(subjectactivitymean)=="Group.2"]="Activity"
dataset2<-subjectactivitymean[order(subjectactivitymean$Subject),]
write.table(dataset2,"average_merge_data.txt")
