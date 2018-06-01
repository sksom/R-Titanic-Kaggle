# Load Raw Data
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Creating a new field first and then combining it with test DF
test.survived <- data.frame(Survived=rep("None",nrow(test)),test[,])

# Earlier survived used to be in the first place but in train it is at second place
# So swap 1 and 2
train <- train[,c(2,1,3:12)]

# Combine datasets
data.combine <- rbind(train,test.survived)

# To know the structure of the R data object
str(data.combine)
summary(data.combine)

# Age has 263 NA's and Fare has 1 NA, Let's fix this
# Impute age to remove NAs
data.combine$Age[is.na(data.combine$Age)] <- median(data.combine$Age, na.rm=T)

# Extract Title and work on it
# First, get the first names containing the titles
library(stringr)
name.split <- str_split(data.combine$Name, ",")
name.split[1]
name.split2 <- sapply(name.split,"[",2)
name.split2[1:10]
# Second, get the list of vectors with each containing 2nd element as the
# title in each vector in the list
title2 <- str_split(name.split2," ")
title2[1]
# Get the title out of it
actualTitles <- sapply(title2,"[",2)
unique(actualTitles)
# What's with the title "the"?
data.combine[which(actualTitles=="the"),] 

# Re-map the title to be more exact
actualTitles[actualTitles %in% c("Dona.","the")] <- "Lady."
actualTitles[actualTitles %in% c("Ms.","Mlle.")] <- "Miss."
actualTitles[actualTitles=="Mme."] <- "Mrs."
actualTitles[actualTitles %in% c("Jonkheer.","Don.")] <- "Sir."
actualTitles[actualTitles %in% c("Col.","Capt.","Major.")] <- "Officer"
table(actualTitles)

# Make it as a new column in DF
data.combine$ActualTitle <- as.factor(actualTitles)

# Collapse some titles on the basis of visualization
indexes <- which(data.combine$ActualTitle=="Lady.")
data.combine$ActualTitle[indexes] <- "Mrs." 

indexes <- which(data.combine$ActualTitle=="Dr." |
                   data.combine$ActualTitle=="Rev." |
                   data.combine$ActualTitle=="Sir." |
                   data.combine$ActualTitle=="Officer")
data.combine$ActualTitle[indexes] <- "Mr."

# Resetting the factor levels
data.combine$ActualTitle <- factor(data.combine$ActualTitle)

# One female in "Mr."? Sorry my bad, She could be a Dr.
data.combine[which(data.combine$Sex=="female" & data.combine$ActualTitle=="Mr."),]

# Oops...Fixing it and checkinf if such mistake in other categories also
indexes <- which(data.combine$Sex=="female" & data.combine$ActualTitle=="Mr.")
data.combine$ActualTitle[indexes] <- "Mrs."

# Just a safety check
length(indexes <- which(data.combine$Sex=="female" & 
                          (data.combine$ActualTitle=="Mr." |
                             data.combine$ActualTitle=="Master.")))
# No aberrations found above

# Averaging the fare because multiple people have same ticket so their fare
# should be divided
# let's engineer some avg fare feature
tickets <- unique(data.combine$Ticket)
# Let's create the vector to hold new avg fares
avg.fares <- rep(0.0,nrow(data.combine))
peopleOnTicket <- rep(0,nrow(data.combine))

for(i in 1:length(tickets)){
  current.ticket <- tickets[i]
  peopleOnTicket.indexes <- which(data.combine$Ticket==current.ticket)
  current.avg.fare <- data.combine$Fare[peopleOnTicket.indexes[1]]/length(peopleOnTicket.indexes)
  
  for (k in 1:length(peopleOnTicket.indexes)) {
    avg.fares[peopleOnTicket.indexes[k]] <- current.avg.fare
    peopleOnTicket[peopleOnTicket.indexes[k]] <- length(peopleOnTicket.indexes)
  }
}

data.combine$AvgFare <- avg.fares
data.combine$PeopleOnTicket <- peopleOnTicket

# There is one NA in fare, Let's find out who is that
data.combine[is.na(data.combine$AvgFare),]

# let's see some similar ticket no.
indexes <- with(data.combine,which(Pclass=="3" & ActualTitle=="Mr." & PeopleOnTicket=="1" & Ticket!="3701"))
na.similar.passengers <- data.combine[indexes,]
summary(na.similar.passengers$AvgFare)
# Now populate the NA value with the median value found in the summary
# of the similar passengers
data.combine[is.na(data.combine$AvgFare),"AvgFare"] <- 7.84

# binarize all factors
library(caret)
nonvars <- c("Name","Ticket","Cabin")
data.combine2 <- data.combine[1:891,!(names(data.combine) %in% nonvars)]
data.combine2$Survived <- factor(data.combine2$Survived)
data.combine2$Survived <- as.numeric(as.character(data.combine2$Survived))

# Reorder data set so target is last column
# So swap 1 and 12
data.combine2 <- data.combine2[,c(12,2:11,1)]

titanicDummy <- dummyVars("~.",data=data.combine2, fullRank=F)
data.combine2 <- as.data.frame(predict(titanicDummy,data.combine2))
str(data.combine2)

# Running a simple Gradient Boosted Machine (GBM) model on the data 
# split data set into train and test portion
set.seed(1234)
splitIndex <- sample(nrow(data.combine2), floor(0.5*nrow(data.combine2)))
trainDF <- data.combine2[ splitIndex,]
testDF  <- data.combine2[-splitIndex,]

outcomeName <- c("Survived")
predictorNames <- setdiff(names(trainDF),outcomeName)

# transform outcome variable to text as this is required in caret for classification 
trainDF[,outcomeName] <- ifelse(trainDF[,outcomeName]==1,"yes","nope")

# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method="cv", number=2, returnResamp="none", summaryFunction = twoClassSummary, classProbs = TRUE)

objGBM <- train(trainDF[,predictorNames],  as.factor(trainDF[,outcomeName]),
                method="gbm",
                trControl=objControl,
                metric = "ROC",
                tuneGrid = expand.grid(n.trees = 5, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode=1))

predictions <- predict(object=objGBM, testDF[,predictorNames], type='prob')

# AUC(area under curve) approximation
GetROC_AUC = function(probs, true_Y){
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix) 
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)   
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(auc)
}

refAUC <- GetROC_AUC(predictions[[2]],testDF[,outcomeName])
print(paste('AUC score:', refAUC))

# Shuffle predictions for variable importance
AUCShuffle <- NULL
shuffletimes <- 500

featuresMeanAUCs <- c()
for (feature in predictorNames) {
  featureAUCs <- c()
  shuffledData <- testDF[,predictorNames]
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    predictions <- predict(object=objGBM, shuffledData[,predictorNames], type='prob')
    featureAUCs <- c(featureAUCs,GetROC_AUC(predictions[[2]], testDF[,outcomeName]))
  }
  featuresMeanAUCs <- c(featuresMeanAUCs, mean(featureAUCs < refAUC))
}
AUCShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanAUCs)
AUCShuffle <- AUCShuffle[order(AUCShuffle$importance, decreasing=TRUE),]
print(AUCShuffle)

# bonus - great package for fast variable importance
library(mRMRe)
ind <- sapply(data.combine2, is.integer)
data.combine2[ind] <- lapply(data.combine2[ind], as.numeric)
dd <- mRMR.data(data = data.combine2)
feats <- mRMR.classic(data = dd, target_indices = c(ncol(data.combine2)), feature_count = 10)
variableImportance <-data.frame('importance'=feats@mi_matrix[nrow(feats@mi_matrix),])
variableImportance$feature <- rownames(variableImportance)
row.names(variableImportance) <- NULL
variableImportance <- na.omit(variableImportance)
variableImportance <- variableImportance[order(variableImportance$importance, decreasing=TRUE),]
print(variableImportance)

################################################################################
# Feature importance finding done
################################################################################

# Let's start prediction
library(randomForest)
# Train the randomForest with the two best features we found, Title and Pclass
rf.train.1 <- data.combine2[,-19]
rf.train.1 <- rf.train.1[,-2]
rf.train.1 <- rf.train.1[,-8]
rf.label <- as.factor(data.combine2$Survived)

# setting seed for Random number generator for RF
set.seed(1234)
rf.1 <- randomForest(x=rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Create a test DF subset out of the original test data provided but
# containing only the features you used to build your model
nonvars2 <- c("Name","Ticket","Cabin")
data.combine3 <- data.combine[892:1309,!(names(data.combine) %in% nonvars)]
data.combine3 <- data.combine3[,-1]
data.combine3 <- data.combine3[,-1]
data.combine3 <- data.combine3[,-6]

# Reorder data set so target is last column
# So swap 1 and 12
data.combine3 <- data.combine3[,c(9,1:8)]

titanicDummy <- dummyVars("~.",data=data.combine3, fullRank=F)
data.combine3 <- as.data.frame(predict(titanicDummy,data.combine3))
str(data.combine3)



testSubmit <- data.combine3

# Make predictions using the random forest model rf.5 above
predictSubmit <- predict(rf.1,testSubmit)

# Save these predictions into CSV file
csvDF <- data.frame(PassengerId=rep(892:1309),Survived=predictSubmit)

write.csv(csvDF,file="submission_6.csv",row.names = FALSE)
