
# Part 1, Data Wrangling
# Load Raw Data
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Creating a new field first and then combining it with test DF
test.survived <- data.frame(survived=rep("None",nrow(test)),test[,])

# Earlier survived used to be in the first place but in train it is at second place
# So swap 1 and 2
train <- train[,c(2,1,3:12)]

# Combine datasets
data.combine <- rbind(train,test.survived)

# Renaming column "survived" to "Survived" in test.survived to avoid name mismatch
colnames(test.survived)[1] <- "Survived" 

# (Run again) Combine datasets
data.combine <- rbind(train,test.survived)

# To know the structure of the R data object
str(data.combine)

# To change the data type of some field in a DF
data.combine$Survived <- as.factor(data.combine$Survived)
data.combine$Pclass <- as.factor(data.combine$Pclass)

# (Run Again) To know the structure of the R data object
str(data.combine)


# Part 2, Data Analytics
# Get a summary of a field in tabular format
table(data.combine$Survived)

table(data.combine$Pclass)

# Load ggplot2 library if it's already installed else first install then load
library(ggplot2)

# Changing the data type of pclass for train DF
train$Pclass <- as.factor(train$Pclass)

# Now we can use ggplot2 for visualisation
ggplot(train,aes(x=Pclass, fill=factor(Survived)))+
  geom_bar(width=0.5)+
  xlab("PClass")+
  ylab("Total_Count")+
  labs(fill="Survived")

# To grab first few entries from a list of entries
head(as.character(train$Name))

# To find the count of unique names the combined DF
length(unique(as.character(data.combine$Name)))

# So we know now that there are two duplicate records
# Now finding out the duplicate records and saving them as a vector
dup.names <- as.character(data.combine[which(duplicated(as.character(data.combine$Name))), "Name"])

# Fetch these duplicate entry records from our DF
data.combine[which(data.combine$Name %in% dup.names),]

# Load library stringr
library(stringr)

# Fetch all the records from DF with "Miss." in it
misses <- data.combine[which(str_detect(data.combine$Name, "Miss.")),]
misses[1:5,]

# Do the same above thing with "Mrs."
mrses <- data.combine[which(str_detect(data.combine$Name, "Mrs.")),]
mrses[1:5,]

# Searching based on column value itself
males <- data.combine[which(train$Sex == "male"),]
males[1:5,]

# Create a utility function to extract Titles from names
extractTitle <- function(name){
  name <- as.character(name)
  if(length(grep("Mrs.",name)>0)){
    return("Mrs.")
  }else if(length(grep("Miss.",name)>0)){
    return("Miss.")
  }else if(length(grep("Mr.",name)>0)){
    return("Mr.")
  }else if(length(grep("Master.",name)>0)){
    return("Master.")
  }else{
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combine)) {
  titles <- c(titles,extractTitle(data.combine[i,"Name"]))
}

data.combine$Title <- as.factor(titles)

# Now lets plot the information we have created
ggplot(data.combine[1:891,], aes(x=Title,fill=Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("Title")+
  ylab("Total_Count")+
  labs(fill="Survived")







# Part 2, Analysis continued....
# Distribution of data on the basis of Sex
table(data.combine$Sex)

# Visualisation on the basis of above distribution
ggplot(data.combine[1:891,], aes(x=Sex,fill=Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("Sex")+
  ylab("Total_Count")+
  labs(fill="Survived")

# Now let's examine the age feature for a moment
summary(data.combine$Age)
summary(data.combine[1:891,"Age"])

# Just to be thorough that we actually can't use this feature 
# at least not as it is due to missing data values
# So let's have a ggplot for it
ggplot(data.combine[1:891,], aes(x=Age,fill=Survived))+
  facet_wrap(~Sex + Pclass)+
  geom_histogram(binwidth = 10)+
  xlab("Age")+
  ylab("Total Count")

# Let's extract all the "Master." Title records and see if they're all children
boys <- data.combine[which(data.combine$Title=="Master."),]
summary(boys$Age)

# Let's extract all the "Miss." Title records and see what we can say about it
misses <- data.combine[which(data.combine$Title=="Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived!="None",], aes(x=Age,fill=Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)+
  ggtitle("Age for 'Miss.' by PClass")+
  xlab("Age")+
  ylab("Total Count")

# Now let's dig a little more about Misses who are travelling alone
misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<=14.5))

# Now let's examine feature "sibsp"
summary(data.combine$SibSp)

# Well as we can see that no. of sibsp is limited to 8
# Can we use sibsp as factor?
length(unique(data.combine$SibSp))

# Since it's 7 we can pretty much use it as a factor, So Let's do it
data.combine$SibSp <- as.factor(data.combine$SibSp)

# We converted it into factor let's visualize it now
ggplot(data.combine[1:891,], aes(x=SibSp,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Sibsp")+
  ylab("Total_Count")+
  ylim(0,300)+
  labs(fill="Survived")

# Let's do the same thing with the "parch" feature
data.combine$Parch <- as.factor(data.combine$Parch)
ggplot(data.combine[1:891,], aes(x=Parch,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Parch")+
  ylab("Total_Count")+
  ylim(0,300)+
  labs(fill="Survived")

# Now let's do some feature(column) extraction, Fancy term: Feature engineering
temp.sibsp <- c(train$SibSp, test$SibSp)
# why train and test why not data.combine? bcz we converted it into factors
temp.parch <- c(train$Parch, test$Parch)
data.combine$FamilySize <- as.factor(temp.sibsp+temp.parch+1)
# +1 for including self

# Let's plot it
ggplot(data.combine[1:891,], aes(x=FamilySize,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Family Size")+
  ylab("Total_Count")+
  ylim(0,300)+
  labs(fill="Survived")


# Part 3, Feature engineering contd....
# Take a look at the ticket variable
str(data.combine$Ticket)

# AS we can see it's a factor with 929 levels which means it really shouldn't 
# be factor but string
# So, Let's convert it
data.combine$Ticket <- as.character(data.combine$Ticket)
data.combine$Ticket[1:20]

# Well there's no clear pattern that we can see here but can we
# determine something from the first character of the ticket no.?
# Let's extract the first character from the ticket no.
ticket.firstChar <- ifelse(data.combine$Ticket==""," ",substr(data.combine$Ticket,1,1))
unique(ticket.firstChar)

# Now this can be a factor with just 16 levels
# Let's convert it
data.combine$Ticket.firstChar <- as.factor(ticket.firstChar)

# ggplot it
ggplot(data.combine[1:891,], aes(x=Ticket.firstChar, fill= Survived))+
  geom_bar()+
  ggtitle("Survivability by Ticket.FirstChar")+
  xlab("Ticket.FirstChar")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

# Not that clear idea, Let's break it on the basis of PClass
ggplot(data.combine[1:891,], aes(x=Ticket.firstChar, fill= Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("Ticket.FirstChar")+
  ylab("Total Count")+
  ylim(0,150)+
  labs(fill="Survived")

# still not satisfactory
# Let's break it further more on the basis of Titles we extracted
ggplot(data.combine[1:891,], aes(x=Ticket.firstChar, fill= Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Ticket.FirstChar")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")
# Well ticket no. is probably not very helpful to give ani information

# Let's proceed to next feature, Fare
str(data.combine$Fare)
summary(data.combine$Fare)
length(unique(data.combine$Fare))

# ggplot this
ggplot(data.combine[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram(binwidth = 5)+
  ggtitle("Survivability by Fare")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

# Let's break it down further
ggplot(data.combine[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,50)+
  labs(fill="Survived")
# More or less the same predictive power as already found title feature

# What about the cabin variable?
str(data.combine$Cabin)
# Again not really a factor bcz of 187 levels

# Convert it into string
data.combine$Cabin <- as.character(data.combine$Cabin)
data.combine$Cabin[1:100]

# As found, we can extract the first letter of the cabin and probably know their deck
# but there are lots of blanks, Let's first give them a letter
data.combine[which(data.combine$Cabin==""),"Cabin"] <- "U"
data.combine$Cabin[1:100]

# Now extract the first letter from the cabin
cabin.firstChar <- as.factor(substr(data.combine$Cabin,1,1))
str(cabin.firstChar)
levels(cabin.firstChar)

# Combine it with the data.combine DF as a new column
data.combine$Cabin.firstChar <- cabin.firstChar

# ggplot this
ggplot(data.combine[1:891,], aes(x=Cabin.firstChar, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by Cabin.firstChar")+
  xlab("Cabin.firstChar")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

# Same info, nothing fancy
ggplot(data.combine[1:891,], aes(x=Cabin.firstChar, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Cabin.firstChar")+
  ylab("Total Count")+
  ylim(0,500)+
  labs(fill="Survived")

# What about the folks with the multiple cabins?
data.combine$MultipleCabins <- as.factor(ifelse(str_detect(data.combine$Cabin," "),"Y","N"))

# ggplot this
ggplot(data.combine[1:891,], aes(x=MultipleCabins, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Multiple cabins")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

# Last but not the least the "embarked" feature
str(data.combine$Embarked)
levels(data.combine$Embarked)

# ggplot this
ggplot(data.combine[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("PClass, Title")+
  xlab("Embarked")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")


####################################################################
# Part 4, Data Modelling starts from here (Exploratory Modelling)
####################################################################

# Loading Random Forest library
library(randomForest)

# Train the randomForest with the two best features we found, Title and Pclass
rf.train.1 <- data.combine[1:891,c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

# setting seed for Random number generator for RF
set.seed(1234)
rf.1 <- randomForest(x=rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Train the randomForest with an additional feature "SibSp"
rf.train.2 <- data.combine[1:891,c("Pclass", "Title" , "SibSp")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.2 <- randomForest(x=rf.train.2, y=rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train the randomForest with an additional feature "Parch"
rf.train.3 <- data.combine[1:891,c("Pclass", "Title" , "Parch")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3, y=rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Train the randomForest with an additional feature "Parch" and "SibSp"
rf.train.4 <- data.combine[1:891,c("Pclass", "Title" , "SibSp" , "Parch")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.4 <- randomForest(x=rf.train.4, y=rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train the randomForest with an additional feature "FamilySize"
rf.train.5 <- data.combine[1:891,c("Pclass", "Title" , "FamilySize")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train the randomForest with an additional feature "SibSp" & "FamilySize"
rf.train.6 <- data.combine[1:891,c("Pclass", "Title" ,"SibSp" , "FamilySize")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.6 <- randomForest(x=rf.train.6, y=rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Train the randomForest with an additional feature "Parch" & "FamilySize"
rf.train.7 <- data.combine[1:891,c("Pclass", "Title" ,"Parch" , "FamilySize")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.7 <- randomForest(x=rf.train.7, y=rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# Train the randomForest with an additional feature "SibSp", Parch" & "FamilySize"
rf.train.8 <- data.combine[1:891,c("Pclass", "Title" ,"SibSp" ,"Parch" , "FamilySize")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.8 <- randomForest(x=rf.train.8, y=rf.label, importance = TRUE, ntree = 1000)
rf.8
varImpPlot(rf.8)

#####################BEST################################
# Train the randomForest with an additional feature "FamilySize"
rf.train.5 <- data.combine[1:891,c("Pclass", "Title" , "FamilySize")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

########################################################
# Part 5, Cross Validation
########################################################

# Getting error estimate from the training data itself
# Like K-Fold cross validation; Helps to avoid overfitting

# Create a test DF subset out of the original test data provided but
# containing only the features you used to build your model
testSubmit <- data.combine[892:1309, c("Pclass", "Title", "FamilySize")]

# Make predictions using the random forest model rf.5 above
predictSubmit <- predict(rf.5,testSubmit)

# Save these predictions into CSV file
csvDF <- data.frame(passengerId=rep(892:1309),survived=predictSubmit)

write.csv(csvDF,file="submission_1.csv",row.names = FALSE)

# Now we can start doing Cross-Validation
# Load two libraries: caret(can work in parallel) and doSNOW,
# caret for K-fold CV, doSNOW for parallelisation of work for caret
library(caret)
library(doSNOW)

# Best place to start CV is to do 10-fold 10 times repeated (courtesy. Research)
# which means: 1-time 1000 trees, 10times-10fold=100000 trees

# Let's start with creating 10 folds from the data first
set.seed(2348)
tenFolds <- createMultiFolds(rf.label,k=10,times = 10)

# Check Stratification
table(rf.label)
# original rf.label had 1 to 0 ratio as 0.6229508
# Let's check the Stratification of a single fold out of 10*10
table(rf.label[tenFolds[[33]]])
# 33th fold of rf.label had 1 to 0 ratio as 0.6234818
# So createMultiFolds maintained the ratio (stratification) as well

# Set up caret trainControl i.e. it tells the train method later on what kind of 
# CV & with what parameters you wanna use
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = tenFolds)

# Set up doSNOW to actually do the parallel processing and use multicore
# in short make your cluster and register it
c1 <- makeCluster(6,type = "SOCK")
registerDoSNOW(c1)

# Let's train using CV
set.seed(34324)
rf.5.cv.1 <- train(x=rf.train.5,y=rf.label,method="rf", tuneLength=3, ntree=1000, trControl=ctrl.1)

# Stop the cluster since the work of cluster is done
stopCluster(c1)

# Check results
rf.5.cv.1

# Well results aren't pretty impressive let's try with k=5 fold
set.seed(5983)
fiveFolds <- createMultiFolds(rf.label,k=5,times = 10)

# Set up caret trainControl i.e. it tells the train method later on what kind of 
# CV & with what parameters you wanna use
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = fiveFolds)

# Set up doSNOW to actually do the parallel processing and use multicore
# in short make your cluster and register it
c1 <- makeCluster(6,type = "SOCK")
registerDoSNOW(c1)

# Let's train using CV
set.seed(89472)
rf.5.cv.2 <- train(x=rf.train.5,y=rf.label,method="rf", tuneLength=3, ntree=1000, trControl=ctrl.2)

# Stop the cluster since the work of cluster is done
stopCluster(c1)

# Check results
rf.5.cv.2

# Well we wanted to decrease our accuracy because we know we're overfitting
# Since kaggle showed accuracy of 79.131%, 5 fold reducedit a bit
# but let's try with k=3 fold
set.seed(37596)
threeFolds <- createMultiFolds(rf.label,k=3,times = 10)

# Set up caret trainControl i.e. it tells the train method later on what kind of 
# CV & with what parameters you wanna use
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = threeFolds)

# Set up doSNOW to actually do the parallel processing and use multicore
# in short make your cluster and register it
c1 <- makeCluster(6,type = "SOCK")
registerDoSNOW(c1)

# Let's train using CV
set.seed(94622)
rf.5.cv.3 <- train(x=rf.train.5,y=rf.label,method="rf", tuneLength=3, ntree=1000, trControl=ctrl.3)

# Stop the cluster since the work of cluster is done
stopCluster(c1)

# Check results
rf.5.cv.3


##########################################################################
# Part 6, Exploratory Modelling 2
##########################################################################

# We will be using a Single decision tree to train our model here
# for that we'll need the below two libraries
library(rpart)
library(rpart.plot)

# AS per previous video, we found that 3 fold CV repaeted 10 times is
# the best way to avoid overfitting IN OUR DATA
# Create a utility function to do CV
rpart.cv <- function(seed,training,labels,ctrl){
  #make cluster and register it
  cl <- makeCluster(6,type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # actual CV code
  rpart.cv <-  train(x=training,y=labels,method="rpart", tuneLength=30, trControl=ctrl)
  
  # Shutdown cluster
  stopCluster(cl)
  
  # return results
  return(rpart.cv)
  
}

# Prepare the training dataframe
features <- c("Pclass","Title","FamilySize")
rpart.train.1 <- data.combine[1:891,features]

# Now we are ready to call our utility function
rpartModel.1 <- rpart.cv(94622,rpart.train.1,rf.label,ctrl.3)
rpartModel.1

# Plot the tree of the model (decision tree)
prp(rpartModel.1$finalModel, type = 0,extra = 1, under = TRUE)

# As we can see the accuracy of the first left branch is not that good and 
# I guess we can further divide it and get better accuracy
table(data.combine$Title)

# Try and see how names are in the database
data.combine[1:25,"Name"]

name.split <- str_split(data.combine$Name, ",")
name.split[1]
last.names <- sapply(name.split,"[",1)
last.names[1:10]

# Add them as a column in the data.combine DF
data.combine$LastName <- last.names

# Extract the titles also, though we already have title but we wanna know the "Others"
# First, get the first names containing the titles
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

# Let's do some visualisation on it
ggplot(data.combine[1:891,], aes(x=ActualTitle,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("Actual Titles")+
  ylab("Total_Count")+
  labs(fill="Survived")

# Collapse some titles on the basis of visualization
indexes <- which(data.combine$ActualTitle=="Lady.")
data.combine$ActualTitle[indexes] <- "Mrs." 

indexes <- which(data.combine$ActualTitle=="Dr." |
                data.combine$ActualTitle=="Rev." |
                data.combine$ActualTitle=="Sir." |
                data.combine$ActualTitle=="Officer")
data.combine$ActualTitle[indexes] <- "Mr."

# Let's do some visualisation on it
ggplot(data.combine[1:891,], aes(x=ActualTitle,fill=Survived))+
  geom_bar(width = 1)+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("Actual Titles")+
  ylab("Total_Count")+
  labs(fill="Survived")

# Grab this new feature we built
# Prepare the training dataframe
features <- c("Pclass","ActualTitle","FamilySize")
rpart.train.2 <- data.combine[1:891,features]

# Now we are ready to call our utility function
rpartModel.2 <- rpart.cv(94622,rpart.train.2,rf.label,ctrl.3)
rpartModel.2

# Plot the tree of the model (decision tree)
prp(rpartModel.2$finalModel, type = 0,extra = 1, under = TRUE)

# Still there are lots of Mr. misclassified
# So let's dive in to 1st class "Mr." bcz we know they survived at a higher rate
# than other class "Mr."
indexes.first.mr <- which(data.combine$Pclass=="1" & data.combine$ActualTitle=="Mr.")
first.mr.df <- data.combine[indexes.first.mr,]
summary(first.mr.df)

# One female in "Mr."? Sorry my bad, She could be a Dr.
first.mr.df[which(first.mr.df$Sex=="female"),]

# Oops...Fixing it and checkinf if such mistake in other categories also
indexes <- which(data.combine$Sex=="female" & data.combine$ActualTitle=="Mr.")
data.combine$ActualTitle[indexes] <- "Mrs."

# Just a safety check
length(indexes <- which(data.combine$Sex=="female" & 
                   (data.combine$ActualTitle=="Mr." |
                    data.combine$ActualTitle=="Master.")))

# Again refresh our first class Mr.
indexes.first.mr <- which(data.combine$Pclass=="1" & data.combine$ActualTitle=="Mr.")
first.mr.df <- data.combine[indexes.first.mr,]
summary(first.mr.df)

# Now we are interested in first class Mr. that survived, Bcz they are the one who 
# are being misclassified as dead
summary(first.mr.df[which(first.mr.df$Survived=="1"),])
View(first.mr.df[which(first.mr.df$Survived=="1"),])

# Let's take a look at the rich folks here
indexes <- which(data.combine$Ticket=="PC 17755" |
                   data.combine$Ticket=="PC 17611"|
                   data.combine$Ticket=="113760")
View(data.combine[indexes,])

# So we see, higher you pay better your survivability
# Let's visualize Suvivability on the basis of fare but only in first class obviously
ggplot(first.mr.df, aes(x=Fare, fill=Survived))+
  geom_density(alpha=0.5)+
  ggtitle("Suvivability of first class on the basis of fare")

# There are people travelling with same ticket no., probably family together
# but their fares is written of whole family in each entry
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

# Refreshing first class Mr. DF
indexes.first.mr <- which(data.combine$Pclass=="1" & data.combine$ActualTitle=="Mr.")
first.mr.df <- data.combine[indexes.first.mr,]
summary(first.mr.df)

# Let's visualize Suvivability on the basis of # of people on ticket
# but only in first class obviously
ggplot(first.mr.df[first.mr.df$Survived!="None",], aes(x=PeopleOnTicket, fill=Survived))+
  geom_density(alpha=0.5)+
  ggtitle("Suvivability of first class on the basis of # of people on Ticket")

# Let's visualize Suvivability on the basis of avg fare 
# but only in first class obviously
ggplot(first.mr.df[first.mr.df$Survived!="None",], aes(x=AvgFare, fill=Survived))+
  geom_density(alpha=0.5)+
  ggtitle("Suvivability of first class on the basis of Avg fare")

# Well as we can see that avg fare appears to be interesting bcz of two 
# completely non-overlapping areas
summary(data.combine$AvgFare)

# There's one NA let's find out which
data.combine[is.na(data.combine$AvgFare),]
indexes <- with(data.combine,which(Pclass=="3" & Title=="Mr." & FamilySize=="1" & Ticket!="3701"))
na.similar.passengers <- data.combine[indexes,]
summary(na.similar.passengers$AvgFare)

# Now populate the NA value with the median value found in the summary
# of the similar passengers
data.combine[is.na(data.combine$AvgFare),"AvgFare"] <- 7.84

# Let's leverage caret's preprocess funtion to normalize data
preproc.data.combine <- data.combine[,c("AvgFare","PeopleOnTicket")]
preprocModel <- preProcess(preproc.data.combine,method = c("center","scale"))

postProc.data.combine <- predict(preprocModel,preproc.data.combine)

# Finding correlation between two variables
cor(postProc.data.combine$AvgFare,postProc.data.combine$PeopleOnTicket)

# Above is for all data, now let's do it for 1st class only
indexes <- which(data.combine$Pclass=="1")
cor(postProc.data.combine$AvgFare[indexes],postProc.data.combine$PeopleOnTicket[indexes])

# Prepare the training dataframe
features <- c("Pclass","FamilySize","ActualTitle", "AvgFare", "PeopleOnTicket")
rpart.train.3<- data.combine[1:891,features]

# Now we are ready to call our utility function
rpartModel.3 <- rpart.cv(94622,rpart.train.3,rf.label,ctrl.3)
rpartModel.3

# Plot the tree of the model (decision tree)
prp(rpartModel.3$finalModel, type = 0,extra = 1, under = TRUE)
# We can notice that one decision tree trained can neglect one more
# less uselful features whereas Rf uses all features forcefully


############################################################################
# Part 7, Submissions and Final thoughts
############################################################################

# Second submission to kaggle
testSubmit <- data.combine[892:1309, features]

# Make predictions using the random forest model rf.5 above
predictSubmit2 <- predict(rpartModel.3$finalModel,testSubmit,type = "class")
table(predictSubmit2)

# Save these predictions into CSV file
csvDF2 <- data.frame(PassengerId=rep(892:1309),Survived=predictSubmit2)

write.csv(csvDF2,file="submission_2.csv",row.names = FALSE)

# Accuracy from Kaggle , 80.382% not bad but not great also
# Let's do random forest
trainSubmit <- data.combine[1:891, c("Pclass","ActualTitle", "AvgFare", "PeopleOnTicket")]


set.seed(1234)
rf.temp <- randomForest(x=trainSubmit,y=rf.label,ntree = 1000)
rf.temp

testSubmit <- data.combine[892:1309,c("Pclass","ActualTitle", "AvgFare", "PeopleOnTicket")]
# Make predictions using the random forest model rf.temp above
predictSubmit3 <- predict(rf.temp,testSubmit)
table(predictSubmit3)

# Save these predictions into CSV file
csvDF3 <- data.frame(passengerId=rep(892:1309),survived=predictSubmit3)

write.csv(csvDF3,file="submission_3.csv",row.names = FALSE)

##########################################################################
# If you want to improve the model further, a good place to start is to see
# where our model gets things wrong
##########################################################################
library(infotheo)

# Get mutual information for variable A wrt variable B
# It tells how good a variable is in predicting the final class 
mutinformation(rf.label,data.combine$Pclass[1:891])
mutinformation(rf.label,data.combine$Sex[1:891])
mutinformation(rf.label,data.combine$SibSp[1:891])
mutinformation(rf.label,data.combine$Parch[1:891])
mutinformation(rf.label,discretize(data.combine$Fare[1:891]))
mutinformation(rf.label,data.combine$Embarked[1:891])
mutinformation(rf.label,data.combine$Title[1:891])
mutinformation(rf.label,data.combine$FamilySize[1:891])
mutinformation(rf.label,data.combine$Ticket.firstChar[1:891])
mutinformation(rf.label,data.combine$MultipleCabins[1:891])
mutinformation(rf.label,data.combine$ActualTitle[1:891])
mutinformation(rf.label,data.combine$PeopleOnTicket[1:891])
mutinformation(rf.label,discretize(data.combine$AvgFare[1:891]))

# Use tsne algo for 2-D representation of our data
library(Rtsne)

# We know our model does great on everything other than Mr.
most.correct <- data.combine[which(data.combine$ActualTitle!="Mr."),]
indexes <- which(most.correct$Survived!="None")
features <- c("Pclass","ActualTitle", "AvgFare", "PeopleOnTicket")

tsne.1 <- Rtsne(most.correct[,features], check_duplicates=FALSE)
ggplot(NULL,aes(x=tsne.1$Y[indexes,1],y=tsne.1$Y[indexes,2],color=most.correct$Survived[indexes]))+
  geom_point()+
  labs(color="Survived")+
  ggtitle("tsne 2D visualization for females and boys")

# What if we wanna check dependability of more than 1 variable then mutual info is
# not sufficient, so we use conditional mutual information (like conditional probability)
# what essentially it does is it tells about the separation of dots in previous graph
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))

# let's see for our initial two features
condinformation(rf.label, data.combine[1:891,c("Pclass","ActualTitle")])

# Let's see SAME THING AGAIN for the misters
misters <- data.combine[which(data.combine$ActualTitle=="Mr."),]
indexes <- which(most.correct$Survived!="None")

tsne.2 <- Rtsne(misters[,features], check_duplicates=FALSE)
ggplot(NULL,aes(x=tsne.2$Y[indexes,1],y=tsne.2$Y[indexes,2],color=misters$Survived[indexes]))+
  geom_point()+
  labs(color="Survived")+
  ggtitle("tsne 2D visualization for misters")

condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))

# We can do dimensionality reduction and use the reduced features for creating our model
# How? Let's see
tsne.3 <- Rtsne(data.combine[,features], check_duplicates=FALSE)
ggplot(NULL,aes(x=tsne.3$Y[1:891,1],y=tsne.3$Y[1:891,2],color=data.combine$Survived[1:891]))+
  geom_point()+
  labs(color="Survived")+
  ggtitle("tsne 2D visualization for misters")

condinformation(data.combine$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add tsne engineered variables and add them to our DF
data.combine$tsne.x <- tsne.3$Y[,1]
data.combine$tsne.y <- tsne.3$Y[,2]

############################### ALL VIDEOS END HERE ##############################

# FROM HERE MY OWN WORK STARTS
# let's train Xgboost model
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)


# input data i already have
trData <- data.combine[1:891,]

# Resetting factors
trData$Survived <- as.numeric(as.character(rf.label))
trData$Pclass <- factor(trData$Pclass)
trData$ActualTitle <- factor(trData$ActualTitle)

# input data i already have
tsData <- data.combine[892:1309,]

# Resetting factors
tsData$Survived <- as.numeric(as.character(0))
tsData$Pclass <- factor(tsData$Pclass)
tsData$ActualTitle <- factor(tsData$ActualTitle)

trainData <- trData
testData <- tsData
trainLabel <- trainData$Survived
testLabel <- testData$Survived

# Create matrix- one hot encoding for factor variables
trainM <- sparse.model.matrix(Survived~.-2,data=trainData)
head(trainM)
train_Matrix <- xgb.DMatrix(data=as.matrix(trainM),label=trainLabel)

testM <- sparse.model.matrix(Survived~.-2,data=testData)
head(testM)
test_Matrix <- xgb.DMatrix(data=as.matrix(testM),label=testLabel)

# Parameters
nc <- length(unique(trainLabel))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_Matrix, test = test_Matrix)

# eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_Matrix,
                       nrounds = 1000,
                       watchlist = watchlist,
                       eta = 0.001,
                       max.depth = 3,
                       gamma = 0,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 333)

# Training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

min(e$test_mlogloss)
e[e$test_mlogloss == 0.625217,]

# Feature importance
imp <- xgb.importance(colnames(train_Matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_Matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = testLabel, max_prob = max.col(., "last")-1)
table(Prediction = pred$max_prob, Actual = pred$label)

# Save these predictions into CSV file
csvDF4 <- data.frame(passengerId=rep(892:1309),survived=pred$max_prob)

write.csv(csvDF4,file="submission_4.csv",row.names = FALSE)


# XgBoost didn't work as I expected, Probably bcz the data isn't suitable for XgBoost
# Let's try SVM now
library(e1071)

svmtrain <- data.combine[1:891,c("Survived","Pclass","ActualTitle", "AvgFare", "PeopleOnTicket")]
svmtest  <- data.combine[892:1309,c("Pclass","ActualTitle", "AvgFare", "PeopleOnTicket")]

svmtrain[is.na(svmtrain)] <-0
svmtest[is.na(svmtest)]  <-0

# Reset their factor levels
svmtrain$Survived <- factor(svmtrain$Survived)
svmtrain$Pclass <- factor(svmtrain$Pclass)
svmtrain$ActualTitle <- factor(svmtrain$ActualTitle)

svmtest$Pclass <- factor(svmtest$Pclass)
svmtest$ActualTitle <- factor(svmtest$ActualTitle)


# We can inspect the train data. The results of this are printed in the log tab below
str(svmtrain)


str(svmtest)

SVMmodel<-svm(Survived~ Pclass+ActualTitle+AvgFare+PeopleOnTicket, data = svmtrain, cost = 100, gamma = 1)

prediction<-predict(SVMmodel, svmtest)
plot(prediction)
Pclass=table(prediction,svmtest [,2])
plot(Pclass)
me=mean(Pclass)
print(me)

output<-data.frame(data.combine$PassengerId[892:1309], prediction)

colnames(output)=cbind("PassengerId","Survived")

write.csv(output, file = 'Rushton_Solution.csv', row.names = F)

# Let's try random forest once again
# Train the randomForest with an additional feature "FamilySize"
rf.train.final <- data.combine[1:891,c("Pclass","ActualTitle","AvgFare", "FamilySize")]

# setting seed for Random number generator for RF
set.seed(1234)
rf.final <- randomForest(x=rf.train.final, y=rf.label, importance = TRUE, ntree = 1000)
rf.final
varImpPlot(rf.final)

testSubmit2 <- data.combine[892:1309, c("Pclass", "ActualTitle" ,"AvgFare", "FamilySize")]

# Make predictions using the random forest model rf.5 above
predictSubmit2 <- predict(rf.final,testSubmit2)

# Save these predictions into CSV file
csvDF5 <- data.frame(passengerId=rep(892:1309),survived=predictSubmit2)

write.csv(csvDF,file="submission_12.csv",row.names = FALSE)

# Let's try Logistic regression
Ltrain <- data.combine[1:891,]
Ltest  <- data.combine[892:1309,]

# Reset their factor levels
Ltrain$Survived <- factor(Ltrain$Survived)
Ltrain$Pclass <- factor(Ltrain$Pclass)
Ltrain$ActualTitle <- factor(Ltrain$ActualTitle)

Ltest$Pclass <- factor(Ltest$Pclass)
Ltest$ActualTitle <- factor(Ltest$ActualTitle)

summary(Ltrain)
summary(Ltest)

# fill in missing values for Age
Ltrain$Age[is.na(Ltrain$Age)] = mean(Ltrain$Age, na.rm = TRUE)
Ltest$Age[is.na(Ltest$Age)] = mean(Ltest$Age, na.rm = TRUE)

# Step 2: Create DF of independent/dependent variables
nonvars <- c("PassengerId","Name","Ticket","Embarked","Cabin","tsne.x","tsne.y","Parch","SibSp","LastName")
Ltrain <- Ltrain[,!(names(Ltrain) %in% nonvars)]
str(Ltrain)

# Step 4: Build a Logistic Regression Model
TitanicLog1 = glm(Survived~., data = Ltrain, family = binomial)
summary(TitanicLog1)

# Step 7: Use Model to predict survivability for Test Data
predictTestL = predict(TitanicLog1, type = "response", newdata = Ltest)

# no preference over error t = 0.5
Ltest$Survived = as.numeric(predictTestL >= 0.5)
table(Ltest$Survived)

PredictionsL = data.frame(PassengerId=rep(892:1309),Survived=Ltest$Survived)
write.csv(PredictionsL,file="submission_14.csv",row.names = FALSE)
