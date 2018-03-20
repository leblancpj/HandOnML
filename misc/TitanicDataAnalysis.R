
# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add in a "survived" variable / feature to the test data set
# to allow for combining the data set
test.survived <- data.frame(PassengerId = test[,1],Survived = rep("None", nrow(test)), test[,2:ncol(test)])

# Combine data set into super set
data.combined <- rbind(train, test.survived)

# A bit about R data types str = structure
str(data.combined)

# change some of these to factors

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# take a look at gross survival rates
table(data.combined$Survived)

# distribution across classes
table(data.combined$Pclass)

# load up ggplot package to use for visualization
library(ggplot2)


# Hypothesis: Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_histogram(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Count") +
  labs(fill = "Survived")


# examine first few names in data
head(as.character(train$Name))

# how many unique names across train and test set
length(unique(as.character(data.combined$Name)))

# It appears there are duplicates, lets take a look at those
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# take a look at these records
data.combined[which(data.combined$Name %in% dup.names),]

# seems legit duplicates

# What's up with Miss., Mister, etc.
library(stringr)

# are there any correlation with other variables?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#Hypothesis - Title correlates with age?

# what about men?
males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,]

# expand upon the relationship between 'Survived' and 'Pclass' by adding title
# variable to the dataset, and then explore a 3-d relationship

#Create a utility function
extractTitle <- function(name){
  name <- as.character(name)
  if(length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}

data.combined$title <- as.factor(titles)


# since we only have survived labels for the training set
# only use the first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) + 
  geom_bar() + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") + 
  xlab("Title") +
  ylab("Total Count") + 
  labs(fill = "Survived")

## End of Video 1 ##

# What's the distribution of females to males across train & test
table(data.combined$Sex)

# Sex, class, and survival
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) + 
  geom_bar() + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") + 
  xlab("Sex") +
  ylab("Total Count") + 
  labs(fill = "Survived")

# OK, age and sex seem pretty important, let's try 
# to look at the distributions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

# Sex, class, and survival
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) + 
  geom_bar(width = 10) + 
  facet_wrap(~Sex + Pclass) +
  xlab("Age") +
  ylab("Total Count")


# validate that "Master" is a good proxy for young male
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

# Miss is more complicated
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)


# Misses, class, and survival
ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) + 
  geom_bar(width = 5) + 
  facet_wrap(~Pclass) +
  ggtitle("Age for 'Misses' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# It seems that female children mmay have different survival rates
# could be a candidate for future feature engineering

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# check out the sibsp feature
summary(data.combined$SibSp)
# heavily skewed to 0

# can we treat as a factor
length(unique(data.combined$SibSp))

# factorize
data.combined$SibSp <- as.factor(data.combined$SibSp)

# we believe that title is predictive. Visualize  survival rates by sibsp, pclass and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) + 
  geom_bar(width = 5) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(Fill = "Survived")


# treat the parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) + 
  geom_bar(width = 1) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(Fill = "Survived")

# Let's try some feature engineering.  What about creating a family size feature
# go back to original data to get integers (not factors)
temp.Sibsp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
# add sibling / spouse + parent / children + myself
data.combined$family.size <- as.factor(temp.Sibsp + temp.Parch + 1)


ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) + 
  geom_bar(width = 1) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(Fill = "Survived")

# take a look at the ticket variable
str(data.combined$Ticket)

# based on the huge number of levels ticket really isn't a factor variable, it is a string
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# No immediate structure that's obvious, let's see if we can find some
# start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == "", "", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

# OK, we can make a factor for the analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)


# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) + 
  geom_bar(width = 1) + 
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(Fill = "Survived")

# ticket might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) + 
  geom_bar(width = 1) + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(Fill = "Survived")

# lastly, see if we get a pattern when using cobination of pclass and title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) + 
  geom_bar(width = 1) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(Fill = "Survived")

#...probably don't want to use ticket...not a lot of "signal" in this data


# Next up - the fares the passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# can't really make it a factor, so treat as numeric $ visualize with histogram
ggplot(data.combined, aes(x = Fare)) + 
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# let's check to see if it has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(Fill = "Survived")



# Move on to the cabin variable
str(data.combined$Cabin)

# Cabin really isn't a factor, make it a string and diplay the first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]



#=================================================
# Video #4 - Exploratory Modeling
#=================================================

library(randomForest)

# train a randon forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)


set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# OOB = "Out of Bag" --> how many samples were part of no tree

# Train a rf using Pclass, title & sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a rf using Pclass, title & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


# Train a rf using Pclass, title, SibSp, & Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "title", "SibSp" , "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)


# Train a rf using Pclass, title, & family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


# Train a rf using Pclass, title, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)


# Train a rf using Pclass, title, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


#===================================================
#
# Video # 5
#
#===================================================


# Before we jump into feature engineering we need to establish a methodology
# for estimating our error rate on teh test set (i.e. unseen data).  This is
# critical, for without this we are more likely to overfit.  Let's start with a
# submission of rf.5 to Kaggle to see if our OOB error estimate is accurate


# Submit our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)


# Write out to a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20170224_1.csv", row.names = FALSE)

# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross validation using the caret package to see if we can get
# more accurate estimates

# Caret Package: "Classification and Regression Training"
# doSNOW: "Foreach Parallel Adaptor for the 'snow' Package

library(caret)
library(doSNOW)

# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# however there are no hard and fast rules - this is where the experience of the
# Data Scientist (i.e., the "art") comes into play.  We'll start with 10-fold CV, 
# repeated 10 times and see how it goes.


# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training sets.  This
# is know as stratified cross validation and generally provides better results

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
# Global skew
table(rf.label)
342/549
# Specific skew 
table(rf.label[cv.10.folds[[33]]])
308/494

# Set up caret's trainControl object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)



# set up doSNOW package for multi-core training.  This is helpful as we're going
# to be training a lot of trees
# NOTE - This works on Windows, and MAC unlike doMC

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, 
                   ntree = 1000, trControl = ctrl.1)

# Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# The above is only slightly more pessimistic than the OOB

# Repeat with 5-fold CV...repeat above with k=5...


# 5-fold isn't better.  Move to 3-fold repeated 10 times...
# In our case, 3-fold more closely mimics the actual train / test ratio of full data set.


set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, 
                   ntree = 1000, trControl = ctrl.3)

# Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3



#===================================================
#
# Video # 6 - Exploratory Modeling 2
#
#===================================================


# Let's use a single decision tree to better understand what's going on with our 
# features.  Obviously Randon Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand

# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


# Per Video#5, let's use 3-fold CV repeated 10 times

#Create a utility function
rpart.cv <- function(seed, training, labels, ctrl){
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training model
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  return (rpart.cv)
}

# Grab Features
features <- c("Pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# The plot brings out some interesting lines of investigations, mainly:
#        1 - Titles of "Mr." and "Other" are predicted to perish at an
#            overall accuracy rate of 83.2%.
#        2 - Titles of "Master.", "Miss.", & "Mrs." in 1st / 2nd class
#            are predicted to survice at an overall rate of accuracy of 94.9%
#        3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with
#            family size of 5,6,8, or 11 are predicted to perish with an 
#            accuracy of 100%
#        4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with
#            family size not of 5,6,8, or 11 are predicted to perish with an 
#            accuracy of 59.6%

# Both rpart and rf confirm that title is important, let's investigate further.
table(data.combined$title)

# Parse out last name and title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]

last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last name to dataframe in case we find it userful later
data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with the title of 'the'?
data.combined[which(titles == 'the'),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$new.title <- as.factor(titles)


# Let's look at it...
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) + 
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rate for new.titles by Pclass")


# Collapse titles based on visual analysis
indexs <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexs] <- "Mrs."

indexs <- which(data.combined$new.title == "Dr." |
                data.combined$new.title == "Rev." |
                data.combined$new.title == "Sir." |
                data.combined$new.title == "Officer")
data.combined$new.title[indexs] <- "Mr."
data.combined$new.title[indexs] <- "Mrs."

ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) + 
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rate for new.titles by Pclass")

# Grab features
features <- c("Pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]


# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# one female?
first.mr.df[first.mr.df$Sex == "female",]

indexes <- which(data.combined$new.title == "Mr." & data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

# Any other slip ups?
length(which(data.combined$Sex == "female" & (data.combined$new.title == "Master." | data.combined$new.title == "Mr.")))


# Refresh the dataframe
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# LEt's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1", ])
View(first.mr.df[first.mr.df$Survived == "1",])

# Some outliers in fare...let's take a look 
indexes <- which(data.combined$Ticket == "PC 17755" | data.combined$Ticket == "PC 17611" | data.combined$Ticket == "113760")
View(data.combined[indexes,])


# Is there a relationship with Fare for first class "Mr."
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) + 
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rate By Fare")


# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)


for (i in 1:length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for(k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}


data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh the 1st class 
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) + 
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rate By ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) + 
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rate By avg.fare")

# Hypothesis
summary(data.combined$avg.fare)

# one missing value, take a look
data.combined[is.na(data.combined$avg.fare),]

# get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(Pclass == "3" & title == "Mr." & family.size == 1 & Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size","avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)


# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about for just the 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again!


# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#===================================================
#
# Video # 7 - Submission, Scoring & Some Analysis
#
#===================================================

#
# Rpart scores 0.80383
#

# Subset our test records and features
test.submit.df <- data.combined[892:1309,features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for Kaggle Submission
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20170310_1.csv", row.names = FALSE)


#
# Random forest scores 0.80861
#

features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

# Run CV and check out results
set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]

# Make Predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for Kaggle Submission
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20170310_1.csv", row.names = FALSE)


#
# If we want to improve, start where it gets things wrong
#

# First, let's explore our collection of features using mutial information to
# gain some additional insights.  Out intuition is that the plot of our tree
# should align well to the definition of mutial information.
#install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$Cabin[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data
# suitable for visualization starting with folks our model get right very often - females
# and boys

#install.packages("Rtsne")
library(Rtsne)

most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[,features], check_duplicates = FALSE)

ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2],
                 color = most.correct$Survived[indexes])) + 
  geom_point() +
  labs(color = "Survived") + 
  ggtitle("tsne 2D Visualization of Features for Females and Boys")
  
  
  
# To get a baseline, let's use conditional mutual information on the tsne X and 
# Y features for females and boys in the 1st and 2nd class.  The intuition here is 
# that the combination of these features should be higher than any individual feature
# we look at.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "Pclass")])

# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e. the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[,features], check_duplicates = FALSE)

ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2],
                 color = misters$Survived[indexes])) + 
  geom_point() +
  labs(color = "Survived") + 
  ggtitle("tsne 2D Visualization of Features for Mr.")

condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))

#
# Idea - How about creating tsne features for all of the training data
# and using them in our model
#
tsne.3 <- Rtsne(data.combined[,features], check_duplicates = FALSE)

ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2],
                 color = data.combined$Survived[1:891])) + 
  geom_point() +
  labs(color = "Survived") + 
  ggtitle("tsne 2D Visualization of All Features")

condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))


# Add the tsne features to the data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]



