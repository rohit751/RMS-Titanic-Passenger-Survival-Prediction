# Clean the environment
rm(list = ls())

# Load the libraries
library(dplyr)
library(stringr)
library(ggplot2)

# Load raw data
train = read.csv("train.csv")
test = read.csv("test.csv")

# Add the "Survived" column to the test data
test_Survived = mutate(test, Survived = "None")

# Combine the datasets
titanic_df = rbind(train, test_Survived)

glimpse(titanic_df)
titanic_df$Survived = as.factor(titanic_df$Survived)
titanic_df$Pclass = as.factor(titanic_df$Pclass)

# Survival Rates
table(titanic_df$Survived)

# Class distribution
table(titanic_df$Pclass)


# Hypotheses: Rich folks Survived at a higher rate
train$Pclass = as.factor(train$Pclass)
train$Survived = as.factor(train$Survived)
ggplot(train, aes(Pclass, fill = Survived)) +
    geom_bar(width = 0.5) +
    xlab("Pclass") +
    ylab("Total Count") +
    labs(fill = "Survived")


# Examine the names in the dataset
head(titanic_df$Name)
length(unique(titanic_df$Name))
indeces = which(duplicated(titanic_df$Name))
passNames = titanic_df$Name[c(892, 898)]
titanic_df[titanic_df$Name %in% passNames,]


# Any correlation b/w 'Miss.', 'Mr.', and other variables
misses = titanic_df[str_detect(titanic_df$Name, "Miss."),]
mrs = titanic_df[str_detect(titanic_df$Name, "Mrs."),]
misses[1:5,]
mrs[1:5,]


# Extract title from the names
getTitle = function(data) {

    titleStart = regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE) + 2
    titleEnd = titleStart + attr(titleStart, "match.length") - 4
    data$Title = substr(data$Name, titleStart, titleEnd)
    
    return (data$Title)
    
}
titanic_df$Title = as.factor(getTitle(titanic_df))


# Use the first 891 rows to check the title stats
ggplot(titanic_df[1:891,], aes(Title, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived") +
    coord_flip()


# Distribution of gender
table(titanic_df$Sex)

# Visualization of 3-way relationship of Sex, Pclass and Survival
ggplot(titanic_df[1:891,], aes(Sex, fill = Survived)) +
    geom_bar() +
    xlab("Gender") +
    ylab("Total Count") +
    labs(fill = "Survived") +
    facet_wrap(~ Pclass)


# Distribution of Age
summary(titanic_df$Age)

# Survival rates broken out by Age
ggplot(titanic_df[1:891,], aes(Age, fill = Survived)) +
    geom_histogram(binwidth = 10) +
    facet_wrap( ~ Sex + Pclass) +
    xlab("Age") +
    ylab("Total Count") +
    labs(fill = "Survived")


# Validate the title "Master" if its a good proxy for male children
boysAge = titanic_df$Age[titanic_df$Title == "Master"]
summary(boysAge)

# Is is valid to "Miss"?
girls = titanic_df[titanic_df$Title == "Miss",]
summary(girls$Age) #Looks like it isn't

ggplot(girls[girls$Survived != "None",], aes(Age, fill = Survived)) +
    geom_histogram(binwidth = 5) +
    facet_wrap( ~ Pclass) + 
    xlab("Age") +
    ylab("Total Count") +
    ggtitle("Age for 'Miss' by Pclass") +
    labs(fill = "Survived")

# Female children have a different survival rate. Could be a candidate
# for feature engineering later.
girls_alone = girls[girls$SibSp == 0 & girls$Parch == 0,]
summary(girls_alone$Age)
length(which(girls_alone$Age <= 14.5)) # Reason: 14.5 -> max value for boys


# Moving on to sbsp variable;
summary(titanic_df$SibSp)

# Can we treat Sibsp as a factor
length(unique(titanic_df$SibSp))
titanic_df$SibSp = as.factor(titanic_df$SibSp)

# Visualize
ggplot(titanic_df[1:891,], aes(SibSp, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Siblings/Spouse") +
    ylab("Total Count") +
    labs(fill = "Survived")
    
# Convert Parch to factor
titanic_df$Parch = as.factor(titanic_df$Parch)

# Visualize
ggplot(titanic_df[1:891,], aes(Parch, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Parent/Children") +
    ylab("Total Count") +
    labs(fill = "Survived")

# Feature engineering: Create a family size
titanic_df$familySize = as.factor(as.numeric(as.character(titanic_df$SibSp))
                                  + as.numeric(as.character(titanic_df$Parch))
                                  + 1)

# Visualize
ggplot(titanic_df[1:891,], aes(familySize, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Family Size") +
    ylab("Total Count") +
    labs(fill = "Survived")


# Ticket variable
str(titanic_df$Ticket)

# Based on huge number of levels, it is not exactly a factor variable
# but a string. Hence convert accordingly
titanic_df$Ticket = as.character(titanic_df$Ticket)
titanic_df$Ticket[1:20]

# No apparent structure visible. Try with first char
ticket_firstChar = ifelse(titanic_df$Ticket == "", " ",
                          substr(titanic_df$Ticket, 1, 1))
table(ticket_firstChar)

# Make a factor for analysis purpose and visualize
titanic_df$ticket_firstChar = as.factor(ticket_firstChar)
ggplot(titanic_df[1:891,], aes(ticket_firstChar, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Ticket First Character") +
    ylab("Total Count") +
    ggtitle("Survivability based on Ticket's first character") +
    labs(fill = "Survived")


# Next up - Ticket fares
summary(titanic_df$Fare)
length(unique(titanic_df$Fare))

# Can't be a factor. Treat it as a numeric
ggplot(titanic_df[1:891,], aes(Fare, fill = Survived)) +
    geom_histogram(binwidth = 10) +
    facet_wrap( ~ Pclass) +
    xlab("Ticket Fare") +
    ylab("Total Count") +
    ggtitle("Survivability based on Ticket Fare") +
    labs(fill = "Survived")


# Analysis of cabin variable
str(titanic_df$Cabin)

# Cabin isn't really a factor, make it a string
titanic_df$Cabin = as.character(titanic_df$Cabin)
titanic_df$Cabin[1:20]

# Replace empty cabin with "U" for Unknown
titanic_df$Cabin[titanic_df$Cabin == ""] = "U"
titanic_df$Cabin[1:20]

# Take a look at the first character of the Cabin
cabin_firstChar = substr(titanic_df$Cabin, 1, 1)
unique(cabin_firstChar)
titanic_df$cabin_firstChar = as.factor(cabin_firstChar)

# High level plots
ggplot(titanic_df[1:891,], aes(cabin_firstChar, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Cabin First Character") +
    ylab("Total Count") +
    ggtitle("Survivability based on Cabin's first character") +
    labs(fill = "Survived")

# What about folks with multiple cabins?
titanic_df$multipleCabins = as.factor(ifelse(str_detect(titanic_df$Cabin, " "),
                                             "Y", "N"))
ggplot(titanic_df[1:891,], aes(multipleCabins, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Multiple Cabins") +
    ylab("Total Count") +
    ggtitle("Survivability based on Multiple Cabins") +
    labs(fill = "Survived")


# Does survivability depend on where you boarded the ship
str(titanic_df$Embarked)
levels(titanic_df$Embarked)

# Visualize
ggplot(titanic_df[1:891,], aes(Embarked, fill = Survived)) +
    geom_bar() +
    facet_wrap( ~ Pclass) +
    xlab("Embarkment") +
    ylab("Total Count") +
    ggtitle("Survivability based on Embarkment") +
    labs(fill = "Survived")

# Feature Engineering Done!!
# Features selected: Pclass, SibSp, Parch, Title, familySize


#==============================================================================
#
# Video #4 - Exploratory Modeling
#
#==============================================================================


library(randomForest)

# Train a Random Forest with the default parameters using Pclass & Title
rf.train.1 <- titanic_df[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)



# Train a Random Forest using Pclass, Title, & SibSp
rf.train.2 <- titanic_df[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)



# Train a Random Forest using Pclass, Title, & Parch
rf.train.3 <- titanic_df[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)



# Train a Random Forest using Pclass, Title, SibSp, Parch
rf.train.4 <- titanic_df[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)



# Train a Random Forest using Pclass, Title, & familySize
rf.train.5 <- titanic_df[1:891, c("Pclass", "Title", "familySize")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)



# Train a Random Forest using Pclass, Title, SibSp, & familySize
rf.train.6 <- titanic_df[1:891, c("Pclass", "Title", "SibSp", "familySize")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)



# Train a Random Forest using Pclass, Title, Parch, & familySize
rf.train.7 <- titanic_df[1:891, c("Pclass", "Title", "Parch", "familySize")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)



#==============================================================================
#
# Cross Validation
#
#==============================================================================
# Before we jump into features engineering we need to establish a methodology
# for estimating our error rate on the test set (i.e., unseen data). This is
# critical, for without this we are more likely to overfit. Let's start with a 
# submission of rf.5 to Kaggle to see if our OOB error estimate is accurate.

# Subset our test records and features
test.submit.df <- titanic_df[892:1309, c("Pclass", "Title", "familySize")]

# Make prediction with rf.5
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
write.csv(submit.df, file = "RF_SUB_20170225_1.csv", row.names = FALSE)

# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates
library(caret)
library(doSNOW)


# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# however there are no hard and fast rules - this is where the experience of the 
# Data Scientist (i.e., the "art") comes into play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.


# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that Survived and perished in each fold matches the overall training set. This
# is known as stratified cross validation and generally provides better results.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
342 / 549

table(rf.label[cv.10.folds[[33]]])
308 / 494


# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

library(e1071)
# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2

# Make prediction with rf.5.cv.2
rf.5.preds <- predict(rf.5.cv.2, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
write.csv(submit.df, file = "RF_SUB_20170225_5.csv", row.names = FALSE)


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3




#==============================================================================
#
# Video #6 - Exploratory Modeling 2
#
#==============================================================================

# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand.

# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Per video #5, let's use 3-fold CV repeated 10 times 

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
    cl <- makeCluster(6, type = "SOCK")
    registerDoSNOW(cl)
    
    set.seed(seed)
    # Leverage formula interface for training
    rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                      trControl = ctrl)
    
    #Shutdown cluster
    stopCluster(cl)
    
    return (rpart.cv)
}

# Grab features
features <- c("Pclass", "Title", "familySize")
rpart.train.1 <- titanic_df[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)






# The plot bring out some interesting lines of investigation. Namely:
#      1 - Titles of "Mr." and "Other" are predicted to perish at an 
#          overall accuracy rate of 83.2 %.
#      2 - Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class
#          are predicted to survive at an overall accuracy rate of 94.9%.
#      3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes equal to 5, 6, 8, & 11 are predicted to perish
#          with 100% accuracy.
#      4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes not equal to 5, 6, 8, or 11 are predicted to 
#          survive with 59.6% accuracy.


# Both rpart and rf confirm that title is important, let's investigate further
table(titanic_df$Title)

# Parse out last name and title
titanic_df[1:25, "Name"]

name.splits <- str_split(titanic_df$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
titanic_df$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a title of 'the'?
titanic_df[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
titanic_df$new.title <- as.factor(titles)

# Visualize new version of title
ggplot(titanic_df[1:891,], aes(x = new.title, fill = Survived)) +
    geom_bar() +
    facet_wrap(~ Pclass) + 
    ggtitle("Surival Rates for new.title by Pclass")

# Collapse titles based on visual analysis
indexes <- which(titanic_df$new.title == "Lady.")
titanic_df$new.title[indexes] <- "Mrs."

indexes <- which(titanic_df$new.title == "Dr." | 
                     titanic_df$new.title == "Rev." |
                     titanic_df$new.title == "Sir." |
                     titanic_df$new.title == "Officer")
titanic_df$new.title[indexes] <- "Mr."

# Visualize 
ggplot(titanic_df[1:891,], aes(x = new.title, fill = Survived)) +
    geom_bar() +
    facet_wrap(~ Pclass) +
    ggtitle("Surival Rates for Collapsed new.title by Pclass")


# Grab features
features <- c("Pclass", "new.title", "familySize")
rpart.train.2 <- titanic_df[1:891, features]

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Dive in on 1st class "Mr."
indexes.first.mr <- which(titanic_df$new.title == "Mr." & titanic_df$Pclass == "1")
first.mr.df <- titanic_df[indexes.first.mr, ]
summary(first.mr.df)

# One female?
first.mr.df[first.mr.df$Sex == "female",]

# Update new.title feature
indexes <- which(titanic_df$new.title == "Mr." & 
                     titanic_df$Sex == "female")
titanic_df$new.title[indexes] <- "Mrs."

# Any other gender slip-ups?
length(which(titanic_df$Sex == "female" & 
                 (titanic_df$new.title == "Master." |
                      titanic_df$new.title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(titanic_df$new.title == "Mr." & titanic_df$Pclass == "1")
first.mr.df <- titanic_df[indexes.first.mr, ]

# Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

# Take a look at some of the high fares
indexes <- which(titanic_df$Ticket == "PC 17755" |
                     titanic_df$Ticket == "PC 17611" |
                     titanic_df$Ticket == "113760")
View(titanic_df[indexes,])

# Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
    geom_density(alpha = 0.5) +
    ggtitle("1st Class 'Mr.' Survival Rates by fare")


# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(titanic_df))
avg.fare <- rep(0.0, nrow(titanic_df))
tickets <- unique(titanic_df$Ticket)

for (i in 1:length(tickets)) {
    current.ticket <- tickets[i]
    party.indexes <- which(titanic_df$Ticket == current.ticket)
    current.avg.fare <- titanic_df[party.indexes[1], "Fare"] / length(party.indexes)
    
    for (k in 1:length(party.indexes)) {
        ticket.party.size[party.indexes[k]] <- length(party.indexes)
        avg.fare[party.indexes[k]] <- current.avg.fare
    }
}

titanic_df$ticket.party.size <- ticket.party.size
titanic_df$avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- titanic_df[indexes.first.mr, ]
summary(first.mr.df)


# Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
    geom_density(alpha = 0.5) +
    ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
    geom_density(alpha = 0.5) +
    ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


# Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(titanic_df$avg.fare)

# One missing value, take a look
titanic_df[is.na(titanic_df$avg.fare), ]

# Get records for similar passengers and summarize avg.fares
indexes <- with(titanic_df, which(Pclass == "3" & Title == "Mr." & familySize == 1 &
                                         Ticket != "3701"))
similar.na.passengers <- titanic_df[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
titanic_df[is.na(avg.fare), "avg.fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.titanic_df <- titanic_df[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.titanic_df, method = c("center", "scale"))

postproc.titanic_df <- predict(preProc, preproc.titanic_df)

# Hypothesis refuted for all data
cor(postproc.titanic_df$ticket.party.size, postproc.titanic_df$avg.fare)

# How about for just 1st class all-up?
indexes <- which(titanic_df$Pclass == "1")
cor(postproc.titanic_df$ticket.party.size[indexes], 
    postproc.titanic_df$avg.fare[indexes])
# Hypothesis refuted again


# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "new.title", "familySize", "ticket.party.size", "avg.fare")
rpart.train.3 <- titanic_df[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)



#==============================================================================
#
# Video #7 - Submitting, scoring, and some analysis.
#
#==============================================================================

#
# Rpart scores 0.80383
#
# Subset our test records and features
test.submit.df <- titanic_df[892:1309, features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20160228_9.csv", row.names = FALSE)


#
# Random forest scores 0.80861
#
features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- titanic_df[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- titanic_df[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160228_10.csv", row.names = FALSE)



#
# If we want to improve our model, a good place to start is focusing on where it
# gets things wrong!
#


# First, let's explore our collection of features using mutual information to
# gain some additional insight. Our intuition is that the plot of our tree
# should align well to the definition of mutual information.
#install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, titanic_df$Pclass[1:891])
mutinformation(rf.label, titanic_df$sex[1:891])
mutinformation(rf.label, titanic_df$sibsp[1:891])
mutinformation(rf.label, titanic_df$parch[1:891])
mutinformation(rf.label, discretize(titanic_df$fare[1:891]))
mutinformation(rf.label, titanic_df$embarked[1:891])
mutinformation(rf.label, titanic_df$title[1:891])
mutinformation(rf.label, titanic_df$family.size[1:891])
mutinformation(rf.label, titanic_df$ticket.first.char[1:891])
mutinformation(rf.label, titanic_df$cabin.multiple[1:891])
mutinformation(rf.label, titanic_df$new.title[1:891])
mutinformation(rf.label, titanic_df$ticket.party.size[1:891])
mutinformation(rf.label, discretize(titanic_df$avg.fare[1:891]))


# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with titles other than 'Mr."
#install.packages("Rtsne")
library(Rtsne)
most.correct <- titanic_df[titanic_df$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")


# NOTE - Bug fix for original version. Rtsne needs a seed to ensure consistent
# output between runs.
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
    geom_point() +
    labs(color = "Survived") +
    ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")


# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and Pclass
condinformation(rf.label, titanic_df[1:891, c("new.title", "Pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- titanic_df[titanic_df$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
    geom_point() +
    labs(color = "Survived") +
    ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(titanic_df[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = titanic_df$Survived[1:891])) +
    geom_point() +
    labs(color = "Survived") +
    ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(titanic_df$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
titanic_df$tsne.x <- tsne.3$Y[,1]
titanic_df$tsne.y <- tsne.3$Y[,2]

# Modelling
features <- c("Pclass", "new.title", "ticket.party.size",
              "avg.fare", "tsne.x", "tsne.y")
rf.train.temp <- titanic_df[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- titanic_df[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160228_11.csv", row.names = FALSE)
