# Estudo usando o tutorial do video https://www.youtube.com/watch?v=Zx2TguRHrJE

titanic.train <- read.csv(file = "dataset/train.csv", stringsAsFactors = FALSE,  header = TRUE)
titanic.test <- read.csv(file = "dataset/test.csv", stringsAsFactors = FALSE,  header = TRUE)

tail(titanic.train)
tail(titanic.test)

str(titanic.test)

median(titanic.train$Age, na.rm = TRUE)
median(titanic.test$Age, na.rm = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

tail(titanic.train$IsTrainSet)
tail(titanic.test$IsTrainSet)

ncol(titanic.test)
ncol(titanic.train)

names(titanic.train)
names(titanic.test)

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

table(titanic.full$Embarked)

titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'

table(is.na(titanic.full$Age))

age.media <- median(titanic.full$Age, na.rm = TRUE, "Age") 
titanic.full[is.na(titanic.full$Age), "Age"] <- age.media
titanic.full[is.na(titanic.full$Age)]
table(is.na(titanic.full$Age))

table(is.na(titanic.full$Fare))
fare.media <- median(titanic.full$Fare, na.rm = TRUE)
fare.media
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.media

titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]

# Categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Age)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
str(titanic.full)

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

library(randomForest)
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, 
                              mtry = 3, nodesize = 0.01 * nrow(titanic.test))
