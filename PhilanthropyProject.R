# -------Import the cleaned file------

base_file <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/Predictive Analytics/Philanthropy Project/Manage_v1_Cleaned.csv")
summary(base_file)

# -------Exclude the uncleaned columns------

columns_being_excluded <- names(base_file) %in% 
  c("ID_NUMBER","FACULTY_STAFF_IND","STATE_CODE","ZIPCODE","STUDENT_ACTIVITY_PARTICIPANT",
    "DEGREE_CODE4","DEGREE_YEAR4", "SCHOOL4","LIFETIME_RECOGNITION_AMT","NO_EVENTS_2008",
    "NO_EVENTS_2018","NO_EVENTS_2017", "NO_EVENTS_2016", "NO_EVENTS_2015", "NO_EVENTS_2014",
    "NO_EVENTS_2013", "NO_EVENTS_2012", "NO_EVENTS_2011", "NO_EVENTS_2010", 
    "NO_EVENTS_2009", "GIFT_AMOUNT_2018","GIFT_AMOUNT_2017", "GIFT_AMOUNT_2016",
    "GIFT_AMOUNT_2015", "GIFT_AMOUNT_2014", "GIFT_AMOUNT_2013", 
    "GIFT_AMOUNT_2012", "GIFT_AMOUNT_2011", "GIFT_AMOUNT_2010",
    "GIFT_AMOUNT_2009","GIFT_AMOUNT_2008", "LAST_GIFT_DATE","GIFT_AMOUNT_2018_Cleaned",
    "GIFT_AMOUNT_2017_Cleaned","GIFT_AMOUNT_2016_Cleaned","GIFT_AMOUNT_2015_Cleaned","GIFT_AMOUNT_2014_Cleaned",
    "GIFT_AMOUNT_2013_Cleaned","GIFT_AMOUNT_2012_Cleaned","GIFT_AMOUNT_2011_Cleaned","GIFT_AMOUNT_2010_Cleaned",
    "GIFT_AMOUNT_2009_Cleaned","GIFT_AMOUNT_2008_Cleaned","FIRST_GIFT_AMOUNT","FIRST_GIFT_DATE")

base_file <- base_file[!(columns_being_excluded)]
summary(base_file)

#base_file$Y_N_2018 <- ifelse(base_file$DONATED_1_0_2018 == 1, "Yes","No")
names(base_file)
glimpse(base_file)


#-----Splitting into 70:30 Train & Test set

library(caret)
library(dplyr)


index <- createDataPartition(base_file$DONATED_1_0_2018, p = 0.7, list = FALSE)
train_set <- base_file[index,]
test_set <- base_file[-index,]

dim(train_set)
dim(test_set)
names(base_file)
glimpse(base_file)




#----Checking the split of 1's & 0's

table(train_set$DONATED_1_0_2018)
table(test_set$DONATED_1_0_2018)

# ----Building a simple Tree

library(rpart.plot)

tree_model_1 <- rpart(DONATED_1_0_2018 ~ ., data = train_set, method = "class")
rpart.plot(tree_model_1)

predict <- predict(object = tree_model_1, newdata = test_set, type = "class" )
confusionMatrix(data = predict, reference = test_set$DONATED_1_0_2018)

# ----Building a RandomForest

install.packages("randomForest")
library(randomForest)
summary(base_file)
library(caret)
further_columns_being_excluded <- names(base_file) %in% 
  c("GIVING_SCHOOL_UNIT1","GIVING_SCHOOL_UNIT2","GIVING_SCHOOL_UNIT3",
    "DEGREE_CODE1", "DEGREE_CODE2","DEGREE_CODE3","DEGREE_YEAR1","DEGREE_YEAR2",
    "DEGREE_YEAR3","SCHOOL1","SCHOOL2","SCHOOL3")

base_file <- base_file[!(further_columns_being_excluded)]

index <- createDataPartition(base_file$DONATED_1_0_2018, p = 0.7, list = FALSE)
train_set <- base_file[index,]
test_set <- base_file[-index,]

rf_1 <- randomForest(DONATED_1_0_2018 ~ ., data = train_set)
rf_1


install.packages("pROC")
library(pROC)


ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(1)

tree_model_3 <- train( DONATED_1_0_2018 ~ ., 
                      data = train, 
                      method = "treebag",
                      metric = "ROC",
                      trControl = ctrl)

tree_model_3

# Bagging
install.packages("ipred")
library(ipred)

bagging_1 <- bagging(DONATED_1_0_2018 ~ ., data = train_set)

table(test_set$DONATED_1_0_2018)
table(train_set$DONATED_1_0_2018)


predictions <- predict(object = bagging_1, newdata = test_set, type = "class")
predictions

confusionMatrix(predictions, test_set$DONATED_1_0_2018)

