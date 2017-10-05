#rm(list = ls())

# loading libraries
library(dplyr)
library(caret)
library(sqldf)
library(xgboost)
library(DiagrammeR)

#CWD
setwd("/Users/mohamednassef/Documents/Airbnb/")

# loading data
train = read.csv("train_users_2.csv")
test = read.csv("test_users.csv")
sessions <- read.csv("sessions.csv")

# Removing NAs and empty values
sessions_final <- na.omit(sessions)
sessions_final <- sessions_final[!(sessions_final$user_id=="" | sessions_final$action_type=="" | sessions_final$action_detail=="" | sessions_final$action=="" |
                                   sessions_final$action_type=="-unknown-" | sessions_final$action_detail=="-unknown-" | 
                                   sessions_final$action=="-unknown-"), ]

# Data Wrangling and Feature Engineering 
# Computing the session time per user
train <- train[(train$id %in% sessions$user_id),]
secs_per_session <- aggregate(sessions_final$secs_elapsed, list(sessions_final$user_id), sum)
secs_per_session_DF <- as.data.frame(secs_per_session)
colnames(secs_per_session_DF) <- c('userid','total_secs_per_session')

# Replacing missing entries with the average session time
train <- train %>% left_join(secs_per_session_DF, by = c("id" = "userid"))
mean_secs_per_session_train <- mean(train$total_secs_per_session, na.rm = TRUE)
train <- train %>% mutate(total_secs_per_session = ifelse(is.na(total_secs_per_session),mean_secs_per_session_train,total_secs_per_session))


test <- test %>% left_join(secs_per_session_DF, by = c("id" = "userid"))
mean_secs_per_session_test <- mean(test$total_secs_per_session, na.rm = TRUE)
test <- test %>% mutate(total_secs_per_session = ifelse(is.na(total_secs_per_session),mean_secs_per_session_test,total_secs_per_session))


## Computing a histogram of action types per user 
session_action_types <- sqldf("SELECT DISTINCT user_id FROM sessions_final")
session_action_types_DF <- data.frame(session_action_types)

actionTypes <- as.character(unique(sessions_final$action_type))
query_1 <- 'SELECT user_id,COUNT(action_type) AS'
query_2 <- 'FROM temp GROUP BY user_id'

# Building a datafram by continually joining the action feature per user_id and looping over all actions
for (actiontype in actionTypes)
{
        temp <- filter(sessions_final,action_type==actiontype)
        query <- paste(query_1,actiontype,query_2)
        actionTypeCount=sqldf(query)
        actionTypeCount_DF <- data.frame(actionTypeCount)
        session_action_types_DF <- session_action_types_DF %>% left_join(actionTypeCount_DF, by = 'user_id')
}

session_action_types_DF$booking_response <- NULL
session_action_types_DF[is.na(session_action_types_DF)] <- 0

# Joining the newly created feature vector with the train/test data
train <- train %>% left_join(session_action_types_DF, by = c("id" = "user_id"))
test <- test %>% left_join(session_action_types_DF, by = c("id" = "user_id"))


# Replacing empty coumns of actions with the column mean and a zero for highly sparse columns
mean_click_train <- ceiling(mean(train$click, na.rm = TRUE))
mean_data_train <- ceiling(mean(train$data, na.rm = TRUE))
mean_view_train <- ceiling(mean(train$view, na.rm = TRUE))
mean_submit_train <- ceiling(mean(train$submit, na.rm = TRUE))
train <- train %>% mutate(click = ifelse(is.na(click),mean_click_train,click))
train <- train %>% mutate(data = ifelse(is.na(data),mean_data_train,data))
train <- train %>% mutate(view = ifelse(is.na(view),mean_view_train,view))
train <- train %>% mutate(submit = ifelse(is.na(submit),mean_submit_train,submit))
train <- train %>% mutate(booking_request = ifelse(is.na(booking_request),0,booking_request))
train <- train %>% mutate(message_post = ifelse(is.na(message_post),0,message_post))
train <- train %>% mutate(partner_callback = ifelse(is.na(partner_callback),0,partner_callback))
train <- train %>% mutate(modify = ifelse(is.na(modify),0,modify))
mean_click_test <- ceiling(mean(test$click, na.rm = TRUE))
mean_data_test <- ceiling(mean(test$data, na.rm = TRUE))
mean_view_test <- ceiling(mean(test$view, na.rm = TRUE))
mean_submit_test <- ceiling(mean(test$submit, na.rm = TRUE))
test <- test %>% mutate(click = ifelse(is.na(click),mean_click_test,click))
test <- test %>% mutate(data = ifelse(is.na(data),mean_data_test,data))
test <- test %>% mutate(view = ifelse(is.na(view),mean_view_test,view))
test <- test %>% mutate(submit = ifelse(is.na(submit),mean_submit_test,submit))
test <- test %>% mutate(booking_request = ifelse(is.na(booking_request),0,booking_request))
test <- test %>% mutate(message_post = ifelse(is.na(message_post),0,message_post))
test <- test %>% mutate(partner_callback = ifelse(is.na(partner_callback),0,partner_callback))
test <- test %>% mutate(modify = ifelse(is.na(modify),0,modify))



#Extracting Classes
classes = train['country_destination']
# Removing destination country as it has been seperated in a seperated into the classes vector
train$country_destination <- NULL
# Binding training data to testing data to operate on the full dataset once
full = bind_rows(train,test)
# Date_First_Booking --> This is only available in the train dataset ,, wont be used in prediction
full$date_first_booking <- NULL
# replace missing values
full[is.na(full)] <- -1

# 1- Dates -- Extracting days and months which can reflect holidays ,,, vacations .. similar preferences 
# Year seems unnecessary in this regard
full$account_creation_day <- sapply(as.character(full$date_account_created), function(x) strsplit(x, split = '[-]')[[1]][3])
full$account_creation_month <- sapply(as.character(full$date_account_created), function(x) strsplit(x, split = '[-]')[[1]][2])
full$first_active_day <- substr(full$timestamp_first_active, 7, 8)
full$first_active_month <- substr(full$timestamp_first_active, 5, 6)

# Combining Similar/Famous Browsers and putting everything else in the others category
full$first_browser[full$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
full$first_browser[full$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
full$first_browser[full$first_browser %in% c("IE","IE Mobile")] <- "IE"
full$first_browser[full$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
full$first_browser[full$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Firefox"
famousBrowsersSet <- c("Chrome","Chrome Mobile","Chromium","Firefox","Mobile Firefox","Mozilla",
                       "IE","IE Mobile","Mobile Safari","Safari","Opera","Opera Mini","Opera Mobile")
full$first_browser[!(full$first_browser %in% famousBrowsersSet)] <- "Other"


# Removing the dates columns after creating separate columns for days and months 
full$date_account_created <- NULL
full$timestamp_first_active <- NULL

# You can't use Kaggle if you are less than 18
full[full$age < 18 | full$age > 100,'age'] <- -1


# Encoding all categorical variables
catVars <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = full)
catVarsEnc <- data.frame(predict(catVars, newdata = full))
# Replacing categorical variables with binary encoded ones
#Droping
dropSet <- c("gender" , "signup_method" , "signup_flow" , "language" , "affiliate_channel" , "affiliate_provider" , "first_affiliate_tracked" , "signup_app" , "first_device_type" , "first_browser")
full <- full[ , !(names(full) %in% dropSet)]
#Adding Encoded Columns
full <- bind_cols(full,catVarsEnc)


# Divide dataset into training and testing based on the originially provided IDs
train_final = full[full$id %in% train$id,]
test_final = full[full$id %in% test$id,]

classes <- as.integer(recode(classes$country_destination,NDF=0,US=1,other=2,FR=3,CA=4,GB=5,ES=6,IT=7,PT=8,NL=9,DE=10,AU=11))

# train xgboost
xgb <- xgboost(data = data.matrix(train_final[,-1]), 
               label = classes, 
               eta = 0.1,
               max_depth = 9, 
               nround=200, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)


# Computing Feature Importance
model <- xgb.dump(xgb, with.stats = T)
feature_names <- dimnames(data.matrix(train_final[,-1]))[[2]]
feature_importance_matrix <- xgb.importance(feature_names, model = xgb)

## THe training error is printed per iteration as train-merror
## Accruarcy = 1-train-merror


# Kaggle Standard Submission Format for this comeptition - available online (tweaked it slightly to create submissions)
# In case you want to create a submission, uncomment the following:

# y_pred <- predict(xgb, data.matrix(test_final[,-1]))
# predictions <- as.data.frame(matrix(y_pred, nrow=12))
# rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
# predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
# ids <- NULL
# for (i in 1:NROW(test_final)) {
#         idx <- test_final$id[i]
#         ids <- append(ids, rep(idx,5))
# }
# submission <- NULL
# submission$id <- ids
# submission$country <- predictions_top5
# # generate submission file
# submission <- as.data.frame(submission)
# write.csv(submission, "submission4.csv", quote=FALSE, row.names = FALSE)


