source("utilities.R")
library(randomForest)
library(xgboost)
library(e1071)
library(pROC)     # plot ROC
library(ROSE)     # for undersampling



# read data and split into train and test =====================================
# remember to remove duration, month, and day
dat <- fread("bank/bank-full.csv",
             stringsAsFactors = TRUE) %>%
    .[, y := factor(y, levels = c("yes", "no"))] %>%
    .[, duration := NULL] %>%
    .[, month := NULL] %>%
    .[, day := NULL]

# random train-test split instead of based on time. 
set.seed(98765)
in_train <- createDataPartition(dat$y, p = 0.7, list = F)
dat_train <- dat[in_train,]
dat_test <- dat[-in_train,]



# baseline model ==============================================================
# Use logistic regression on data without preprocessing to quickly build a 
# baseline model

base <- glm(y ~ ., data = dat_train, family = binomial)

base_pred_train <- predict(base, dat_train, type = "response")
base_roc_train <- roc(dat_train$y, base_pred_train)

base_pred_test <- predict(base, dat_test, type = "response")
base_roc_test <- roc(dat_test$y, base_pred_test)

# plot roc curves on train and test data. Name the ggplot object so we can 
# save the plot with data for future use. Plot lift curves takes a long time
base_roc <- plot_rocs(`base train` = base_roc_train, `base test` = base_roc_test)
base_roc    

# plot lift curve using test data only
base_lift <- plot_lifts(y = dat_test$y, base_test = 1 - base_pred_test)
base_lift



# data preprocessing ===========================================================
# Try some data preprocessing in hope to improve model performance

add_feature <- function(data = dat_train){
    # need more work at this place
    
    # pdays into categorical feature pcontact of two groups
    data[, previous_contact := factor(ifelse(pdays == -1, "no", "yes"))]
    
    return(data)
}

dat_train <- add_feature(dat_train)
dat_test <- add_feature(dat_test) 

# fix skewness
fix_skewness <- preProcess(dat_train, method = c("center", "scale", "YeoJohnson"))

train <- predict(fix_skewness, dat_train)
test <- predict(fix_skewness, dat_test)

# under-sampling train data. Data after oversampling is too big for my computer
# and does not improve model performance after a few test runs.
train_un <- ovun.sample(
    y ~.,
    data = train,
    method = "under",  # under or over
    seed = 4321
)$data %>%
    setDT()  %>%
    .[, y := factor(y, levels = c("yes", "no"))]
    


# logistic regression again ===================================================
# apply logistic regression on preprocessed data again.

lr <- glm(y ~ ., data = train_un, family = binomial)

lr_pred_train <- predict(lr, train, type = "response")
lr_roc_train <- roc(train$y, lr_pred_train)

lr_pred_test <- predict(lr, test, type = "response")
lr_roc_test <- roc(test$y, lr_pred_test)

# plot roc and lift curves using train and test data
lr_roc <- plot_rocs(`logit train` = lr_roc_train, `logit test` = lr_roc_test)
lr_roc

lr_lift <- plot_lifts(test$y, logit_test = 1 - lr_pred_test)
lr_lift



# support vector machine =======================================================
# random search for hyper parameters.

train_control <- trainControl(
    method = "cv",
    number = 5,
    search = "random",
    classProbs = TRUE,  # for metric = "ROC"
    summaryFunction=twoClassSummary,  # for metric "ROC"
    verboseIter = TRUE
)

svm <- train(
    y ~., 
    data = train_un,
    method = "svmRadial",
    probabilities = TRUE,
    metric = "ROC",
    trControl = train_control,
    tuneLength = 20
)



svm_pred_train <- predict(svm, train, type = "prob")[, "yes"]
svm_roc_train <- roc(train$y, svm_pred_train)

svm_pred_test <- predict(svm, test, type = "prob")[, "yes"]
svm_roc_test <- roc(test$y, svm_pred_test)

# plot roc and lift curves using train and test data
svm_roc <- plot_rocs(`svm train` = svm_roc_train, `svm test` = svm_roc_test)
svm_roc

svm_lift <- plot_lifts(test$y, svm_test = svm_pred_test)
svm_roc



# random forest model ==========================================================
# to reduce model size and avoid over fitting, we set a limit to max number of 
# nodes in a tree. The comment-out code below is to find out the optimal number.

# train_control <- trainControl(
#     method = "cv",      # "oob" does not support twoClassSummary
#     number = 5,
#     search = "random",
#     classProbs = TRUE, 
#     summaryFunction=twoClassSummary, 
#     verboseIter = FALSE
# )
# 
# tune_grid <- expand.grid(mtry = 5:7)
# 

# Print out results and eyeball to select max_node for random forest. turns out
# AUC is not sensitive to max_node. Select 32 anyway.
# set.seed(1111)
# for (max_nodes in 2^(2:10)){
#     cat(paste0("\nmaxnodes = ", max_nodes, ":\n"))
#     
#     rf <- train(
#         y ~., 
#         data = train_un,
#         method = "rf",
#         maxnodes = max_nodes,
#         metric = "ROC",
#         trControl = train_control,
#         tuneGrid = tune_grid
#     )
#     
#     print(rf$results)
# }


# use the max_node = 32 to train a random forest model
rf <- randomForest(y ~ ., train_un, 
                   maxnodes = 32, 
                   ntree = 1000)

rf_pred_train <- predict(rf, train, type = "prob")[, "yes"]
rf_roc_train <- roc(train$y, rf_pred_train)

rf_pred_test <- predict(rf, test, type = "prob")[, "yes"]
rf_roc_test <- roc(test$y, rf_pred_test)

# plot roc and lift curves
rf_roc <- plot_rocs(`rf train` = rf_roc_train, `rf test` = rf_roc_test)
rf_roc

rf_lift <- plot_lifts(test$y, rf_test = rf_pred_test)
rf_lift



# xgboost ======================================================================
# use random search find the best hyper parameters

train_control <- trainControl(
    method = "cv",
    number = 5,
    search = "random",
    classProbs = TRUE,  # for metric = "ROC"
    summaryFunction=twoClassSummary,  # for metric "ROC"
    verboseIter = TRUE
)

xgb <- train(
    y ~., 
    data = train_un,
    method = "xgbTree",
    metric = "ROC",
    trControl = train_control,
    tuneLength = 100
)

xgb_pred_train <- predict(xgb, train, type = "prob")[, "yes"]
xgb_roc_train <- roc(train$y, xgb_pred_train)

xgb_pred_test <- predict(xgb, test, type = "prob")[, "yes"]
xgb_roc_test <- roc(test$y, xgb_pred_test)

# plot auc and lift curves
xgb_roc <- plot_rocs(`xgb train` = xgb_roc_train, `xgb test` = xgb_roc_test)
xgb_roc

xgb_lift <- plot_lifts(test$y, xgb_test = xgb_pred_test)
xgb_lift



# plot models together and compare models ======================================

# all ROCs using test data
all_rocs <- plot_rocs(base = base_roc_test, 
          logistic_regression = lr_roc_test,
          svm = svm_roc_test,
          random_forest = rf_roc_test,
          xgb = xgb_roc_test)

# all LIFT curves using test data
all_lifts <- plot_lifts(test$y,
           base = 1 - base_pred_test,
           logistic_regression = 1 - lr_pred_test,
           svm = svm_pred_test,
           random_forest = rf_pred_test,
           xgb = xgb_pred_test)

# select xgb as the final model and compare to baseline model
xgb_base_roc <- plot_rocs(base = base_roc_test, 
                          xgb = xgb_roc_test)
xgb_base_lift <- plot_lifts(test$y,
                            base = 1 - base_pred_test,
                            xgb = xgb_pred_test)

# save data, models, and plots
save(train, test, train_un, 
     base, lr, svm, rf, xgb,
     all_rocs, all_lifts,
     base_roc, base_lift,
     lr_roc, lr_lift,
     svm_roc, svm_lift,
     rf_roc, rf_lift,
     xgb_roc, xgb_lift,
     xgb_base_roc, xgb_base_lift,
     file = "model_20200610.RData")
