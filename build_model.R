source("utilities.R")
library(randomForest)
library(xgboost)
library(e1071)
library(pROC)     # plot ROC
library(ROSE)     # for oversampling



# read data and split into train and test =====================================
# remember to remove duration
dat <- fread("bank/bank-full.csv",
             stringsAsFactors = TRUE) %>%
    .[, y := factor(y, levels = c("yes", "no"))] %>%
    .[, duration := NULL] %>%
    .[, month := NULL] %>%
    .[, day := NULL] %>%
    # randomForest() hates "-" in column names (happens after creating dummies)
    .[, job := factor(stringr::str_replace_all(job, "-", ""))]

# train-test split based on time. the last 9000 samples in test
set.seed(98765)
in_train <- createDataPartition(dat$y, p = 0.7, list = F)
dat_train <- dat[in_train,]
dat_test <- dat[-in_train,]



# baseline model ==============================================================

base <- glm(y ~ ., data = dat_train, family = binomial)

base_pred_train <- predict(base, dat_train, type = "response")
base_roc_train <- roc(dat_train$y, base_pred_train)

base_pred_test <- predict(base, dat_test, type = "response")
base_roc_test <- roc(dat_test$y, base_pred_test)

# plot roc curves using train and test data
base_roc <- plot_rocs(`base train` = base_roc_train, `base test` = base_roc_test)

base_lift <- plot_lifts(y = dat_test$y, base_test = 1 - base_pred_test)



# data preprocessing ===========================================================

clean_data <- function(data = dat_train){
    # To treat categorical feature. The unknowns are treated as its own category
    # as there are reasons to be missing.
    #
    # Arguments:
    #   data: data frame with features to be preprocessed
    #
    # Return:
    #   A data frame with preproccessed features
    
    # pdays into categorical feature pcontact of two groups
    data[, previous_contact := factor(ifelse(pdays == -1, "no", "yes"))]
    
    # previous into "yes" and "no" and is exactly the same as pdays, delete

    return(data)
}

dat_train <- clean_data(dat_train)
dat_test <- clean_data(dat_test) 


# fix skewness
pre_proc <- preProcess(dat_train, method = c("center", "scale", "YeoJohnson"))

train <- predict(pre_proc, dat_train)
test <- predict(pre_proc, dat_test)

# over-sampling train data
train_un <- ovun.sample(
    y ~.,
    data = train,
    method = "under",  # under or over
    seed = 4321
)$data %>%
    setDT()  %>%
    .[, y := factor(y, levels = c("yes", "no"))]
    


# logistic regression again ===================================================

lr <- glm(y ~ ., data = train_un, family = binomial)

lr_pred_train <- predict(lr, train, type = "response")
lr_roc_train <- roc(train$y, lr_pred_train)

lr_pred_test <- predict(lr, test, type = "response")
lr_roc_test <- roc(test$y, lr_pred_test)

# plot roc curves using train and test data
lr_roc <- plot_rocs(`logit train` = lr_roc_train, `logit test` = lr_roc_test)

lr_lift <- plot_lifts(test$y, logit_test = 1 - lr_pred_test)



# support vector machine ======================================================

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



#svm <- svm(y ~ ., data = train_un, probability = TRUE)

svm_pred_train <- predict(svm, train, type = "prob")[, "yes"]
#svm_pred_train <-    attr(svm_pred_train, "probabilities")[, "yes"]
svm_roc_train <- roc(train$y, svm_pred_train)

svm_pred_test <- predict(svm, test, type = "prob")[, "yes"]
#svm_pred_test <- attr(svm_pred_test, "probabilities")[, "yes"]
svm_roc_test <- roc(test$y, svm_pred_test)

# plot roc curves using train and test data
svm_roc <- plot_rocs(`svm train` = svm_roc_train, `svm test` = svm_roc_test)

svm_lift <- plot_lifts(test$y, svm_test = svm_pred_test)



# random forest model ==========================================================

# train_control <- trainControl(
#     method = "cv",      # "oob" nnot support twoClassSummary
#     number = 5,
#     search = "random",
#     classProbs = TRUE,  # for metric = "ROC"
#     summaryFunction=twoClassSummary,  # for metric "ROC"
#     verboseIter = FALSE
# )
# 
# tune_grid <- expand.grid(mtry = 5:7)
# 

# use a subset of train_un to quickly find the optimal max_node of random forest
# turns out AUC is not sensitive to max_node. Select 32 to reduce computation
# load.
# set.seed(1111)
# for (max_nodes in 2^(2:10)){
#     cat(paste0("\nmaxnodes = ", max_nodes, ":\n"))
#     
#     rf <- train(
#         y ~., 
#         data = train_un[sample(1:nrow(train_un), 5000)],
#         method = "rf",
#         maxnodes = max_nodes,
#         metric = "ROC",
#         trControl = train_control,
#         tuneGrid = tune_grid
#     )
#     
#     print(rf$results)
# }


# use the max_node to train a random forest model
rf <- randomForest(y ~ ., train_un, 
                   maxnodes = 32, 
                   ntree = 1000)

rf_pred_train <- predict(rf, train, type = "prob")[, "yes"]
rf_roc_train <- roc(train$y, rf_pred_train)

rf_pred_test <- predict(rf, test, type = "prob")[, "yes"]
rf_roc_test <- roc(test$y, rf_pred_test)

rf_roc <- plot_rocs(`rf train` = rf_roc_train, `rf test` = rf_roc_test)

rf_lift <- plot_lifts(test$y, rf_test = rf_pred_test)



# xgboost ======================================================================

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
    #weights = ifelse(dat_train$y == "no", 0.1, 0.9),  
    metric = "ROC",
    trControl = train_control,
    tuneLength = 100
)

xgb_pred_train <- predict(xgb, train, type = "prob")[, "yes"]
xgb_roc_train <- roc(train$y, xgb_pred_train)

xgb_pred_test <- predict(xgb, test, type = "prob")[, "yes"]
xgb_roc_test <- roc(test$y, xgb_pred_test)

xgb_roc <- plot_rocs(`xgb train` = xgb_roc_train, `xgb test` = xgb_roc_test)

xgb_lift <- plot_lifts(test$y, xgb_test = xgb_pred_test)


# plot together to compare models ==============================================

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

# select xgb as the final model and compare to base
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
