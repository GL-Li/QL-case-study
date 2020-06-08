library(data.table)
library(magrittr)
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(pROC)     # plot ROC
library(ROSE)     # for oversampling
source("utilities.R")


# read data and split into train and test =====================================
# remember to remove duration
dat <- fread("bank-additional/bank-additional-full.csv",
             stringsAsFactors = TRUE) %>%
    .[, duration := NULL] %>%
    .[, month := NULL]

set.seed(1234)
in_train <- createDataPartition(dat$y, p = 0.8, list = FALSE)

dat_train <- dat[in_train,]
dat_test <- dat[-in_train,]

# data preprocessing ===========================================================

clean_data <- function(data = dat_train){
    # To treat categorical feature. The unknowns are treated as its own category
    # as there are reasons to be missing.
    #
    # Arguments
    # ---------
    #   data: data frame with features to be preprocessed
    #
    # Return
    # ------
    #   A data frame with preproccessed features
    
    # "unknown" and "yes" in "default combined as one group as only 3 "yes"
    data[default %in% c("unknown", "yes"), default := "TBD"]
    
    # create a new categorical feature for those age > 60
    data[age > 60, over_60 := "yes"] 
    data[age <= 60, over_60 := "no"]
    data[, over_60 := factor(over_60)]
    
    # pdays into categorical feature pcontact of two groups
    data[pdays == 999, pcontact := "no"]
    data[pdays != 999, pcontact := "yes"]
    data[, pcontact := factor(pcontact) ]
    data[, pdays := NULL]
    
    # loan not important
    data[, loan := NULL]
    data[, cons.conf.idx := NULL]
    data[, nr.employed := NULL]
    
    return(data)
}

dat_train <- clean_data(dat_train)
dat_test <- clean_data(dat_test) 

pre_proc <- preProcess(dat_train, method = c("center", "scale", "BoxCox"))
dat_train <- predict(pre_proc, dat_train)
dat_test <- predict(pre_proc, dat_test)

# over-sampling train data
dat_os <- ovun.sample(
    y ~.,
    data = dat_train,
    method = "over",  # under or over
    seed = 4321
)$data 
    

# logistic regression again ===================================================
lr <- glm(y ~ ., data = dat_os, family = binomial)

lr_pred_train <- predict(lr, dat_train, type = "response")
lr_roc_train <- roc(dat_train$y, lr_pred_train)

lr_pred_test <- predict(lr, dat_test, type = "response")
lr_roc_test <- roc(dat_test$y, lr_pred_test)

# plot roc curves using train and test data
plot_rocs(`logit train` = lr_roc_train, `logit test` = lr_roc_test)





# random forest model ==========================================================
train_control <- trainControl(
    method = "oob",
    search = "random",
    classProbs = TRUE,  # for metric = "ROC"
    summaryFunction=twoClassSummary,  # for metric "ROC"
    verboseIter = TRUE
)

rf <- train(
    y ~., 
    data = dat_os,
    method = "rf",
    metric = "ROC",
    trControl = train_control,
    tuneLength = 400
)





rf <- randomForest(y ~ ., dat_os, maxnodes = 32, ntree = 2000)

rf_pred_train <- predict(rf, dat_train, type = "prob")[, 2]
rf_roc_train <- roc(dat_train$y, rf_pred_train)

rf_pred_test <- predict(rf, dat_test, type = "prob")[, 2]
rf_roc_test <- roc(dat_test$y, rf_pred_test)

plot_rocs(`rf train` = rf_roc_train, `rf test` = rf_roc_test)


# # support vector machine =====================================================
# # very slow
# svm <- svm(y ~ ., dat_os, kernel = "linear", probability = TRUE)
# 
# svm_pred_train <- predict(svm, dat_train, probability = TRUE)
# svm_roc_train <- roc(dat_train$y, attr(svm_pred_train, "probabilities")[, 2])
# 
# svm_pred_test <- predict(svm, dat_test, probability = TRUE)
# svm_roc_test <- roc(dat_test$y, attr(svm_pred_test, "probabilities")[, 2])
# 
# plot_rocs(`svm train` = svm_roc_train, `svm test` = svm_roc_test)


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
    data = dat_train,
    method = "xgbTree",
    class.wt = c(0.2, 0.8),  # do not use weights, which is special in R
    metric = "ROC",
    trControl = train_control,
    tuneLength = 10
)

xgb_pred_train <- predict(xgb, dat_train, type = "prob")[, 2]
xgb_roc_train <- roc(dat_train$y, xgb_pred_train)

xgb_pred_test <- predict(xgb, dat_test, type = "prob")[, 2]
xgb_roc_test <- roc(dat_test$y, xgb_pred_test)

plot_rocs(`xgb train` = xgb_roc_train, `xgb test` = xgb_roc_test)


# compare ======================================================================
plot_rocs(logit_test = lr_roc_test, xgb_test = xgb_roc_test)

