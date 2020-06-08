### 
### Create a baseline model using logistic regression
###

library(pROC)
source("utilities.R")

# read data, remove duration and month
dat <- read.csv("bank-additional/bank-additional-full.csv",
                stringsAsFactors = TRUE, 
                sep = ";")
dat$duration <- NULL
dat$month <- NULL


# train-test split
set.seed(1234)
in_train <- caret::createDataPartition(dat$y, p = 0.8, list = FALSE)

dat_train <- dat[in_train,]
dat_test <- dat[-in_train,]


# baseline model: logistic regression
base <- glm(y ~ ., data = dat_train, family = binomial)

base_pred_train <- predict(base, dat_train, type = "response")
base_roc_train <- roc(dat_train$y, base_pred_train)

base_pred_test <- predict(base, dat_test, type = "response")
base_roc_test <- roc(dat_test$y, base_pred_test)

# plot roc curves using train and test data
plot_rocs(`base train` = base_roc_train, `base test` = base_roc_test)
