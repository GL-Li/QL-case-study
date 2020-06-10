### 
### Create a baseline model using logistic regression
###

library(pROC)
source("utilities.R")

# read data, remove duration and change months
dat <- read.csv("bank/bank-full.csv",
                stringsAsFactors = TRUE, 
                sep = ";")
dat$duration <- NULL
dat$month <- NULL
dat$day <- NULL
dat$y <- factor(dat$y, levels = c("yes", "no"))


# train-test split based on time. the last 9000 samples in test

dat_train <- dat[1:36211,]
dat_test <- dat[36212:45211,]


# baseline model: logistic regression
base <- glm(y ~ ., data = dat_train, family = binomial)

base_pred_train <- predict(base, dat_train, type = "response")
base_roc_train <- roc(dat_train$y, base_pred_train)

base_pred_test <- predict(base, dat_test, type = "response")
base_roc_test <- roc(dat_test$y, base_pred_test)

# plot roc curves using train and test data
plot_rocs(`base train` = base_roc_train, `base test` = base_roc_test)


plot_lifts(y = dat_test$y, base_test = 1 - base_pred_test) # prob for level 1
