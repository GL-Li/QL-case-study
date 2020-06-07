library(ggplot2)
library(caret)
library(pROC)
library(ROSE)
library(randomForest)
library(doSNOW)

cl <- makeCluster(3)
registerDoSNOW(cl)

# read data and set "yes" as positive class
dat <- read.csv("bank-additional/bank-additional-full.csv",
                stringsAsFactors = TRUE, 
                sep = ";")

# set "yes" as positive class
#dat$y <- factor(dat$y, levels = c("yes", "no"))

set.seed(1234)
in_train <- createDataPartition(dat$y, p = 0.8, list = FALSE)

dat_train <- dat[in_train,]
dat_test <- dat[-in_train,]


# baseline model: logistic regression ==========================================
lr <- glm(y ~ ., data = dat_train, family = binomial)

lr_pred <- predict(lr, dat_test, type = "response")
lr_roc <- roc(dat_test$y, lr_pred)

plot_roc(logit = lr_roc)




# over-sampling train data
train_over <- ovun.sample(
    y ~.,
    data = dat_train,
    method = "over",
    seed = 4321
)$data 
    

X_train <- train_over[, 1:16]
y_train <- factor(train_over$y, levels = c("yes", "no"))

X_test <- dat_test[, 1:16]
y_test <- factor(dat_test$y, levels = c("yes", "no"))

# a quick random forest model =================================================
rf <- randomForest(X_train, y_train)

y_pred <- predict(rf, X_test)
confusionMatrix(y_pred, y_test)


# caret =======================================================================
train_control <- trainControl(
    method = "oob",
    search = "grid",
    verboseIter = TRUE
)

tune_grid <- expand.grid(
    mtry = 2:6
)

metric <- "ROC"

rf <- train(
    X_train, y_train,
    method = "rf",
    trControl = train_control,
    tuneGrid = tune_grid
)

y_pred <- predict(rf$finalModel, X_test)
confusionMatrix(y_pred, y_test)
