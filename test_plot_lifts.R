# positive class as level 1

set.seed(2)
lift_training <- twoClassSim(1000)
lift_testing  <- twoClassSim(1000)

ctrl <- trainControl(method = "cv", classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(1045)
fda_lift <- train(Class ~ ., data = lift_training,
                  method = "fda", metric = "ROC",
                  tuneLength = 20,
                  trControl = ctrl)
set.seed(1045)
lda_lift <- train(Class ~ ., data = lift_training,
                  method = "lda", metric = "ROC",
                  trControl = ctrl)

library(C50)
set.seed(1045)
c5_lift <- train(Class ~ ., data = lift_training,
                 method = "C5.0", metric = "ROC",
                 tuneLength = 10,
                 trControl = ctrl,
                 control = C5.0Control(earlyStopping = FALSE))

## Generate the test set results
lift_results <- data.frame(Class = lift_testing$Class)
lift_results$FDA <- predict(fda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$LDA <- predict(lda_lift, lift_testing, type = "prob")[,"Class1"]
lift_results$C5.0 <- predict(c5_lift, lift_testing, type = "prob")[,"Class1"]
head(lift_results)



# plot lift
trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ FDA + LDA + C5.0, data = lift_results)
plot(lift_obj, values = 60, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))





ggplot(lift_obj)


# data used for plot
ggplot(lift_obj$data, aes(CumTestedPct, CumEventPct, color = liftModelVar)) + 
    geom_line()


# area under lift curve
alift <- setDT(lift_obj$data) %>%
    .[, .(area = 0.001 * sum(CumEventPct)), by = liftModelVar]

plot_lifts(lift_testing$Class, 
           fda = lift_results$FDA, 
           lda = lift_results$LDA, 
           c50 = lift_results$C5.0)
