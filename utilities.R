###
###  define functions used in this case study
###

library(ggplot2)

plot_avg <- function(dt, feature, target = "y"){
    # To plot the dependence of mean ourcome as function of a feature as well 
    # as to mark the count samples for each feature value.
    #
    # Arguments
    # ---------
    # dt: data.table of the bank dataset
    # feature: one of the features in the datset
    # target: target of the dataset
    #
    # Return
    # ------
    # make a plot, no return
    
    dat_avg <- dt[, 
                  .(avg_success = mean(get(target)),
                    count = .N), 
                  by = get(feature)] %>%
        set_colnames(c(feature, "avg_success", "samples"))
    ggplot(dat_avg, aes_string(x = feature)) +
        geom_point(aes(y = avg_success, size = samples), 
                   alpha = 0.5) +
        scale_size_area() +
        ylim(0, 1.05 * max(dat_avg$avg_success)) +
        geom_text(aes(y = avg_success + 0.04 * max(avg_success), label = samples)) +
        theme_bw() +
        theme(legend.position = "none")
}


plot_rocs <- function(...){
    # To plot ROC curve of multiple roc objects of binary classification.
    #
    # ARguments:
    #   ...: multiple named roc objects in the form of 
    #        aaa = roc_1, bbb = roc_2, `ccc cc` = roc_3, 
    #        each roc object is created with roc() in pROC package as
    #        roc_1 <- pROC::roc(y_test_1, pred_probability_1)
    #
    # Return:
    #   no return
    
    df <- data.frame(tpr = numeric(0),
                     fpr = numeric(0),
                     method = character(0))
    
    rocs <- list(...)
    for (i in 1:length(rocs)){
        roc <- rocs[[i]]
        tpr <- roc$sensitivities
        fpr <- 1 - roc$specificities
        name <- paste0("\n", names(rocs)[i], "\nAUC = ", round(roc$auc, 5))
        
        df_i <- data.frame(tpr = tpr,
                           fpr = fpr,
                           method = name)
        df <- rbind(df, df_i)
    }
    
    ggplot() +
        geom_line(data = df, aes(fpr, tpr, color = method)) + 
        geom_line(data = data.frame(x = c(0, 1), y = c(0, 1)),
                  mapping = aes(x, y),
                  color = "gray90") +
        scale_x_continuous(breaks = (0:5) * 0.2) +
        scale_y_continuous(breaks = (0:5) * 0.2) +
        theme_bw() +
        theme(legend.position = c(0.85, 0.6))
}
