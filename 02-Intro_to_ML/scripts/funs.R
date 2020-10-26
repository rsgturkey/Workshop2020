# This script contains functions for visualizing confusion matrices and statistics
# Note that these are custom fuctions that were written for a specific example.
#
# In order to use these functions in the main.R script, you need to either run them
# in the R session, or you need to use 'source()' function as used in main.R
library(tidyverse)
library(reshape2)

plot_cm = function(cm_list){
  cm_list %>%
    reshape2::melt() %>%
    group_by(L1, Reference) %>%
    mutate(prop = value/sum(value),
           Prediction = factor(Prediction, levels = c("Died","Recovered")),
           Reference = factor(Reference, levels = c("Recovered", "Died"))) %>%
    ggplot(aes(x=Prediction, y=Reference, fill=prop)) +
    geom_tile(color="white") +
    facet_grid(.~L1) +
    scale_fill_gradient(low = "lightsteelblue1", high = "steelblue4") +
    geom_text(aes(label = value), vjust = .5) +
    theme_bw() +
    theme(legend.position = "none")
}

stat_cm = function(cm_list){
  sapply(cm_list, function(x){
    data.frame(
      "Accuracy" = x$overall["Accuracy"],
      "Sensitivity" = x$byClass["Sensitivity"],
      "Specificity" = x$byClass["Specificity"],
      "Precision" = x$byClass["Precision"],
      "MCC" = Matt_Coef(x)
    )
  })
}


Matt_Coef <- function (conf_matrix)
{
  TP <- conf_matrix$table[1,1]
  TN <- conf_matrix$table[2,2]
  FP <- conf_matrix$table[1,2]
  FN <- conf_matrix$table[2,1]
  
  mcc_num <- (TP*TN - FP*FN)
  mcc_den <- 
    as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
  
  mcc_final <- mcc_num/sqrt(mcc_den)
  return(mcc_final)
}

