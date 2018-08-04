#!/usr/bin/Rscript

# roc curve example
# https://community.alteryx.com/t5/Data-Science-Blog/ROC-Curves-in-Python-and-R/ba-p/138430

# model : logistics regression to predict if a record in a diamonds dataset is over $2400

library(ggplot2)

diamonds$is_expensive <- diamonds$price > 2400
is_test <- runif(nrow(diamonds)) > 0.75
train <- diamonds[is_test==FALSE,]
test <- diamonds[is_test==TRUE,]

summary(fit <- glm(is_expensive ~ carat + cut + clarity, data=train))

library(ROCR)

prob <- predict(fit, newdata=test, type="response")
pred <- prediction(prob, test$is_expensive)

library(ROCR)

prob <- predict(fit, newdata=test, type="response")
pred <- prediction(prob, test$is_expensive)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(
                       fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM"
                       )

x11() # to open the plot in a window
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_ribbon(alpha=0.2) + 
  geom_line(aes(y=tpr)) + 
  ggtitle(paste0("ROC Curve w/ AUC=", auc))
invisible(readLines("stdin", n=1)) # wait for user input to close window
