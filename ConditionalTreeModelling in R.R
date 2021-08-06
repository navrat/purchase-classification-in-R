rm(list=ls())

# install.packages("partykit")
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
install.packages("caret")
require(rsample) # for dataset and splitting also loads broom and tidyr
require(dplyr)
require(ggplot2)
theme_set(theme_bw()) # set theme
require(CHAID)
require(purrr)
require(caret)
require(e1071)

setwd("C:\\Users\\naveen.rathani\\Downloads\\Antuit 2018-2019\\altair")

df = read.csv("finaldat.csv")
str(df)
df$outcome_alt = as.factor(ifelse(df$outcome==0,"no purchase", "yes purchase"))

# CTREE2 FULLBLOWN TREE with no parameter tuning
ctreeBase <- ctree(outcome_alt ~ relationship+marital.status+occupation+education+new_age+new_revenue+new_value+new_hours.per.week+gender+workclass, data = df)

print(ctreeBase)
plot(ctreeBase)
table(predict(ctreeBase), df$outcome_alt)

# Compute the confusion matrix and all the statistics
result <- confusionMatrix(predict(ctree2), df$outcome_alt, mode="prec_recall",
                          positive = "yes purchase"))

result
result$byClass["Precision"]
result$byClass["Recall"]
result$byClass["F1"]

###
# CTREE1 CURTAILED - CTREE_ALT_F
ctree2 <- ctree(outcome_alt ~ relationship+marital.status+occupation+education+new_age+new_revenue+new_value+new_hours.per.week+gender+workclass, data = df,
                      control = ctree_control(minsplit = 100, minprob = 0.05))
print(ctree2)
plot(ctree2)
table(predict(ctree2), df$outcome_alt)

# Compute the confusion matrix and all the statistics
result <- confusionMatrix(predict(ctree2), df$outcome_alt, mode="prec_recall",
                          positive = "yes purchase")

result
result$overall['Accuracy']
result$byClass["Precision"]
result$byClass["Recall"]
result$byClass["F1"]

