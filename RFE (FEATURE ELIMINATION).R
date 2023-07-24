#----Set Seed and Preparation----
set.seed(100)
options(warn=-1)
subsets <- c(1:20)

#----Train Control----
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 10,
                   verbose = FALSE)

#----Building a Variable Importances Model----
lmProfile <- rfe(x=rasvalue[,c(1:20)], y=rasvalue[,c(21)],
                 sizes = subsets,
                 rfeControl = ctrl)
Predictors<- predictors(lmProfile)
print(Predictors)
lmProfile$results

#----Plot Variable Importances----
library(ggplot2)
ggplot(data = lmProfile, metric = "Rsquared") + theme_bw()
varimp_data <- data.frame(feature = row.names(varImp(lmProfile))[1:18],
                          importance = varImp(lmProfile)[1:18, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")
