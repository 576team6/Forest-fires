#######################
##### Environment #####
#######################

library(tidyr)
library(dplyr)
library(caret)
library(e1071)
library(ggplot2)
library(rpart.plot)
library(tidyverse)

#####################
##### Functions #####
#####################

#---- Bootstrap ----
bootStrap_data = function(samp_data, samp_size, Class){
  index = seq(1, dim(samp_data)[1], length.out = dim(samp_data)[1])
  full_data = cbind("index"=index, samp_data)
  boot_data = full_data[full_data$FIRE_SIZE_CLASS==paste(Class), ]
  ind = sample(boot_data$index, samp_size, replace = T)
  bootset = samp_data[ind,]
}

#---- SVM ----
svm_linear_select = function(trt, test, f_control, para_vec){
  n = length(para_vec)
  testing_x = test[,-1]
  testing_y = test[,1]
  accuracy = seq(0,0,length.out = n+1)
  cm_final = list()
  for (i in 1:n) {
    fit = train(class~., data = trt, method = "svmLinear", trControl = f_control, cost= para_vec[i])
    y_pred = as.vector(predict(fit, testing_x))
    cm = confusionMatrix(as.factor(y_pred), testing_y)
    accuracy[i+1] = cm$overall[1]
    if (accuracy[i+1] > accuracy[i]){
      cm_final = cm
    }
  }
  max_acc = max(accuracy)
  best_para = para_vec[which.max(accuracy)-1]
  return(list("max_accuracy" = max_acc, "best_cost"= best_para, "cm" = cm_final))
}

#---- Decision tree ----
dtree_select = function(trt, test, f_control, tune_len){
  n = length(tune_len)
  testing_x = test[,-1]
  testing_y = test[,1]
  accuracy = seq(0,0,length.out = n+1)
  cm_final = list()
  for (i in 1:n) {
    fit = train(class~., data = trt, method = "rpart", trControl = f_control, tuneLength = tune_len[i])
    y_pred = as.vector(predict(fit, testing_x))
    cm = confusionMatrix(as.factor(y_pred), testing_y)
    accuracy[i+1] = cm$overall[1]
    if (accuracy[i+1] > accuracy[i]){
      cm_final = cm
    }
  }
  max_acc = max(accuracy)
  best_para = tune_len[which.max(accuracy)-1]
  return(list("max_accuracy" = max_acc, "best_cost"= best_para, "cm" = cm_final))
}

#---- Random Forest ----
rf_select = function(trt, test, f_control, best_grid, tune_len){
  n = length(tune_len)
  testing_x = test[,-1]
  testing_y = test[,1]
  accuracy = seq(0,0,length.out = n+1)
  cm_final = list()
  for (i in 1:n) {
    fit = train(class~., data = trt, method = "rf", trControl = f_control, tuneLength = tune_len[i], tuneGrid = best_grid)
    y_pred = as.vector(predict(fit, testing_x))
    cm = confusionMatrix(as.factor(y_pred), testing_y)
    accuracy[i+1] = cm$overall[1]
    if (accuracy[i+1] > accuracy[i]){
      cm_final = cm
    }
  }  
  max_acc = max(accuracy)
  best_para = tune_len[which.max(accuracy)-1]
  return(list("max_accuracy" = max_acc, "best_cost"= best_para, "cm" = cm_final))
}

#---- Confusion matrix heatmap ----
cm_plot = function(cm, name){
  cm$table %>%
    as.data.frame %>%
    ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = round(Freq, 2)), size = 3, color = 'gray20') + 
    scale_fill_gradient(low = 'lightcyan', high = 'cyan4', limits = c(0,4000), name = 'Relative Frequency') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ggtitle(paste(name))
}

################
##### Data #####
################

#---- Read data ----
state = read.delim("state.txt", sep = "|")
climate = read.csv("~/Desktop/CS 575/Project/climdiv_state_year.csv")
fire_data = read.csv("~/Desktop/CS 575/Project/fire_data4.csv")

#---- Data preprocess ----
climate = climate%>% filter(year>1994 & year<2005) %>% left_join(state, by = c("fips" = "STATE"))
climate = climate[, -c(1,6,7)]
fire = fire_data%>% left_join(climate, by = c("STATE" = "STUSAB", "FIRE_YEAR" = "year"))
fire$DAYS_LAST = fire$CONT_DATE-fire$DISCOVERY_DATE
dat = fire[complete.cases(fire),]

#---- Stratified sampling ----
sample_dat = dat%>% group_by(FIRE_SIZE_CLASS, FIRE_YEAR, STAT_CAUSE_CODE, STATE)%>%slice_sample(n=25)
#write.csv(sample_dat, file = "sample_data.csv", row.names = F)
sample_dat$STAT_CAUSE_CODE = as.factor(sample_dat$STAT_CAUSE_CODE)

#---- Bootstrap sampling due to imbalance for classes D-G ----
set_D = bootStrap_data(sample_dat, 5000, "D")
set_E = bootStrap_data(sample_dat, 5000, "E")
set_F = bootStrap_data(sample_dat, 5000, "F")
set_G = bootStrap_data(sample_dat, 5000, "G")
full_data = rbind(sample_dat, set_D, set_E, set_F, set_G)

####################
##### Modeling #####
####################

# feature selection
model_dat = data.frame("class" = sample_dat$FIRE_SIZE_CLASS, 
                       "year" = sample_dat$FIRE_YEAR,
                       "cause_code" = sample_dat$STAT_CAUSE_CODE,
                       "annual_temp" = sample_dat$tempc,
                       "last_days" = sample_dat$DAYS_LAST,
                       "state" = sample_dat$STATE)
# data with bootstrap
full_model = data.frame("class" = full_data$FIRE_SIZE_CLASS, 
                        "year" = full_data$FIRE_YEAR,
                        "cause_code" = full_data$STAT_CAUSE_CODE,
                        "annual_temp" = full_data$tempc,
                        "last_days" = full_data$DAYS_LAST,
                        "state" = full_data$STATE)
set.seed(123)
# sample data train vs test splitting
split_ind = as.numeric(createDataPartition(model_dat$class, p=0.7, list = F))
training = model_dat[split_ind,]
testing = model_dat[-split_ind,]
testing_x = testing[,-1]
testing_y = testing[,1]
# bootstrapped data train vs test splitting
split_ind_b = as.numeric(createDataPartition(full_model$class, p=0.7, list = F))
training_b = full_model[split_ind_b,]
testing_b = full_model[-split_ind_b,]
testing_xb = testing_b[,-1]
testing_yb = testing_b[,1]
# cross validation setup: 5 folds
fitControl = trainControl(method = "cv", number = 5, allowParallel = T)

#########################################
##### Model training and evaluation #####
#########################################

#---- SVM ----
cost = c(0.01, 0.1, 1, 10, 50) 
svm_linear_fit = svm_linear_select(training, testing, fitControl, cost) #0.01 is the best
svm_l_acc = svm_linear_fit$max_accuracy
svm_l_cm = svm_linear_fit$cm
cm_plot(svm_l_cm, "Linear SVM Confusion Matrix")

#---- Decision Tree ----
# Parameters chosen
tune_length = c(1, 3, 10, 20, 50, 100, 500) 
# Fit model with sample data
dtree_fit = dtree_select(training, testing, fitControl, tune_length) #50 is the best
dtree_cm = dtree_fit$cm
cm_plot(dtree_cm, "Decision Tree Confusion Matrix")
# Fit model with boot data
dtrboot_fit = dtree_select(training_b, testing_b, fitControl, tune_length) #100 is the best
dtr_boot_cm = dtrboot_fit$cm
cm_plot(dtr_boot_cm, "DTree with Bootstrapping Data Confusion Matrix")

#---- Random Forest ----
# Parameter chosen
grid = expand.grid(.mtry = c(1:10))
rf_fit = train(class~., data = training, method = "rf", trControl = fitControl, tuneLength = 3, ntree = 100)#tuneGrid = grid)
plot(rf_fit)
rf_fit$results
# grid = 9 gives the best accuracy
tune_length2 = c(10, 50, 100, 200) 
grid_opt = expand.grid(.mtry = 9)
# Fit model with sample data
rf_fit_new = rf_select(training, testing, fitControl, grid_opt, tune_length2) #50 is the best
rf_cm = rf_fit_new$cm
cm_plot(rf_cm, "Random Forest Confusion Matrix")
# Fit model with boot data
rf_fit_boot = rf_select(training_b, testing_b, fitControl, grid_opt, tune_length2) #100 is the best
rfboot_cm = rf_fit_boot$cm
cm_plot(rfboot_cm, "RF with Bootstrapping Data Confusion Matrix")

#---- Boosting ----
### XBG
# cross validation
fitControl_boost = trainControl(method = 'cv', number = 2, verboseIter = FALSE, allowParallel = TRUE)
# parameter chosen
boost_grid = expand.grid(nrounds = c(100),max_depth = c(8),eta = c(0.1),gamma = c(0.01),
  colsample_bytree = c(0.75),subsample = c(0.5),min_child_weight = c(0))
# Fit model with sample data
xbg_tree_fit = train(class~., data=training, method="xgbTree", trControl = fitControl_boost, tuneGrid = boost_grid)
xgb_y_pred = as.vector(predict(xbg_tree_fit, testing_x))
xgb_cm = confusionMatrix(as.factor(xgb_y_pred), testing_y)
cm_plot(xgb_cm, "xgbTree Confusion Matrix")
# Fit model with boot data
xgbboot_fit = train(class~., data=training_b, method="xgbTree", trControl = fitControl_boost, tuneGrid = boost_grid)
xgbboot_pred = as.vector(predict(xgbboot_fit, testing_xb))
xgbboot_cm = confusionMatrix(as.factor(xgbboot_pred), testing_yb)
cm_plot(xgbboot_cm, "xgbTree with Bootstrapped Data Confusion Matrix")
### ADA
adabag_fit = train(class~., data=training, method="AdaBag", trControl = fitControl_boost)
ada_y_pred = as.vector(predict(adabag_fit, testing_x))
ada_cm = confusionMatrix(as.factor(ada_y_pred), testing_y)
cm_plot(ada_cm, "AdaBoosting Confusion Matrix")

###############################
##### Results comparisons #####
###############################

# Accuracy vector
bAcc_rfboot = mean(rfboot_cm$byClass[,11])
bAcc_svm = mean(svm_l_cm$byClass[,11])
bAcc_dtree = mean(dtree_cm$byClass[,11])
bAcc_rf = mean(rf_cm$byClass[,11])
bAcc_xgb = mean(xgb_cm$byClass[,11])
bAcc_dtrboot = mean(dtr_boot_cm$byClass[,11])
bAcc_xgbboot = mean(xgbboot_cm$byClass[,11])
accuracy = c(bAcc_svm, bAcc_dtree, bAcc_rf, bAcc_xgb, bAcc_rfboot, bAcc_dtrboot, bAcc_xgbboot)
accuracy

# Sensitivity (Recall)
sensi_rfboot = mean(rfboot_cm$byClass[,1])
sensi_svm = mean(svm_l_cm$byClass[,1])
sensi_dtree = mean(dtree_cm$byClass[,1])
sensi_rf = mean(rf_cm$byClass[,1])
sensi_xgb = mean(xgb_cm$byClass[,1])
sensi_dtrboot = mean(dtr_boot_cm$byClass[,1])
sensi_xgbboot = mean(xgbboot_cm$byClass[,1])
sensitivi = c(sensi_svm, sensi_dtree, sensi_rf, sensi_xgb, sensi_rfboot, sensi_dtrboot, sensi_xgbboot)
sensitivi

# Specificity (TNR)
spec_rfboot = mean(rfboot_cm$byClass[,2])
spec_svm = mean(svm_l_cm$byClass[,2])
spec_dtree = mean(dtree_cm$byClass[,2])
spec_rf = mean(rf_cm$byClass[,2])
spec_xgb = mean(xgb_cm$byClass[,2])
spec_dtrboot = mean(dtr_boot_cm$byClass[,2])
spec_xgbboot = mean(xgbboot_cm$byClass[,2])
specificity = c(spec_svm, spec_dtree, spec_rf, spec_xgb, spec_rfboot, spec_dtrboot, spec_xgbboot)
specificity

# F1 score
f1_rfboot = mean(rfboot_cm$byClass[,7])
f1_svm = mean(svm_l_cm$byClass[,7])
f1_dtree = mean(dtree_cm$byClass[,7])
f1_rf = mean(rf_cm$byClass[,7])
f1_xgb = mean(xgb_cm$byClass[,7])
f1_dtrboot = mean(dtr_boot_cm$byClass[,7])
f1_xgbboot = mean(xgbboot_cm$byClass[,7])
f1_score = c(f1_svm, f1_dtree, f1_rf, f1_xgb, f1_rfboot, f1_dtrboot, f1_xgbboot)
f1_score

evaluation = data.frame("classifiers" = c("SVM", 'DecisionTree', 'RandomForest', 'GradientBoosting',
                                          'RF_Bootstrap', 'DTree_bootstrap', 'Xgb_bootstrap'),
                        'Accuracy' = accuracy,
                        'Sensitivity' = sensitivi,
                        'Specificity' = specificity,
                        'F1-score' = f1_score)


