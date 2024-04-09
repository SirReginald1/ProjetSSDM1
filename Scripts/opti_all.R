################### General parameters ##################################

data_path = "OldData"

base_output_path = "OldDataOutput"

#' Only use classes that are shared between original datasets in the original data
#' (used to compar balenced datasets with non balenced)
original_test_classes = FALSE


save_models = FALSE

################### XGBoost parameters #################################

xgb_lr = c(0.10)
xgb_gamma = c(0)
xgb_lambda = c(4, 5)
xgb_alpha = c(0,1,2)
xgb_minchild =c(0.5,0.6)
xgb_coltree = c(0.2, 0.1)
xgb_max_depth = c(14,10,6,5)


################### SVM parameters #################################

svm_kernel = c("linear", "polynomial", "radial")
svm_gamma =  1/10000
svm_coef0 = c(0,10,30,100,200)
svm_degree = c(1,2,3,4,5,6)
svm_cost = c(1,1000)

####################################################################

#' Alows script to be run both from Scripts folder and from parent folder
#' If file exists, script is being run from Scripts folder
cmd = file.exists("functions.R")

if(cmd){
  source("functions.R")
} else{
  source("Scripts/functions.R")
}

###### Adjusting path ######
if(cmd){
  base_output_path = paste0("../", base_output_path)
  data_path = paste0("../", data_path)
}

######################### Run XGBoost models #######################

model_name = "xgboost_xgboost_param"
if(cmd){
  source("xgboost_opti_script.R")
} else{
  source("Scripts/xgboost_opti_script.R")
}



######################### Run SVM models #######################

model_name = "e1071_svm_param"
if(cmd){
  source("SVM_opti_script.R")
} else{
  source("Scripts/SVM_opti_script.R")
}