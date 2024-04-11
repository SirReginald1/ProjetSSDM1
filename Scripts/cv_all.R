############## General parameters ##################################

base_output_path = "OldDataOutput"

data_path = "OldData"

# If is null wil use num cutoff
acc_cutoff = NULL

# Only run the first n best models
xgb_num_cutoff = 1:150

svm_num_cutoff = 1:64

#nb_folds = 3

# Run with balenced data and 69 classes
original_test_classes = FALSE


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
################## XGBoost ###################

model_name = "xgboost_CV"

# if(cmd){
#   source("cv_val_xgb.R")
# } else{
#   source("Scripts/cv_val_xgb.R")
# }

################## SVM ###################

model_name = "svm_CV"

if(cmd){
  source("cv_val_svm.R")
} else{
  source("Scripts/cv_val_svm.R")
}
