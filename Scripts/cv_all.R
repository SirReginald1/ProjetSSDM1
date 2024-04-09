source("functions.R")

base_output_path = "../OldDataOutput"

data_path = "../OldData"

# If is null wil use num cutoff
acc_cutoff = NULL

# Only run the first n best models
num_cutoff = 1:150

nb_folds = 3

original_test_classes = FALSE

################## XGBoost ###################

model_name = "xgboost_CV"

source("cv_val_xgb.R")


################## SVM ###################

model_name = "svm_CV"

source("cv_val_svm.R")