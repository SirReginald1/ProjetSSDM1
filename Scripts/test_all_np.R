source("functions.R")

base_output_path = "../ReducedOutput"

data_path = "../Data"

save_models = FALSE

# Set number of variables to include
newas = c(10, 20, 30, 50, 100, 200, 300, 500, 1000, 2000, 3000, 5000, 10000)

# Set the number of n to test
N = c(10, 20, 30, 50, 100, 200, 300, 500, 1000, 1600)

#' Only use classes that are shared between original datasets in the original data
#' (used to compar balenced datasets with non balenced)
original_test_classes = TRUE

#### SVM ####

model_name = "e1071_svm"
source("SVM_script.R")

#### Random Forest ####

model_name = "randomForest_rf"
source("rf_script.R")

#### XGBoost ####

model_name = "xgboost_xgboost"
source("xgboost_script.R")

# Run python nn test

stop("Stoping here!")

model_name = "nn_600_300_np"


# If runing on windows
if(sum(grep("Windows", Sys.info())) > 0){
  system(paste("run_Python.bat", model_name,
               base_output_path, data_path,
               paste0("\"[",paste(newas, collapse = ","),"]\""),
               paste0("\"[",paste(N, collapse = ","),"]\""),
               original_test_classes,
               save_models))
}else{
  message("MAC or Linux script is untested! You may need to run the python script yourself if you are using these systems.")
  system(paste("run_Python.sh", model_name,
               base_output_path, data_path,
               paste0("\"[",paste(newas, collapse = ","),"]\""),
               paste0("\"[",paste(N, collapse = ","),"]\""),
               original_test_classes,
               save_models))
}