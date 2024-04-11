package = "e1071"

if(!require(package, character.only = TRUE)){
  install.packages(package)
  library(package)
}

# source("functions.R")
#
# output_path = "../Output"
#
# data_path = "../Data"
#
# model_name = "svm_CV"
#
# The testing accuracy cutoff for model selection
# acc_cutoff = 89
#
nb_folds = 3


# Check that the directory for the models exist or creat it
if(!dir.exists(base_output_path)) {
  dir.create(base_output_path)
}

# Creat output path
output_path = paste0(base_output_path, "/", model_name)

if(!dir.exists(output_path)){
  dir.create(output_path)
}

# if(!dir.exists(paste0(output_path,"/Models"))){
#   dir.create(paste0(output_path,"/Models"))
# }



# Load data set
data_train = readRDS(paste(data_path,"/df_newas10000_train.rds", sep = ""))
data_test = readRDS(paste(data_path,"/df_newas10000_test.rds", sep = ""))
data_test$meth_class = factor(data_test$meth_class)

# Remove classes that are not present in both datasets
if(length(unique(data_train$meth_class)) != length(unique(data_test$meth_class))){
  shared_classes = intersect(data_train$meth_class, data_test$meth_class)

  data_train = data_train[data_train$meth_class %in% shared_classes,]
  data_test = data_test[data_test$meth_class %in% shared_classes,]
}


if(original_test_classes){
  data_train = data_train[data_train$meth_class %in% Original_test,]
  data_test = data_test[data_test$meth_class %in% Original_test,]
}

data_train$meth_class = factor(data_train$meth_class)


# Seting folds
# set.seed(1)
# #folds = caret::createFolds(1:nrow(full_data), k=3, returnTrain = TRUE)
# folds = groupdata2::fold(full_data, k = 3, method = "n_fill")$.folds
# folds = list(c(which(folds == 1), which(folds == 3)),
#              c(which(folds == 1), which(folds == 3)),
#              c(which(folds == 2), which(folds == 3)))

# set.seed(1)
# folds = caret::createDataPartition(full_data$meth_class, p = 0.6, times = 3)

folds = balenced_fold(data_train$meth_class, nb_folds)
folds = list("fold1" = c(folds$fold1, folds$fold2),
             "fold2" = c(folds$fold1, folds$fold3),
             "fold3" = c(folds$fold2, folds$fold3))

# Geting models to be tested
svm_opti = res_param_df(readRDS(paste0(base_output_path, "/e1071_svm_param/e1071_svm_param_res.rds")), order = "acc_test")
if(!is.null(acc_cutoff)){
  svm_opti = svm_opti[svm_opti$acc_test >= acc_cutoff,]
} else{
  svm_opti = svm_opti[svm_num_cutoff,]
}

if(file.exists(paste0(output_path, "/", model_name, "_res.rds"))){

  out = readRDS(paste0(output_path, "/", model_name, "_res.rds"))

}else{
  # Final output
  out = list()
  # Set class
  class(out) = "res_cv_svm"
}




for(idx in 1:nrow(svm_opti)){

  # Seting result vectors
  acc_train_out = c()
  acc_test_out = c()
  time_out = c()
  res_out_idx = 1
  results_out = c()

  for(fold in folds){

    cat(paste0("\rTest nb: ", idx, " out of ", nrow(svm_opti), " | fold: ", res_out_idx, " out of 3\n"))


    if(svm_opti[idx,1] == "linear"){
    # Run model
    time = system.time({model = svm(x = data_train[fold,-1],
                                    y = data_train[fold,1],
                                    type = "C-classification",
                                    kernel = svm_opti[idx,1],
                                    cost = as.numeric(svm_opti[idx,2]),
                                    class.weights = table(data_train[fold,1]),
                                    probability = TRUE)})["elapsed"]
    }


    if(svm_opti[idx,1] == "polynomial"){
      # Run model
      time = system.time({model = svm(x = data_train[fold,-1],
                                      y = data_train[fold,1],
                                      type = "C-classification",
                                      kernel = svm_opti[idx,1],
                                      cost = as.numeric(svm_opti[idx,2]),
                                      gamma = as.numeric(svm_opti[idx,3]),
                                      coef0 = as.numeric(svm_opti[idx,4]),
                                      degree = as.numeric(svm_opti[idx,5]),
                                      class.weights = table(data_train[fold,1]),
                                      probability = TRUE)})["elapsed"]
    }

    if(svm_opti[idx,1] == "radial"){
      # Run model
      time = system.time({model = svm(x = data_train[fold,-1],
                                      y = data_train[fold,1],
                                      type = "C-classification",
                                      kernel = svm_opti[idx,1],
                                      cost = as.numeric(svm_opti[idx,2]),
                                      gamma = as.numeric(svm_opti[idx,3]),
                                      class.weights = table(data_train[fold,1]),
                                      probability = TRUE)})["elapsed"]
    }


    #saveRDS(model, paste0(output_path, "/Models/", model_name,"_ker_", k, "_cost", c, ".rds"), compress = TRUE)


    ## prediction
    pred_train = predict(model, newdata = data_train[fold,-1])

    pred_test_lab = predict(model, newdata = data_test[,-1])


    # Putting all results in result vectors
    pred_train = as.character(pred_train) == as.character(data_train[fold,1])

    pred_test = as.character(pred_test_lab) == as.character(data_test$meth_class)




    #print(sum(pred_test)*100/length(pred_test))

    # Place results in vectors
    acc_train_out = c(acc_train_out, sum(pred_train)*100/length(pred_train))
    acc_test_out = c(acc_test_out, sum(pred_test)*100/length(pred_test))
    time_out = c(time_out, time)
    results_out[[as.character(res_out_idx)]] = list("test_labs" = data_test[,1],
                                                    "pred_labs" = factor(pred_test_lab))
    res_out_idx = res_out_idx + 1


  }

  # Put all results in list with kernel as reference
  out[[paste0("kernel_",svm_opti[idx,1],"_cost",svm_opti[idx,2],"_gamma:", svm_opti[idx,3], "_coef0:", svm_opti[idx,4], "_degree:", svm_opti[idx,5])]] = list("param" = c("kernel" = svm_opti[idx,1], "cost" = svm_opti[idx,2], "gamma" = svm_opti[idx,3], "coef0" = svm_opti[idx,4], "degree" = svm_opti[idx,5]),
                                              "acc_train" = acc_train_out,
                                              "acc_test" = acc_test_out,
                                              "time" = time_out,
                                              "results" = results_out)


  saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))

}