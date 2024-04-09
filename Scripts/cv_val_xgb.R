package = "xgboost"

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
# model_name = "xgboost_CV"

# # The testing accuracy cutoff for model selection
# acc_cutoff = 89

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
# full_data = rbind(readRDS(paste(data_path,"/df_newas10000_train.rds", sep = "")),
#                   readRDS(paste(data_path,"/df_newas10000_test.rds", sep = "")))
# full_data$meth_class = factor(full_data$meth_class)

data_train_in = readRDS(paste(data_path,"/df_newas10000_train.rds", sep = ""))
data_test_in = readRDS(paste(data_path,"/df_newas10000_test.rds", sep = ""))




# Remove classes that are not present in both datasets
if(length(unique(data_train_in$meth_class)) != length(unique(data_test_in$meth_class))){
  shared_classes = intersect(data_train_in$meth_class, data_test_in$meth_class)

  data_train_in = data_train_in[data_train_in$meth_class %in% shared_classes,]
  data_test_in = data_test_in[data_test_in$meth_class %in% shared_classes,]
}


if(original_test_classes){
  data_train_in = data_train_in[data_train_in$meth_class %in% Original_test,]
  data_test_in = data_test_in[data_test_in$meth_class %in% Original_test,]
}

# Seting folds
set.seed(1)
#folds = caret::createFolds(1:nrow(full_data), k=3, returnTrain = TRUE)
# folds = groupdata2::fold(full_data, k = 3, method = "n_fill")[,".folds"]
# folds = list(c(which(folds == 1), which(folds == 2)),
#              c(which(folds == 1), which(folds == 3)),
#              c(which(folds == 2), which(folds == 3)))

folds = balenced_fold(data_train_in$meth_class, nb_folds)
folds = list("fold1" = c(folds$fold1, folds$fold2),
             "fold2" = c(folds$fold1, folds$fold3),
             "fold3" = c(folds$fold2, folds$fold3))




# Geting models to be tested
xgb_opti = res_param_df(readRDS(paste0(base_output_path, "/xgboost_xgboost_param/xgboost_xgboost_param_res.rds")), order = "acc_test")
if(!is.null(acc_cutoff)){
  xgb_opti = xgb_opti[xgb_opti$acc_test >= acc_cutoff,]
} else {
  xgb_opti = xgb_opti[xgb_num_cutoff,]
}

# If result file already exists load it
if(file.exists(paste0(output_path, "/", model_name, "_res.rds"))){

  out = readRDS(paste0(output_path, "/", model_name, "_res.rds"))

}else{
  # Final output
  out = list()
  # Set class
  class(out) = "res_cv_xgboost"
}



for(idx in xgb_num_cutoff){

  # Seting result vectors
  acc_train_out = c()
  acc_test_out = c()
  time_out = c()
  res_out_idx = 1
  results_out = c()
  mlogloss_out = c()

  for(fold in folds){

    cat(paste0("\rTest nb: ", idx, " out of ", nrow(xgb_opti), " | fold: ", res_out_idx, " out of 3\n"))

    # Set training data
    data_train = xgb.DMatrix(as.matrix(data_train_in[fold,-1]), label = as.integer(factor(data_train_in[fold,1]))-1)

    # Set testing data
    data_test = xgb.DMatrix(as.matrix(data_test_in[,-1]), label = as.integer(factor(data_test_in[,1]))-1)

    set.seed(1)
    # Run model
    time = system.time({model = xgboost::xgboost(data = data_train,
                                                 # max number of boosting iterations (default 6)
                                                 nrounds = 30,
                                                 #watchlist = list(val1=test.mat, val2= test.mat),
                                                 params = list(booster = "gbtree",
                                                               # learning rate
                                                               eta = xgb_opti[idx,1],
                                                               max_depth = xgb_opti[idx,7],
                                                               # L2 regularization
                                                               lambda = xgb_opti[idx,3],
                                                               # L1 regularization
                                                               alpha = xgb_opti[idx,4],
                                                               # minimum loss reduction required to make a further partition
                                                               gamma = xgb_opti[idx,2],
                                                               min_child_weight = xgb_opti[idx,5],
                                                               colsample_bytree = xgb_opti[idx,6],
                                                               # Proportion of datapoints to use for training
                                                               subsample = 1,
                                                               # The loss function
                                                               objective = "multi:softprob",
                                                               #eval_metric = "mlogloss",
                                                               num_class = length(unique(data_train_in$meth_class))))})["elapsed"]

  print("model end")
    #saveRDS(model, paste0(output_path, "/Models/", model_name,"_newas", i, "_n", n, ".rds"), compress = TRUE)


    ## xgboost prediction
    ### Train
    pred_train = predict(model,data_train,reshape=T)
    pred_train = as.data.frame(pred_train)
    colnames(pred_train) = levels(factor(data_train_in[fold,1]))
    pred_train = apply(pred_train, 1, function(x){names(which.max(x))})

    pred_train = as.character(pred_train) == as.character(data_train_in[fold,1])

    ### Test
    pred_test = predict(model,data_test,reshape=T)
    pred_test = as.data.frame(pred_test)
    colnames(pred_test) = levels(factor(data_test_in[,1]))
    pred_test_lab = apply(pred_test, 1, function(x){names(which.max(x))})

    pred_test = as.character(pred_test_lab) == as.character(data_test_in[,1])


    #print(sum(pred_test)*100/length(pred_test))

    # Place results in vectors
    acc_train_out = c(acc_train_out, sum(pred_train)*100/length(pred_train))
    acc_test_out = c(acc_test_out, sum(pred_test, na.rm = TRUE)*100/length(pred_test))
    time_out = c(time_out, time)
    results_out[[as.character(res_out_idx)]] = list("test_labs" = data_test_in[,1],
                                                    "pred_labs" = factor(pred_test_lab))
    res_out_idx = res_out_idx + 1
    mlogloss_out = c(mlogloss_out, model$evaluation_log$train_mlogloss)


  }

  # Put all results in list with kernel as reference
  out[[paste0("lr:", xgb_opti[idx,1],
              "_gamma:", xgb_opti[idx,2],
              "_lambda:", xgb_opti[idx,3],
              "_alpha:", xgb_opti[idx,4],
              "_minchild:", xgb_opti[idx,5],
              "_coltree:", xgb_opti[idx,6],
              "_maxdepth:", xgb_opti[idx,7])]] = list("param" = xgb_opti[idx,1:7],
                                                      "acc_train" = acc_train_out,
                                                      "acc_test" = acc_test_out,
                                                      "time" = time_out,
                                                      "results" = results_out,
                                                      "mlogloss" = mlogloss_out)


# saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))
# if(idx %% 2 == 0){stop("Stop!")}


  if(idx %% 10 == 0){
    saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))
  }

}

saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))
