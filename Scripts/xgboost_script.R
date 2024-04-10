package = "xgboost"

if(!require(package, character.only = TRUE)){
  install.packages(package)
  library(package)
}

####################### Uncoment if want to run script on it's own ####################

# source("functions.R")
#
# base_output_path = "../Output"
#
# data_path = "../Data"
#
# model_name = "xgboost_xgboost"
#
# save_models = TRUE
#
# # Set number of variable list (used to call )
# newas = c(10, 20, 30, 50, 100, 200, 300, 500, 1000, 2000, 3000, 5000, 10000)
#
# # Set the number of n to test
# N = c(10, 20, 30, 50, 100, 200, 300, 500, 1000, 1600)


####################################################################################

# Check that the directory for the models exist or creat it
if(!dir.exists(base_output_path)) {
  dir.create(base_output_path)
}

# Creat output path
output_path = paste0(base_output_path, "/", model_name)

if(!dir.exists(output_path)){
  dir.create(output_path)
}

if(save_models & !dir.exists(paste0(output_path,"/Models"))){
  dir.create(paste0(output_path,"/Models"))
}

# Final output
out = list()
# The N sequence to use as reference
out[["n"]] = N


# Run through list of newas
for(i in newas){

  # Load training set
  train = mreadRDS(paste(data_path,"/df_newas", i, "_train.rds", sep = ""))
  train[["meth_class"]] = factor(train[["meth_class"]])

  test = mreadRDS(paste(data_path,"/df_newas", i, "_test.rds", sep = ""))
  test[["meth_class"]] = factor(test[["meth_class"]])

  # Remove classes that are not present in both datasets
  if(length(unique(train$meth_class)) != length(unique(test$meth_class))){
    shared_classes = intersect(train$meth_class, test$meth_class)

    train = train[train$meth_class %in% shared_classes,]
    test = test[test$meth_class %in% shared_classes,]
  }


  if(original_test_classes){
    train = train[train$meth_class %in% Original_test,]
    test = test[test$meth_class %in% Original_test,]
  }



  #rownames(out) = N
  res_pred_train = c()
  res_pred_test = c()
  res_time = c()
  #CV_acc = list() # Only if cross validating

  # Run through all the N's
  for(n in N){
    cat(paste("\r", model_name, "  newas:", i, " | N:", n,"    "))

    # Selecting sample
    n = min(n, nrow(train))
    set.seed(1)
    sample_train = train[sample(nrow(train),n),]



    data_train = xgb.DMatrix(as.matrix(sample_train[,-1]), label = as.integer(factor(sample_train[,1]))-1)
    data_test = xgb.DMatrix(as.matrix(test[,-1]), label = as.integer(factor(test[,1]))-1)


    model_filename = paste0(output_path, "/Models/", model_name,"_newas", i, "_n", n, ".rds")
    if (!file.exists(model_filename)) {
      # Run model
      res_time[[paste0("n",n)]] = system.time({model = xgboost::xgboost(data = data_train,
                                                                        # max number of boosting iterations (default 6)
                                                                        nrounds = 30,
                                                                        #watchlist = list(val1=test.mat, val2= test.mat),
                                                                        params = list(booster = "gbtree",
                                                                                      # learning rate
                                                                                      eta = 0.15,#0.3,
                                                                                      # minimum loss reduction required to make a further partition
                                                                                      gamma = 0, # none
                                                                                      max_depth = 13, #6,
                                                                                      # Proportion of datapoints to use for training
                                                                                      subsample = 1,
                                                                                      # L2 regularization
                                                                                      lambda = 3, #1,
                                                                                      # L1 regularization
                                                                                      alpha = 0, #0,
                                                                                      # The loss function
                                                                                      objective = "multi:softprob",
                                                                                      min_child_weight = 0.1, # none
                                                                                      colsample_bytree = 0.1,#, # none
                                                                                      num_class = length(unique(train[,1]))#87
                                                                        ))})

      model$res_time = res_time[[paste0("n",n)]]      
      if(save_models){
        saveRDS(model, model_filename, compress = TRUE)
      }
    } else {
      model = mreadRDS(model_filename)
      res_time[[paste0("n",n)]] = model$res_time
    }

    ## xgboost prediction
    pred_train = predict(model,data_train,reshape=T)
    #print("pred-train")
    pred_train = as.data.frame(pred_train)
    #print("1")
    colnames(pred_train) = levels(factor(sample_train[,1]))
    #print("2")
    pred_train = apply(pred_train, 1, function(x){names(which.max(x))})

    #stop("On purpus")

    #print("pred-test")
    pred_test = predict(model,data_test,reshape=T)
    #print("1")
    pred_test = as.data.frame(pred_test)
    #print("2")
    colnames(pred_test) = levels(factor(test[,1]))
    #print("3")
    pred_test_lab = apply(pred_test, 1, function(x){names(which.max(x))})
    #print("4")



    # Putting all results in result vectors
    pred_train = as.character(pred_train) == as.character(sample_train$meth_class)
    #print("4")
    res_pred_train = c(res_pred_train, sum(pred_train, na.rm = TRUE)*100/length(pred_train))


    pred_test = as.character(pred_test_lab) == as.character(test$meth_class)

    res_pred_test = c(res_pred_test, sum(pred_test, na.rm = TRUE)*100/length(pred_test))

  }



  # Put all results in list with newas as reference
  out[[paste0("newas",i)]] = list("acc_train" = res_pred_train,
                                  "acc_test" = res_pred_test,
                                  "time" = res_time,
                                  "results" = list("test_labs" = test[["meth_class"]],
                                                   "pred_labs" = factor(pred_test_lab)))



  saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))

}