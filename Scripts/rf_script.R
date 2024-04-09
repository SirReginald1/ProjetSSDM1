package = "randomForest"

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
# model_name = "randomForest_rf"
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
  train = readRDS(paste(data_path,"/df_newas", i, "_train.rds", sep = ""))
  #train$meth_class = factor(train$meth_class)

  test = readRDS(paste(data_path,"/df_newas", i, "_test.rds", sep = ""))
  test$meth_class = factor(test$meth_class)

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

  # Run through all the N's
  for(n in N){
    cat(paste("\rnewas:", i, " | N:", n,"    "))
    # Selecting sample
    n = min(n, nrow(train))
    set.seed(1)
    sample_train = train[sample(nrow(train),n),]

    # Make factor here for smaller n's all classes
    sample_train$meth_class = factor(sample_train$meth_class)



    # Run model
    res_time[[paste0("n",n)]] = system.time({model = randomForest(formula = meth_class~.,
                                                                  data = sample_train)})

    if(save_models){
      saveRDS(model, paste0(output_path, "/Models/", model_name,"_newas", i, "_n", n, ".rds"), compress = TRUE)
    }

    ## prediction
    pred_train = predict(model, newdata = sample_train)

    pred_test_lab = predict(model, newdata = test)


    # Putting all results in result vectors
    pred_train = as.character(pred_train) == as.character(sample_train$meth_class)

    res_pred_train = c(res_pred_train, sum(pred_train)*100/length(pred_train))


    pred_test = as.character(pred_test_lab) == as.character(test$meth_class)

    res_pred_test = c(res_pred_test, sum(pred_test)*100/length(pred_test))

  }



  # Put all results in list with newas as reference
  out[[paste0("newas",i)]] = list("acc_train" = res_pred_train,
                                  "acc_test" = res_pred_test,
                                  "time" = res_time,
                                  "results" = list("test_labs" = test[["meth_class"]],
                                                   "pred_labs" = factor(pred_test_lab)))



  saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))


  if(!file.exists(paste0(output_path, "/", model_name, "_res.rds"))){
    stop("File not saving properly!")
  }

}
