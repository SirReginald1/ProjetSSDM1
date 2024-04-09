package = "xgboost"

if(!require(package, character.only = TRUE)){
  install.packages(package)
  library(package)
}

# base_output_path = "../Output"
#
# data_path = "../Data"
#
# model_name = "xgboost_xgboost_param"

# lr = c(0.15)
#
# gamma = c(0,2,3)
#
# lambda = c(2,3)
#
# alpha = c(0)
#
# minchild =c(0.1)
#
# coltree = c(0.1)
#
# max_depth = c(6,7,8,9,10,11,12,13,14)



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



# Load training set
train = readRDS(paste(data_path,"/df_newas10000_train.rds", sep = ""))

test = readRDS(paste(data_path,"/df_newas10000_test.rds", sep = ""))
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

train$meth_class = factor(train$meth_class)

data_train = xgb.DMatrix(as.matrix(train[,-1]), label = as.integer(factor(train[,1]))-1)

data_test = xgb.DMatrix(as.matrix(test[,-1]), label = as.integer(factor(test[,1]))-1)


# Setting testing variables

# lr = c(0.1,0.15,0.2,0.25)
#
# gamma = c(0,0.5,1,1.5)
#
# lambda = c(0,0.5,1,1.5)
#
# alpha = c(0,0.5,1,1.5)
#
# minchild =c(0.1,0.5,1,1.5)
#
# coltree = c(0.1,0.3,0.5)
#
# max_depth = c(3,6,10,30)






# If result file already exists load it
if(file.exists(paste0(output_path, "/", model_name, "_res.rds"))){
  out = readRDS(paste0(output_path, "/", model_name, "_res.rds"))


  # Update references
  # out[["lr"]] = sort(c(out[["lr"]], lr[! lr %in% out[["lr"]]]))
  #
  # out[["gamma"]] = sort(c(out[["gamma"]], gamma[! gamma %in% out[["gamma"]]]))
  #
  # out[["lambda"]] = sort(c(out[["lambda"]], lambda[! lambda %in% out[["lambda"]]]))
  #
  # out[["alpha"]] = sort(c(out[["alpha"]], alpha[! alpha %in% out[["alpha"]]]))
  #
  # out[["minchild"]] = sort(c(out[["minchild"]], minchild[! minchild %in% out[["minchild"]]]))
  #
  # out[["coltree"]] = sort(c(out[["coltree"]], coltree[! coltree %in% out[["coltree"]]]))
  #
  # out[["maxdepth"]] = sort(c(out[["maxdepth"]], max_depth[! max_depth %in% out[["maxdepth"]]]))



  # Remember to set the right parameter vectors
} else {
  # Final output
  out = list()


  # Plot references

  # out[["lr"]] = lr
  #
  # out[["gamma"]] = gamma
  #
  # out[["lambda"]] = lambda
  #
  # out[["alpha"]] = alpha
  #
  # out[["minchild"]] = minchild
  #
  # out[["coltree"]] = coltree
  #
  # out[["maxdepth"]] = max_depth

  # Set class
  class(out) = "res_param_xgboost"
}



for(l in xgb_lr){

  for(g in xgb_gamma){

    for(la in xgb_lambda){

      for(alp in xgb_alpha){

        for(min in xgb_minchild){

          for(col in xgb_coltree){

            for(md in xgb_max_depth){

              print(paste0("\rlr:", l,"_gamma:", g,"_lambda:", la,"_alpha:", alp,"_minchild:", min,"_coltree:", col,"_maxdepth:", md, "                 "))

    # Run model
    time = system.time({model = xgboost::xgboost(data = data_train,
                                                # max number of boosting iterations (default 6)
                                                nrounds = 5,
                                                #watchlist = list(val1=test.mat, val2= test.mat),
                                                params = list(booster = "gbtree",
                                                              # learning rate
                                                              eta = l,
                                                              max_depth = md,
                                                              # L2 regularization
                                                              lambda = la,
                                                              # L1 regularization
                                                              alpha = alp,
                                                              # minimum loss reduction required to make a further partition
                                                              gamma = g,
                                                              min_child_weight = min,
                                                              colsample_bytree = col,
                                                              # Proportion of datapoints to use for training
                                                              subsample = 1,
                                                              # The loss function
                                                              objective = "multi:softprob",
                                                              #eval_metric = "mlogloss",
                                                              num_class = length(unique(train[,1]))))})["elapsed"]


    #saveRDS(model, paste0(output_path, "/Models/", model_name,"_newas", i, "_n", n, ".rds"), compress = TRUE)


    ## xgboost prediction
    pred_train = predict(model,data_train,reshape=T)

    pred_train = as.data.frame(pred_train)

    colnames(pred_train) = levels(factor(train[,1]))

    pred_train = apply(pred_train, 1, function(x){names(which.max(x))})

    pred_train = as.character(pred_train) == as.character(train$meth_class)


    pred_test = predict(model,data_test,reshape=T)

    pred_test = as.data.frame(pred_test)

    colnames(pred_test) = levels(factor(test[,1]))

    pred_test_lab = apply(pred_test, 1, function(x){names(which.max(x))})



    pred_test = as.character(pred_test_lab) == as.character(test$meth_class)







  # Put all results in list with kernel as reference
  out[[paste0("lr:", l,
              "_gamma:", g,
              "_lambda:", la,
              "_alpha:", alp,
              "_minchild:", min,
              "_coltree:", col,
              "_maxdepth:", md)]] = list("param" = c("lr" = l, "gamma" = g, "lambda" = la, "alpha" = alp, "minchild" = min, "coltree" = col, "maxdepth" = md),
                                         "acc_train" = sum(pred_train)*100/length(pred_train),
                                         "acc_test" = sum(pred_test)*100/length(pred_test),
                                         "time" = time,
                                         "results" = list("test_labs" = test[["meth_class"]],
                                                          "pred_labs" = factor(pred_test_lab)),
                                         "mlogloss" = model$evaluation_log$train_mlogloss)



            }
          }

          saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"))
          print("#######################File saved!#########################")
        }
      }
    }
  }
}