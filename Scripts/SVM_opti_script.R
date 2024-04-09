package = "e1071"

if(!require(package, character.only = TRUE)){
  install.packages(package)
  library(package)
}

# output_path = "../Output"
#
# data_path = "../Data"
#
# model_name = "e1071_svm_param"



# Seting parameter lists
# kernel = c("linear", "polynomial", "radial")
#
# gamma =  1/10000
#
# coef0 = c(0,10,30,100,200)
#
# degree = c(1,2,3,4,5,6)
#
# cost = c(1,1000)




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

print(paste0("Estimated max time: ", ((length(svm_gamma) * length(svm_coef0) *length(svm_degree) * length(svm_cost)) +
                                  (length(svm_cost)) +
                                  (length(svm_gamma) * length(svm_cost)) +
                                  (length(svm_gamma) * length(svm_coef0))) * 1.5, " min"))


# If result file already exists load it
if(file.exists(paste0(output_path, "/", model_name, "_res.rds"))){
  out = readRDS(paste0(output_path, "/", model_name, "_res.rds"))

  # Remember to set the right parameter vectors
} else {
  # Final output
  out = list()

  # Plot references

  # out[["cost"]] = cost
  #
  # out[["gamma"]] = gamma
  #
  # out[["coef0"]] = coef0
  #
  # out[["degree"]] = degree

  # Set class
  class(out) = "res_param_svm"
}





# Run through list of newas
for(k in svm_kernel){


  #### linear ####

  if(k == "linear"){

    for(c in svm_cost){

      cat(paste("\rKernel:", k, " | Cost:", c,"               "))


      # Run model
      time = system.time({model = svm(formula = meth_class~.,
                                      data = train,
                                      type = "C-classification",
                                      kernel = k,
                                      cost = c,
                                      class.weights = table(train[,1]),
                                      probability = TRUE)})["elapsed"]

      #saveRDS(model, paste0(output_path, "/Models/", model_name,"_ker_", k, "_cost", c, ".rds"), compress = TRUE)


      ## prediction
      pred_train = predict(model, newdata = train)

      pred_test_lab = predict(model, newdata = test)


      # Putting all results in result vectors
      pred_train = as.character(pred_train) == as.character(train$meth_class)

      pred_test = as.character(pred_test_lab) == as.character(test$meth_class)

      # Put all results in list with kernel as reference
      out[[paste0("kernel_",k,"_cost",c)]] = list("param" = c("kernel" = k, "cost" = c, "gamma" = NA, "coef0" = NA, "degree" = NA),
                                                  "acc_train" = sum(pred_train)*100/length(pred_train),
                                                  "acc_test" = sum(pred_test)*100/length(pred_test),
                                                  "time" = time,
                                                  "results" = list("test_labs" = test[["meth_class"]],
                                                                   "pred_labs" = factor(pred_test_lab)))


    }

    saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"), compress = TRUE)
    print("#######################File saved!#########################")
  }


  #### polynomial ####

  if(k == "polynomial"){

    for(c in svm_cost){

      for(d in svm_degree){

        for(g in svm_gamma){

          for(co in svm_coef0){

            cat(paste("\rKernel:", k, " | Cost:", c, " | degree:", d, " | gamma:", g, " | coef0:", co,"               "))


            # Run model
            time = system.time({model = svm(formula = meth_class~.,
                                            data = train,
                                            type = "C-classification",
                                            kernel = k,
                                            cost = c,
                                            class.weights = table(train[,1]),
                                            gamma = g,
                                            coef0 = co,
                                            probability = TRUE)})["elapsed"]

            #saveRDS(model, paste0(output_path, "/Models/", model_name,"_ker_", k, "_n", n, ".rds"), compress = TRUE)


            ## prediction
            pred_train = predict(model, newdata = train)

            pred_test_lab = predict(model, newdata = test)


            # Putting all results in result vectors
            pred_train = as.character(pred_train) == as.character(train$meth_class)

            pred_test = as.character(pred_test_lab) == as.character(test$meth_class)

            # Put all results in list with kernel as reference
            out[[paste0("kernel_", k, "_Cost:", c, "_deg:", d, "_gam:", g, "_coef:", co)]] = list("param" = c("kernel" = k, "cost" = c, "gamma" = g, "coef0" = co, "degree" = d),
                                                                                                  "acc_train" = sum(pred_train)*100/length(pred_train),
                                                                                                  "acc_test" = sum(pred_test)*100/length(pred_test),
                                                                                                  "time" = time,
                                                                                                  "results" = list("test_labs" = test[["meth_class"]],
                                                                                                                   "pred_labs" = factor(pred_test_lab)))

          }
          saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"), compress = TRUE)
          print("#######################File saved!#########################")
        }
      }
    }





  }

  #### radial basis ####

  if(k == "radial"){

    for(c in svm_cost){

      for(g in svm_gamma){

        cat(paste("\rKernel:", k, " | Cost:", c, " | gamma:", g, "               "))


        # Run model
        time = system.time({model = svm(formula = meth_class~.,
                                        data = train,
                                        type = "C-classification",
                                        kernel = k,
                                        cost = c,
                                        class.weights = table(train[,1]),
                                        gamma = g,
                                        probability = TRUE)})["elapsed"]

        #saveRDS(model, paste0(output_path, "/Models/", model_name,"_ker_", k, "_n", n, ".rds"), compress = TRUE)


        ## prediction
        pred_train = predict(model, newdata = train)

        pred_test_lab = predict(model, newdata = test)


        # Putting all results in result vectors
        pred_train = as.character(pred_train) == as.character(train$meth_class)

        pred_test = as.character(pred_test_lab) == as.character(test$meth_class)

        # Put all results in list with kernel as reference
        out[[paste0("kernel_", k, "_cost:", c, "_gam:", g)]] = list("param" = c("kernel" = k, "cost" = c, "gamma" = g, "coef0" = NA, "degree" = NA),
                                                      "acc_train" = sum(pred_train)*100/length(pred_train),
                                                      "acc_test" = sum(pred_test)*100/length(pred_test),
                                                      "time" = time,
                                                      "results" = list("test_labs" = test[["meth_class"]],
                                                                       "pred_labs" = factor(pred_test_lab)))



      }
    }

    saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"), compress = TRUE)
    print("#######################File saved!#########################")

  }



  #### sigmoid ####

  if(k == "sigmoid"){

    for(g in svm_gamma){

      for(co in svm_coef0){


          cat(paste("\rKernel:", k, " | degree:", d, " | coef0:", co,"               "))


          # Run model
          time = system.time({model = svm(formula = meth_class~.,
                                          data = train,
                                          type = "C-classification",
                                          kernel = k,
                                          cost = c,
                                          gamma = g,
                                          class.weights = table(train[,1]),
                                          coef0 = co,
                                          probability = TRUE)})["elapsed"]

          #saveRDS(model, paste0(output_path, "/Models/", model_name,"_ker_", k, "_n", n, ".rds"), compress = TRUE)


          ## prediction
          pred_train = predict(model, newdata = train)

          pred_test_lab = predict(model, newdata = test)


          # Putting all results in result vectors
          pred_train = as.character(pred_train) == as.character(train$meth_class)

          pred_test = as.character(pred_test_lab) == as.character(test$meth_class)

          # Put all results in list with kernel as reference
          out[[paste0("kernel_", k, "_cost:", c, "_gam:", g, "_coef:", co)]] = list("param" = c("kernel" = k, "cost" = c, "gamma" = g, "coef0" = co, "degree" = NA),
                                                                                    "acc_train" = sum(pred_train)*100/length(pred_train),
                                                                                    "acc_test" = sum(pred_test)*100/length(pred_test),
                                                                                    "time" = time,
                                                                                    "results" = list("test_labs" = test[["meth_class"]],
                                                                                                     "pred_labs" = factor(pred_test_lab)))



      }
    }


    saveRDS(out,paste0(output_path, "/", model_name, "_res.rds"), compress = TRUE)


  }


}