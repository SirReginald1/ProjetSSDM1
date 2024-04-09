if(!require(limma)){
  if(!require("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
  }
  BiocManager::install("limma")
  library(limma)
}

#####################################################################################

#' If TRUE will build balanced training and testing sets for each newas of at data_output_path.
balenced = TRUE

#' If TRUE will build training and testing sets for each newas for the original data the folder
#' at oldData_output_path. If it does not exist it will be created.
old_datasets = FALSE

# Remove classes with unique instances in the full dataset. Only does this for the balanced datasets.
remove_singles = TRUE

data_input_path = "../Data"

oldData_output_path = "../OldData"

data_output_path = "../Data"

newas_selected = c(10, 20, 30, 50, 100, 200, 300, 500, 1000, 2000, 3000, 5000, 10000)

gse = "train"

y_key = "meth_class"

#######################################################################################


########################################################################################################
##################################### Original datasets ################################################
########################################################################################################

# If TRUE will build datasets with the original data
if(old_datasets){
  ######################## Clculating original best predictors ######################################

  data_train = readRDS(paste0(data_input_path, "/df_preproc_train.rds"))

  markers_start = grep("^cg",colnames(data_train))[1]
  idx_cpg = colnames(data_train)[markers_start:ncol(data_train)]


  # Look to see if "old_limma_fit" file exists otherwise duild it (!!!Very computationaly costly!!!)
  if(sum(grepl("old_limma_fit",list.files("../",recursive=TRUE))) == 0){
    warning("Computing best predictors for original datasets. The folowing will be very computationaly costly!")

    idx_samples = rownames(data_train)
    idx_clinicals = colnames(data_train)[1:(markers_start-1)]

    # Create dummy matrix of meth_classes with intercepts to use for regrresion
    design_matrix <- model.matrix(formula(paste0("~", y_key)), data_train[idx_samples, idx_clinicals])

    # Get cpg data in matrix format with cpg as rows
    cpg_matrix = t(as.matrix(data_train[, colnames(data_train)[markers_start:ncol(data_train)]]))

    # Remove data to free up memory. Loading it back later.
    rm(data_train, idx_samples, idx_clinicals)
    gc()

    if(dim(design_matrix)[1] != dim(cpg_matrix)[2]){
      stop("Dimention problem between dummy matrix and cpg matrix.")
    }

    fit = limma::lmFit(cpg_matrix, design_matrix)
    rm(design_matrix, cpg_matrix)
    gc()

    #' compute moderated t-statistics, moderated F-statistic, and log-odds of differential expression
    #' by empirical Bayes moderation of the standard errors towards a global value
    fit = limma::eBayes(fit)

    if(!dir.exists(oldData_output_path)){
      dir.create(oldData_output_path)
    }

    saveRDS(fit, paste0(oldData_output_path ,"/old_limma_fit.rds"), compress = TRUE)

    # Reload the data
    data_train = readRDS(paste0(data_input_path, "/df_preproc_train.rds"))
  }

  ######################## Building original datasets ######################################

  # Load fit if it hase not just been built
  if(!exists("fit")){
    file_list = list.files("../",recursive=TRUE)
    fit = readRDS(paste0("../", file_list[grepl("old_limma_fit",file_list)]))
    rm(file_list)
  }

  data_test = readRDS(paste0(data_input_path, "/df_preproc_test.rds"))


  if(!dir.exists(oldData_output_path)){
    dir.create(oldData_output_path)
  }

  # Build individual training and testing block fore each newas
  for (newas in rev(newas_selected)) {
    idx_cpg_oi = rownames(fit$p.value)[rank(fit$p.value[,2]) <= newas]

    # Build training sets
    saveRDS(data_train[,c(y_key, intersect(idx_cpg, idx_cpg_oi))],
            paste0(oldData_output_path, "/df_newas", newas, "_", gse, ".rds"), compress = TRUE)

    # Build testing sets
    saveRDS(data_test[,c(y_key, intersect(idx_cpg, idx_cpg_oi))],
            paste0(oldData_output_path, "/df_newas", newas, "_", gse, ".rds"), compress = TRUE)
  }
  rm(fit, markers_start, idx_cpg)
}


########################################################################################################
##################################### Balanced datasets ################################################
########################################################################################################

# If both datasets are to be built with a balanced distribution of classes.
if(balenced){

  if(!exists("data_train")){data_train = readRDS(paste0(data_input_path, "/df_preproc_train.rds"))}

  if(!exists("data_test")){data_test = readRDS(paste0(data_input_path, "/df_preproc_test.rds"))}

  markers_start = grep("^cg",colnames(data_train))[1]

  # Keeping variables that are only present in both the training and testing data
  intersect = intersect(names(data_train)[c(1,markers_start:ncol(data_train))],
                        names(data_test))

  # Merging the data sets
  full_data = rbind(data_test[,intersect],
                    data_train[,intersect])
  rm(data_test, data_train)
  #gc()

  # Remove classes with unique instances in the full dataset
  if(remove_singles){
    sorted_tab = sort(table(full_data$meth_class))
    full_data = full_data[!full_data$meth_class %in% names(sorted_tab[sorted_tab == 1]),]
  }

  # Create partition
  order = caret::createDataPartition(full_data$meth_class, p = .6, list = FALSE)

  if(length(intersect(rownames(full_data[order,1:2]), rownames(full_data[-order,1:2]))) != 0){
    stop("The training and testing datasets share observations!")
  }


  # If balanced fit object does not exist
  fit_not_exists = sum(grepl("balanced_limma_fit",list.files("../",recursive=TRUE))) == 0

  # Temporaraly saving the merged dataset
  if(fit_not_exists){
    saveRDS(full_data[-order,], paste0(data_input_path, "/tempData_test.rds"))
  } else{
    data_test = full_data[-order,]
  }

  data_train = full_data[order,]
  rm(full_data)

  if(fit_not_exists){
    saveRDS(data_train, paste0(data_input_path, "/tempData_train.rds"))
  }

  ######################## Balanced datasets variable fit ################################

  # Look to see if "balanced_limma_fit" file exists otherwise duild it (!!!Very computationaly costly!!!)

  if(fit_not_exists){
    warning("Computing best predictors for balanced datasets. The folowing will be very computationaly costly!")

    markers_start = grep("^cg",colnames(data_train))[1]
    idx_samples = rownames(data_train[order,1:2])
    idx_clinicals = colnames(data_train)[1:(markers_start)] # - 1)]

    # Create dummy matrix of meth_classes with intercepts to use for regression
    design_matrix <- model.matrix(formula(paste0("~", y_key)), data_train[idx_samples, idx_clinicals])

    # Get cpg data in matrix format with cpg as rows
    cpg_matrix = t(as.matrix(data_train[order, colnames(data_train)[markers_start:ncol(data_train)]]))


    # Remove data to free up memory. Loading it back later.
    rm(data_train, idx_samples, idx_clinicals)
    gc()

    if(dim(design_matrix)[1] != dim(cpg_matrix)[2]){
      stop("Dimention problem between dummy matrix and cpg matrix.")
    }

    fit = limma::lmFit(cpg_matrix, design_matrix)
    rm(design_matrix, cpg_matrix)
    gc()

    #' compute moderated t-statistics, moderated F-statistic, and log-odds of differential expression
    #' by empirical Bayes moderation of the standard errors towards a global value
    fit = limma::eBayes(fit)

    if(!dir.exists(data_output_path)){
      dir.create(data_output_path)
    }

    saveRDS(fit, paste0(data_output_path ,"/balanced_limma_fit.rds"), compress = TRUE)
  }

  if(fit_not_exists){
    data_train = readRDS(paste0(data_input_path, "/tempData_test.rds"))
    data_test = readRDS(paste0(data_input_path, "/tempData_test.rds"))
    }

  # Load fit if it has not just been built
  if(!exists("fit")){
    file_list = list.files("../",recursive=TRUE)
    fit = readRDS(paste0("../", file_list[grepl("balanced_limma_fit",file_list)]))
    rm(file_list)
  }

  idx_cpg = colnames(data_train)[markers_start:ncol(data_train)]

  # Build individual training and testing block fore each newas
  for(newas in rev(newas_selected)) {
    idx_cpg_oi = rownames(fit$p.value)[rank(fit$p.value[,2]) <= newas]

    # Build training sets
    saveRDS(data_train[,c(y_key, intersect(idx_cpg, idx_cpg_oi))],
            paste0(data_output_path, "/df_newas", newas, "_", gse, ".rds"), compress = TRUE)

    # Build testing sets
    saveRDS(data_test[,c(y_key, intersect(idx_cpg, idx_cpg_oi))],
            paste0(data_output_path, "/df_newas", newas, "_test.rds"), compress = TRUE)
  }

}

gc()
