if(!require("ggplot2", character.only = TRUE)){
  install.packages("ggplot2")
}

sorted_order_vector = c("MB, G4", "O IDH", "GBM, RTK II", "A IDH", "EPN, PF A", "MB, G3", "A IDH, HG", "MNG", "DMG, K27", "GBM, MES", "LGG, DNT", "GBM, RTK I", "PXA", "PLEX, PED B", "EPN, RELA", "MB, SHH INF", "PITUI", "HMB", "SCHW", "MB, SHH CHL AD", "LGG, PA/GG ST", "EPN, MPE", "ANA PA", "LGG, PA PF", "ETMR", "CNS NB, FOXR2", "CN", "PITAD, FSH LH", "LGG, MYB", "SUBEPN, PF", "PLEX, AD", "LGG, SEGA", "GBM, G34", "EPN, SPINE", "EPN, PF B", "ATRT, SHH", "PGG, nC", "MB, WNT", "PTPR, B", "PITAD, ACTH", "PIN T, PPT", "ENB, A", "LGG, GG", "PITAD, STH SPA", "CPH, ADM", "CPH, PAP", "SFT HMPC", "MELCYT", "ATRT, TYR", "ATRT, MYC", "LGG, PA MID", "HGNET, MN1", "PLEX, PED A", "PITAD, STH DNS B", "GBM, MID", "ENB, B", "CONTR, REACT", "MELAN", "LYMPHO", "CONTR, PINEAL", "CHGL", "SUBEPN, ST", "PITAD, TSH", "LIPN", "GBM, RTK III", "CONTR, PONS", "PIN T, PB B", "HGNET, BCOR", "CHORDM", "GBM, MYCN", "CONTR, WM", "CONTR, HYPTHAL", "CONTR, ADENOPIT", "PTPR, A", "PLASMA", "PITAD, STH DNS A", "PITAD, PRL", "LGG, RGNT", "EWS", "DLGNT", "SUBEPN, SPINE", "SCHW, MEL", "LGG, DIG/DIA", "IHG", "EFT, CIC", "EPN, YAP", "PIN T, PB A")

#' Takes object outputed by test script and transforms it to data frame.
#'
#' @author Andrew Hardwick
#' @param res_obj Object outputed by the test script.
#' @param include_n Boolean indicating if the n's should be included as a column.
#' @return A list of lists containing all the testing measurements.
#' @export
res_to_df = function(res_obj, include_n = TRUE){
  # Define matrix of appropriate dimentions
  df_out = matrix(ncol = length(names(res_obj[["newas10"]])) * (length(names(res_obj)) - 1), nrow = length(res_obj[["n"]]))

  # Output columne df index
  col_idx = 1

  # Vector of column names
  col_names = c()

  # Run through the number of variables list names
  for(newas in attributes(res_obj)[["newas10"]][-c(1,2)]){
    if(substr(newas,1,5) != "newas"){break}

    # Run through the list of metric names
    for(c in names(res_obj[["newas10"]])){
      # Place each newas vector in df_out column
      df_out[,col_idx] = unlist(out[[newas]][c])
      col_idx = col_idx + 1

      # Create the column name
      col_names = c(col_names, paste(newas, c, sep = "_"))
    }
  }
  # Format and name output df
  df_out = as.data.frame(df_out)
  rownames(df_out) = res_obj[["n"]]
  colnames(df_out) = col_names

  if(include_n){
    df_out$n = res_obj[["n"]]
  }

  return(df_out)
}

#' Takes object outputed by test script and transforms it to data frame. Intended to be used for ggplot.
#'
#' @author Andrew Hardwick
#' @param res_obj Object outputed by the test script.
#' @return Sum of all the model training time
#' @export
sum_time = function(res_obj, time_type = "elapsed"){
  out = 0
  for(i in names(res_obj)[grep("newas",names(res_obj))]){
    #if(substr(i,1,5) != "newas"){break}
    out = out + sum(unpack_time(res_obj, i, time_type))
  }
  return(out)
}

#' Returns a result object whith only the specified time type.
#'
#' @author Andrew Hardwick
#' @param res_obj The object resulting from model test script.
#' @param time_type The time type to be kept
#' @export
format_time = function(res_obj, time_type = "elapsed"){
  for(i in grep("newas",names(res_obj))){
    res_obj[[i]][["time"]] = as.vector(unpack_time(res_obj, i, time_type))
  }
  return(res_obj)
}


#' Extracts the specified \code{time_type} from a given newas element in a Result object.
#'
#' @author Andrew Hardwick
#' @param res_obj The object resulting from model test script.
#' @param i Names of the newas element to extract the time type from.
#' @param time_type Which time_type to extract. Accepted values: \code{{"user", "system", "elapsed"}}.
#' @export
unpack_time = function(res_obj, i, time_type){
  if(!time_type %in% c("user", "system", "elapsed")){
    stop("Non valid time_type!")
  }
  if("system" == time_type){time_type = "sys.self"}
  else if(time_type == "user"){time_type = "user.self"}
  # Check that time element only hase 1 vector if yes return that vector
  # if(length(names(res_obj[[i]][["time"]])) <=1){
  #   return(res_obj[[i]][["time"]])
  # }
  return(sapply(res_obj[[i]][["time"]],
                function(x){if(length(intersect(c("user.self","sys.self", "elapsed", "user.child", "sys.child"), names(x))) == 0){return(x)}
                            else{return(as.vector(x[[time_type]]))}
                            }))
}

#' Renames all files in given directory. Changes the \code{from} patern present in fil names, to the \code{to} patern.
#' Usses sapply to parralle prosece.
#'
#' @author Andrew Hardwick
#' @param directory The path to the directory in which to change file names.
#' @param from The patern to be replaced in the file names.
#' @param to The patern to replace with.
#' @param return_output Boolean indicating if the function should return the boolean vector indicating if each file
#' name hase been changed.
#' @export
rename_files = function(directory, from, to, return_output = FALSE){
  out = sapply(list.files(directory),function(x){file.rename(paste0(directory, "/", x),
                                                             paste0(directory, "/", gsub(from, to, x)))})
  if(return_output){return()}

  return(out)
}




#' Converts a res object to data frame to be used ggplots.
#'
#' @author Andrew Hardwick
#' @param res_obj The object resulting from model test script.
#' @param type Which element to plot. Accepted values: \code{{"acc_train", "acc_test", "time"}}.
#' @export
res_to_plot_df = function(res_obj, type, time_type = NULL){

  # Check that result object has at least 1 element to plot.
  if(length(res_obj) <= 1){
    stop("res_obj too short to plot!")
  }

  #Check that type is valid
  if(!type %in% c("acc_train", "acc_test", "time")){
    stop("Invalide type!")
  }

  if(type == "time" & is.null(time_type)){
    stop("If time is selected, time_type must be specified!")
  }

  # Initialize data frame with first "dataset". *
  # df_out[,1] = The type value to be ploted.
  # df_out[,2] = The number of observations n.
  # df_out[,3] = Factor label used to identify the number of variables.

  if(type != "time"){
    df_out = as.data.frame(cbind(unlist(res_obj[["newas10"]][type]), res_obj$n, rep(10,length(res_obj[["newas10"]][type]))))

    # Append new rows for each res_obj elements except for the first 2
    for(i in names(res_obj)[grep("newas",names(res_obj))][-1]){
      #if(substr(i,1,5) != "newas"){break}
      df_out = rbind(df_out,
                     cbind(unlist(res_obj[[i]][type]), res_obj$n, rep(as.numeric(substr(i, 6, nchar(i))),length(res_obj[[i]][type])))
      )
    }

  }

  else{
    temp = unpack_time(res_obj, "newas10", time_type)
    if("matrix" %in% class(temp)){
      temp = t(temp)[,time_type]
    }
    df_out = as.data.frame(cbind(temp, res_obj$n, rep(10,length(temp))))

    # Append new rows for each res_obj elements except for the first 2
    for(i in names(res_obj)[grep("newas",names(res_obj))][-1]){
      #if(substr(i,1,5) != "newas"){break}
      temp = unpack_time(res_obj, i, time_type)
      if("matrix" %in% class(temp)){
        temp = t(temp)[,time_type]
      }
      df_out = rbind(df_out,
                     cbind(temp, res_obj$n, rep(as.numeric(substr(i, 6, nchar(i))),length(temp)))
      )
    }

  }





  # Set col names
  names(df_out) = c(type, "n", "newas")

  # Format col type
  df_out[,1] = as.numeric(df_out[,1])
  df_out[,2] = as.numeric(df_out[,2])
  df_out[,3] = factor(as.numeric(df_out[,3]), ordered = TRUE)

  # Revers the factors to reverse the legend values
  df_out[,3] = factor(df_out[,3], levels = rev(levels(df_out[,3])))

  return(df_out)
}

#' Plots the specified measure of the given Result object.
#'
#' @author Andrew Hardwick
#' @param res_obj The object resulting from model test script.
#' @param type Which element to plot. Accepted values: \code{{"acc_train", "acc_test", "time", "CV_acc}}.
#' @param Time_type The time type to be ploted
#' @param use_log Transform the y axis with log
#' @param colour The colour palette to be used for ploting.
#' @param plot_FUN A ggplot geom_ function used to plot the graph. Default \code{geom_line}.
#' @param use_log Boolean indicating if the y axis should be loged. !!!! DO NOT USE !!!
#' @export
plot_res_np = function(res_obj, type, time_type = "elapsed", colour = NULL, plot_FUN = geom_line, use_log = FALSE){
  # Creating the apropriate data frame
  data = res_to_plot_df(res_obj, type, time_type)

  if(use_log){
    stop("Do not use use_log!")
    data[[type]] = log(data[[type]])
  }

  # Selecting appropriate plot labes
  switch(type,
         "acc_train" = {title = "Training set accuracy"
                        ylab = "Accuracy"},
         "acc_test" = {title = "Testing set accuracy"
                       ylab = "Accuracy"},
         "time" = {title = paste0("Time to compute (", time_type,")")
                   ylab = "Time in seconds"},
         "CV_acc" = {title = "Mean cross validation accuracy score"
                     ylab = "Accuracy"}
        )

  # Set the default colour palette of (length(res_obj)-1)
  if(is.null(colour)){
    colour = colorRampPalette(c("#1b98e0","red"))(length(grep("newas",names(res_obj))))
  }

  # Revers the colour palette so that the warmer colours are at the top
  colour = rev(colour)

  # Plot the graph
  out = ggplot(data, aes(x = n, y = !!sym(type), color = newas)) +
    plot_FUN() +
    scale_color_manual(values = colour, name = "Nb var") +
    labs(title = title, x = "Number of observations", y = ylab) +
    guides(fill=guide_legend(title="Nb var"))


  # Add % to y scale if ploting accuracy
  if(type %in% c("acc_train", "acc_test", "CV_acc") & !use_log){
    out = out + scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))
  }

  #out = out + guides(fill=guide_legend(title="Nb of variables"))

  # if(type != "time"){
  #   suppressWarnings({out = out + ylim(0,100)})
  # }

  return(out)
}


#' Plot the distribution of classes between training and testing sets
#'
#' @author Andrew Hardwick
#' @param data_train The training data.
#' @param data_test The testing data.
#' @export
plot_split = function(data_train, data_test){
  ggplot(data.frame("Class" = c(data_train$meth_class, data_test$meth_class),
                    "Dataset" = factor(c(rep("Train", nrow(data_train)), rep("Test", nrow(data_test))))),
         aes(Class))+
    geom_bar(aes(fill = Dataset), position="fill") +
    theme(axis.text.x = element_text(angle = 45, face = "bold", hjust = 1)) +
    labs(y = "Proportion of observations")
}

#' Plot clasification perfomance.
#'
#' @author Andrew Hardwick
#' @param test_label The training data.
#' @param pred_label The testing data.
#' @export
plot_class = function(test_label, pred_label = NULL){

  if(!is.null(pred_label)){
    data = as.data.frame(table(test_label, pred_label))
    names(data) = c("Class", "pred", "Freq")
  }

  else if(class(test_label) == "table"){
    data = as.data.frame(test_label)
    names(data) = c("Class", "pred", "Freq")
  }

  else{
    stop("Wrong input types!")
  }

  suppressWarnings(
      ggplot(data,
             aes(x = factor(Class, levels = sorted_order_vector), y = Freq, fill = factor(pred, levels = sorted_order_vector)))+
        geom_col(position = "fill") +
        labs(y = "Proporition of observations", x = "Class", caption = "Classes ordered by total number in full dataset") +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1,
                                         face = "bold",
                                         color = scales::hue_pal()(length(unique(test_label)))),
              legend.position = "null",
              plot.margin = unit(c(2,6,3,3), 'mm'))
  )

}


#' Outputs all results for a given result object
#'
#' @author Andrew Hardwick
#' @param res_obj The object resulting from model test script.
#' @param colour The colour palette used for ploting.
#' @param plot_FUN A ggplot geom_ function used to plot the graph. Default \code{geom_line}.
#' @export
print_results_np = function(res_obj, colour = NULL, plot_FUN = geom_line){
  print(plot_res_np(res_obj, "acc_train", NULL, colour, plot_FUN))
  print(plot_res_np(res_obj, "acc_test", NULL, colour, plot_FUN))
  print(plot_res_np(res_obj, "time", "elapsed", colour, plot_FUN))
  if("CV_acc" %in% names(res_obj[["newas10"]])){
    print(plot_res_np(res_obj, "CV_acc", colour, Plot_FUN))
  }
  cat(paste0("Total elapsed calculation time:\n",
            "Seconds: ",sum_time(res_obj, "elapsed"),
            "\nMinutes: ", sum_time(res_obj, "elapsed")/60,
            "\nHours: ", sum_time(res_obj, "elapsed")/3600))
  cat("\n\n")
  cat(paste0("Total CPU calculation time:\n",
             "Seconds: ",sum_time(res_obj, "system"),
             "\nMinutes: ", sum_time(res_obj, "system")/60,
             "\nHours: ", sum_time(res_obj, "system")/3600))
  cat("\n\n")
  cat(paste0("Total user calculation time:\n",
             "Seconds: ",sum_time(res_obj, "user"),
             "\nMinutes: ", sum_time(res_obj, "user")/60,
             "\nHours: ", sum_time(res_obj, "user")/3600))

  # print("Total calculation time:")
  # print(paste("Seconds: ",sum_time(res_obj)))
  # print(paste("Minutes: ",sum_time(res_obj)/60))
  # print(paste("Hours: ",sum_time(res_obj)/3600))
}




#' Calculates the differences between 2 models.
#'
#' @param m1 The result object of the main model you want to compaire others to.
#' @param m2 The result object of the second model you want to compaire others to.
#' @param FUN The function used to compaire each element
#' @export
comp_models = function(m1,m2,FUN = function(x,y){abs(x-y)}){
  if("model_name" %in% names(m1)){
    m1 = format_time(m1)
    m2 = format_time(m2)
  }


  if(!is.list(m1) & !(is.factor(m1) | is.character(m1))){
    if(is.factor(m1) | is.character(m1)){
      return(NA)
    }

    return(FUN(m1,m2))
  }

  out = list()

  for(i in names(m1)[!names(m1) %in% c("results", "pred_labs", "test_labs", "history", "loss", "accuracy", "lr")]){
    if(i %in% names(m2)){
      out[[i]] = comp_models(m1[[i]],m2[[i]], FUN)
    }
  }


  if("model_name" %in% names(m1)){
    out[["n"]] = m1[["n"]]
  }

  return(out)

}





#' Converts Result objects saved in json format (produced by python) to exploitable R frormat.
#'
#' @param res_obj The result object in json format to be converted to R format.
#' @export
convert_py_res = function(res_obj){
  out = res_obj
  for(i in names(res_obj)){
    if((length(unlist(out[[i]])) == 10) | (length(unlist(out[[i]])) == 1066) | (length(unlist(out[[i]])) == 60)){
      out[[i]] = unlist(out[[i]])
    }
    else{
      out[[i]] = convert_py_res(out[[i]])
    }
  }
  return(out)
}


#' Find the maximum of the performing model in result param object for given metric.
#'
#' @param res_obj The result object to plot.
#' @param kernel The kernel to be used.
#' @param type The metric to find the max in.
#' @param get_val Boolean indicating if the value should be returned. If not returns index of max object in res_obj.
#' @export
max_param = function(res_obj, kernel = "kernel", type = "acc_test", get_val = TRUE){

  vect = sapply(res_obj[grep(kernel,names(res_obj))], function(res){return(res[[type]])})

  val = which.max(vect)

  if(get_val){
    out = c()
    out[names(val)] = vect[val]
    return(out)
  }

  return(val + sum(sapply(res_obj, function(x){!is.list(x)})))
}

#' Convert param result object to usable data frame.
#'
#' @param res_obj The result object to convert.
#' @param order A string naming the column to sort in decending order.
#' @export
res_param_df = function(res_obj, order = NULL){

  if(class(res_obj) == "res_param_svm"){
    out = t(sapply(res_obj[grep("kernel_",names(res_obj))], function(x){return(c(x[["param"]], x[["acc_train"]], x[["acc_test"]], x[["time"]]))}))
    colnames(out) = c("kernel", "cost", "gamma", "coef0", "degree",  "acc_train", "acc_test", "time")
    out = as.data.frame(out)
  }

  else if(class(res_obj) == "res_param_xgboost"){
    out = t(sapply(res_obj[grep("lr:",names(res_obj))], function(x){return(c(x[["param"]], x[["acc_train"]], x[["acc_test"]], x[["time"]], x[["mlogloss"]][1], x[["mlogloss"]][5]))}))
    colnames(out) = c("lr", "gamma", "lambda", "alpha", "minchild", "coltree", "maxdepth", "acc_train", "acc_test", "time", "mlogloss1", "mlogloss5")
    out = as.data.frame(out)
  }

  else{stop("Wrong res_obj class!")}

  if(!is.null(order)){
    if(!order %in% colnames(out)){message(paste0(order, " is not a valid column name! Has returned the dataframe unsorted."))}

    out = out[order(out[[order]], decreasing = T),]

  }

  return(out)
}


#' Convert CV result object to usable data frame.
#'
#' @param res_obj The result object to convert.
#' @param order A string naming the column to sort in decending order. Set to null to not order.
#' @param FUN The function used to sum up the CV results.
#' @param rem_first If true does not include the first acc_test measure in the summary of results as there are problems
#' with that first fold prediction at least un some models.
#' @export
cv_eval = function(res_obj, order = "acc_test", FUN = mean, rem_first = FALSE){


  if(!rem_first){
    out = t(sapply(res_obj, function(x){return(unlist(c(x[["param"]], FUN(x[["acc_train"]]), FUN(x[["acc_test"]]), FUN(x[["time"]]))))}))
  }
  else{
    out = t(sapply(res_obj, function(x){return(unlist(c(x[["param"]], FUN(x[["acc_train"]]), FUN(x[["acc_test"]][2:3]), FUN(x[["time"]]))))}))
  }

  if("res_cv_xgboost" %in% class(res_obj)){
    colnames(out) = c("lr", "gamma", "lambda", "alpha", "minchild", "coltree", "maxdepth", "acc_train", "acc_test", "time")
  }
  if("res_cv_svm" %in% class(res_obj)){
    colnames(out) = c("kernel", "cost", "gamma", "coef0", "degree", "acc_train", "acc_test", "time")
  }

  out = as.data.frame(out)

  if(!is.null(order)){
    if(!order %in% colnames(out)){message(paste0(order, " is not a valid column name! Has returned the dataframe unsorted."))}

    out = out[order(out[[order]], decreasing = T),]
  }




  return(out)
}




#' Plot the results of parameter test.
#'
#' @param res_obj The result object to plot.
#' @param kernel The
#' @export
plot_param = function(res_obj,
                      x,
                      y,
                      kernel){

  # Plot SVM results
  if(class(res_obj) ==  "res_param_svm"){

    # element = paste0("kernel_", kernel)
    #
    # sapply(res_obj, function(res){res[[element]]})
    #
    # strsplit()



  }

}


#' Returns the indexes of a balenced k fold split.
#'
#' @param input_vect The vector used to make the split.
#' @param k The nuber of folds.
#' @export
balenced_fold = function(input_vect, k){
  out = list()
  residual = list()

  for(class in unique(input_vect)){
    # Get all indexes for class
    indexes = which(input_vect == class)
    # Get number of element per fold
    nb = as.integer(length(indexes)/k)

    # Split indexes into each fold
    for(idx in 0:(k-1)){
      out[[paste0("fold",idx+1)]] = c(out[[paste0("fold",idx+1)]], indexes[1:nb + (nb*idx)])
    }

    # Get residuals
    residual[[as.character(class)]] = indexes[1:nb + (nb*(idx+1))] #indexes[((nb*(idx+1)) + 1):length(indexes)]
    residual[[as.character(class)]] = residual[[as.character(class)]][!is.na(residual[[as.character(class)]])]
  }

  # Place resuduals in to each fold sequentially starting with the first
  for(res in residual){
    if(length(res) > 0){
      for(i in 1:length(res)){
        out[[paste0("fold",i)]] = append(out[[i]], res[i])
      }
    }
  }

  return(out)
}

#' Returns the vector of indexes of length n with classes balanced as much as possible.
#' If n is less that input_vect length then \code{sample(1:lrngth(input_vect), n)} is returned.
#'
#' @param input_vect The vector used to make the split.
#' @param n The size of the sample.
#' @export
balenced_sample = function(input_vect, n, residual_method = "small_nb_first"){

  nb_classes = length(unique(input_vect))

  if(nb_classes > n){
    set.seed(1)
    return(sample(1:length(input_vect), n))
    #stop("Sample size smaller than number of classes in input_vect!")
  }

  take_nb = as.integer(n/nb_classes)
  out = c()
  free_space = n - take_nb * nb_classes

  # Use the classes with the smallest number of observations first to fill free space
  if(residual_method == "small_nb_first"){
    class_order = names(sort(table(input_vect)))
  }


  out = lapply(class_order, function(class){
    #Get all indexes for class
    indexes = which(input_vect == class)

    # If more instances of class than spaces fore them
    if(length(indexes) >= take_nb){
      return(list(indexes[1:take_nb], 0))
    }

    else{
      return(list(indexes,  take_nb - length(indexes)))
    }

  })

  # Add the total number of "under represented" classes to free_space
  free_space = free_space + sum(sapply(out, function(x){x[[2]]}))

  out = unlist(sapply(out, function(x){x[[1]]}))

  # Loop through first element of what is left in each remaining class until there is no more free space
  while(free_space > 0){
    for(i in names(sort(table(input_vect[-out])))){
      indexes = which(input_vect == i)
      if(!is.na(indexes[!indexes %in% out][1])){
        out = c(out, indexes[!indexes %in% out][1])
        free_space = free_space - 1
      }
      if(free_space == 0){break}
    }
  }

  return(out)

}

# The list of classes that are present in the original test set.
Original_test <- c(
  "A IDH",
  "A IDH, HG",
  "ANA PA",
  "ATRT, MYC",
  "ATRT, SHH",
  "ATRT, TYR",
  "CHORDM",
  "CN",
  "CNS NB, FOXR2",
  "DLGNT",
  "DMG, K27",
  "ENB, A",
  "EPN, MPE",
  "EPN, PF A",
  "EPN, PF B",
  "EPN, RELA",
  "EPN, SPINE",
  "ETMR",
  "EWS",
  "GBM, G34",
  "GBM, MES",
  "GBM, MID",
  "GBM, MYCN",
  "GBM, RTK I",
  "GBM, RTK II",
  "GBM, RTK III",
  "HGNET, BCOR",
  "HGNET, MN1",
  "HMB",
  "IHG",
  "LGG, DNT",
  "LGG, GG",
  "LGG, MYB",
  "LGG, PA MID",
  "LGG, PA PF",
  "LGG, PA/GG ST",
  "LGG, RGNT",
  "LGG, SEGA",
  "LIPN",
  "LYMPHO",
  "MB, G3",
  "MB, G4",
  "MB, SHH CHL AD",
  "MB, SHH INF",
  "MB, WNT",
  "MELAN",
  "MELCYT",
  "MNG",
  "O IDH",
  "PGG, nC",
  "PIN T, PB B",
  "PIN T, PPT",
  "PITAD, ACTH",
  "PITAD, FSH LH",
  "PITAD, STH DNS B",
  "PITAD, TSH",
  "PITUI",
  "PLEX, AD",
  "PLEX, PED A",
  "PLEX, PED B",
  "PTPR, A",
  "PTPR, B",
  "PXA",
  "SCHW",
  "SCHW, MEL",
  "SFT HMPC",
  "SUBEPN, PF",
  "SUBEPN, SPINE",
  "SUBEPN, ST"
)
