#' The purpose of this script is to build matching testing sets from pre-existing training sets
#' so as to reduce the amount of memory used in computation by not loading the full testing set each time.

# input_path = "OldData"
#
# output_path = "OldOutput"
#
# test_data_path = "OldData"


#' Alows script to be run both from Scripts folder and from parent folder
#' If file exists, script is being run from Scripts folder
cmd = file.exists("functions.R")

###### Adjusting path ######
if(cmd){
  input_path = paste0("../", input_path)
  output_path = paste0("../", output_path)
  test_data_path = paste0("../", test_data_path)
}

files = list.files(input_path)
files = files[grep("df_newas[0-9]*_train.rds", files)]

test_in = readRDS(paste0(test_data_path, "/df_preproc_test.rds"))

nb_var = as.numeric(unlist(regmatches(files, gregexpr("[[:digit:]]+", files))))

idx = 1
for(f in paste0(input_path, "/", files)){
  train = names(readRDS(f))
  saveRDS(test_in[,train], paste0(output_path, "/df_newas",nb_var[idx],"_test.rds"))
  # print(train)
  # print(paste0(output_path, "/df_newas",nb_var[idx],"_test.rds"))
  idx = idx + 1
}





