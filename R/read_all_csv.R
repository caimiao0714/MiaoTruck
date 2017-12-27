#' read_all_csv
#'
#' This function provides a way of reading all .csv file in the folder.
#'
#' @importfrom readr read_csv
#' @param path The path from which you want to read all the Microsoft Excel files (with the extension of .xlsx OR .xls). The default value of the parameter path is the current working directory.
#' @param add_variable If the add_variable is TRUE, then an extra variable "file_name" will be added to each data.frame in the output list. The default value is set as TRUE.
#'
#' @return a list of dataframes, which can be combined using the rbindlist function in data.table package.
#'
#' @examples
#' # Let's assume there are three files (A.csv, B.csv, C.csv) in the path "C:/data".
#' all_csv = read_all_csv(path = "C:/data")
#' # The above code should read all .csv & .csv files into the list "all_csv". You can access each data.frame by all_csv[["A"]], all_csv[["B"]], and all_csv[["C"]].
#' # If the parameter "add_variable" is set as TRUE, an extra variable "file_name" will be added to each data.frame in the output list.
#'
#' @export
read_all_csv <- function(path = getwd(), add_variable = TRUE){
  original_path = getwd()
  setwd(path)

  mydata = suppressWarnings(suppressMessages(lapply(list.files(pattern="*.csv"), readr::read_csv)))
  # Name each data.frame in the list
  names(mydata) = gsub("\\.csv", "", list.files(pattern = "*.csv"))

  if(add_variable){
  #names all the data.frames in the list
  for(i in 1:length(names(mydata))){
  mydata[[i]]$file_name = names(mydata)[i]
  }
  }

  setwd(original_path)
  return(mydata)
}
