#' read_all_xls
#'
#' This function provides a way of reading all Microsoft Excel files (.xlsx & .xls) in the folder.
#'
#' @importfrom readxl read_excel
#' @param path The path from which you want to read all the Microsoft Excel files (with the extension of .xlsx OR .xls). The default value of the parameter path is the current working directory.
#' @param add_variable If the add_variable is TRUE, then an extra variable "file_name" will be added to each data.frame in the output list. The default value is set as TRUE.
#'
#' @return a list of dataframes, which can be combined using the rbindlist function in data.table package.
#'
#' @examples
#' # Let's assume there are three files (A.xlsx, B.xlsx, C.xls) in the path "C:/data".
#' all_excel = read_all_excel(path = "C:/data")
#' # The above code should read all .xlsx & .xls files into the list "all_excel". You can access each data.frame by all_excel[["A"]], all_excel[["B"]], and all_excel[["C"]].
#' # If the parameter "add_variable" is set as TRUE, an extra variable "file_name" will be added to each data.frame in the output list.
#'
#' @export
read_all_excel <- function(path = getwd(), add_variable = TRUE){
  original_path = getwd()
  setwd(path)

  mydata = suppressWarnings(suppressMessages(lapply(list.files(pattern="(*.xlsx)|(*.xls)"), readxl::read_excel)))
  # Name each data.frame in the list
  names(mydata) = gsub("(\\.xlsx)|(\\.xls)", "", list.files(pattern = "(*.xlsx)|(*.xls)"))

  if(add_variable){
    #names all the data.frames in the list
    for(i in 1:length(names(mydata))){
      mydata[[i]]$file_name = names(mydata)[i]
    }
  }
  mydata = data.table::rbindlist(mydata)
  setwd(original_path)
  return(mydata)
}


