#' @title Check for included columns
#' @description Check for included columns
#' @param .df dataframe to look in, Default: rawdata
#' @param columns vector of columns that should be included, Default: all_cols
#' @return message with Yes or a list of missing columns
#' @rdname check_columns
#' @export 
#' @importFrom crayon blue
check_columns <- function(.df = rawdata, columns = all_cols){
  
  if(all(names(.df) %in% columns)){
    message("Yes")
  } else {
    missing <- paste(names(.df)[!(names(.df) %in% columns)], collapse = ", ")
    message("No, columns ", crayon::blue(missing), " are missing.")
  }
  
}