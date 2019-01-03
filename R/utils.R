#' @title Check for included columns
#' @description Check for included columns
#' @param vector1 vector of column names included
#' @param vector2 vector of columns expected to be included
#' @return message with Yes or a list of missing columns
#' @rdname check_columns
#' @export 
#' @importFrom crayon blue
check_columns <- function(vector1, vector2){
  
  if(!is.character(vector1) | !is.character(vector2)){
    stop("One of the inputs are not a character vector")
  }
  
  if(all(vector1 %in% vector2)){
    message("Yes")
  } else {
    missing <- paste(vector1[!vector1 %in% vector2], collapse = ", ")
    message("No, ", crayon::blue(missing), " are missing.")
  }
  
}
