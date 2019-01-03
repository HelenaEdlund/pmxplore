#' @title Check for included columns
#' @description Check for included columns
#' @param vector1 vector of column names included
#' @param vector2 vector of columns expected to be included
#' @return message with Yes or a list of missing columns
#' @rdname check_columns
#' @export 
#' @importFrom crayon blue
check_columns <- function(v1, v2){
  
  if(!is.character(v1) | !is.character(v2)){
    stop("One of the inputs are not a character vector")
  }
  
  if(all(v1 %in% v2)){
    message("Passed")
  } else {
    missing <- paste(v1[!v1 %in% v2], collapse = ", ")
    message(crayon::blue(missing), " are defined in first argument but not in second")
  }
  
}
