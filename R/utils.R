#' @title Check for included columns
#' @description Check for included columns
#' @param v1 vector of column names included
#' @param v2 vector of columns expected to be included
#' @return message with "Passed" or a list of columns in diff
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

#' @title Prints a message based on a single TRUE / FALSE
#' @description Prints a message based on a single TRUE / FALSE. Only included to be printed as
#' output in markdown files wihtout using print()
#' @param logical Single logical 
#' @return message with "Yes" if TRUE and "No" if FALSE
#' @rdname check_message
#' @export 
check_message <- function(logical){
  
  if(!is.logical(logical)){
    stop("Inputs are not a logical")
  }
  if(length(logical) > 1){
    warning("input not a single logical")
  }
  
  if(logical){
    message("Yes")
  } else {
    message("No")
  }
}
