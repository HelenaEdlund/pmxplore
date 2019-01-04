#' @title Check for included columns
#' @description Check for included columns
#' @param v1 vector of column names included
#' @param v2 vector of columns expected to be included
#' @return message with "Passed" or a list of columns in diff
#' @rdname check_columns
#' @export 
#' @importFrom crayon blue
check_columns <- function(v1, v2){
  
  if(!is.vector(v1) | !is.vector(v2)){
    stop("One of the inputs are not a vector")
  }
  
  if(typeof(v1) != typeof(v2)){
    stop("The two arguments are not of same type")
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

#' @title Collapses unique values 
#' @description Prints a comma separated string with unique values of a vector 
#' @param x vector
#' @return character vector of length 1
#' @rdname collapse_unique
#' @export 
collapse_unique <- function(x){
  values <- unique(x)
  paste(values, collapse=", ")
}

#' @title Renames data frame columns based on mapping
#' @description Prints a comma separated string with unique values of a vector 
#' @param df data frame to rename
#' @param mapping_df data frame with 2 columns: first column is original names and second new names
#' @return data frame
#' @rdname rename_cols_mapping
#' @export 
rename_cols_mapping <- function(df, mapping_df){
  
  if(!is.data.frame(df) | !is.data.frame(mapping_df)){
    stop("One of the inputs is not a data frame")
  }
  
  original_col_name <- mapping_df[,1]
  new_col_name      <- mapping_df[,2]
  
  # check that all columns are found in original dataset
  if( ! length(original_col_name %in% names(df)) == length(original_col_name)) {
    
    missing <- paste(original_col_name[!original_col_name %in% names(df)], collapse = ", ")
    stop(crayon::blue(missing), " not found in original data frame")
  }
  
  for(i in 1:nrow(mapping_df)){
    df <- df %>% 
      rename(!!new_col_name[i] := !!original_col_name[i])
  }
  return(df)
}
