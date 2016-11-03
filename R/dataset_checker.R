#' Dataset checker
#'
#' This function checks whether the dataset variable is present and if the user wants to remove it after processing.
#' Its sole purpose is a helper function that is called when necessary.
#' @keywords microscopR
#' @export
#'

dataset_checker <- function (x){
  var_exist <- exists("micimportR_list")
  user_in <- readline("Would you like to remove the dataset from the R buffer? (y/n): ")
  if (substr(user_in, 1, 1) == "n") {
    remove_set <- FALSE
  } else {
    remove_set <- TRUE
  }
  if (var_exist == TRUE && remove_set == TRUE) {
    safe_set_remove <<- TRUE
  } else {
    safe_set_remove <<- FALSE
  }
}
