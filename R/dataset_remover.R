#' Dataset remover
#'
#' This function follows dataset_checker and actually removes the dataset if requested.
#' Its sole purpose is a helper function that is called when necessary.
#' @keywords microscopR
#' @export
#'


dataset_remover <- function(x) {
  if (safe_set_remove == TRUE) {
    rm(micimportR_list, envir = .GlobalEnv)
  }
  rm(safe_set_remove, envir = .GlobalEnv)
}
