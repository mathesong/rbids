#' Check if a path is the root folder of a BIDS dataset
#'
#' This function checks for the presence of a dataset_description.json file within a specified folder to make sure that the path is correctly specified as the root path of a BIDS dataset.
#'
#' @param studypath The main folder path for a BIDS dataset
#'
#' @return TRUE/FALSE for if a dataset_description.json file was found in the specified folder
#' @export
#'
#' @examples
#' check_dataset_root(getwd())
#'
check_dataset_root <- function(studypath) {
  file.exists(glue::glue("{studypath}/dataset_description.json"))
}

#' Fix Slashes between OSes
#'
#' This command replaces the \\ notation in Windows paths with / notation of Mac and Linux in filepaths
#'
#' @param path The file path
#'
#' @return The corrected file path as a character
#' @export
#'
#' @examples
#' fix_path(getwd())
#'
fix_path <- function(path) {
  gsub("\\", "/", path, fixed = T)
}





# stop(glue::glue(
#   "This does not appear to be the root folder of a valid BIDS",
#   "dataset, as no dataset_description.json file is found there"))


#
# get_layout <- function(studypath) {
#
# }
