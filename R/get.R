

#' Get subjects in BIDS project
#'
#' Lists all subjects in BIDS project that have .json files
#'
#' @param studypath The main folder path for a BIDS dataset. If 'json_info' is not specified 'studypath' has to be specified.
#' @param json_info Data fare containing all json information, from 'get_json_data' function. If 'studypath' is not specified 'json_info' has to be specified.
#'
#' @return A vector of all subjects in the BIDS project
#' @export
#'
#' @examples
#' get_subjects(studypath = studypath)
get_subjects <- function(studypath=NULL, json_info = NULL) {
  if ((is.null(studypath) + is.null(json_info)) != 1) {
    stop("Specify either studypath to BIDS project or a dataframe from rbids::get_json_data ")
  }

  if (!is.null(studypath)) {
    json_info <- get_json_data(studypath)
  }

  if (!is.null(json_info)) {
    json_info <- json_info
  }

  as.character(na.omit(unique(json_info$subject))) # omit NAs
}

# get_metadata <- function(filename){}
