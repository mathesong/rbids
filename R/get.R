

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

#' Get metadata from json file
#'
#' Retrive a list containing the metadata from a json file located in a folder in the BIDS project
#'
#' @param fullfilename Full name of .json file (including path).
#' @param filepath Path to .json file, but no name of the actual file.
#'
#' @return Meta-infomration contained in a .json file
#' @export
#'
#' @examples
#' get_metadata(fullfilename = './sub-01/anat/sub-01_T1w.json')
get_metadata <- function(fullfilename = NULL, filepath = NULL) {
  if ((is.null(fullfilename) + is.null(filepath)) != 1) {
    stop("Specify either fullfilename to .json file or a filepath were json file is located")
  }

  if (!is.null(fullfilename)) {
    jsonfile <- get_json_data(fullfilename)
  }

  if (!is.null(filepath)) {
    jsonfile <- fs::dir_info(path = filepath, glob = "*.json")
    if (length(jsonfile$path) > 1) {
      warning("More than one .json file in folder. Will only return content of the first one.")
    }
    jsonfile <- jsonfile$path[1]
  }
  jsonlite::read_json(jsonfile)
}
