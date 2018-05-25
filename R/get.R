#' Get subjects in BIDS project
#'
#' Lists all subjects in BIDS project that have .json files
#'
#' @param studypath The main folder path for a BIDS dataset. If 'json_info' is not specified 'studypath' has to be specified.
#' @param json_info Data frame containing all json information, from 'get_json_data' function. If 'studypath' is not specified 'json_info' has to be specified.
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



#' Get files and filenames
#'
#' Query the data files from a BIDS folder
#'
#' @param studypath The main folder path for a BIDS dataset.
#' @param extensions A vector of file extensions, e.g. c(".nii.gz", ".nii")
#' @param ... Other conditions to filter the dataset on, such as sub, ses, task,
#'   acq, rec, run, modality. Note that these arguments are passed to
#'   dplyr::filter, so they are evaluated. Thus they should be inputted as such.
#'   e.g. sub %in% c("01", "02") or modality=="T1w".
#'
#' @return Tibble containing the relevant file information
#' @export
#'
#' @examples
#' subList <- c("01", "03")
#' get_files(studypath, extensions=c('.nii', '.nii.gz'), sub %in% subList, modality=="T1w")
get_files <- function(studypath, extensions, ...) {
  extension_globs <- paste0("*", extensions)
  ext_glob <- paste(extension_globs, collapse = "|")
  # exts_re <- paste( stringr::str_replace( extensions, '^\\.', ''), collapse='|')

  fs::dir_info(studypath, recursive = T, glob = ext_glob) %>%
    dplyr::mutate(
      relpath = fs::path_rel(path, studypath),
      pathdepth = stringr::str_count(relpath, "/"),
      filename = stringr::str_match(path, ".+/(.+?\\..*)")[, 2]
    ) %>%
    dplyr::bind_cols(filename_attributes(.$filename)) %>%
    dplyr::filter(...) %>%
    select(filename, relpath, path, rev(colnames(.)))
}


#' Get Data from JSON files in a BIDS Study Folder
#'
#' @param studypath The main folder path for a BIDS dataset
#'
#' @return A nested tibble with file information, and JSON file contents
#' @export
#'
#' @examples
#'
get_jsondata <- function(studypath) {
  if (check_dataset_root(studypath) == FALSE) {
    stop(glue::glue(
      "This does not appear to be the root folder of a valid BIDS",
      "dataset, as no dataset_description.json file is found there"
    ))
  }

  fs::dir_info(studypath, recursive = T, glob = "*.json") %>%
    dplyr::mutate(
      relpath = fs::path_rel(path, studypath),
      pathdepth = stringr::str_count(relpath, "/"),
      filename = stringr::str_match(path, ".+/(.*\\.json)")[, 2]
    ) %>%
    dplyr::bind_cols(filename_attributes(.$filename)) %>%
    dplyr::group_by(path) %>%
    dplyr::mutate(jsondata = list(jsonlite::fromJSON(path))) %>%
    dplyr::ungroup()
}

#' Get File Metadata
#'
#' Fetches the metadata stored in json sidecars for a given image file.
#'
#' @param relpath The relative path and filename of the image file.
#' @param studypath The path of the study
#'
#' @return A list containing all the metadata from relevant json sidecars which applies to a given image file.
#' @export
#'
#' @examples
#' get_metadata("sub-01/anat/sub-01_T1w.nii.gz", studypath)
# get_metadata <- function(relpath, studypath) {
#
#   json_info <- get_jsondata(studypath) %>%
#     select(sub:jsondata) %>%
#     select(-extension)
#
#
#   filename <- stringr::str_match(relpath, ".+/(.+?\\..*$)")[, 2]
#   file_info <- filename_attributes(filename)
#
#   # Need to figure out how to ignore NA values, and join them to any value in the other df
#
#   joined_info <- left_join(file_info, json_info)
#
#   rlist::list.merge(joined_info$jsondata)
#
#   # Need to figure out how to get this last function to work too
# }
#
