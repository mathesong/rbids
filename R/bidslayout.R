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
  fs::file_exists(glue::glue("{studypath}/dataset_description.json"))
}


#' Get Attributes of a File from the JSON
#'
#' @param filename The name of the json file
#'
#' @return A tibble of the attributes of the file
#' @export
#'
#' @examples
#' filename_attributes("sub-01_T1w.json")
#'
filename_attributes <- function(filename) {
  subject <- stringr::str_match(filename, "sub-(.+?)_")[, 2]
  session <- stringr::str_match(filename, "ses-(.+?)_")[, 2]
  task <- stringr::str_match(filename, "task-(.+?)_")[, 2]
  acq <- stringr::str_match(filename, "acq-(.+?)_")[, 2]
  rec <- stringr::str_match(filename, "rec-(.+?)_")[, 2]
  run <- stringr::str_match(filename, "run-(.+?)_")[, 2]
  modality <- stringr::str_match(filename, ".+_(.*)\\.json")[, 2]

  tibble::tibble(
    subject = subject,
    session = session,
    task = task,
    acq = acq,
    rec = rec,
    run = run,
    modality = modality
  )
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
get_json_data <- function(studypath) {
  if (check_dataset_root == FALSE) {
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
    dplyr::mutate(jsondata = list(jsonlite::fromJSON(path)))
}
