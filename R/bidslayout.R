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

  extension <- stringr::str_match(filename, ".+?\\.(.*)")[, 2]

  sub <- stringr::str_match(filename, "sub-(.+?)_")[, 2]
  ses <- stringr::str_match(filename, "ses-(.+?)_")[, 2]
  task <- stringr::str_match(filename, "task-(.+?)_")[, 2]
  acq <- stringr::str_match(filename, "acq-(.+?)_")[, 2]
  rec <- stringr::str_match(filename, "rec-(.+?)_")[, 2]
  run <- stringr::str_match(filename, "run-(.+?)_")[, 2]
  modality <- stringr::str_match(filename, ".+_(.+?)\\..*$")[, 2]
  extension <- stringr::str_match(filename, ".+_.+?(\\..*$)")[, 2]

  tibble::tibble(
    sub = sub,
    ses = ses,
    task = task,
    acq = acq,
    rec = rec,
    run = run,
    modality = modality,
    extension = extension
  )
}




