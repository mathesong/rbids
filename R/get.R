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

# #' Get metadata from json file
# #'
# #' Retrive a list containing the metadata from a json file located in a folder in the BIDS project
# #'
# #' @param fullfilename Full name of .json file (including path).
# #' @param filepath Path to .json file, but no name of the actual file.
# #'
# #' @return Meta-infomration contained in a .json file
# #' @export
# #'
# #' @examples
# #' get_metadata(fullfilename = './sub-01/anat/sub-01_T1w.json')
# get_metadata <- function(fullfilename = NULL, filepath = NULL) {
#   if ((is.null(fullfilename) + is.null(filepath)) != 1) {
#     stop("Specify either fullfilename to .json file or a filepath were json file is located")
#   }
#
#   if (!is.null(fullfilename)) {
#     jsonfile <- get_json_data(fullfilename)
#   }
#
#   if (!is.null(filepath)) {
#     jsonfile <- fs::dir_info(path = filepath, glob = "*.json")
#     if (length(jsonfile$path) > 1) {
#       warning("More than one .json file in folder. Will only return content of the first one.")
#     }
#     jsonfile <- jsonfile$path[1]
#   }
#   jsonlite::read_json(jsonfile)
# }

#' Get filenames and their relative and full paths
#'
#' Retrieve filenames and their relative and full paths from a BIDS folder structure
#'
#' @param studypath The main folder path for a BIDS dataset. If 'json_info' is not specified 'studypath' has to be specified.
#' @param json_info Data frame containing all json information, from 'get_json_data' function. If 'studypath' is not specified 'json_info' has to be specified.
#' @param extension Specify what extension the queried imagefiles have.
#' @param subjects (Optional) Vector specifying which subjects' imagefile-paths to retrieve. If left empty all subjects will be queried.
#' @param sessions (Optional) Vector specifying which sessions' imagefile-paths to retrieve. If left empty all sessions will be queried.
#' @param tasks (Optional) Vector specifying which tasks' imagefile-paths to retrieve. If left empty all tasks will be queried.
#' @param acqs (Optional) Vector specifying which acquisition protocols' imagefile-paths to retrieve. If left empty all acquisition protocols will be queried.
#' @param recs (Optional) Vector specifying which recs' imagefile-paths to retrieve. If left empty all recs will be queried.
#' @param runs (Optional) Vector specifying which runs' imagefile-paths to retrieve. If left empty all runs will be queried.
#' @param modalities (Optional) Vector specifying which modalities' imagefile-paths to retrieve. If left empty all modalities will be queried.
#'
#' @return Tibble containing relative paths, filenames, full paths and full filenames (full paths + filenames)
#' @export
#'
#' @examples
#'
#' get(studypath = getwd(), extension = '.nii.gz', subjects = c('01','02','04'), sessions = '2', modalities = c('pet','mr')  )
#'
get <- function(studypath = NULL, json_info=NULL, extension = NULL,
                subjects = NULL, sessions = NULL,
                tasks = NULL, acqs = NULL,
                recs = NULL, runs = NULL,
                modalities = NULL) {
  if ((is.null(studypath) + is.null(json_info)) != 1) {
    stop("Specify either studypath to BIDS project or a dataframe from rbids::get_json_data ")
  }

  if (!is.null(studypath)) {
    json_info <- get_json_data(studypath)
  }

  if (!is.null(json_info)) {
    json_info <- json_info
  }

  filterAll <- function(filtervar, dfcolumn) {
    unlist(ifelse(test = is.null(filtervar), yes = list(dfcolumn), no = list(filtervar)))
  }

  # Filter for "subject","session","task","acq","rec","run","modality":
  json_files <- json_info %>%
    dplyr::ungroup() %>%
    dplyr::filter(json_info$relpath != "dataset_description.json") %>%
    dplyr::filter(subject %in% filterAll(subjects, .$subject)) %>% # if subject == NULL, filter on all subjects
    dplyr::filter(session %in% filterAll(sessions, .$session)) %>%
    dplyr::filter(task %in% filterAll(tasks, .$task)) %>%
    dplyr::filter(acq %in% filterAll(acqs, .$acq)) %>%
    dplyr::filter(rec %in% filterAll(recs, .$rec)) %>%
    dplyr::filter(run %in% filterAll(runs, .$run)) %>%
    dplyr::filter(modality %in% filterAll(modalities, .$modality))

  # Remove filename from relpath
  stringr::str_sub(string = json_files$relpath, start = nchar(json_files$relpath) - nchar(json_files$filename), end = nchar(json_files$relpath)) <- ""
  stringr::str_sub(string = json_files$path, start = nchar(json_files$path) - nchar(json_files$filename), end = nchar(json_files$path)) <- ""

  if (extension == "nii.gz" | extension == ".nii.gz") {
    stringr::str_sub(string = json_files$filename, start = nchar(json_files$filename) - 4, end = nchar(json_files$filename)) <- ".nii.gz"
  } else if (extension == "nii" | extension == ".nii") {
    stringr::str_sub(string = json_files$filename, start = nchar(json_files$filename) - 4, end = nchar(json_files$filename)) <- ".nii"
  } else if (extension == "EEG_file_extension" | extension == ".EEG_file_extension") {
    stringr::str_sub(string = json_files$filename, start = nchar(json_files$filename) - 4, end = nchar(json_files$filename)) <- ".EEG_file_extension"
  } else {
    stop('extension must be specified as ".nii", ".nii.gz" or ".EEG_file_extension"')
  }

  tibble::tibble(
    relative_paths = json_files$relpath,
    filenames = json_files$filename,
    full_paths = json_files$path,
    full_filenames = paste0(json_files$path, "/", json_files$filename)
  )
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

  extension_globs <- paste0('*', extensions)
  ext_glob <- paste(extension_globs,collapse='|')
  # exts_re <- paste( stringr::str_replace( extensions, '^\\.', ''), collapse='|')

  fs::dir_info(studypath, recursive = T, glob = paste(extension_globs,collapse='|')) %>%
    dplyr::mutate(
      relpath = fs::path_rel(path, studypath),
      pathdepth = stringr::str_count(relpath, "/"),
      filename = stringr::str_match(path, glue::glue(".+/(.*\\.{exts_re}])"))[, 2]
    ) %>%
    dplyr::bind_cols(filename_attributes(.$filename)) %>%
    dplyr::filter(...)

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
get_metadata <- function(relpath, studypath) {

  json_info <- get_jsondata(studypath) %>%
    select(sub:jsondata) %>%
    select(-extension)


  filename <- stringr::str_match(relpath, ".+/(.+?\\..*$)")[, 2]
  file_info <- filename_attributes(filename)

  # Need to figure out how to ignore NA values, and join them to any value in the other df

  joined_info <- left_join(file_info, json_info)

  rlist::list.merge(joined_info$jsondata)

}
