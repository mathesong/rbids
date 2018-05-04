

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




#' Get files and their relative paths
#'
#' Retrieve files and their relative paths from BIDS project
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
#' @return Vector containing relative paths
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
    ungroup() %>%
    filter(subject %in% filterAll(subjects, .$subject)) %>% # if subject == NULL, filter on all subjects
    filter(session %in% filterAll(sessions, .$session)) %>%
    filter(task %in% filterAll(tasks, .$task)) %>%
    filter(acq %in% filterAll(acqs, .$acq)) %>%
    filter(rec %in% filterAll(recs, .$rec)) %>%
    filter(run %in% filterAll(runs, .$run)) %>%
    filter(modality %in% filterAll(modalities, .$modality)) %>%
    select(relpath)

  if (extension == "nii.gz" | extension == ".nii.gz") {
    stringr::str_sub(string = json_files$relpath, start = nchar(json_files$relpath) - 4, end = nchar(json_files$relpath)) <- ".nii.gz"
  } else if (extension == "nii" | extension == ".nii") {
    stringr::str_sub(string = json_files$relpath, start = nchar(json_files$relpath) - 4, end = nchar(json_files$relpath)) <- ".nii"
  } else if (extension == "EEG_file_extension" | extension == ".EEG_file_extension") {
    stringr::str_sub(string = json_files$relpath, start = nchar(json_files$relpath) - 4, end = nchar(json_files$relpath)) <- ".EEG_file_extension"
  } else {
    stop('extension must be specified as ".nii", ".nii.gz" or ".EEG_file_extension"')
  }

  return(json_files$relpath)
}






# get_fullpaths <- function(filenames){
#
#   json_info$filename_no_extentions <- json_info$filename
#   stringr::str_sub(json_info$filename_no_extentions,
#                    start = nchar(json_info$filename_no_extentions)-4,
#                    end = nchar(json_info$filename_no_extentions)) <-''
#
#   filenamesWOExtension <- stringr::str_match(filenames, pattern = '(.+)(.*\\.nii.*)')[,2]
#   filenamesExtension <- stringr::str_match(filenames, pattern = '(.+)(.*\\.nii.*)')[,3]
#
#   filespaths_out <- json_info %>%
#     filter(filename_no_extentions %in% filenames) %>%
#     select(relpath) %>%
#     as.character()
#
#   return(filespaths_out)
# }

# get_relativepaths <- function(filenames){}
