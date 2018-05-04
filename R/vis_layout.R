#' Title
#'
#' @param studypath The main folder path for a BIDS dataset
#' @return a data.tree S6 object outlining the project layout
#' @export
#'
#' @examples
#' vis_layout(studypath = getwd())
vis_layout <- function(studypath) {
  files <- fs::dir_info(studypath,
    recursive = T
  ) %>%
    dplyr::mutate(relpath = fs::path_rel(path, studypath))

  df <- data.frame(
    filename = sapply(
      files$relpath,
      function(fl) paste0("layout", "/", fl)
    ),
    file.info(paste(studypath, files$relpath, sep = "/")),
    stringsAsFactors = FALSE
  )

  fileStructure <- data.tree::as.Node(df, pathName = "filename")
  fileStructure
}
