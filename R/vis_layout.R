#' Title
#'
#' @param studypath path to BIDS project
#' @param write_to_csv_name csv file name (optional)
#' @return a data.tree S6 object outlining the project layout
#' @export
#'
#' @examples
#' vis_layout(studypath = getwd(), write_to_csv_name =paste0(getwd(),'/BIDslayout.csv')  )
vis_layout <- function(studypath, write_to_csv_name=NULL) {
  files <- list.files(path,
    pattern = ".json",
    recursive = T
  )
  df <- data.frame(
    filename = sapply(
      files,
      function(fl) paste0("layout", "/", fl)
    ),
    file.info(paste(path, files, sep = "/")),
    stringsAsFactors = FALSE
  )
  fileStructure <- data.tree::as.Node(df, pathName = "filename")
  if (!is.null(write_to_csv_name)) {
    write.table(fileStructure, file = write_to_csv_name, row.names = F, col.names = F)
  }
  fileStructure
}
