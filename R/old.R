
#' Unprocessed Files
#'
#' Filter only those file names that are not already processed
#'
#' @param .dir_out A directory with Files
#' @param .dir_in A directory with Files
#' @param .names A vector of file names
#'
#' @return ...
#' @export
upf <- function(.dir_out, .dir_in = NULL, .names = NULL) {

  if (!is.null(.dir_in)) {
    names_  <- fs::path_ext_remove(list.files(.dir_in))
  } else {
    names_ <- .names
  }

  use_ <- names_[!names_ %in% fs::path_ext_remove(list.files(.dir_out))]
  return(use_)
}

#' Filter Unprocessed Files
#'
#' This function filters out files that have already been processed.
#'
#' @param .tab A data frame that should at least contain the column 'doc_id' (usually created by lft()).
#' @param .dir Directory where the files are located.
#' @param .verbose Logical. If TRUE (default), prints out the number of files to be processed.
#'
#' @return A filtered data frame containing only the unprocessed files.
#'
#' @export
filter_unprocessed_files <- function(.tab, .dir, .verbose = TRUE) {

  doc_id <- NULL

  if (!"doc_id" %in% colnames(.tab)) {
    stop("Files Table must contain the column 'doc_id'", call. = FALSE)
  }

  fils_ <- lft(.dir)

  out_ <- dplyr::filter(.tab, !doc_id %in% fils_$doc_id)

  if (.verbose) {
    cat(glue::glue("\n {scales::comma(nrow(out_))} Files to be Processed"))
  }

  return(out_)

}
