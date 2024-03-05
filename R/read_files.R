#' List Files in Table
#'
#' @param .dirs Vector/List or single paths to directory/directories
#' @param .reg RegEx to find files (defaults to '*' all files)
#' @param .id Column name containing the file name (defaults to 'doc_id')
#' @param .rec Should the directories be searched recursively?
#'
#' @return A dataframe with file paths
#' @export
#'
#' @examples
#' library(rUtils)
#' dir <- system.file("extdata", package = "rUtils")
#' lft(dir)
#' rm(dir)
lft <- function(.dirs, .reg = NULL, .id = "doc_id", .rec = FALSE) {
  path <- file_ext <- id <- NULL

  purrr::map_dfr(
    .x = .dirs,
    .f = ~ tibble::tibble(path = list.files(.x, .reg, FALSE, TRUE, .rec))
  ) %>%
    dplyr::mutate(
      file_ext = paste0(".", tools::file_ext(path)),
      id = stringi::stri_replace_last_fixed(basename(path), file_ext, ""),
      path = purrr::set_names(path, id)
    ) %>%
    dplyr::select(id, file_ext, path) %>%
    `colnames<-`(c(.id, "file_ext", "path"))
}

#' List Files in Character (Vector)
#'
#' @param .dirs Vector/List or single paths to directory/directories
#' @param .reg RegEx to find files (defaults to '*' all files)
#' @param .rec Should the directories be searched recursively?
#'
#' @return A character vector with paths
#' @export
#'
#' @examples
#' library(rUtils)
#' dir <- system.file("extdata", package = "rUtils")
#' lfc(dir)
#' rm(dir)
lfc <- function(.dirs, .reg = NULL, .rec = FALSE) {
  fils <- unlist(purrr::map(.dirs, ~ list.files(.x, .reg, FALSE, TRUE, .rec)))
  names(fils) <- stringi::stri_replace_all_fixed(
    str         = basename(fils),
    pattern     = paste0(".", tools::file_ext(fils)),
    replacement = ""
  )
  return(fils)
}

#' Read Files
#'
#' @description
#' Read any of the following files: .rds, .fst, .xlsx, .xls, .csv, .dta
#'
#' @param .path Full path to the file
#'
#' @return An R object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(rUtils)
#'
#' dir   <- system.file("extdata/files", package = "rUtils")
#' files <- lfc(dir)
#'
#' read_files(files[1])
#' read_files(files[2])
#'
#' rm(dir, files)
#' }
read_files <- function(.path) {
  ext_ <- tools::file_ext(.path)

  if (ext_ == "rds") {
    obj_ <- readr::read_rds(.path)
  } else if (ext_ == "fst") {
    obj_ <- fst::read_fst(.path)
  } else if (ext_ == "xlsx") {
    obj_ <- openxlsx::read.xlsx(.path)
  } else if (ext_ == "xls") {
    obj_ <- openxlsx::read.xlsx(.path)
  } else if (ext_ == "csv") {
    obj_ <- data.table::fread(.path)
  } else if (ext_ == "dta") {
    obj_ <- rio::import(.path)
  } else if (ext_ == "parquet") {
    obj_ <- arrow::read_parquet(.path)
  } else if (ext_ == "feather") {
    obj_ <- arrow::read_feather(.path)
  } else {
    stop("Format not supported.", call. = FALSE)
  }

  if (is.data.frame(obj_)) {
    obj_ <- tibble::as_tibble(obj_)
  }

  return(obj_)
}

#' Assign Files to the Global Environment
#'
#' @param .lst A list with unique names and file paths
#' @param .prefix Prefix for the names
#' @param .suffix Suffix for the names
#'
#' @return Object(s) in the Global Environment
#' @export
#'
#' @examples
#'
#' library(rUtils)
#'
#' dir <- system.file("extdata/files", package = "rUtils")
#'
#' files <- lfc(dir)
#' assign_files(files)
#'
#' rm(dir, files, fst, rds)
assign_files <- function(.lst, .prefix = "", .suffix = "") {
  path <- name <- f0 <- NULL

  tab_ <- tibble::enframe(.lst, value = "path") %>%
    tidyr::unnest(path) %>%
    dplyr::filter(!purrr::map_lgl(name, exists)) %>%
    dplyr::mutate(
      name = paste0(.prefix, name, .suffix),
      path = purrr::set_names(path, name)
    )

  if (nrow(tab_) == 0) {
    return(message("All variable already assigned"))
  }

  f0 <- function(.path, .name) {
    assign(.name, read_files(.path), globalenv())
    return("Successfully Loaded")
  }
  f1 <- purrr::safely(f0)

  lst_ <- purrr::transpose(purrr::map2(tab_$path, tab_$name, f1))
  err_ <- purrr::transpose(purrr::compact(lst_$error))$message
  res_ <- purrr::compact(lst_$result)

  for (i in seq_len(length(err_))) {
    cat("\n", crayon::red(paste0(names(err_)[i], ": ", err_[i])))
  }

  for (i in seq_len(length(res_))) {
    cat("\n", crayon::green(paste0(names(res_)[i], ": ", res_[i])))
  }
}


#' Get Backup Name
#'
#' @param .name A String
#' @param .prec
#' 1 = "%Y"
#' 2 = "%Y-%m"
#' 3 = "%Y-%m-%d"
#' 4 = "%Y-%m-%d-%H"
#' 5 = "%Y-%m-%d-%H-%M"
#' 6 = "%Y-%m-%d-%H-%M-%S"
#' @param .sep S separatot between time and name
#'
#' @return A string
#' @export
#'
#' @examples
#' bu_name("file", 6)
#' bu_name("file", 5)
#' bu_name("file", 4)
#' bu_name("file", 3)
#' bu_name("file", 2)
#' bu_name("file", 1)
bu_name <- function(.name = "", .prec = 6, .sep = "_") {
  format_ <- c(
    "%Y",
    "%Y-%m",
    "%Y-%m-%d",
    "%Y-%m-%d-%H",
    "%Y-%m-%d-%H-%M",
    "%Y-%m-%d-%H-%M-%S"
  )

  paste(format(Sys.time(), format = format_[.prec]), .name, sep = .sep)
}


#' Unprocessed Files
#'
#' Filter only those file names that are not already processed
#'
#' @param .dir_out A directory with Files
#' @param .dir_in A directory with Files
#' @param .names A vector of file names
#'
#' @return
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
