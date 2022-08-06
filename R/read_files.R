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
#'
#' dir <- system.file("extdata", package = "rUtils")
#'
#' lft(dir)
#'
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
#'
#' dir <- system.file("extdata", package = "rUtils")
#'
#' lfc(dir)
#'
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
#' library(rUtils)
#'
#' dir   <- system.file("extdata", package = "rUtils")
#' files <- lfc(dir)
#'
#' read_files(files[1])
#' read_files(files[2])
#'
#' rm(dir, files)
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
#' dir <- system.file("extdata", package = "rUtils")
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


