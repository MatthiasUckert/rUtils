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

#' Read Files in Various Formats
#'
#' This function reads a file from the specified path and converts it into a tibble.
#' Supported file formats include RDS, FST, XLSX, XLS, CSV, DTA, Parquet, and Feather.
#'
#' @param .path A character string representing the path to the file to be read.
#' @return A tibble containing the data from the file.
#' @examples
#' \dontrun{
#'   data <- read_files("data/sample.csv")
#'   data <- read_files("data/sample.rds")
#' }
#' @export
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
  } else if (ext_ == "qs") {
    obj_ <- qs::qread(.path)
  } else {
    stop("Format not supported.", call. = FALSE)
  }

  if (is.data.frame(obj_)) {
    obj_ <- tibble::as_tibble(obj_)
  }

  return(obj_)
}


#' Write Files in Various Formats
#'
#' This function writes a tibble to the specified path in various formats.
#' Supported file formats include RDS, FST, XLSX, CSV, DTA, Parquet, and Feather.
#'
#' @param .obj An R Object
#' @param .path A character string representing the path where the file should be saved.
#' @return NULL
#'
#' @examples
#' \dontrun{
#'   write_files(data, "data/sample.csv")
#'   write_files(data, "data/sample.rds")
#' }
#' @export
write_files <- function(.obj, .path) {
  ext_ <- tools::file_ext(.path)
  fs::dir_create(dirname(.path))

  if (ext_ == "rds") {
    fun_ <- function(.obj, .path) readr::write_rds(.obj, .path, compress = "gz")
  } else if (ext_ == "fst") {
    fun_ <- function(.obj, .path) fst::write_fst(.obj, .path, 100)
  } else if (ext_ == "xlsx") {
    fun_ <- function(.obj, .path) openxlsx::write.xlsx(.obj, .path, TRUE, TRUE)
  } else if (ext_ == "csv") {
    fun_ <- function(.obj, .path) data.table::fwrite(.obj, .path, sep = ";")
  } else if (ext_ == "dta") {
    fun_ <- function(.obj, .path) rio::export(.obj, .path)
  } else if (ext_ == "parquet") {
    fun_ <- function(.obj, .path) arrow::write_parquet(.obj, .path)
  } else if (ext_ == "feather") {
    fun_ <- function(.obj, .path) arrow::write_feather(.obj, .path)
  } else if (ext_ == "qs") {
    fun_ <- function(.obj, .path) qs::qsave(.obj, .path, preset = "archive")
  } else {
    stop("Format not supported.", call. = FALSE)
  }

  .save <- try(fun_(.obj, .path), silent = TRUE)
  for (i in 1:10) {
    if (!inherits(.save, "try-error")) break
    try(file.remove(.path), silent = TRUE)
    Sys.sleep(1)
    .save <- try(fun_(.obj, .path), silent = TRUE)
  }

  if (inherits(.save, "try-error")) try(file.remove(.path), silent = TRUE)
}

# .path <- "_debug/test.parquet"

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
    dplyr::mutate(
      name = paste0(.prefix, name, .suffix),
      path = purrr::set_names(path, name)
    ) %>%
    dplyr::filter(!purrr::map_lgl(name, exists))

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




#' Read Multiple Tables
#'
#' This function reads multiple files concurrently and combines them into a single data frame.
#'
#' @param .paths A character vector of file paths to read.
#' @param .id A string indicating the column name to use for the source identifier. Default is NULL.
#' @param .workers An integer specifying the number of workers to use for parallel processing. Default is 1L.
#' @param .verbose A logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return A combined data frame of all the read files.
#' @export
read_tables <- function(.paths, .id = NULL, .workers = 1L, .verbose = TRUE) {
  future::plan("multisession", workers = .workers)
  out_ <- furrr::future_map(
    .x = .paths,
    .f = read_files,
    .options = furrr::furrr_options(seed = TRUE, globals = "read_files"),
    .progress = .verbose
  ) %>% dplyr::bind_rows(.id = .id)
  future::plan("default")
  on.exit(future::plan("default"))

  return(out_)
}
