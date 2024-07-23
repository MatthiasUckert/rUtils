# Cache Object ------------------------------------------------------------
#' Cache an Object to Disk
#'
#' Caches an R object to disk using either the 'fst' format for data frames or
#' 'qs' format for other objects. It creates a specified directory if it does
#' not exist and saves the object in that directory.
#'
#' @param .expr
#' An expression that generates the object to be cached.
#' This is evaluated only if the object does not exist in the cache
#' or if `.rerun` is `TRUE`.
#' @param .name
#' A character string specifying the name of the file to save the
#' cached object. This name is used as the base for the filename,
#' with appropriate file extensions added for the storage format.
#' @param .dir
#' A character string specifying the directory in which to store the
#' cached object. The directory will be created if it does not exist.
#' @param .rerun
#' A logical value indicating whether to re-run the expression
#' and re-cache the object, even if it already exists in the cache.
#' Defaults to `FALSE`.
#'
#' @return
#' The function does not explicitly return a value. It saves or loads the
#' cached object as a side effect.
#' @export
cache_obj <- function(.expr = { }, .dir, .name, .rerun = FALSE) {
  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- --
  if (is.null(sys.calls())) {
    .expr  <- tibble::tibble(id = 1:10)
    .expr  <- 1:10
    .dir   <- "_debug_data/cache_obj"
    .name  <- "cache_test"
    .rerun <- FALSE
    cat("DEBUGING MODE: This Message should not appear when calling the function")
  }

  # Create Directory -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  dir_ <- fs::dir_create(.dir)
  lft_ <- dplyr::filter(lft(.dir), doc_id == .name)

  if (nrow(lft_) > 0) {
    return(read_files(lft_$path))
  }

  obj_ <- .expr
  ext_ <- ifelse(is.data.frame(obj_), ".parquet", ".qs")
  fil_ <- file.path(dir_, paste0(.name, ext_))

  write_files(obj_, fil_)
  return(obj_)
}

