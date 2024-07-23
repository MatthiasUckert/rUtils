#' Default Location
#'
#' This function sets the default directory and returns the path of the files and directories
#' within that directory. The function also initializes the default directory if `.init` is set to TRUE.
#'
#' @param ... Character vectors representing file paths. Long vectors are not supported.
#' @param .init Logical, if TRUE, initializes the default directory. Default is FALSE.
#'
#' @return A path or a directory. If `.init` is set to TRUE, the function returns NULL.
#' @export
dloc <- function(..., .init = FALSE) {
  `.__init__` <- NULL

  if (.init) {
    path_0_ <- fs::path_abs(file.path(...))
  } else {
    path_0_ <- file.path(...)
  }

  ext_ <- tools::file_ext(path_0_)

  if (.init & ext_ != "") {
    stop("You can only initialize a directory not a file", call. = FALSE)
  }

  if (.init) {
    assign(.__init__, path_0_, globalenv())
    # .__init__ <<- path_0_
    fs::dir_create(path_0_)
    return(NULL)
  }

  if (!.init & !exists(".__init__")) {
    stop("You must initialize a directory before using default locations", call. = FALSE)
  }

  path_1_ <- file.path(.__init__, path_0_)

  if (ext_ == "") {
    fs::dir_create(path_1_)
  } else {
    fs::dir_create(dirname(path_1_))
  }

  return(path_1_)
}


#' Initialize Project
#'
#' This function initializes a project by creating three directories named "0_data", "1_code", and "2_output".
#' It checks if the directories exist, and if not, creates them.
#'
#' @return Directories created in the current working directory.
#' @export
init_project <- function() {
  dirs_ <- c("0_data", "1_code", "2_output")
  for (dir_ in dirs_) {
    if (!dir.exists(dir_)) dir.create(dir_, FALSE, TRUE)
  }

}

#' Create Script Function File
#'
#' This function creates a script function file within the specified directory and sources it.
#' Optionally, the function can open the file in a web browser.
#'
#' @param .dir_here Main Directory.
#' @param .dir_script Script Directory (optional).
#' @param .name_script Script Name.
#' @param .browse Logical, if TRUE, opens the file in a web browser. Default is FALSE.
#'
#' @return A path to the created script function file.
#' @export
use_functions <- function(.dir_here, .dir_script = NULL, .name_script, .browse = FALSE) {
  if (!is.null(.dir_script)) {
    .path <- file.path(.dir_here, "1_code", .dir_script, paste0("f-", .dir_script, "-", .name_script, ".R"))
  } else {
    .path <- file.path(.dir_here, "1_code", paste0("f-", .name_script, ".R"))
  }

  .path <- fs::path_tidy(.path)
  if (!file.exists(.path)) write("", .path)
  source(.path)
  if (.browse) utils::browseURL(.path)

  return(.path)
}

#'
#' #' Get Templates
#' #'
#' #' This function retrieves a template file from the "rUtils" package, replaces a placeholder
#' #' with the given file name, and saves the modified file to the "1_code" directory.
#' #' Optionally, the function can open the file in a web browser.
#' #'
#' #' @param .template Not relevant (NULL by default).
#' #' @param .name File Name.
#' #' @param .open Logical, if TRUE, opens the script in a web browser. Default is TRUE.
#' #'
#' #' @return Copied and modified template file.
#' #' @export
#' get_templates <- function(.template = NULL, .name, .open = TRUE) {
#'   path_ <- system.file("extdata/quarto_templates/template_01.qmd", package = "rUtils")
#'   text_ <- readLines(path_)
#'   text_ <- gsub("AAAAAAA", .name, text_)
#'   out_ <- file.path("1_code", paste0(.name, ".qmd"))
#'   print(out_)
#'   writeLines(text = text_, con = out_)
#'
#'   if (.open) {
#'     utils::browseURL(out_)
#'   }
#' }
#'
#' #' Backup Script
#' #'
#' #' This function creates a backup of the specified Rmd and function files by copying them to the "_backup"
#' #' subdirectory within the "1_code" directory.
#' #'
#' #' @param .dir_here Main Directory.
#' #' @param .dir_script Script Directory (optional).
#' #' @param .name_script Script Name.
#' #'
#' #' @return No return value; creates backup copies of specified files.
#' #' @export
#' backup_script <- function(.dir_here, .dir_script = NULL, .name_script) {
#'   dir_co_ <- file.path(.dir_here, "1_code")
#'   dir_bu_ <- file.path(dir_co_, "_backup")
#'   if (!dir.exists(dir_bu_)) dir.create(dir_bu_, FALSE, TRUE)
#'
#'
#'   if (!is.null(.dir_script)) {
#'     fil_rmd_ <- file.path(dir_co_, .dir_script, paste0(.name_script, ".Rmd"))
#'     fil_fun_ <- file.path(dir_co_, .dir_script, paste0("f-", .dir_script, "-", .name_script, ".R"))
#'   } else {
#'     fil_rmd_ <- file.path(dir_co_, paste0(.name_script, ".Rmd"))
#'     fil_fun_ <- file.path(dir_co_, paste0("f-", .name_script, ".R"))
#'   }
#'
#'   file.copy(fil_rmd_, file.path(dir_bu_, bu_name(basename(fil_rmd_), 6)))
#'   file.copy(fil_fun_, file.path(dir_bu_, bu_name(basename(fil_fun_), 6)))
#'
#' }
