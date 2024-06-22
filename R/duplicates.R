#' Get Duplicates in a Data Frame
#'
#' This function identifies duplicate rows in a data frame based on specified columns.
#'
#' @param .tab A data frame or tibble to check for duplicates.
#' @param ... Columns to consider for identifying duplicates. These should be unquoted column names.
#'
#' @return A tibble containing the rows from the original data frame that have duplicates, along with additional columns:
#' \describe{
#'   \item{id_dup}{A unique identifier for each group of duplicate rows.}
#'   \item{n_dup}{The number of occurrences of each duplicate group.}
#' }
#'
#' @details
#' The Function groups the data by the specified columns, assigns a unique identifier to each group of duplicates,
#' counts the number of duplicates, and then filters out non-duplicate rows.
#' The resulting tibble is sorted by the duplicate identifier and includes all original columns
#' with the addition of `id_dup` and `n_dup`.
#'
#' @examples
#' df <- tibble::tibble(
#'   id = 1:10,
#'   group = c(rep("A", 3), rep("B", 4), rep("C", 3)),
#'   value = c(1, 2, 2, 3, 3, 3, 4, 5, 5, 6)
#' )
#' get_dups(df, group, value)
#'
#' @import dtplyr
#' @import data.table
#' @export
get_dups <- function(.tab, ...) {
  # DEBUG -- -- -- -- -- -- -- -- -- --
  if (is.null(sys.calls())) {
    cat("DEBUGING MODE: This Message should not appear when calling the function")
  }

  # Global Vars -- -- -- -- -- -- -- --
  id_dup <- n_dup <- id <- NULL

  # Function -- -- -- -- -- -- -- -- --
  vars_ <- dplyr::quos(...)
  .tab %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(!!!vars_) %>%
    dplyr::mutate(
      id_dup = dplyr::cur_group_id(),
      n_dup = dplyr::n()
      ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_dup > 1) %>%
    dplyr::arrange(id_dup) %>%
    dplyr::select(id, id_dup, n_dup, dplyr::everything()) %>%
    tibble::as_tibble()
}
