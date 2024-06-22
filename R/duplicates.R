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
    dplyr::mutate(id_dup = dplyr::cur_group_id(), n_dup = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_dup > 1) %>%
    dplyr::arrange(id_dup) %>%
    dplyr::select(id, id_dup, n_dup, dplyr::everything()) %>%
    tibble::as_tibble()
}
