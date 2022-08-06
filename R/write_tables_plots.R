.table <- tibble::tribble(
  ~ title, ~ desc, ~ obj,
  "...", "...", flextable::flextable(tibble::tibble(a = 5)),
  "...", "...", flextable::flextable(tibble::tibble(a = 6)),
  "...", "...", flextable::flextable(tibble::tibble(a = 6))
)



.path <- tempfile(fileext = ".docx")
.template = "inst/extdata/templates/_tables_template.docx"
.verbose = TRUE
.open = FALSE

write_to_word <- function(.table, .template = NULL, .path, .verbose = TRUE, .open = FALSE) {
  docx_ <- officer::read_docx(path = .template)


  for (ii in seq_len(nrow(.table))) {
    docx_ <- officer::body_add_par(docx_, .table$title[ii], style = "heading 1")
    docx_ <- rrtable::add_flextable(docx_, .table$obj[[ii]])
    docx_ <- officer::body_add_par(docx_, .table$desc[ii], style = "Normal")
    docx_ <- officer::body_end_section_landscape(docx_)

    if (.verbose) {
      cat("\rWriting Table", ii, "of", nrow(.table), "             ")
    }

  }
  print(docx_, target = .path)

  if (.open)  {
    browseURL(.path)
  }

}
