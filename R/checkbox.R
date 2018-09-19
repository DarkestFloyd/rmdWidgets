#' Add checkbox
#' 
#' @param choices A character vector for all the choices
#' @param selected A numeric vector for the index of all selected/checked items.
#' @param label Text string for label row. Default NULL.
#' @param inline T/F for whether choices should be aligned inline. Default T.
#' @param label_inline T/F for whether the label row should be placed inline 
#' with the choices. Default T. If label_inline is set to be T, then inline 
#' will be set to T as well.
#' @param format "latex" or "html". Controlled by global option 
#' `rmdWidgets.format`
#' 
#' @importFrom knitr asis_output
#' @export
rmd_checkbox <- function(choices, selected = NULL, label = NULL, 
                         inline = T, label_inline = T, 
                         format = "latex") {
  if (label_inline) inline <- T
  if (format == "latex") {
    return(
      rmd_checkbox_latex(choices, selected, label, inline, label_inline)
    )
  }
}

rmd_checkbox_latex <- function(choices, selected, label, inline, label_inline) {
  if (!is.null(label)) {
    after_label <- switch(label_inline + 1, "\n\n", "\\hfill")
    label <- paste(label, after_label)
  }
  check_symbols <- rep("$\\square$", length(choices))
  check_symbols[selected] <- "$\\boxtimes$"
  check_items <- paste(check_symbols, choices)
  if (inline) {
    check_items <- paste(check_items, collapse = " ")
  } else {
    check_items <- paste(check_items, collapse = "\n\n")
  }
  knitr::asis_output(paste(label, check_items))
}