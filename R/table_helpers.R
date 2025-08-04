#' Adjust row spacing for a gt table
#'
#' Sets the row padding of a `gt` table using preset levels (`"condensed"`, `"regular"`, `"relaxed"`)
#' or a custom numeric pixel value.
#'
#' @param gt_object A `gt` table object.
#' @param padding_setting Either one of `"condensed"`, `"regular"`, or `"relaxed"` (default),
#'   or a numeric value specifying custom padding in pixels.
#'
#' @return A `gt` table object with modified row padding.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt_tbl <- gt(head(mtcars))
#' adjust_row_spacing(gt_tbl, "condensed")
#' adjust_row_spacing(gt_tbl, 10)  # custom px value
#' }
#'
#' @importFrom gt px tab_options
adjust_row_spacing <- function(gt_object, padding_setting = "regular") {
  if (is.numeric(padding_setting)) {
    padding_value <- gt::px(padding_setting)
  } else {
    padding_setting <- match.arg(padding_setting, choices = c("condensed", "regular", "relaxed"))

    padding_value <- switch(padding_setting,
                            condensed = gt::px(3),
                            regular   = gt::px(7),
                            relaxed   = gt::px(12))
  }

  gt::tab_options(gt_object, data_row.padding = padding_value)
}


#' Style column headers in a gt table
#'
#' Applies custom text color to all column headers in a `gt` table using `gt::tab_style()`.
#' By default, it uses a high-contrast dark gray, but you can pass any valid
#' R color name or hex code.
#'
#' @param .data A `gt` table object.
#' @param color A color to apply to the column header text. Defaults to `gt::grey90k`.
#'   You can use any valid color string (e.g., `"black"`, `"#333333"`).
#'
#' @return A `gt` table object with styled column labels.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' # Use default dark gray
#' gt_tbl <- gt::gt(head(mtcars))
#' drkn_clmn_hdr(gt_tbl)
#'
#' # Use a custom color
#' drkn_clmn_hdr(gt_tbl, color = "blue")
#' drkn_clmn_hdr(gt_tbl, color = "#336699")
#' }
#'
#' @importFrom gt tab_style cell_text cells_column_labels
#' @importFrom glitr grey90k
#' @importFrom cli cli_abort
drkn_clmn_hdr <- function(.data, color = "#414042") {
  if (!inherits(.data, "gt_tbl")) {
    cli::cli_abort("`.data` must be a `gt` table.")
  }

  .data %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = color)
      ),
      locations = gt::cells_column_labels()
    )
}





