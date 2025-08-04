#' Add directional arrow symbols to a column based on comparisons
#'
#' Appends a new column with directional symbols (arrows) indicating whether values
#' in a target column are higher, lower, or equal to a baseline or another column.
#' For left and right arrows works best if using Source Sans Pro font.
#'
#' Useful for dashboards, tables, or data exports that visually signal changes or trends.
#'
#' @param .data A data frame or tibble.
#' @param column The column to evaluate (unquoted).
#' @param compare_to (Optional) A second column to compare against (unquoted).
#' @param baseline (Optional) A numeric constant to compare against.
#' @param new_col (Optional) Name of the new column to add. Defaults to `column_arrow`.
#' @param direction Character; either `"updown"` for ▲▼ or `"leftright"` for ▶◀. Default is `"updown"`.
#' @param same_symbol Character symbol to use when values are equal. Default is a medium circle.
#'
#' @return A data frame with an additional column of arrow symbols.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   current = c(90, 75, 80),
#'   previous = c(85, 80, 80)
#' )
#'
#' # Compare to another column
#' add_arrows(df, current, compare_to = previous)
#'
#' # Compare to fixed baseline
#' add_arrows(df, current, baseline = 80)
#'
#' # Custom symbol and direction
#' add_arrows(df, current, baseline = 80, direction = "leftright", same_symbol = "•")
#' }
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang enquo as_name !!
add_arrows <- function(.data,
                       column,
                       compare_to = NULL,
                       baseline = NULL,
                       new_col = NULL,
                       direction = c("updown", "leftright"),
                       same_symbol = "\u2022") {

  direction <- match.arg(direction)

  # Use Unicode-safe arrows
  symbols <- switch(
    direction,
    "updown" = list(up = "\u25B2", down = "\u25BC"),      # ▲ ▼
    "leftright" = list(up = "\u25B6", down = "\u25C0")    # ▶ ◀
  )

  col <- rlang::enquo(column)
  col_name <- rlang::as_name(col)

  # Only capture compare_col if passed
  has_compare_col <- !rlang::quo_is_null(rlang::enquo(compare_to))

  if (has_compare_col) {
    compare_col <- rlang::enquo(compare_to)
    compare_name <- rlang::as_name(compare_col)

    if (!compare_name %in% names(.data)) {
      stop("`compare_to` must refer to a column in `.data`.")
    }
  }

  if (is.null(new_col)) {
    new_col <- paste0(col_name, "_arrow")
  }

  if (has_compare_col) {
    .data <- dplyr::mutate(
      .data,
      !!new_col := dplyr::case_when(
        !!col > !!compare_col ~ symbols$up,
        !!col < !!compare_col ~ symbols$down,
        TRUE ~ same_symbol
      )
    )
  } else if (!is.null(baseline)) {
    .data <- dplyr::mutate(
      .data,
      !!new_col := dplyr::case_when(
        !!col > baseline ~ symbols$up,
        !!col < baseline ~ symbols$down,
        TRUE ~ same_symbol
      )
    )
  } else {
    stop("You must provide either `compare_to` or `baseline`.")
  }

  return(.data)
}



#' Add a directional arrow column to a gt table
#'
#' Adds a new column (default `"delta"`) containing directional arrows (▲▼ or ▶◀),
#' based on comparisons to another column or numeric baseline. Arrows are color-coded and centered.
#'
#' @param data A data frame to render as a `gt` table.
#' @param column The numeric column to annotate with arrows (unquoted or quoted).
#' @param compare_to A column to compare against (unquoted or quoted). Use for row-wise comparison.
#' @param baseline A numeric value or vector (used if `compare_to` is NULL).
#' @param direction `"updown"` (▲▼) or `"leftright"` (▶◀). Default is `"updown"`.
#' @param same_symbol Symbol for equal values. Default is `""`.
#' @param up_color Color for upward arrows. Default is `"forestgreen"`.
#' @param down_color Color for downward arrows. Default is `"firebrick"`.
#' @param new_col_name Optional name for the new arrow column. Default is `"delta"`.
#'
#' @return A styled `gt` table with a new arrow column.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   region = c("A", "B", "C"),
#'   score = c(90, 85, 92),
#'   goal = c(88, 88, 90)
#' )
#'
#' add_arrow_column_gt(df, column = score, compare_to = goal)
#'
#' add_arrow_column_gt(df, column = score, compare_to = goal, new_col_name = "change")
#' }
#'
#' @importFrom dplyr mutate pull case_when relocate
#' @importFrom rlang enquo quo_is_null as_name sym !!
#' @importFrom gt gt tab_style cells_body cell_text
add_arrow_column_gt <- function(data,
                                column,
                                compare_to = NULL,
                                baseline = NULL,
                                direction = c("updown", "leftright"),
                                same_symbol = "",
                                up_color = "#67A784",
                                down_color = "#E571B0",
                                new_col_name = "delta") {

  direction <- match.arg(direction)

  col_expr <- rlang::enquo(column)
  col_name <- rlang::as_name(col_expr)
  compare_expr <- rlang::enquo(compare_to)

  # Resolve comparison vector
  compare_vals <- if (!rlang::quo_is_null(compare_expr)) {
    dplyr::pull(data, !!compare_expr)
  } else if (!is.null(baseline)) {
    if (length(baseline) == 1) {
      rep(baseline, nrow(data))
    } else if (length(baseline) == nrow(data)) {
      baseline
    } else {
      stop("`baseline` must be length 1 or equal to number of rows.")
    }
  } else {
    stop("You must provide either `compare_to` or `baseline`.")
  }

  # Arrow logic
  col_vals <- dplyr::pull(data, !!col_expr)

  symbols <- switch(direction,
                    "updown"    = list(up = "▲", down = "▼"),
                    "leftright" = list(up = "▶", down = "◀")
  )

  arrows <- dplyr::case_when(
    col_vals > compare_vals ~ symbols$up,
    col_vals < compare_vals ~ symbols$down,
    TRUE ~ same_symbol
  )

  # Add and relocate arrow column
  data <- data %>%
    dplyr::mutate(!!new_col_name := arrows) %>%
    dplyr::relocate(!!rlang::sym(new_col_name), .after = !!col_expr)

  # Identify rows for color styling
  up_rows <- which(arrows == symbols$up)
  down_rows <- which(arrows == symbols$down)

  # Build gt table with styling
  gt_tbl <- gt::gt(data) %>%
    gt::tab_style(
      style = gt::cell_text(color = up_color),
      locations = gt::cells_body(columns = !!rlang::sym(new_col_name), rows = up_rows)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = down_color),
      locations = gt::cells_body(columns = !!rlang::sym(new_col_name), rows = down_rows)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = !!rlang::sym(new_col_name))
    )

  return(gt_tbl)
}

