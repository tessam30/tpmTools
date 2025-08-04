#' Recode Likert-scale responses into a new column
#'
#' Recodes a column of Likert-style responses either from text to numeric or
#' from numeric to text, depending on its current type. A new column is created
#' with the recoded values.
#'
#' @param data A data frame.
#' @param column A column in `data` to recode (unquoted).
#' @param .to Optional name for the new column. Defaults to `<column>_recode`.
#' @param scale A named character vector where names are numeric values (as strings)
#'   and values are regex patterns used to match Likert-style text (e.g., `"strong.*agree"`).
#' @param labels A named character vector mapping numeric codes (as strings)
#'   to their Likert-style text labels, used when converting from numeric to text.
#'
#' @return A data frame with the recoded column added.
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom rlang ensym as_name .data `%||%`
#' @importFrom cli cli_abort
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble(response = c("Agree", "Disagree", "Strongly agree"))
#' df |> recode_likert(response)
#'
#' df2 <- tibble(response = c(3, 1, 4))
#' df2 |> recode_likert(response)
#' }
recode_likert <- function(data,
                          column,
                          .to = NULL,
                          scale = c(
                            "1" = "strong.*disagree",
                            "2" = "^disagree$",
                            "3" = "^agree$",
                            "4" = "strong.*agree"
                          ),
                          labels = c(
                            "1" = "Strongly disagree",
                            "2" = "Disagree",
                            "3" = "Agree",
                            "4" = "Strongly agree"
                          )) {
  column_sym <- rlang::ensym(column)
  column_name <- rlang::as_name(column_sym)
  new_col <- .to %||% paste0(column_name, "_recode")

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  if (!column_name %in% names(data)) {
    cli::cli_abort("Column `{column_name}` not found in `data`.")
  }

  col_data <- data[[column_name]]

  if (is.character(col_data)) {
    input <- stringr::str_to_lower(trimws(col_data))
    result <- rep(NA_integer_, length(input))

    for (code in names(scale)) {
      pattern <- scale[[code]]
      match_idx <- stringr::str_detect(input, pattern)
      result[match_idx & is.na(result)] <- as.integer(code)
    }

  } else if (is.numeric(col_data)) {
    char_result <- labels[as.character(col_data)]
    result <- unname(char_result)

  } else {
    cli::cli_abort("Column `{column_name}` must be character or numeric.")
  }

  dplyr::mutate(data, !!new_col := result)
}


#' List Excel sheet names from a file or all files in a folder
#'
#' Given a path to an Excel file or folder, returns a tibble listing the
#' sheet (tab) names for each file. Handles `.xls`, `.xlsx`, and `.xlsm`
#' extensions. Gracefully handles errors and missing files.
#'
#' @param path Path to an Excel file or folder containing Excel files.
#' @param pattern Optional regex pattern to match Excel files in folders.
#'   Default is `"\\.xls[xm]?$"` to match `.xls`, `.xlsx`, and `.xlsm`.
#'
#' @return A tibble with columns `file` and `sheet`. Returns `NA` for unreadable files.
#' @export
#'
#' @examples
#' \dontrun{
#' # For a single file
#' list_excel_tabs("data/survey_results.xlsx")
#'
#' # For a folder of Excel files
#' list_excel_tabs("data/")
#' }
#'
#' @importFrom readxl excel_sheets
#' @importFrom purrr map2
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom fs is_file is_dir
list_excel_tabs <- function(path, pattern = "\\.xls[xm]?$") {
  safe_list_tabs <- function(file_path) {
    tryCatch(
      readxl::excel_sheets(file_path),
      error = function(e) {
        message("Error reading '", file_path, "': ", e$message)
        return(NA_character_)
      }
    )
  }

  if (fs::is_file(path)) {
    sheets <- safe_list_tabs(path)
    return(tibble::tibble(
      file = basename(path),
      sheet = sheets
    ))
  }

  if (fs::is_dir(path)) {
    files <- list.files(
      path = path,
      pattern = pattern,
      full.names = TRUE,
      ignore.case = TRUE
    )

    # Filter out directories (just in case)
    files <- files[file.info(files)$isdir == FALSE]

    tab_data <- purrr::map2(
      .x = files,
      .y = purrr::map(files, safe_list_tabs),
      .f = function(file, sheets) {
        tibble::tibble(
          file = basename(file),
          sheet = sheets
        )
      }
    )

    return(dplyr::bind_rows(tab_data))
  }

  cli::cli_abort("`path` must be a valid Excel file or folder containing Excel files.")
}


#' Print all rows of a data frame or tibble
#'
#' A safer wrapper around `print()` that shows all rows of a data frame or
#' tibble (`n = Inf`). Automatically falls back to base `print()` for
#' non-tibble objects.
#'
#' @param df A data frame or tibble to print.
#' @param n Number of rows to print. Defaults to `Inf`.
#' @param width Optional width of printed output.
#' @param ... Additional arguments passed to `print()`.
#'
#' @return Invisibly returns the printed object.
#' @export
#'
#' @examples
#'\dontrun{
#' prinf(mtcars)
#' prinf(starwars, width = 60)
#'
#' # Fallback to base print
#' prinf(data.frame(x = 1:3)) }
#'
#' @importFrom cli cli_abort
prinf <- function(df, n = Inf, width = NULL, ...) {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("`df` must be a data frame or tibble.")
  }

  if (inherits(df, "tbl_df")) {
    invisible(print(df, n = n, width = width, ...))
  } else {
    invisible(print(df, ...))
  }
}


#' Compare values between two data frame columns
#'
#' Compares two columns from two data frames and returns the values
#' that are only present in one but not the other.
#'
#' @param df1 First data frame.
#' @param col1 Column in `df1` (unquoted).
#' @param df2 Second data frame.
#' @param col2 Column in `df2` (unquoted).
#'
#' @return A list with elements `only_in_df1` and `only_in_df2`.
#' @export
#'
#' @examples
#'  \dontrun{
#' df1 <- tibble::tibble(id = c(1, 2, 3))
#' df2 <- tibble::tibble(id = c(3, 4, 5))
#' compare_column_sets(df1, id, df2, id)
#' }
#'
#' @importFrom dplyr pull
#' @importFrom rlang enquo
compare_column_sets <- function(df1, col1, df2, col2) {
  col1 <- rlang::enquo(col1)
  col2 <- rlang::enquo(col2)

  col1_vals <- dplyr::pull(df1, !!col1)
  col2_vals <- dplyr::pull(df2, !!col2)

  list(
    only_in_df1 = base::setdiff(col1_vals, col2_vals),
    only_in_df2 = base::setdiff(col2_vals, col1_vals)
  )
}



#' Rate HMIS Indicator Performance
#'
#' This function evaluates the performance of a numeric HMIS (Health Management Information System) indicator
#' against a specified threshold and assigns corresponding labels and colors. It is intended for use in
#' performance reporting or dashboards to quickly classify results as "Optimal" or "Failure".
#'
#' @param .data A data frame or tibble containing the HMIS indicator column.
#' @param hmis_col Unquoted column name representing the HMIS indicator (numeric values between 0 and 1).
#' @param label_col A string giving the name of the new column to hold the performance classification label. Defaults to `"hmis_label"`.
#' @param color_col A string giving the name of the new column to hold the color associated with each performance class. Defaults to `"hmis_color"`.
#' @param threshold Numeric threshold above which the indicator is considered "Optimal". Defaults to `0.85`.
#' @param label_optimal Label assigned to values above the threshold. Defaults to `"Optimal"`.
#' @param label_failure Label assigned to non-missing values below or equal to the threshold. Defaults to `"Failure"`.
#' @param color_optimal Hex color code for optimal performance. Defaults to `"#67A784"` (green).
#' @param color_failure Hex color code for failure. Defaults to `"#E571B0"` (pink).
#'
#' @return A modified version of `.data` with two new columns: one for labels (`label_col`) and one for color codes (`color_col`).
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- tibble::tibble(hmis_score = c(0.9, 0.8, NA))
#' rate_hmis(df, hmis_score)
#' }
#' @export
rate_hmis <- function(.data, hmis_col, label_col = "hmis_label",
                      color_col = "hmis_color",
                      threshold = 0.85,
                      label_optimal = "Optimal", label_failure = "Failure",
                      color_optimal = "#67A784", color_failure = "#E571B0") {

  hmis_col <- enquo(hmis_col)
  label_col <- sym(label_col)
  color_col <- sym(color_col)

  .data %>%
    mutate(
      !!label_col := case_when(
        !!hmis_col > threshold ~ label_optimal,
        !is.na(!!hmis_col)     ~ label_failure,
        TRUE                   ~ NA_character_
      ),
      !!color_col := case_when(
        !!hmis_col > threshold ~ color_optimal,
        !is.na(!!hmis_col)     ~ color_failure,
        TRUE                   ~ NA_character_
      )
    )
}



