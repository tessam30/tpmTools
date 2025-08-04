#' List data, gt tables, and functions in the tpmTools package
#'
#' Returns grouped summaries of data objects, preformatted `gt` tables, and exported functions.
#'
#' @return An object of class `tpm_tools_list` with a custom print method.
#' @export
#'
#' @examples
#' list_tpm_tools()
list_tpm_tools <- function() {
  data_objects <- tibble::tibble(
    name = c(
      "ess_list",
      "hmis_verif_summary",
      "p4p_indicators",
      "qqc_structural",
      "qqm",
      "qqm_calc",
      "qqm_content_scores",
      "qqm_content"
    ),
    description = c(
      "Environmental and Social Standards list and numbers applicable to HER",
      "HMIS verification indicator coverage by facility type",
      "P4P indicator listing with full and short names",
      "Structural domains and their weighting for QQM",
      "QQM domains (structure, content, quality)",
      "Description of calculated QQM scores across domains",
      "Content of care scores and bonus indicator",
      "Content of care domain-level content descriptions (Vignettes)"
    )
  )

  gt_tables <- tibble::tibble(
    name = c("p4p_gt", "qqm_gt", "qqm_calc_gt"),
    description = c(
      "Formatted gt table of P4P indicators",
      "Styled gt table of QQM components",
      "Display-ready table of calculated QQM scores"
    )
  )

  function_tbl <- tibble::tibble(
    name = c(
      "add_arrow_column_gt",
      "add_arrows",
      "adjust_row_padding",
      "compare_column_sets",
      "drkn_clmn_hdr",
      "fetch_prov_list",
      "list_excel_tabs",
      "prinf",
      "rate_hmis",
      "recode_likert"
    ),
    description = c(
      "Add a column with directional icons to a `gt` table",
      "Append Unicode arrows based on column/baseline comparisons",
      "Reduce or customize row padding in `gt` tables",
      "Compare column names between two data frames",
      "Format and darken `gt` table column headers",
      "Retrieve standardized list of provinces (source-defined)",
      "List all worksheet names from an Excel workbook",
      "Simple formatted printing wrapper (e.g., glue/sprintf)",
      "Encode HMIS indicator completeness ratings for a given column",
      "Recode Likert-scale text or numeric responses to ordinal values"
    )
  )

  structure(
    list(
      data_objects = data_objects,
      gt_tables = gt_tables,
      functions = function_tbl
    ),
    class = "tpm_tools_list"
  )
}



#' Print method for tpm_tools_list objects
#'
#' Nicely formats the grouped data, gt tables, and functions in the tpmTools package.
#'
#' @param x An object of class `tpm_tools_list` (as returned by [list_tpm_tools()]).
#' @param ... Additional arguments passed to [print()].
#' @importFrom cli rule
#'
#' @method print tpm_tools_list
#' @export
print.tpm_tools_list <- function(x, ...) {
  cat(cli::rule(left = "📊 Data Objects", line = 2), "\n")
  print(x$data_objects, n = Inf, row.names = FALSE)

  cat("\n", cli::rule(left = "🖼 GT Tables", line = 2), "\n")
  print(x$gt_tables, n = Inf, row.names = FALSE)

  cat("\n", cli::rule(left = "🛠 Functions", line = 2), "\n")
  print(x$functions, n = Inf, row.names = FALSE)

  invisible(x)
}


