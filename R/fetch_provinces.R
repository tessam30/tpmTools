#' Fetch a provincial list from a Google Sheet
#'
#' Reads a Google Sheet using its `drive_id` via the `googlesheets4` package.
#' Handles errors gracefully by returning `NULL` and printing a message.
#'
#' @param drive_id A Google Drive file ID (not the full URL). Defaults to a known sheet ID.
#'
#' @return A data frame (`tibble`) containing the sheet contents, or `NULL` if fetching fails.
#' @export
#'
#' @examples
#' \dontrun{
#' prov_list <- fetch_prov_list()
#' }
#'
#' @importFrom googlesheets4 read_sheet
fetch_prov_list <- function(drive_id = "1i4aIHq0XoNLEB-Ii8Z4paCbT9Q6zxTBHLrDRgbhhXnE") {
  tryCatch({
    googlesheets4::read_sheet(ss = drive_id)
  }, error = function(e) {
    message("Failed to read Google Sheet: ", e$message)
    return(NULL)
  })
}

