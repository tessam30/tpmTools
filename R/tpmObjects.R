
#' Ordered facility size levels
#'
#' A character vector defining the standard order of facility types by size.
#' Useful for setting factor levels for consistent sorting in plots and tables.
#'
#' @format A character vector of length 7.
#'
#' @examples
#' factor(c("CHC", "SHC", "RH"), levels = facility_size_order, ordered = TRUE)
#'
#' @export
facility_size_order <- c("SHC", "BHC", "CHC", "CHC+", "DH", "PH", "RH")



