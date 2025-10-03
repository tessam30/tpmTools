# Colors

# Official TPM Color Definitions ---------------------------------------------
#' TPM Official Colors
#'
#' Individual hex values for the official TPM Monitoring color system,
#' aligned with glitr-style palettes and designed for use in data visualization.
#' These colors are optimized for accessibility and colorblind-friendly contrasts.
#'
#' @format Character scalars of HEX codes.
#' @examples \dontrun{
#' tpm_red
#' tpm_blue_dark
#' tpm_colors["red"]
#' names(tpm_colors)
#' }
#' @name tpm_colors
#' @docType data
NULL

# Individual scalars -----------------------------------------------------------

#' @rdname tpm_colors
#' @export
tpm_blue_dark <- "#1F2A58"

#' @rdname tpm_colors
#' @export
tpm_blue_light <- "#60B7E7"

#' @rdname tpm_colors
#' @export
tpm_green_light <- "#7ACDCF"

#' @rdname tpm_colors
#' @export
tpm_green_dark <- "#307B7E"

#' @rdname tpm_colors
#' @export
tpm_green_mid <- "#25918B"

#' @rdname tpm_colors
#' @export
tpm_red <- "#D73639"

#' @rdname tpm_colors
#' @export
tpm_yellow <- "#F7B538"

#' @rdname tpm_colors
#' @export
tpm_maroon <- "#740F21"

#' @rdname tpm_colors
#' @export
tpm_purple <- "#68226D"

#' @rdname tpm_colors
#' @export
tpm_gray_dark <- "#415469"

#' @rdname tpm_colors
#' @export
tpm_gray_mid <- "#657C91"

#' @rdname tpm_colors
#' @export
tpm_gray_light <- "#B5C3CC"

# Helper vector ---------------------------------------------------------------

#' @rdname tpm_colors
#' @export
tpm_colors <- c(
  blue_dark   = tpm_blue_dark,
  blue_light  = tpm_blue_light,
  green_light = tpm_green_light,
  green_dark  = tpm_green_dark,
  green_mid   = tpm_green_mid,
  red         = tpm_red,
  yellow      = tpm_yellow,
  maroon      = tpm_maroon,
  purple      = tpm_purple,
  gray_dark   = tpm_gray_dark,
  gray_mid    = tpm_gray_mid,
  gray_light  = tpm_gray_light
)





# TPM Color Palettes ----------------------------------------------------------


#' TPM Color Palettes
#'
#' A collection of categorical, sequential, and diverging palettes based on the TPM official colors.
#' These palettes are designed to be visually distinct, accessible, and consistent with
#' best practices in data visualization.
#'
#' @details
#' Available palettes:
#' \itemize{
#'   \item \code{"cat"} — extended categorical palette
#'   \item \code{"seq_blue"} — sequential palette from light to dark blue
#'   \item \code{"seq_red"} — sequential palette from light to dark red
#'   \item \code{"div_blue_red"} — diverging palette (blue ↔ red)
#'   \item \code{"div_green_purple"} — diverging palette (green ↔ purple)
#' }
#'
#' @param palette Character name of the palette to return.
#' @return A character vector of hex colors.
#' @examples \dontrun{
#' tpm_pal("cat")
#' tpm_pal("seq_blue")
#' }
#' @export
tpm_pal <- function(palette = c("cat", "seq_blue", "seq_red",
                                "div_blue_red", "div_green_purple")) {
  palette <- match.arg(palette)
  switch(palette,
         "cat"              = c(tpm_blue_dark, tpm_blue_light, tpm_green_light,
                                tpm_green_dark, tpm_green_mid, tpm_red, tpm_yellow,
                                tpm_maroon, tpm_purple, tpm_gray_dark, tpm_gray_mid, tpm_gray_light),
         "seq_blue"         = c(tpm_gray_light, tpm_blue_light, tpm_blue_dark),
         "seq_red"          = c("#fde0e2", "#f28b8e", tpm_red, "#7f1419"),
         "div_blue_red"     = c(tpm_blue_light, "#FFFFFF", tpm_red),
         "div_green_purple" = c(tpm_green_light, "#FFFFFF", tpm_purple)
  )
}


# Continuous Diverging Palettes ---------------------------------------------

#' TPM Continuous Diverging Palettes
#'
#' Generate continuous color ramps from TPM diverging palettes
#' for use in ggplot2 and base R graphics.
#'
#' @param palette Character, either \code{"div_blue_red"} or \code{"div_green_purple"}.
#' @param n Number of colors to return (defaults to 256).
#' @return A character vector of hex colors.
#' @examples \donttest{
#' # Get 5 colors from each
#' tpm_pal_cont("div_blue_red", 5)
#' tpm_pal_cont("div_green_purple", 5)
#'
#' # Example with ggplot2
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, color = mpg)) +
#'   geom_point(size = 4) +
#'   scale_color_gradientn(colors = tpm_pal_cont("div_blue_red"))
#' }
#' @export
tpm_pal_cont <- function(palette = c("div_blue_red", "div_green_purple", "seq_blue", "seq_red"),
                         n = 256) {
  if (is.character(palette) && length(palette) == 1) {
    palette <- match.arg(palette)
    pal <- switch(palette,
                  "div_blue_red"     = c(tpm_blue_light, "#FFFFFF", tpm_red),
                  "div_green_purple" = c(tpm_green_light, "#FFFFFF", tpm_purple),
                  "seq_blue"         = c(tpm_gray_light, tpm_blue_light, tpm_blue_dark),
                  "seq_red"          = c("#fde0e2", "#f28b8e", tpm_red, "#7f1419"))
  } else {
    # treat as a custom vector of colors
    pal <- palette
  }
  grDevices::colorRampPalette(pal)(n)
}


#' ggplot2 Scales using TPM Palettes
#'
#' Color and fill scales for ggplot2 based on TPM official palettes.
#'
#' @inheritParams tpm_pal
#' @param ... Additional arguments passed to \code{ggplot2::scale_*_manual}.
#'
#' @examples  \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_tpm("cat")
#'
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_tpm("seq_blue")
#' }
#' @export
scale_color_tpm <- function(palette = "cat", ...) {
  ggplot2::scale_color_manual(values = tpm_pal(palette), ...)
}

#' @rdname scale_color_tpm
#' @export
scale_fill_tpm <- function(palette = "cat", ...) {
  ggplot2::scale_fill_manual(values = tpm_pal(palette), ...)
}


#' Show TPM Palettes
#'
#' Display TPM official palettes as swatches for quick reference.
#' This function is inspired by \code{glitr::si_palettes}.
#'
#' @param palettes Character vector of palette names to display.
#'   Defaults to all available palettes.
#' @param ncol Number of columns in the display.
#' @examples \donttest{
#' tpm_show_palettes()
#' tpm_show_palettes(c("cat", "seq_blue"))
#' }
#' @export
tpm_show_palettes <- function(palettes = c("cat", "seq_blue", "seq_red",
                                           "div_blue_red", "div_green_purple"),
                              ncol = 1) {
  palettes <- match.arg(palettes, several.ok = TRUE)

  # Collect palettes
  pal_list <- lapply(palettes, tpm_pal)
  names(pal_list) <- palettes

  # Plot
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(length(palettes), ncol), mar = c(1, 1, 3, 1))

  for (i in seq_along(pal_list)) {
    n <- length(pal_list[[i]])
    barplot(rep(1, n), col = pal_list[[i]], border = NA, axes = FALSE)
    title(names(pal_list)[i], line = -1, cex.main = 0.9)
  }
}

#' Show TPM Colors
#'
#' Display TPM brand colors in a grid with HEX codes overlaid.
#' Text color (black/white) is chosen automatically for readability.
#'
#' @param colours A character vector of hex codes. Defaults to TPM official colors.
#' @param labels Logical, whether to show hex code labels. Default = TRUE.
#' @param borders Border color for swatches. Default = NA.
#' @param cex_label Size of text labels. Default = 1.
#' @param ncol Number of columns in the grid. Defaults to square layout.
#' @examples \donttest{
#' tpm_show_colors()
#' tpm_show_colors(ncol = 6)  # wider grid
#' }
#' @export
tpm_show_colors <- function(
    colours = c(
      tpm_blue_dark,
      tpm_blue_light,
      tpm_green_light,
      tpm_green_dark,
      tpm_green_mid,
      tpm_red,
      tpm_yellow,
      tpm_maroon,
      tpm_purple,
      tpm_gray_dark,
      tpm_gray_mid,
      tpm_gray_light
    ),
    labels = TRUE,
    borders = "#FFFFFF",
    cex_label = 0.75,
    ncol = NULL
) {
  n <- length(colours)

  # default to square grid
  if (is.null(ncol)) {
    ncol <- ceiling(sqrt(n))
  }
  nrow <- ceiling(n / ncol)

  # fill up with NA if not perfect rectangle
  colours <- c(colours, rep(NA, nrow * ncol - n))
  colours <- matrix(colours, ncol = ncol, byrow = TRUE)

  oldpar <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(oldpar))

  size <- max(dim(colours))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "", axes = FALSE)

  rect(col(colours) - 1, -row(colours) + 1,
       col(colours), -row(colours),
       col = colours, border = borders)

  if (labels) {
    # choose white/black text based on luminance
    hcl <- farver::decode_colour(colours, "rgb", "hcl")
    label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
    text(col(colours) - 0.5,
         -row(colours) + 0.5,
         colours,
         cex = cex_label,
         col = label_col)
  }
}


#' Choose Contrasting Label Colors
#'
#' Given a vector of fill colors, returns black or white for text labels
#' depending on the luminance of the background.
#'
#' @param fill_colors A character vector of hex colors (backgrounds).
#' @param cutoff Luminance cutoff (0–100) above which labels are black.
#'   Default = 50.
#' @return A character vector of "black" or "white" for each fill.
#' @examples \dontrun{
#' fills <- c("#1f2a58", "#60B7E7", "#F7B538")
#' label_cols <- tpm_label_contrast(fills)
#' label_cols
#' }
#' @export
tpm_label_contrast <- function(fill_colors, cutoff = 50) {
  hcl <- farver::decode_colour(fill_colors, from = "rgb", to = "hcl")
  ifelse(hcl[, "l"] > cutoff, "black", "white")
}

#' Geom Text with Contrast-Aware Labels
#'
#' A wrapper around ggplot2::geom_text() that automatically chooses
#' black or white text depending on the luminance of the fill color.
#'
#' @inheritParams ggplot2::geom_text
#' @param cutoff Luminance cutoff (0–100) above which labels are black. Default = 50.
#' @examples \dontrun{
#' library(ggplot2)
#'
#' df <- expand.grid(x = 1:3, y = 1:3)
#' df$val <- runif(9)
#'
#' ggplot(df, aes(x, y, fill = val)) +
#'   geom_tile() +
#'   geom_text_contrast(aes(label = round(val,2))) +
#'   scale_fill_gradient(low = "white", high = "red")
#' }
#' @export
geom_text_contrast <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               cutoff = 50,
                               ...,
                               parse = FALSE, nudge_x = 0, nudge_y = 0,
                               check_overlap = FALSE,
                               na.rm = FALSE,
                               show.legend = FALSE,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    geom = ggplot2::GeomText,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(
      parse = parse,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...,
      # key trick: compute color dynamically after fill is scaled
      color = ggplot2::after_scale(tpm_label_contrast(fill, cutoff = cutoff))
    )
  )
}

