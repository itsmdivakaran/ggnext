# Theme system -----------------------------------------------------------------
#
# A theme is a plain bag of visual constants for the plot chrome (never for
# data marks — those come from aesthetics and geom params). The theme rides
# inside the computed-geometry buffer, so both render targets consume the
# same values and never hard-code colors of their own.

#' @include classes.R
NULL

# Theme property defaults, in one place: `theme()` validates against these
# names, presets override a subset, and theme_to_list() serializes them all.
THEME_DEFAULTS <- list(
  name = "ggnext",
  # Colors
  background = "#FFFFFF",
  panel_fill = "#F4F4F6",
  grid_color = "#FFFFFF",
  grid_color_minor = "",
  axis_color = "#3A3A3A",
  label_color = "#3A3A3A",
  title_color = "#1A1A22",
  subtitle_color = "#5A5A66",
  strip_fill = "#E4E4EA",
  strip_color = "#2A2A33",
  legend_text_color = "#3A3A3A",
  panel_border = "",
  # Typography
  font = "Helvetica, Arial, sans-serif",
  title_font = "",
  tick_font_size = 11,
  title_font_size = 13,
  plot_title_size = 17,
  plot_subtitle_size = 12,
  caption_size = 10,
  strip_font_size = 11,
  legend_font_size = 11,
  title_face = "bold",
  # Structure
  tick_len = 5,
  grid_major_x = TRUE,
  grid_major_y = TRUE,
  axis_line_x = TRUE,
  axis_line_y = TRUE,
  ticks_x = TRUE,
  ticks_y = TRUE,
  axis_text_x = TRUE,
  axis_text_y = TRUE,
  axis_title_x = TRUE,
  axis_title_y = TRUE,
  legend_position = "right",
  point_palette = character(),
  gradient_low = "",
  gradient_high = ""
)

# The Theme class itself; documented with its theme() constructor (separate
# topics would collide as Theme.Rd vs theme.Rd on case-insensitive
# filesystems).

#' @rdname theme
#' @param name Preset name (informational).
#' @param background Plot background color.
#' @param panel_fill Panel (data area) background color.
#' @param grid_color Major gridline color; `""` hides major gridlines.
#' @param grid_color_minor Minor gridline color; `""` (default) draws none.
#' @param axis_color Axis line and tick color.
#' @param label_color Tick label and axis title color.
#' @param title_color Plot title color.
#' @param subtitle_color Subtitle and caption color.
#' @param strip_fill,strip_color Facet strip background and text colors.
#' @param legend_text_color Legend label color.
#' @param panel_border Panel border color; `""` (default) draws none.
#' @param font CSS font-family stack used for all text.
#' @param title_font Font stack for the plot title; `""` inherits `font`.
#' @param tick_font_size Tick label size (px).
#' @param title_font_size Axis title size (px).
#' @param plot_title_size,plot_subtitle_size,caption_size Title block sizes (px).
#' @param strip_font_size Facet strip label size (px).
#' @param legend_font_size Legend label size (px).
#' @param title_face Plot title weight: `"bold"` or `"normal"`.
#' @param tick_len Tick mark length (px).
#' @param grid_major_x,grid_major_y Draw major gridlines per axis.
#' @param axis_line_x,axis_line_y Draw the axis lines.
#' @param ticks_x,ticks_y Draw tick marks.
#' @param axis_text_x,axis_text_y Draw tick labels.
#' @param axis_title_x,axis_title_y Draw axis titles.
#' @param legend_position `"right"`, `"bottom"`, or `"none"`.
#' @param point_palette Optional character vector overriding the discrete
#'   color palette for this plot.
#' @param gradient_low,gradient_high Optional continuous-gradient endpoints.
#' @export
Theme <- new_class("Theme", properties = list(
  name = class_character,
  background = class_character,
  panel_fill = class_character,
  grid_color = class_character,
  grid_color_minor = class_character,
  axis_color = class_character,
  label_color = class_character,
  title_color = class_character,
  subtitle_color = class_character,
  strip_fill = class_character,
  strip_color = class_character,
  legend_text_color = class_character,
  panel_border = class_character,
  font = class_character,
  title_font = class_character,
  tick_font_size = class_numeric,
  title_font_size = class_numeric,
  plot_title_size = class_numeric,
  plot_subtitle_size = class_numeric,
  caption_size = class_numeric,
  strip_font_size = class_numeric,
  legend_font_size = class_numeric,
  title_face = class_character,
  tick_len = class_numeric,
  grid_major_x = class_logical,
  grid_major_y = class_logical,
  axis_line_x = class_logical,
  axis_line_y = class_logical,
  ticks_x = class_logical,
  ticks_y = class_logical,
  axis_text_x = class_logical,
  axis_text_y = class_logical,
  axis_title_x = class_logical,
  axis_title_y = class_logical,
  legend_position = class_character,
  point_palette = class_character,
  gradient_low = class_character,
  gradient_high = class_character
))

# Build a Theme from THEME_DEFAULTS plus overrides — every preset routes
# through here, so adding a property means editing one list.
make_theme <- function(...) {
  settings <- utils::modifyList(THEME_DEFAULTS, list(...))
  do.call(Theme, settings)
}

#' The default ggnext theme
#'
#' Light background with a softly tinted panel and white gridlines.
#'
#' @return A [Theme] to add to a plot with `+`.
#' @examples
#' p <- ggnext(cars, aes(speed, dist)) + geom_point() + theme_dark()
#' @export
theme_ggnext <- function() make_theme()

#' Minimal theme: no panel fill, light grey gridlines
#'
#' @return A [Theme] to add to a plot with `+`.
#' @export
theme_minimal <- function() {
  make_theme(
    name = "minimal",
    panel_fill = "#FFFFFF",
    grid_color = "#E4E4E8",
    axis_line_x = FALSE,
    axis_line_y = FALSE,
    ticks_x = FALSE,
    ticks_y = FALSE,
    strip_fill = "#FFFFFF"
  )
}

#' Classic theme: white panel, black axes, no gridlines
#'
#' The traditional statistical-journal look — axis lines and ticks only.
#'
#' @return A [Theme] to add to a plot with `+`.
#' @export
theme_classic <- function() {
  make_theme(
    name = "classic",
    panel_fill = "#FFFFFF",
    grid_color = "",
    axis_color = "#000000",
    label_color = "#000000",
    title_color = "#000000",
    font = "Times, 'Times New Roman', Georgia, serif",
    strip_fill = "#FFFFFF",
    title_face = "normal"
  )
}

#' Modern theme: airy, high-contrast, horizontal rules only
#'
#' Editorial styling — a large title, hairline horizontal gridlines, no
#' vertical grid or axis lines.
#'
#' @return A [Theme] to add to a plot with `+`.
#' @export
theme_modern <- function() {
  make_theme(
    name = "modern",
    background = "#FDFDFC",
    panel_fill = "#FDFDFC",
    grid_color = "#E8E6E1",
    grid_major_x = FALSE,
    axis_color = "#9A968E",
    label_color = "#6A665E",
    title_color = "#16150F",
    subtitle_color = "#6A665E",
    axis_line_y = FALSE,
    ticks_y = FALSE,
    plot_title_size = 19,
    strip_fill = "#F2F0EB",
    point_palette = c(
      "#2B6BE0", "#E05A2B", "#12A594", "#B54AC8",
      "#E0A215", "#4A5568", "#D14D72", "#5B8C2A"
    )
  )
}

#' Dark theme
#'
#' @return A [Theme] to add to a plot with `+`.
#' @export
theme_dark <- function() {
  make_theme(
    name = "dark",
    background = "#1C1C22",
    panel_fill = "#26262E",
    grid_color = "#3A3A46",
    axis_color = "#B8B8C4",
    label_color = "#D8D8E0",
    title_color = "#F2F2F7",
    subtitle_color = "#A0A0B0",
    strip_fill = "#32323C",
    strip_color = "#E8E8F0",
    legend_text_color = "#D8D8E0",
    point_palette = c(
      "#7AA2F7", "#F7A072", "#54D6B4", "#C4A7F7",
      "#F7D774", "#8CD98C", "#F78CA8", "#9FB4C7"
    )
  )
}

#' Void theme: data only, no chrome at all
#'
#' Useful for treemaps, network graphs, and sparkline-style output.
#'
#' @return A [Theme] to add to a plot with `+`.
#' @export
theme_void <- function() {
  make_theme(
    name = "void",
    panel_fill = "#FFFFFF",
    grid_color = "",
    axis_line_x = FALSE,
    axis_line_y = FALSE,
    ticks_x = FALSE,
    ticks_y = FALSE,
    axis_text_x = FALSE,
    axis_text_y = FALSE,
    axis_title_x = FALSE,
    axis_title_y = FALSE,
    strip_fill = "#FFFFFF"
  )
}

#' Customize theme settings
#'
#' Overrides individual theme settings. By default the overrides apply to
#' [theme_ggnext()]; pass `base` to restyle a different preset.
#'
#' @param ... Named [Theme] properties to override, e.g.
#'   `theme(panel_fill = "white", grid_color = "grey85")`.
#' @param base A [Theme] to start from; defaults to [theme_ggnext()].
#' @return A [Theme] to add to a plot with `+`.
#' @examples
#' ggnext(cars, aes(speed, dist)) + geom_point() +
#'   theme(panel_fill = "#FFF8F0", grid_major_x = FALSE)
#'
#' # Restyle a preset rather than the default.
#' ggnext(cars, aes(speed, dist)) + geom_point() +
#'   theme(base = theme_dark(), plot_title_size = 22)
#' @export
theme <- function(..., base = NULL) {
  overrides <- list(...)
  out <- base %||% theme_ggnext()
  if (!S7_inherits(out, Theme)) {
    stop("`base` must be a Theme object, e.g. theme_minimal().")
  }
  if (length(overrides) > 0 &&
      (is.null(names(overrides)) || any(names(overrides) == ""))) {
    stop("All theme() arguments must be named.")
  }
  known <- names(THEME_DEFAULTS)
  unknown <- setdiff(names(overrides), known)
  if (length(unknown) > 0) {
    stop(
      "Unknown theme setting(s): ", paste(unknown, collapse = ", "),
      ". Available: ", paste(setdiff(known, "name"), collapse = ", "), "."
    )
  }
  for (nm in names(overrides)) {
    prop(out, nm) <- overrides[[nm]]
  }
  out@name <- "custom"
  out
}

# Flatten a Theme into the plain list the geometry buffer carries. Color
# values are normalized to hex so renderers never re-interpret R color names;
# "" stays "" and means "do not draw this element".
theme_to_list <- function(th) {
  hex <- function(x) if (identical(x, "")) "" else col_to_hex(x)
  out <- lapply(names(THEME_DEFAULTS), function(nm) prop(th, nm))
  names(out) <- names(THEME_DEFAULTS)
  for (nm in c("background", "panel_fill", "grid_color", "grid_color_minor",
               "axis_color", "label_color", "title_color", "subtitle_color",
               "strip_fill", "strip_color", "legend_text_color",
               "panel_border", "gradient_low", "gradient_high")) {
    out[[nm]] <- hex(out[[nm]])
  }
  # The title font falls back to the body font here so renderers never have
  # to implement the fallback themselves.
  if (identical(out$title_font, "")) out$title_font <- out$font
  out$point_palette <- if (length(th@point_palette) > 0) {
    as.list(col_to_hex(th@point_palette))
  } else {
    list()
  }
  out
}
