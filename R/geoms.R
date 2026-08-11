# Core geom catalog ------------------------------------------------------------
#
# Each geom is an S7 subclass of Geom plus one build_marks() method that
# turns scaled values into marks (see R/marks.R), plus a user-facing
# constructor returning a Layer. Geoms never touch renderer code.
#
# Conventions inside build_marks():
#   - `scaled` holds normalized position columns, mapped color/size/alpha,
#     the group column, merged literal params (`scaled$params`), and the
#     axis spans (`scaled$xspan`/`yspan`) for converting data-space widths.
#   - Group-wise geoms split row indices on `scaled$group`; each group
#     inherits the color/alpha of its first row.
#   - Mark lists are unnamed (names would turn JSON arrays into objects).

#' @include classes.R stats.R marks.R
NULL

# Split row indices by group, preserving first-appearance order.
group_rows <- function(scaled) {
  g <- scaled$group %||% rep("all", length(scaled$x))
  unname(split(seq_along(g), factor(g, levels = unique(g))))
}

# Drop NULL entries from a params list (so user NULLs fall through to
# geom defaults).
drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

# Convert a series into a staircase: between successive points, insert the
# corner (x_{i+1}, y_i) so the line holds its value and then steps. Shared
# by geom_step(), geom_km(), and geom_cuminc(), which all draw step
# functions of one kind or another. Input must already be x-ordered.
step_path <- function(x, y) {
  n <- length(x)
  if (n < 2) {
    return(list(x = x, y = y))
  }
  list(
    x = c(x[1], rep(x[-1], each = 2)),
    y = c(rep(y[-n], each = 2), y[n])
  )
}

# Standard Layer construction for a geom constructor: drops the user's
# unset params so geom defaults apply.
layer_new <- function(geom, stat, mapping, data, params = list(),
                      inherit = TRUE) {
  Layer(
    geom = geom, stat = stat, mapping = mapping, data = data,
    params = drop_null(params), inherit = inherit
  )
}

# Internal aes builder for geoms that synthesize their own data
# (hline/vline): maps column names onto aesthetics without user input.
aes_internal <- function(...) {
  cols <- c(...)
  structure(
    list(
      exprs = stats::setNames(lapply(cols, as.name), names(cols)),
      env = baseenv()
    ),
    class = "ggplot3_aes"
  )
}

# --- lines: line / path / step -----------------------------------------------

#' GeomLine: one polyline per group, ordered by x
#' @noRd
GeomLine <- new_class("GeomLine", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "line",
      default_params = list(color = "#000000", linewidth = 1.5, alpha = 1)
    ))
  }
)

method(build_marks, GeomLine) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(group_rows(scaled), function(idx) {
    idx <- idx[order(scaled$x[idx])]
    mk_line(
      scaled$x[idx], scaled$y[idx],
      stroke = scaled$color[idx][1], width = p$linewidth,
      alpha = scaled$alpha[idx][1], dash = p$dash %||% ""
    )
  }))
}

#' Line layer (points connected in x order)
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param linewidth Stroke width in px.
#' @param dash SVG dash pattern (e.g. `"4,3"`); `""` for solid.
#' @return A [Layer].
#' @export
geom_line <- function(mapping = NULL, data = NULL, color = NULL,
                      linewidth = NULL, alpha = NULL, dash = NULL) {
  Layer(
    geom = GeomLine(), stat = stat_identity(), mapping = mapping, data = data,
    params = drop_null(list(color = color, linewidth = linewidth,
                            alpha = alpha, dash = dash))
  )
}

#' GeomPath: one polyline per group, in data order
#' @noRd
GeomPath <- new_class("GeomPath", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "path",
      default_params = list(color = "#000000", linewidth = 1.5, alpha = 1)
    ))
  }
)

method(build_marks, GeomPath) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(group_rows(scaled), function(idx) {
    mk_line(
      scaled$x[idx], scaled$y[idx],
      stroke = scaled$color[idx][1], width = p$linewidth,
      alpha = scaled$alpha[idx][1], dash = p$dash %||% ""
    )
  }))
}

#' Path layer (points connected in data order)
#' @inheritParams geom_line
#' @return A [Layer].
#' @export
geom_path <- function(mapping = NULL, data = NULL, color = NULL,
                      linewidth = NULL, alpha = NULL, dash = NULL) {
  Layer(
    geom = GeomPath(), stat = stat_identity(), mapping = mapping, data = data,
    params = drop_null(list(color = color, linewidth = linewidth,
                            alpha = alpha, dash = dash))
  )
}

#' GeomStep: horizontal-then-vertical steps between points
#' @noRd
GeomStep <- new_class("GeomStep", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "step",
      default_params = list(color = "#000000", linewidth = 1.5, alpha = 1)
    ))
  }
)

method(build_marks, GeomStep) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(group_rows(scaled), function(idx) {
    idx <- idx[order(scaled$x[idx])]
    st <- step_path(scaled$x[idx], scaled$y[idx])
    mk_line(
      st$x, st$y, stroke = scaled$color[idx][1], width = p$linewidth,
      alpha = scaled$alpha[idx][1], dash = p$dash %||% ""
    )
  }))
}

#' Step layer (staircase line)
#' @inheritParams geom_line
#' @return A [Layer].
#' @export
geom_step <- function(mapping = NULL, data = NULL, color = NULL,
                      linewidth = NULL, alpha = NULL, dash = NULL) {
  Layer(
    geom = GeomStep(), stat = stat_identity(), mapping = mapping, data = data,
    params = drop_null(list(color = color, linewidth = linewidth,
                            alpha = alpha, dash = dash))
  )
}

# --- filled: area / ribbon ---------------------------------------------------

#' GeomArea: filled region between the line and the zero baseline
#' @noRd
GeomArea <- new_class("GeomArea", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "area",
      default_params = list(color = "#4A6DB5", alpha = 0.6)
    ))
  }
)

method(build_marks, GeomArea) <- function(geom, scaled) {
  unname(lapply(group_rows(scaled), function(idx) {
    idx <- idx[order(scaled$x[idx])]
    mk_polygon(
      c(scaled$x[idx], rev(scaled$x[idx])),
      c(scaled$y[idx], rev(scaled$ymin[idx])),
      fill = scaled$color[idx][1], alpha = scaled$alpha[idx][1]
    )
  }))
}

#' Area layer (filled to the zero baseline)
#' @inheritParams geom_line
#' @return A [Layer].
#' @export
geom_area <- function(mapping = NULL, data = NULL, color = NULL, alpha = NULL) {
  Layer(
    geom = GeomArea(), stat = StatArea(), mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha))
  )
}

#' GeomRibbon: filled band between ymin and ymax
#' @noRd
GeomRibbon <- new_class("GeomRibbon", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "ribbon",
      default_params = list(color = "#4A6DB5", alpha = 0.4),
      required_aes = c("x", "ymin", "ymax")
    ))
  }
)

method(build_marks, GeomRibbon) <- function(geom, scaled) {
  unname(lapply(group_rows(scaled), function(idx) {
    idx <- idx[order(scaled$x[idx])]
    mk_polygon(
      c(scaled$x[idx], rev(scaled$x[idx])),
      c(scaled$ymax[idx], rev(scaled$ymin[idx])),
      fill = scaled$color[idx][1], alpha = scaled$alpha[idx][1]
    )
  }))
}

#' Ribbon layer (band between `ymin` and `ymax`)
#' @inheritParams geom_line
#' @return A [Layer].
#' @export
geom_ribbon <- function(mapping = NULL, data = NULL, color = NULL, alpha = NULL) {
  Layer(
    geom = GeomRibbon(), stat = stat_identity(), mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha))
  )
}

# --- segments and reference lines --------------------------------------------

#' GeomSegment: one straight segment per row
#' @noRd
GeomSegment <- new_class("GeomSegment", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "segment",
      default_params = list(color = "#000000", linewidth = 1.5, alpha = 1),
      required_aes = c("x", "y", "xend", "yend")
    ))
  }
)

method(build_marks, GeomSegment) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(seq_along(scaled$x), function(i) {
    mk_line(
      c(scaled$x[[i]], scaled$xend[[i]]),
      c(scaled$y[[i]], scaled$yend[[i]]),
      stroke = scaled$color[[i]], width = p$linewidth, alpha = scaled$alpha[[i]]
    )
  }))
}

#' Segment layer (`x`,`y`) to (`xend`,`yend`)
#' @inheritParams geom_line
#' @return A [Layer].
#' @export
geom_segment <- function(mapping = NULL, data = NULL, color = NULL,
                         linewidth = NULL, alpha = NULL) {
  Layer(
    geom = GeomSegment(), stat = stat_identity(), mapping = mapping, data = data,
    params = drop_null(list(color = color, linewidth = linewidth, alpha = alpha))
  )
}

#' GeomHline: horizontal reference line(s)
#' @noRd
GeomHline <- new_class("GeomHline", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "hline",
      default_params = list(color = "#8A8A94", linewidth = 1, alpha = 1),
      required_aes = "y"
    ))
  }
)

method(build_marks, GeomHline) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(seq_along(scaled$y), function(i) {
    # Reference lines span the full panel in normalized space.
    mk_line(
      c(0, 1), rep(scaled$y[[i]], 2),
      stroke = scaled$color[[i]], width = p$linewidth,
      alpha = scaled$alpha[[i]], dash = p$dash %||% "4,3"
    )
  }))
}

#' Horizontal reference line
#'
#' @param yintercept Numeric vector of y positions.
#' @param color,linewidth,alpha Styling.
#' @param dash Dash pattern; defaults to `"4,3"` (dashed).
#' @return A [Layer].
#' @export
geom_hline <- function(yintercept, color = NULL, linewidth = NULL,
                       alpha = NULL, dash = NULL) {
  Layer(
    geom = GeomHline(), stat = stat_identity(),
    mapping = aes_internal(y = "y"), inherit = FALSE,
    data = data.frame(y = yintercept),
    params = drop_null(list(color = color, linewidth = linewidth,
                            alpha = alpha, dash = dash))
  )
}

#' GeomVline: vertical reference line(s)
#' @noRd
GeomVline <- new_class("GeomVline", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "vline",
      default_params = list(color = "#8A8A94", linewidth = 1, alpha = 1),
      required_aes = "x"
    ))
  }
)

method(build_marks, GeomVline) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(seq_along(scaled$x), function(i) {
    mk_line(
      rep(scaled$x[[i]], 2), c(0, 1),
      stroke = scaled$color[[i]], width = p$linewidth,
      alpha = scaled$alpha[[i]], dash = p$dash %||% "4,3"
    )
  }))
}

#' Vertical reference line
#'
#' @param xintercept Numeric vector of x positions.
#' @param color,linewidth,alpha Styling.
#' @param dash Dash pattern; defaults to `"4,3"` (dashed).
#' @return A [Layer].
#' @export
geom_vline <- function(xintercept, color = NULL, linewidth = NULL,
                       alpha = NULL, dash = NULL) {
  Layer(
    geom = GeomVline(), stat = stat_identity(),
    mapping = aes_internal(x = "x"), inherit = FALSE,
    data = data.frame(x = xintercept),
    params = drop_null(list(color = color, linewidth = linewidth,
                            alpha = alpha, dash = dash))
  )
}

# --- text and tiles ----------------------------------------------------------

#' GeomText: one label per row
#' @noRd
GeomText <- new_class("GeomText", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "text",
      default_params = list(color = "#000000", fontsize = 11, alpha = 1),
      required_aes = c("x", "y", "label")
    ))
  }
)

method(build_marks, GeomText) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(seq_along(scaled$x), function(i) {
    mk_text(
      scaled$x[[i]], scaled$y[[i]], scaled$label[[i]],
      size = p$fontsize, color = scaled$color[[i]],
      alpha = scaled$alpha[[i]], anchor = p$anchor %||% "middle"
    )
  }))
}

#' Text label layer
#'
#' @param mapping,data,color,alpha As in [geom_point()]; map `label`.
#' @param fontsize Font size in px.
#' @param anchor `"middle"`, `"start"`, or `"end"`.
#' @return A [Layer].
#' @export
geom_text <- function(mapping = NULL, data = NULL, color = NULL,
                      fontsize = NULL, alpha = NULL, anchor = NULL) {
  Layer(
    geom = GeomText(), stat = stat_identity(), mapping = mapping, data = data,
    params = drop_null(list(color = color, fontsize = fontsize,
                            alpha = alpha, anchor = anchor))
  )
}

#' StatTile: expand tile centers into rect corners
#'
#' @param width,height Tile size in data units; default to the data
#'   resolution so adjacent tiles touch.
#' @noRd
StatTile <- new_class("StatTile", parent = Stat,
  properties = list(width = class_any, height = class_any),
  constructor = function(width = NULL, height = NULL) {
    new_object(Stat(name = "tile"), width = width, height = height)
  }
)

method(compute_stat, StatTile) <- function(stat, values) {
  w <- stat@width %||% resolution(values$x)
  h <- stat@height %||% resolution(values$y)
  values$xmin <- values$x - w / 2
  values$xmax <- values$x + w / 2
  values$ymin <- values$y - h / 2
  values$ymax <- values$y + h / 2
  values
}

#' GeomTile: one filled rectangle per row, centered on (x, y)
#' @noRd
GeomTile <- new_class("GeomTile", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "tile",
      default_params = list(color = "#4A6DB5", alpha = 1)
    ))
  }
)

method(build_marks, GeomTile) <- function(geom, scaled) {
  unname(lapply(seq_along(scaled$x), function(i) {
    mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]],
      scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$alpha[[i]]
    )
  }))
}

#' Tile layer (heatmap cells)
#'
#' Map a continuous `color` for the classic heatmap look.
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param width,height Tile size in data units (default: data resolution).
#' @return A [Layer].
#' @export
geom_tile <- function(mapping = NULL, data = NULL, color = NULL, alpha = NULL,
                      width = NULL, height = NULL) {
  Layer(
    geom = GeomTile(), stat = StatTile(width = width, height = height),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha))
  )
}

# --- bars: bar / col / histogram ---------------------------------------------

#' GeomBar: one rectangle per (possibly stacked/dodged) bar
#' @param name Geom display name (`"bar"`, `"col"`, or `"histogram"` — the
#'   three bar-shaped layers share this class).
#' @noRd
GeomBar <- new_class("GeomBar", parent = Geom,
  constructor = function(name = "bar") {
    new_object(Geom(
      name = name,
      default_params = list(color = "#4A6DB5", alpha = 1, position = "stack"),
      required_aes = "x"
    ))
  }
)

method(build_marks, GeomBar) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(seq_along(scaled$xmin), function(i) {
    mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]],
      scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$alpha[[i]],
      stroke = p$stroke %||% "", stroke_width = p$stroke_width %||% 0
    )
  }))
}

#' Bar chart layer (counts per category)
#'
#' Counts rows at each x. For pre-computed heights use [geom_col()].
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param width Bar width as a fraction of the slot.
#' @param position `"stack"` (default), `"dodge"`, or `"identity"`.
#' @return A [Layer].
#' @export
geom_bar <- function(mapping = NULL, data = NULL, color = NULL, alpha = NULL,
                     width = 0.8, position = "stack") {
  Layer(
    geom = GeomBar(), stat = stat_count(width = width),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, position = position))
  )
}

#' Column chart layer (bar heights from the data)
#'
#' @inheritParams geom_bar
#' @return A [Layer].
#' @export
geom_col <- function(mapping = NULL, data = NULL, color = NULL, alpha = NULL,
                     width = 0.8, position = "stack") {
  Layer(
    geom = GeomBar(name = "col"), stat = StatCol(width = width),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, position = position))
  )
}

#' Histogram layer
#'
#' @inheritParams geom_bar
#' @param bins Number of bins (ignored when `binwidth` is given).
#' @param binwidth Bin width in data units.
#' @return A [Layer].
#' @export
geom_histogram <- function(mapping = NULL, data = NULL, color = NULL,
                           alpha = NULL, bins = 30, binwidth = NULL,
                           position = "stack") {
  Layer(
    geom = GeomBar(name = "histogram"),
    stat = stat_bin(bins = bins, binwidth = binwidth),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, position = position))
  )
}

# --- distributions: density / boxplot / violin -------------------------------

#' GeomDensity: filled density curve
#' @noRd
GeomDensity <- new_class("GeomDensity", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "density",
      default_params = list(color = "#4A6DB5", linewidth = 1.5, alpha = 0.35),
      required_aes = "x"
    ))
  }
)

method(build_marks, GeomDensity) <- function(geom, scaled) {
  p <- scaled$params
  unname(unlist(lapply(group_rows(scaled), function(idx) {
    idx <- idx[order(scaled$x[idx])]
    col <- scaled$color[idx][1]
    list(
      mk_polygon(
        c(scaled$x[idx], rev(scaled$x[idx])),
        c(scaled$y[idx], rev(scaled$ymin[idx])),
        fill = col, alpha = scaled$alpha[idx][1]
      ),
      mk_line(scaled$x[idx], scaled$y[idx], stroke = col, width = p$linewidth)
    )
  }), recursive = FALSE))
}

#' Density curve layer
#'
#' @param mapping,data,color,alpha As in [geom_point()]; map only `x`.
#' @param linewidth Curve stroke width.
#' @param adjust Bandwidth multiplier.
#' @return A [Layer].
#' @export
geom_density <- function(mapping = NULL, data = NULL, color = NULL,
                         alpha = NULL, linewidth = NULL, adjust = 1) {
  Layer(
    geom = GeomDensity(), stat = stat_density(adjust = adjust),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, linewidth = linewidth))
  )
}

#' GeomBoxplot: Tukey box-and-whisker glyph per x slot
#' @noRd
GeomBoxplot <- new_class("GeomBoxplot", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "boxplot",
      default_params = list(color = "#4A6DB5", alpha = 0.6, linewidth = 1.2)
    ))
  }
)

method(build_marks, GeomBoxplot) <- function(geom, scaled) {
  p <- scaled$params
  marks <- list()
  for (i in seq_along(scaled$x)) {
    col <- scaled$color[[i]]
    if (identical(scaled$role[[i]], "outlier")) {
      marks[[length(marks) + 1]] <-
        mk_circle(scaled$x[[i]], scaled$y[[i]], r = 2, fill = col, alpha = 0.9)
      next
    }
    # Half-width in normalized units (xwidth is in data/slot units).
    half <- (scaled$xwidth[[i]] / 2) / scaled$xspan
    x <- scaled$x[[i]]
    lw <- p$linewidth
    marks <- c(marks, list(
      # Whisker stems.
      mk_line(c(x, x), c(scaled$upper[[i]], scaled$ymax[[i]]), stroke = col, width = lw),
      mk_line(c(x, x), c(scaled$lower[[i]], scaled$ymin[[i]]), stroke = col, width = lw),
      # Whisker caps.
      mk_line(c(x - half / 2, x + half / 2), rep(scaled$ymax[[i]], 2), stroke = col, width = lw),
      mk_line(c(x - half / 2, x + half / 2), rep(scaled$ymin[[i]], 2), stroke = col, width = lw),
      # Box.
      mk_rect(x - half, x + half, scaled$lower[[i]], scaled$upper[[i]],
              fill = col, alpha = scaled$alpha[[i]],
              stroke = col, stroke_width = lw),
      # Median line.
      mk_line(c(x - half, x + half), rep(scaled$middle[[i]], 2),
              stroke = col, width = lw * 1.5)
    ))
  }
  marks
}

#' Boxplot layer
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param width Box width in x slot units.
#' @param coef Whisker length multiplier (Tukey's 1.5 by default).
#' @return A [Layer].
#' @export
geom_boxplot <- function(mapping = NULL, data = NULL, color = NULL,
                         alpha = NULL, width = 0.6, coef = 1.5) {
  Layer(
    geom = GeomBoxplot(), stat = stat_boxplot(width = width, coef = coef),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha))
  )
}

#' GeomViolin: mirrored density polygon per x slot
#' @noRd
GeomViolin <- new_class("GeomViolin", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "violin",
      default_params = list(color = "#4A6DB5", alpha = 0.6, linewidth = 1)
    ))
  }
)

method(build_marks, GeomViolin) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(group_rows(scaled), function(idx) {
    col <- scaled$color[idx][1]
    mk_polygon(
      c(scaled$xmin[idx], rev(scaled$xmax[idx])),
      c(scaled$y[idx], rev(scaled$y[idx])),
      fill = col, alpha = scaled$alpha[idx][1],
      stroke = col, stroke_width = p$linewidth
    )
  }))
}

#' Violin layer
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param width Maximum violin width in x slot units.
#' @return A [Layer].
#' @export
geom_violin <- function(mapping = NULL, data = NULL, color = NULL,
                        alpha = NULL, width = 0.9) {
  Layer(
    geom = GeomViolin(), stat = stat_ydensity(width = width),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha))
  )
}

# --- smooth ------------------------------------------------------------------

#' GeomSmooth: trend line with optional confidence band
#' @noRd
GeomSmooth <- new_class("GeomSmooth", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "smooth",
      default_params = list(color = "#4A6DB5", linewidth = 2, alpha = 1)
    ))
  }
)

method(build_marks, GeomSmooth) <- function(geom, scaled) {
  p <- scaled$params
  unname(unlist(lapply(group_rows(scaled), function(idx) {
    idx <- idx[order(scaled$x[idx])]
    col <- scaled$color[idx][1]
    band <- if (!is.null(scaled$ymin)) {
      list(mk_polygon(
        c(scaled$x[idx], rev(scaled$x[idx])),
        c(scaled$ymax[idx], rev(scaled$ymin[idx])),
        fill = col, alpha = 0.2
      ))
    }
    c(band, list(
      mk_line(scaled$x[idx], scaled$y[idx], stroke = col, width = p$linewidth)
    ))
  }), recursive = FALSE))
}

#' Smoothed trend layer
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param method `"loess"` (default) or `"lm"`.
#' @param se Draw the confidence band.
#' @param linewidth Trend line width.
#' @param level Confidence level.
#' @return A [Layer].
#' @export
geom_smooth <- function(mapping = NULL, data = NULL, color = NULL,
                        alpha = NULL, method = "loess", se = TRUE,
                        linewidth = NULL, level = 0.95) {
  Layer(
    geom = GeomSmooth(), stat = stat_smooth(method = method, se = se, level = level),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, linewidth = linewidth))
  )
}

# --- jitter ------------------------------------------------------------------

#' Jittered point layer (strip charts)
#'
#' [geom_point()] with uniform positional noise; deterministic per `seed`.
#'
#' @param mapping,data,color,size,alpha As in [geom_point()].
#' @param width,height Jitter half-ranges in data units.
#' @param seed RNG seed.
#' @return A [Layer].
#' @export
geom_jitter <- function(mapping = NULL, data = NULL, color = NULL,
                        size = NULL, alpha = NULL, width = NULL,
                        height = NULL, seed = 42) {
  Layer(
    geom = GeomPoint(), stat = stat_jitter(width = width, height = height, seed = seed),
    mapping = mapping, data = data,
    params = drop_null(list(color = color, size = size, alpha = alpha))
  )
}
