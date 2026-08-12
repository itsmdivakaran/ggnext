# Additional core geoms --------------------------------------------------------
#
# The remaining everyday layers a Grammar-of-Graphics user expects to find:
# explicit rectangles and polygons, a sloped reference line, labelled text,
# ranges and crossbars, marginal rugs, curves, spokes, and the count /
# frequency-polygon / stepped-function variants.
#
# All of them build on the five mark primitives in R/marks.R, so none of
# them required a renderer change.

#' @include classes.R stats.R marks.R geoms.R
NULL

# --- rectangles and polygons -------------------------------------------------

#' GeomRect: axis-aligned rectangles from explicit corners
#' @noRd
GeomRect <- new_class("GeomRect", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "rect",
      default_params = list(color = "#4A6DB5", alpha = 1, linewidth = 0),
      required_aes = c("xmin", "xmax", "ymin", "ymax")
    ))
  }
)

method(build_marks, GeomRect) <- function(geom, scaled) {
  p <- scaled$params
  lapply(seq_along(scaled$xmin), function(i) {
    mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$alpha[[i]],
      stroke = if ((p$linewidth %||% 0) > 0) p$border %||% "#FFFFFF" else "",
      stroke_width = p$linewidth %||% 0
    )
  })
}

#' Rectangles
#'
#' Draws an axis-aligned rectangle per row from explicit corners. Use it for
#' highlight bands, annotation boxes, and any hand-placed block; for a
#' regular grid shaded by value use [geom_tile()].
#'
#' @param mapping,data Standard layer overrides. Requires `xmin`, `xmax`,
#'   `ymin` and `ymax`.
#' @param color Fill colour when `color` is not mapped.
#' @param alpha Fill opacity.
#' @param linewidth Border width in px; `0` (default) draws no border.
#' @param border Border colour when `linewidth > 0`.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(x1 = c(1, 3), x2 = c(2, 5), y1 = c(1, 2), y2 = c(4, 3))
#' ggnext(d, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2)) + geom_rect()
#' @export
geom_rect <- function(mapping = NULL, data = NULL, color = NULL, alpha = NULL,
                      linewidth = NULL, border = NULL) {
  layer_new(GeomRect(), stat_identity(), mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth,
                 border = border))
}

#' GeomPolygon: one closed polygon per group
#' @noRd
GeomPolygon <- new_class("GeomPolygon", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "polygon",
      default_params = list(color = "#4A6DB5", alpha = 0.7, linewidth = 0)
    ))
  }
)

method(build_marks, GeomPolygon) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(group_rows(scaled), function(idx) {
    mk_polygon(
      scaled$x[idx], scaled$y[idx],
      fill = scaled$color[idx][1], alpha = scaled$alpha[idx][1],
      stroke = if ((p$linewidth %||% 0) > 0) p$border %||% "#333333" else "",
      stroke_width = p$linewidth %||% 0
    )
  }))
}

#' Polygons
#'
#' Joins the points of each group into a closed shape, in data order. Map
#' `group` when the data holds several polygons.
#'
#' @param mapping,data Standard layer overrides.
#' @param color Fill colour when `color` is not mapped.
#' @param alpha Fill opacity.
#' @param linewidth Outline width in px; `0` (default) draws no outline.
#' @param border Outline colour when `linewidth > 0`.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(x = c(1, 3, 2), y = c(1, 1, 3))
#' ggnext(d, aes(x, y)) + geom_polygon()
#' @export
geom_polygon <- function(mapping = NULL, data = NULL, color = NULL,
                         alpha = NULL, linewidth = NULL, border = NULL) {
  layer_new(GeomPolygon(), stat_identity(), mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth,
                 border = border))
}

#' Raster
#'
#' A tile grid drawn without cell borders — the right choice for a dense
#' heatmap or an image, where borders would swamp the data.
#'
#' @inheritParams geom_tile
#' @return A [Layer] to add with `+`.
#' @examples
#' g <- expand.grid(x = 1:20, y = 1:20)
#' g$z <- as.vector(outer(1:20, 1:20, function(a, b) sin(a / 3) * cos(b / 3)))
#' ggnext(g, aes(x, y, color = z)) + geom_raster()
#' @export
geom_raster <- function(mapping = NULL, data = NULL, color = NULL,
                        alpha = NULL) {
  layer_new(GeomTile(), StatTile(), mapping, data,
            list(color = color, alpha = alpha, linewidth = 0))
}

# --- reference lines ---------------------------------------------------------

#' GeomAbline: a sloped reference line across the panel
#' @noRd
GeomAbline <- new_class("GeomAbline", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "abline",
      default_params = list(
        color = "#8A8A94", linewidth = 1, alpha = 1,
        intercept = 0, slope = 1
      ),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomAbline) <- function(geom, scaled) {
  p <- scaled$params
  # Evaluate the line at the panel's own x limits, in data units, then
  # normalize through the y domain. Doing it here (rather than in a stat)
  # is what lets the line span whatever range the finished plot has.
  xd <- scaled$xdomain
  yd <- scaled$ydomain
  y_at <- p$intercept + p$slope * xd
  list(mk_line(
    c(0, 1), (y_at - yd[1]) / (yd[2] - yd[1]),
    stroke = scaled$color[[1]], width = p$linewidth,
    alpha = scaled$alpha[[1]], dash = p$dash %||% ""
  ))
}

#' Sloped reference line
#'
#' Draws `y = intercept + slope * x` across the panel. Like [geom_hline()]
#' and [geom_vline()] it ignores the plot's `aes()`, so it never disturbs
#' the data mapping.
#'
#' @param intercept,slope Line parameters. `slope = 1, intercept = 0` gives
#'   the identity line used in agreement and calibration plots.
#' @param color,linewidth,alpha,dash Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(cars, aes(speed, dist)) +
#'   geom_point() +
#'   geom_abline(intercept = 0, slope = 3, dash = "5,4")
#' @export
geom_abline <- function(intercept = 0, slope = 1, color = NULL,
                        linewidth = NULL, alpha = NULL, dash = NULL) {
  Layer(
    geom = GeomAbline(), stat = stat_identity(),
    # A placeholder row so the layer has both positional aesthetics. It is
    # excluded from scale training (NON_TRAINING_GEOMS), so it only matters
    # when an abline is the plot's only layer.
    mapping = aes_internal(x = "x", y = "y"), inherit = FALSE,
    data = data.frame(x = 0, y = 0),
    params = drop_null(list(intercept = intercept, slope = slope,
                            color = color, linewidth = linewidth,
                            alpha = alpha, dash = dash))
  )
}

#' Blank layer
#'
#' Draws nothing. Useful to establish scales without showing data, or as a
#' placeholder in code that conditionally adds a layer.
#'
#' @param mapping,data Standard layer overrides.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(cars, aes(speed, dist)) + geom_blank()
#' @export
geom_blank <- function(mapping = NULL, data = NULL) {
  layer_new(GeomBlank(), stat_identity(), mapping, data, list())
}

#' GeomBlank: reserves the scales without drawing
#' @noRd
GeomBlank <- new_class("GeomBlank", parent = Geom,
  constructor = function() {
    new_object(Geom(name = "blank", default_params = list()))
  }
)

method(build_marks, GeomBlank) <- function(geom, scaled) list()

# --- text with a background --------------------------------------------------

#' GeomLabel: text on an opaque rounded plate
#' @noRd
GeomLabel <- new_class("GeomLabel", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "label",
      default_params = list(
        color = "#16181D", size = 11, alpha = 1, fill = "#FFFFFF",
        anchor = "middle", padding = 0.4
      ),
      required_aes = c("x", "y", "label")
    ))
  }
)

method(build_marks, GeomLabel) <- function(geom, scaled) {
  p <- scaled$params
  size <- p$size %||% 11
  marks <- list()
  for (i in seq_along(scaled$x)) {
    txt <- as.character(scaled$label[[i]])
    # Approximate the plate from the string length: ~0.6 em per character
    # for a proportional face, converted from px into normalized units.
    half_w <- (nchar(txt) * size * 0.6 / 2 + size * p$padding) / 640
    half_h <- (size * (0.5 + p$padding)) / 480
    marks <- c(marks, list(mk_rect(
      scaled$x[[i]] - half_w, scaled$x[[i]] + half_w,
      scaled$y[[i]] - half_h, scaled$y[[i]] + half_h,
      fill = p$fill, alpha = scaled$alpha[[i]],
      stroke = scaled$color[[i]], stroke_width = 1
    )))
  }
  for (i in seq_along(scaled$x)) {
    marks <- c(marks, list(mk_text(
      scaled$x[[i]], scaled$y[[i]], as.character(scaled$label[[i]]),
      size = size, color = scaled$color[[i]], anchor = p$anchor
    )))
  }
  marks
}

#' Text on a background plate
#'
#' Like [geom_text()], but each label sits on an opaque rounded box, so it
#' stays readable over dense data.
#'
#' @param mapping,data Standard layer overrides. Requires `label`.
#' @param color Text and border colour.
#' @param fill Plate fill colour.
#' @param size Text size in px.
#' @param alpha Plate opacity.
#' @param padding Plate padding as a fraction of the text size.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(x = c(1, 2), y = c(2, 1), l = c("alpha", "beta"))
#' ggnext(d, aes(x, y, label = l)) + geom_label()
#' @export
geom_label <- function(mapping = NULL, data = NULL, color = NULL,
                       fill = NULL, size = NULL, alpha = NULL,
                       padding = NULL) {
  layer_new(GeomLabel(), stat_identity(), mapping, data,
            list(color = color, fill = fill, size = size, alpha = alpha,
                 padding = padding))
}

# --- ranges ------------------------------------------------------------------

#' GeomLinerange: a bare interval per observation
#' @noRd
GeomLinerange <- new_class("GeomLinerange", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "linerange",
      default_params = list(color = "#16181D", linewidth = 1.5, alpha = 1),
      required_aes = c("x", "ymin", "ymax")
    ))
  }
)

method(build_marks, GeomLinerange) <- function(geom, scaled) {
  p <- scaled$params
  lapply(seq_along(scaled$x), function(i) {
    mk_line(
      rep(scaled$x[[i]], 2), c(scaled$ymin[[i]], scaled$ymax[[i]]),
      stroke = scaled$color[[i]], width = p$linewidth,
      alpha = scaled$alpha[[i]]
    )
  })
}

#' Vertical interval without a marker
#'
#' The interval alone; add [geom_point()] for an estimate marker, or use
#' [geom_pointrange()] which draws both.
#'
#' @param mapping,data Standard layer overrides. Requires `x`, `ymin`,
#'   `ymax`.
#' @param color,linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(g = c("a", "b"), lo = c(1, 2), hi = c(4, 5))
#' ggnext(d, aes(g, ymin = lo, ymax = hi)) + geom_linerange()
#' @export
geom_linerange <- function(mapping = NULL, data = NULL, color = NULL,
                           linewidth = NULL, alpha = NULL) {
  layer_new(GeomLinerange(), stat_identity(), mapping, data,
            list(color = color, linewidth = linewidth, alpha = alpha))
}

#' GeomCrossbar: a box spanning the interval with a line at the estimate
#' @noRd
GeomCrossbar <- new_class("GeomCrossbar", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "crossbar",
      default_params = list(
        color = "#16181D", linewidth = 1.5, alpha = 0.25, width = 0.6
      ),
      required_aes = c("x", "y", "ymin", "ymax")
    ))
  }
)

method(build_marks, GeomCrossbar) <- function(geom, scaled) {
  p <- scaled$params
  half <- (p$width %||% 0.6) / 2 / max(scaled$xspan, 1e-9)
  marks <- list()
  for (i in seq_along(scaled$x)) {
    x0 <- scaled$x[[i]] - half
    x1 <- scaled$x[[i]] + half
    marks <- c(marks, list(
      mk_rect(x0, x1, scaled$ymin[[i]], scaled$ymax[[i]],
              fill = scaled$color[[i]], alpha = scaled$alpha[[i]],
              stroke = scaled$color[[i]], stroke_width = p$linewidth),
      mk_line(c(x0, x1), rep(scaled$y[[i]], 2), stroke = scaled$color[[i]],
              width = p$linewidth + 0.5)
    ))
  }
  marks
}

#' Crossbar: an interval box with the estimate marked
#'
#' A hollow box spanning `ymin` to `ymax` with a heavier line at `y` — the
#' compact summary used in dose-response and subgroup tables.
#'
#' @param mapping,data Standard layer overrides. Requires `x`, `y`, `ymin`,
#'   `ymax`.
#' @param color,linewidth,alpha Appearance.
#' @param width Box width in x-axis units.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(g = c("a", "b"), m = c(2, 3), lo = c(1, 2), hi = c(4, 5))
#' ggnext(d, aes(g, m, ymin = lo, ymax = hi)) + geom_crossbar()
#' @export
geom_crossbar <- function(mapping = NULL, data = NULL, color = NULL,
                          linewidth = NULL, alpha = NULL, width = NULL) {
  layer_new(GeomCrossbar(), stat_identity(), mapping, data,
            list(color = color, linewidth = linewidth, alpha = alpha,
                 width = width))
}

#' Horizontal error bars
#'
#' The horizontal counterpart of [geom_errorbar()], for intervals that run
#' along x — forest plots and tornado charts.
#'
#' @param mapping,data Standard layer overrides. Requires `y`, `xmin`,
#'   `xmax`.
#' @param color,linewidth,alpha Appearance.
#' @param height Cap height in y-axis units.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(g = c("a", "b"), lo = c(1, 2), hi = c(4, 5))
#' ggnext(d, aes(y = g, xmin = lo, xmax = hi)) + geom_errorbarh()
#' @export
geom_errorbarh <- function(mapping = NULL, data = NULL, color = NULL,
                           linewidth = NULL, alpha = NULL, height = NULL) {
  layer_new(GeomErrorbarh(), stat_identity(), mapping, data,
            list(color = color, linewidth = linewidth, alpha = alpha,
                 height = height))
}

#' GeomErrorbarh: horizontal interval with end caps
#' @noRd
GeomErrorbarh <- new_class("GeomErrorbarh", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "errorbarh",
      default_params = list(
        color = "#16181D", linewidth = 1.5, alpha = 1, height = 0.3
      ),
      required_aes = c("y", "xmin", "xmax")
    ))
  }
)

method(build_marks, GeomErrorbarh) <- function(geom, scaled) {
  p <- scaled$params
  half <- (p$height %||% 0.3) / 2 / max(scaled$yspan, 1e-9)
  marks <- list()
  for (i in seq_along(scaled$y)) {
    yy <- scaled$y[[i]]
    marks <- c(marks, list(mk_line(
      c(scaled$xmin[[i]], scaled$xmax[[i]]), rep(yy, 2),
      stroke = scaled$color[[i]], width = p$linewidth,
      alpha = scaled$alpha[[i]]
    )))
    for (xx in c(scaled$xmin[[i]], scaled$xmax[[i]])) {
      marks <- c(marks, list(mk_line(
        rep(xx, 2), c(yy - half, yy + half),
        stroke = scaled$color[[i]], width = p$linewidth
      )))
    }
  }
  marks
}

# --- marginal marks ----------------------------------------------------------

#' GeomRug: short marginal ticks showing the raw values
#' @noRd
GeomRug <- new_class("GeomRug", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "rug",
      default_params = list(
        color = "#16181D", linewidth = 0.8, alpha = 0.5, length = 0.03,
        sides = "bl"
      ),
      required_aes = "x"
    ))
  }
)

method(build_marks, GeomRug) <- function(geom, scaled) {
  p <- scaled$params
  len <- p$length %||% 0.03
  sides <- p$sides %||% "bl"
  marks <- list()
  for (i in seq_along(scaled$x)) {
    col <- scaled$color[[i]]
    a <- scaled$alpha[[i]]
    if (grepl("b", sides, fixed = TRUE)) {
      marks <- c(marks, list(mk_line(
        rep(scaled$x[[i]], 2), c(0, len), stroke = col,
        width = p$linewidth, alpha = a
      )))
    }
    if (grepl("t", sides, fixed = TRUE)) {
      marks <- c(marks, list(mk_line(
        rep(scaled$x[[i]], 2), c(1 - len, 1), stroke = col,
        width = p$linewidth, alpha = a
      )))
    }
    if (!is.null(scaled$y)) {
      if (grepl("l", sides, fixed = TRUE)) {
        marks <- c(marks, list(mk_line(
          c(0, len), rep(scaled$y[[i]], 2), stroke = col,
          width = p$linewidth, alpha = a
        )))
      }
      if (grepl("r", sides, fixed = TRUE)) {
        marks <- c(marks, list(mk_line(
          c(1 - len, 1), rep(scaled$y[[i]], 2), stroke = col,
          width = p$linewidth, alpha = a
        )))
      }
    }
  }
  marks
}

#' Marginal rug
#'
#' A short tick per observation along the panel edges, showing the raw
#' values behind a density, smooth, or scatter.
#'
#' @param mapping,data Standard layer overrides.
#' @param sides Which edges to draw on, as a string of `"t"`, `"r"`, `"b"`,
#'   `"l"` (default `"bl"`).
#' @param length Tick length as a fraction of the panel.
#' @param color,linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(cars, aes(speed, dist)) + geom_point() + geom_rug()
#' @export
geom_rug <- function(mapping = NULL, data = NULL, sides = NULL,
                     length = NULL, color = NULL, linewidth = NULL,
                     alpha = NULL) {
  layer_new(GeomRug(), stat_identity(), mapping, data,
            list(sides = sides, length = length, color = color,
                 linewidth = linewidth, alpha = alpha))
}

# --- curves and spokes -------------------------------------------------------

#' GeomCurve: a curved connector between two points
#' @noRd
GeomCurve <- new_class("GeomCurve", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "curve",
      default_params = list(
        color = "#16181D", linewidth = 1.5, alpha = 1, curvature = 0.3
      ),
      required_aes = c("x", "y", "xend", "yend")
    ))
  }
)

method(build_marks, GeomCurve) <- function(geom, scaled) {
  p <- scaled$params
  k <- p$curvature %||% 0.3
  lapply(seq_along(scaled$x), function(i) {
    x0 <- scaled$x[[i]]
    y0 <- scaled$y[[i]]
    x1 <- scaled$xend[[i]]
    y1 <- scaled$yend[[i]]
    # Control point offset perpendicular to the chord; the sign of
    # `curvature` picks the side the curve bows towards.
    mx <- (x0 + x1) / 2
    my <- (y0 + y1) / 2
    b <- quad_bezier(x0, y0, mx - k * (y1 - y0), my + k * (x1 - x0), x1, y1)
    mk_line(b$x, b$y, stroke = scaled$color[[i]], width = p$linewidth,
            alpha = scaled$alpha[[i]])
  })
}

#' Curved connector
#'
#' A quadratic curve between two points, for annotation leaders and
#' relationship diagrams where a straight segment would overlap the data.
#'
#' @param mapping,data Standard layer overrides. Requires `x`, `y`, `xend`,
#'   `yend`.
#' @param curvature How far the curve bows; negative bows the other way.
#' @param color,linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(x = 1, y = 1, xe = 3, ye = 3)
#' ggnext(d, aes(x, y, xend = xe, yend = ye)) + geom_curve()
#' @export
geom_curve <- function(mapping = NULL, data = NULL, curvature = NULL,
                       color = NULL, linewidth = NULL, alpha = NULL) {
  layer_new(GeomCurve(), stat_identity(), mapping, data,
            list(curvature = curvature, color = color,
                 linewidth = linewidth, alpha = alpha))
}

#' GeomSpoke: a segment defined by angle and radius
#' @noRd
GeomSpoke <- new_class("GeomSpoke", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "spoke",
      default_params = list(color = "#16181D", linewidth = 1.2, alpha = 1),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomSpoke) <- function(geom, scaled) {
  p <- scaled$params
  lapply(seq_along(scaled$x), function(i) {
    mk_line(
      c(scaled$x[[i]], scaled$xend[[i]]),
      c(scaled$y[[i]], scaled$yend[[i]]),
      stroke = scaled$color[[i]], width = p$linewidth,
      alpha = scaled$alpha[[i]]
    )
  })
}

#' Spokes (vector field)
#'
#' A segment from each point at a given angle and radius — the usual way to
#' draw a vector or wind field.
#'
#' @param mapping,data Standard layer overrides. Requires `x`, `y`, and the
#'   `angle` (radians) and `radius` columns supplied via `xend`/`yend`.
#' @param color,linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' g <- expand.grid(x = 1:5, y = 1:5)
#' g$angle <- atan2(g$y - 3, g$x - 3)
#' g$radius <- 0.4
#' ggnext(g, aes(x, y, xend = angle, yend = radius)) + geom_spoke()
#' @export
geom_spoke <- function(mapping = NULL, data = NULL, color = NULL,
                       linewidth = NULL, alpha = NULL) {
  layer_new(GeomSpoke(), StatSpoke(), mapping, data,
            list(color = color, linewidth = linewidth, alpha = alpha))
}

# --- counted and binned variants ---------------------------------------------

#' Counted scatter
#'
#' A scatter where the point area encodes how many observations share that
#' position — the fix for a scatter of rounded or discrete values where
#' overplotting hides the mass.
#'
#' @param mapping,data Standard layer overrides.
#' @param color,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(x = c(1, 1, 1, 2, 2, 3), y = c(1, 1, 2, 2, 2, 3))
#' ggnext(d, aes(x, y)) + geom_count()
#' @export
geom_count <- function(mapping = NULL, data = NULL, color = NULL,
                       alpha = NULL) {
  layer_new(GeomPoint(), StatSum(), mapping, data,
            list(color = color, alpha = alpha))
}

#' Frequency polygon
#'
#' The same binning as [geom_histogram()], drawn as a line through the bin
#' centres. Easier to overlay across groups than filled bars.
#'
#' @param mapping,data Standard layer overrides.
#' @param bins,binwidth Binning, as in [geom_histogram()].
#' @param color,linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(cars, aes(speed)) + geom_freqpoly(bins = 8)
#' @export
geom_freqpoly <- function(mapping = NULL, data = NULL, bins = 30,
                          binwidth = NULL, color = NULL, linewidth = NULL,
                          alpha = NULL) {
  layer_new(GeomLine(), stat_bin(bins = bins, binwidth = binwidth),
            mapping, data,
            list(color = color, linewidth = linewidth, alpha = alpha))
}

#' Curve of a function
#'
#' Evaluates `fn` over the panel's x range and draws the result. Handy for
#' overlaying a theoretical density or a reference curve on data.
#'
#' @param fn A function of one numeric argument.
#' @param xlim Length-2 range to evaluate over.
#' @param n Number of evaluation points.
#' @param color,linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(data.frame(x = c(-3, 3)), aes(x)) +
#'   geom_function(dnorm, xlim = c(-3, 3))
#' @export
geom_function <- function(fn, xlim = c(0, 1), n = 101, color = NULL,
                          linewidth = NULL, alpha = NULL) {
  if (!is.function(fn)) {
    stop("`fn` must be a function of one numeric argument.", call. = FALSE)
  }
  xs <- seq(xlim[1], xlim[2], length.out = n)
  Layer(
    geom = GeomLine(), stat = stat_identity(),
    mapping = aes_internal(x = "x", y = "y"), inherit = FALSE,
    data = data.frame(x = xs, y = fn(xs)),
    params = drop_null(list(color = color, linewidth = linewidth,
                            alpha = alpha))
  )
}
