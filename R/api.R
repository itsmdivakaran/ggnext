# User-facing constructors and the `+` grammar --------------------------------

#' @include classes.R aes.R
NULL

#' Create a new ggplot3 plot
#'
#' The entry point of the grammar: bind a default data frame and a default
#' aesthetic mapping, then add layers, scales, and coordinate systems with
#' `+`.
#'
#' @param data A data frame used by all layers unless a layer overrides it.
#' @param mapping Default aesthetic mapping created with [aes()].
#' @param width,height Device size in pixels (default 640 x 480).
#' @return A [Ggplot3Plot] object.
#' @examples
#' p <- ggplot3(cars, aes(speed, dist)) + geom_point()
#' svg <- render(p)
#' @export
ggplot3 <- function(data = NULL, mapping = NULL, width = 640, height = 480) {
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data frame (or NULL).")
  }
  if (!is.null(mapping) && !inherits(mapping, "ggplot3_aes")) {
    stop("`mapping` must be created with aes().")
  }
  Ggplot3Plot(
    data = data,
    mapping = mapping,
    layers = list(),
    scales = list(),
    coord = CoordCartesian(),
    interaction = NULL,
    theme = theme_ggplot3(),
    labels = NULL,
    color_scale = NULL,
    facet = NULL,
    animation = NULL,
    size = c(width, height)
  )
}

#' Set the output size
#'
#' @param width,height Device size in pixels.
#' @return A `ggplot3_size` object to add to a plot with `+`.
#' @examples
#' ggplot3(cars, aes(speed, dist)) + geom_point() + plot_size(900, 600)
#' @export
plot_size <- function(width, height) {
  structure(list(size = c(width, height)), class = "ggplot3_size")
}

#' Add interactivity to a plot
#'
#' Plots are static-first: `render(p)` and printing produce a static SVG
#' image. Adding `interact()` with `+` opts the plot into the interactive
#' HTML/canvas target — `render(p)` then produces the interactive page
#' instead (an explicit `render(p, target = )` always wins).
#'
#' @param tooltip `TRUE` (default) shows the mapped x/y values on hover;
#'   `FALSE` disables the tooltip; a character vector of column names (e.g.
#'   `tooltip = c("model", "hwy")`) shows those columns from the layer data
#'   instead.
#' @param zoom Enable scroll-to-zoom and double-click-to-reset.
#' @param brush Enable brush-to-zoom: drag a rectangle on the plot to zoom
#'   to it (double-click still resets).
#' @return An [Interact] spec to add to a plot with `+`.
#' @examples
#' p <- ggplot3(cars, aes(speed, dist)) + geom_point()
#' render(p)                 # static SVG
#' pi <- p + interact()      # same plot, interactive by default
#' html <- render(pi)        # HTML/canvas with tooltip + zoom + brush
#' @export
interact <- function(tooltip = TRUE, zoom = TRUE, brush = TRUE) {
  if (!isTRUE(tooltip) && !isFALSE(tooltip) && !is.character(tooltip)) {
    stop("`tooltip` must be TRUE, FALSE, or a character vector of column names.")
  }
  Interact(tooltip = tooltip, zoom = zoom, brush = brush)
}

#' Animate a plot over a transition variable
#'
#' Animation is a property of the plot, not a separate verb chain: the same
#' geometry pipeline runs once per level of `by`, and the interactive target
#' plays the resulting frames with a scrubber and play/pause control.
#' Because frames are ordinary geometry buffers, positions are interpolated
#' by the player rather than recomputed.
#'
#' Animated plots render to the interactive target (like [interact()]).
#' `render(p, target = "static")` still produces a static SVG of the whole
#' data, so animation never blocks a static export.
#'
#' @param by Transition variable: an unquoted column name or a string.
#' @param duration Milliseconds per frame.
#' @param easing `"linear"`, `"cubic"`, or `"cubic-in-out"`.
#' @param loop Restart automatically after the last frame.
#' @return An [Animation] spec to add to a plot with `+`.
#' @examples
#' d <- data.frame(
#'   x = rep(1:5, 3), y = c(1:5, (1:5)^1.5, (1:5)^2),
#'   step = rep(c(1, 2, 3), each = 5)
#' )
#' p <- ggplot3(d, aes(x, y)) + geom_point(size = 5) + animate(step)
#' html <- render(p)
#' @export
animate <- function(by, duration = 800, easing = "cubic-in-out",
                    loop = TRUE) {
  expr <- substitute(by)
  var <- if (is.symbol(expr)) as.character(expr) else as.character(by)
  if (length(var) != 1) {
    stop("`by` must name a single column, e.g. animate(year).")
  }
  Animation(
    var = var, duration = duration,
    easing = match.arg(easing, c("linear", "cubic", "cubic-in-out")),
    loop = loop
  )
}

#' Polar coordinates
#'
#' Bends the panel into a circle: the `theta` axis becomes the angle and
#' the other axis becomes the radius. This is the engine behind radar
#' charts ([geom_radar()]), pie/donut wedges, and circular bar charts.
#'
#' @param theta Which axis maps to angle: `"x"` (default) or `"y"`.
#' @param start Angle in radians for the first position; `0` is 12 o'clock.
#' @param direction `1` clockwise (default), `-1` counter-clockwise.
#' @param inner Inner radius as a fraction of the outer radius — use e.g.
#'   `0.3` for a donut hole.
#' @return A [CoordPolar] object to add to a plot with `+`.
#' @examples
#' d <- data.frame(g = c("A", "B", "C", "D"), v = c(4, 7, 3, 6))
#' ggplot3(d, aes(g, v)) + geom_col() + coord_polar()
#' @export
coord_polar <- function(theta = "x", start = 0, direction = 1, inner = 0) {
  CoordPolar(theta = theta, start = start, direction = direction, inner = inner)
}

#' Scatter-plot layer: one point per observation
#'
#' Draws a circle mark for each row of the layer data - the default view of
#' a relationship between two continuous variables.
#'
#' @param mapping Layer-specific aesthetic mapping (optional; merged over
#'   the plot-level mapping, layer winning on conflicts).
#' @param data Layer-specific data frame (optional).
#' @param stat A [Stat] instance; defaults to [stat_identity()].
#' @param color Literal point color (any R color spec) when `color` is not
#'   a mapped aesthetic.
#' @param size Literal point radius in pixels when `size` is not mapped.
#' @param alpha Point opacity in \[0, 1\].
#' @return A [Layer] object to add to a plot with `+`.
#' @examples
#' ggplot3(cars, aes(speed, dist)) + geom_point(color = "steelblue", size = 4)
#' @export
geom_point <- function(mapping = NULL, data = NULL, stat = stat_identity(),
                       color = NULL, size = NULL, alpha = NULL) {
  params <- list(color = color, size = size, alpha = alpha)
  params <- params[!vapply(params, is.null, logical(1))]
  Layer(
    geom = GeomPoint(),
    stat = stat,
    mapping = mapping,
    data = data,
    params = params
  )
}

#' Identity statistical transformation
#'
#' Passes layer data through untransformed. This is the default stat for
#' geoms that draw the data as given, such as [geom_point()] and
#' [geom_line()].
#'
#' @return A `StatIdentity` object, to pass as a layer's `stat`.
#' @export
stat_identity <- function() {
  StatIdentity()
}

#' Continuous scale for the x axis
#'
#' Controls the axis title, limits, tick positions, tick labels, the
#' padding around the data, and the axis transform.
#'
#' @param name Axis title (defaults to the mapped expression).
#' @param limits Optional numeric length-2 domain; `NULL` trains from data.
#' @param breaks Explicit tick positions in data units, or `NULL` for
#'   automatic "nice number" breaks.
#' @param labels Tick labels: a character vector the same length as
#'   `breaks`, a function applied to the break values (e.g.
#'   `function(x) paste0("$", x)`), or `NULL` for automatic formatting.
#' @param expand Fraction of the data span to pad onto each end of the axis
#'   (default 5%). Use `0` for a tight axis.
#' @param trans Axis transform: `"identity"`, `"log10"`, `"sqrt"`, or
#'   `"reverse"`.
#' @return A [ScaleContinuous] object to add with `+`.
#' @examples
#' ggplot3(cars, aes(speed, dist)) +
#'   geom_point() +
#'   scale_x_continuous(
#'     name = "Speed (mph)",
#'     breaks = c(5, 10, 15, 20, 25),
#'     expand = 0
#'   ) +
#'   scale_y_continuous(labels = function(v) paste0(v, " ft"))
#' @export
scale_x_continuous <- function(name = NULL, limits = NULL, breaks = NULL,
                               labels = NULL, expand = 0.05,
                               trans = "identity") {
  ScaleContinuous(
    aesthetic = "x", limits = limits, name = name, breaks = breaks,
    labels = labels, trans = trans, expand = expand
  )
}

#' @rdname scale_x_continuous
#' @export
scale_y_continuous <- function(name = NULL, limits = NULL, breaks = NULL,
                               labels = NULL, expand = 0.05,
                               trans = "identity") {
  ScaleContinuous(
    aesthetic = "y", limits = limits, name = name, breaks = breaks,
    labels = labels, trans = trans, expand = expand
  )
}

#' Log10 and square-root scales
#'
#' Shorthand for `scale_*_continuous(trans = "log10")` / `"sqrt"`.
#'
#' @inheritParams scale_x_continuous
#' @return A [ScaleContinuous] object to add with `+`.
#' @examples
#' d <- data.frame(x = 10^(1:5), y = 1:5)
#' ggplot3(d, aes(x, y)) + geom_point() + scale_x_log10()
#' @export
scale_x_log10 <- function(name = NULL, limits = NULL, breaks = NULL,
                          labels = NULL, expand = 0.05) {
  scale_x_continuous(name, limits, breaks, labels, expand, trans = "log10")
}

#' @rdname scale_x_log10
#' @export
scale_y_log10 <- function(name = NULL, limits = NULL, breaks = NULL,
                          labels = NULL, expand = 0.05) {
  scale_y_continuous(name, limits, breaks, labels, expand, trans = "log10")
}

#' @rdname scale_x_log10
#' @export
scale_x_sqrt <- function(name = NULL, limits = NULL, breaks = NULL,
                         labels = NULL, expand = 0.05) {
  scale_x_continuous(name, limits, breaks, labels, expand, trans = "sqrt")
}

#' @rdname scale_x_log10
#' @export
scale_y_sqrt <- function(name = NULL, limits = NULL, breaks = NULL,
                         labels = NULL, expand = 0.05) {
  scale_y_continuous(name, limits, breaks, labels, expand, trans = "sqrt")
}

#' @rdname scale_x_log10
#' @export
scale_x_reverse <- function(name = NULL, limits = NULL, breaks = NULL,
                            labels = NULL, expand = 0.05) {
  scale_x_continuous(name, limits, breaks, labels, expand, trans = "reverse")
}

#' @rdname scale_x_log10
#' @export
scale_y_reverse <- function(name = NULL, limits = NULL, breaks = NULL,
                            labels = NULL, expand = 0.05) {
  scale_y_continuous(name, limits, breaks, labels, expand, trans = "reverse")
}

#' Set the discrete color palette
#'
#' Overrides the default colorblind-safe palette for discrete color
#' mappings. Levels take colors in order.
#'
#' @param values Character vector of colors (names or hex).
#' @param name Optional legend title.
#' @return A `ColorScale` object to add with `+`.
#' @examples
#' ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_manual(c("#D55E00", "#0072B2", "#009E73"))
#' @export
scale_color_manual <- function(values, name = NULL) {
  ColorScale(palette = col_to_hex(values), name = name, type = "discrete")
}

#' Set the continuous color gradient
#'
#' @param low,high Gradient endpoint colors.
#' @param name Optional legend title.
#' @return A `ColorScale` object to add with `+`.
#' @examples
#' ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Petal.Length)) +
#'   geom_point() +
#'   scale_color_gradient(low = "#FFF3B0", high = "#9E2A2B")
#' @export
scale_color_gradient <- function(low = "#1A2E59", high = "#5FD0A5",
                                 name = NULL) {
  ColorScale(
    palette = col_to_hex(c(low, high)), name = name, type = "continuous"
  )
}

#' Cartesian coordinate system
#'
#' The identity coordinate system.
#'
#' @param flip Swap the x and y axes (horizontal bars, forest plots).
#' @return A [CoordCartesian] object to add with `+`.
#' @export
coord_cartesian <- function(flip = FALSE) {
  CoordCartesian(flip = flip)
}

#' Flipped Cartesian coordinates
#'
#' Draws the plot with x and y swapped — the usual way to get horizontal
#' bars or a readable categorical axis with long labels.
#'
#' @return A [CoordCartesian] object to add with `+`.
#' @examples
#' d <- data.frame(g = c("alpha", "beta", "gamma"), v = c(3, 7, 5))
#' ggplot3(d, aes(g, v)) + geom_col() + coord_flip()
#' @export
coord_flip <- function() CoordCartesian(flip = TRUE)

# --- The `+` operator --------------------------------------------------------

# Adding grammar components composes a new immutable plot value. Dispatch on
# the right-hand side's class decides where the component lands.
plot_add <- function(plot, component) {
  if (S7_inherits(component, Layer)) {
    plot@layers <- c(plot@layers, list(component))
  } else if (S7_inherits(component, Scale)) {
    plot@scales[[component@aesthetic]] <- component
  } else if (S7_inherits(component, Coord)) {
    plot@coord <- component
  } else if (S7_inherits(component, Interact)) {
    plot@interaction <- component
  } else if (S7_inherits(component, Theme)) {
    plot@theme <- component
  } else if (S7_inherits(component, Labels)) {
    plot@labels <- merge_labels(plot@labels, component)
  } else if (S7_inherits(component, ColorScale)) {
    plot@color_scale <- component
  } else if (S7_inherits(component, Facet)) {
    plot@facet <- component
  } else if (S7_inherits(component, Animation)) {
    plot@animation <- component
  } else if (inherits(component, "ggplot3_size")) {
    plot@size <- component$size
  } else if (inherits(component, "ggplot3_aes")) {
    plot@mapping <- merge_aes(plot@mapping, component)
  } else if (is.list(component) && !is.object(component)) {
    # A bare list of components (e.g. lims(), or a user-defined bundle of
    # layers) is added element by element.
    for (el in component) plot <- plot_add(plot, el)
  } else if (is.null(component)) {
    # `p + NULL` is a no-op, convenient for conditional pipelines.
  } else {
    stop(
      "Cannot add an object of class <", paste(class(component), collapse = "/"),
      "> to a ggplot3 plot."
    )
  }
  plot
}

method(`+`, list(Ggplot3Plot, class_any)) <- function(e1, e2) {
  plot_add(e1, e2)
}

method(print, Ggplot3Plot) <- function(x, ...) {
  cat("<ggplot3 plot>\n")
  cat(
    "  data:  ",
    if (is.null(x@data)) "<none>" else paste(dim(x@data), collapse = " x "),
    "\n", sep = ""
  )
  if (!is.null(x@mapping)) {
    labs <- vapply(x@mapping$exprs, function(e) deparse(e), character(1))
    cat("  aes:   ", paste(names(labs), labs, sep = " = ", collapse = ", "),
        "\n", sep = "")
  }
  cat("  layers:", length(x@layers), "\n")
  cat("  coord: ", x@coord@name, "\n", sep = "")
  cat("  mode:  ", if (is.null(x@interaction)) "static" else "interactive",
      "\n", sep = "")

  # In an interactive session with at least one layer, printing displays
  # the plot (RStudio Viewer when available, default browser otherwise).
  # Static plots show the SVG image; `+ interact()` plots show the
  # canvas page.
  if (interactive() && length(x@layers) > 0) {
    show_plot(x)
  }
  invisible(x)
}

# Write the rendered plot to a session temp file and display it.
show_plot <- function(plot) {
  dir <- tempfile("ggplot3-view-")
  dir.create(dir)
  path <- file.path(dir, "plot.html")
  if (is.null(plot@interaction)) {
    # Wrap the SVG in a minimal HTML shell so viewers render it as a page.
    svg <- render(plot, target = "static")
    writeLines(paste0(
      "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"></head>",
      "<body style=\"margin:0\">\n", svg, "\n</body></html>"
    ), path, useBytes = TRUE)
  } else {
    render(plot, target = "interactive", file = path)
  }
  # RStudio exposes its Viewer pane as getOption("viewer"); fall back to
  # the system browser. Neither adds a package dependency.
  viewer <- getOption("viewer")
  if (is.function(viewer)) viewer(path) else utils::browseURL(path)
  invisible(path)
}
