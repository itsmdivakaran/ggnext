#' @include classes.R aes.R
NULL

# Plot validation -----------------------------------------------------------
#
# A lightweight linter over a built GgnextPlot: catches statistical-graphics
# mistakes before you ship the figure - an empty plot, a continuous geom fed
# a categorical y, a scale that can't represent the data it's given, a
# legend about to grow past usefulness, more missing data than the plot lets
# on. Each rule is a small function of the plot; validate_plot() runs them
# all and returns the findings, plot_check() prints them and returns the
# plot unchanged so it can sit inside a pipe:
#
#   p |> plot_check() |> render()
#
# New checks are added by appending to `validate_rules()` below - each rule
# receives the plot and returns a list of findings (possibly empty).

# Geoms whose y is expected to be continuous; a categorical y is very likely
# a mistake (a bar/box/violin geom is probably what was meant instead).
categorical_y_geoms <- c("GeomPoint", "GeomLine", "GeomPath", "GeomArea", "GeomSmooth")

finding <- function(rule, status, message) {
  structure(list(rule = rule, status = status, message = message),
           class = "ggnext_finding")
}

layer_mapping <- function(plot, layer) {
  if (isTRUE(layer@inherit)) merge_aes(plot@mapping, layer@mapping) else layer@mapping
}

layer_values <- function(plot, layer) {
  data <- layer@data %||% plot@data
  mapping <- layer_mapping(plot, layer)
  if (is.null(data) || is.null(mapping)) {
    return(NULL)
  }
  tryCatch(eval_aes(mapping, data), error = function(e) NULL)
}

check_empty <- function(plot) {
  if (length(plot@layers) == 0) {
    return(list(finding(
      "empty_plot", "warn",
      "Plot has no layers - nothing will be drawn."
    )))
  }
  list()
}

check_categorical_y <- function(plot) {
  out <- list()
  for (layer in plot@layers) {
    # S7 class strings are namespace-qualified ("ggnext::GeomPoint").
    geom_class <- sub("^.*::", "", class(layer@geom)[1])
    if (!geom_class %in% categorical_y_geoms) next
    values <- layer_values(plot, layer)
    y <- values$y
    if (is.null(y)) next
    if (is.factor(y) || is.character(y)) {
      out[[length(out) + 1]] <- finding(
        "categorical_y", "warn",
        paste0(geom_class, "-based layer maps y to a categorical column; ",
               "consider geom_boxplot(), geom_violin(), or geom_bar() instead.")
      )
    }
  }
  out
}

check_missing_values <- function(plot, threshold = 0.1) {
  out <- list()
  for (layer in plot@layers) {
    values <- layer_values(plot, layer)
    for (nm in names(values)) {
      v <- values[[nm]]
      if (!is.atomic(v) || length(v) == 0) next
      frac <- mean(is.na(v))
      if (frac > threshold) {
        out[[length(out) + 1]] <- finding(
          "missing_values", "warn",
          sprintf("Aesthetic `%s` is %.0f%% missing.", nm, frac * 100)
        )
      }
    }
  }
  out
}

check_high_cardinality_scale <- function(plot, max_levels = 12) {
  out <- list()
  for (layer in plot@layers) {
    values <- layer_values(plot, layer)
    for (nm in intersect(names(values), c("color", "fill"))) {
      v <- values[[nm]]
      if (!(is.factor(v) || is.character(v))) next
      n_levels <- length(unique(v))
      if (n_levels > max_levels) {
        out[[length(out) + 1]] <- finding(
          "high_cardinality_scale", "warn",
          sprintf(
            "`%s` has %d distinct values mapped to a discrete scale; the legend may be unreadable.",
            nm, n_levels
          )
        )
      }
    }
  }
  out
}

check_sqrt_negative <- function(plot) {
  out <- list()
  for (aes_name in c("x", "y")) {
    scale <- plot@scales[[aes_name]]
    if (is.null(scale) || !S7::S7_inherits(scale, ScaleContinuous)) next
    if (!identical(scale@trans, "sqrt")) next
    for (layer in plot@layers) {
      v <- layer_values(plot, layer)[[aes_name]]
      if (is.null(v) || !is.numeric(v)) next
      if (any(v < 0, na.rm = TRUE)) {
        out[[length(out) + 1]] <- finding(
          "sqrt_negative", "warn",
          sprintf(
            "scale_%s_sqrt() is applied but `%s` contains negative values, which sqrt() cannot represent.",
            aes_name, aes_name
          )
        )
        break
      }
    }
  }
  out
}

validate_rules <- function() {
  list(check_empty, check_categorical_y, check_missing_values,
       check_high_cardinality_scale, check_sqrt_negative)
}

#' Check a plot for common statistical-graphics mistakes
#'
#' Runs a small set of rules over a plot's layers and scales - an empty
#' plot, a continuous geom (`geom_point()`, `geom_line()`, ...) mapped to a
#' categorical `y`, a mapped aesthetic that is mostly missing, a discrete
#' color/fill scale with more levels than a legend can usefully show, and a
#' `sqrt`-transformed scale fed negative values. Each is a heuristic, not a
#' guarantee - `validate_plot()` reports what commonly goes wrong, not what
#' is definitely wrong with this particular plot.
#'
#' @param plot A [GgnextPlot] object.
#' @return An object of class `ggnext_check`: a list of findings, each with
#'   `rule`, `status` (currently always `"warn"`), and `message`. Prints as
#'   a short report; empty when nothing was found.
#' @examples
#' p <- ggnext(mtcars, aes(mpg, as.character(cyl))) + geom_point()
#' validate_plot(p)
#' @export
validate_plot <- function(plot) {
  if (!S7::S7_inherits(plot, GgnextPlot)) {
    stop("validate_plot() expects a ggnext plot.")
  }
  findings <- unlist(lapply(validate_rules(), function(rule) rule(plot)), recursive = FALSE)
  structure(findings, class = "ggnext_check")
}

#' Print a plot's validation findings and return the plot, unchanged
#'
#' A pipe-friendly wrapper around [validate_plot()]: prints the report as a
#' side effect and returns `plot` invisibly, so it can sit inside a pipeline
#' without interrupting it.
#'
#' @param plot A [GgnextPlot] object.
#' @return `plot`, invisibly.
#' @examples
#' ggnext(cars, aes(speed, dist)) |>
#'   geom_point() |>
#'   plot_check() |>
#'   invisible()
#' @export
plot_check <- function(plot) {
  print(validate_plot(plot))
  invisible(plot)
}

#' @export
print.ggnext_check <- function(x, ...) {
  if (length(x) == 0) {
    cat("\u2713 No issues found\n")
    return(invisible(x))
  }
  for (f in x) {
    cat("\u26a0 ", f$message, "\n", sep = "")
  }
  invisible(x)
}
