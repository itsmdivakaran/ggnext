# Discrete positional scale ----------------------------------------------------
#
# Categories map onto integer slots 1..n, and the slot axis then behaves like
# a continuous axis: normalized position = (slot - domain_min) / span. The
# expanded domain pads 0.6 of a slot on each side, so a bar of width 0.8
# centered on slot 1 (i.e. [0.6, 1.4]) sits fully inside the panel.

#' @include classes.R
NULL

#' ScaleDiscrete: positional scale for categorical data
#'
#' Automatically selected when a positional aesthetic maps to character,
#' factor, or logical data; construct explicitly via [scale_x_discrete()] /
#' [scale_y_discrete()] to fix the level order.
#'
#' @param aesthetic `"x"` or `"y"`.
#' @param limits Optional character vector fixing the levels (and their
#'   order); `NULL` trains levels from the data.
#' @param name Optional axis title.
#'
#' The `levels` property (the trained categories) is filled in during the
#' plot build, not at construction.
#' @return An S7 object of class `ScaleDiscrete`; usually built by [scale_x_discrete()] rather than called directly.
#' @export
ScaleDiscrete <- new_class("ScaleDiscrete", parent = Scale,
  properties = list(
    limits = class_any,
    levels = class_any
  ),
  constructor = function(aesthetic = "x", limits = NULL, name = NULL) {
    if (!is.null(limits) && !is.character(limits)) {
      stop("`limits` must be NULL or a character vector of levels.")
    }
    new_object(
      Scale(aesthetic = aesthetic, name = name),
      limits = limits, levels = NULL
    )
  }
)

method(scale_train, ScaleDiscrete) <- function(scale, values) {
  if (is.numeric(values)) {
    stop(
      "Aesthetic `", scale@aesthetic, "` is numeric; a discrete scale ",
      "expects character, factor, or logical data."
    )
  }
  scale@levels <- if (!is.null(scale@limits)) {
    scale@limits
  } else if (is.factor(values)) {
    # Factors keep their declared level order (this is how e.g. waterfall
    # charts preserve category order).
    levels(values)
  } else {
    sort(unique(as.character(values)))
  }
  scale
}

# Convert raw categorical values to their integer slot (1..n).
scale_discrete_index <- function(scale, values) {
  idx <- match(as.character(values), scale@levels)
  if (anyNA(idx) && !anyNA(values)) {
    bad <- setdiff(unique(as.character(values)), scale@levels)
    stop(
      "Value(s) not in the levels of the discrete `", scale@aesthetic,
      "` scale: ", paste(bad, collapse = ", ")
    )
  }
  idx
}

method(scale_map, ScaleDiscrete) <- function(scale, values, expand = NULL) {
  domain <- if (is.null(expand)) {
    c(1 - 0.6, length(scale@levels) + 0.6)
  } else {
    expand
  }
  # Values may arrive raw (categorical) or already as numeric slots (after
  # the build's discretize step, or offset by a position adjustment).
  slots <- if (is.numeric(values)) values else scale_discrete_index(scale, values)
  normalize(slots, domain)
}

method(scale_breaks, ScaleDiscrete) <- function(scale, n = 5) {
  seq_along(scale@levels)
}

#' Discrete positional scale for the x axis
#'
#' @param limits Optional character vector fixing the levels and their order.
#' @param name Optional axis title.
#' @return A [ScaleDiscrete] object to add with `+`.
#' @export
scale_x_discrete <- function(limits = NULL, name = NULL) {
  ScaleDiscrete(aesthetic = "x", limits = limits, name = name)
}

#' Discrete positional scale for the y axis
#'
#' @param limits Optional character vector fixing the levels and their order.
#' @param name Optional axis title.
#' @return A [ScaleDiscrete] object to add with `+`.
#' @export
scale_y_discrete <- function(limits = NULL, name = NULL) {
  ScaleDiscrete(aesthetic = "y", limits = limits, name = name)
}
