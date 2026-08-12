# Specialty geoms: intervals, dumbbells, waterfalls, survival, ROC -------------
#
# All built from the same five mark primitives — no renderer changes needed.

#' @include classes.R stats.R stats-special.R geoms.R marks.R
NULL

# --- errorbar / pointrange ---------------------------------------------------

#' GeomErrorbar: vertical interval with caps
#' @noRd
GeomErrorbar <- new_class("GeomErrorbar", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "errorbar",
      default_params = list(color = "#000000", linewidth = 1.2, alpha = 1,
                            width = 0.25),
      required_aes = c("x", "ymin", "ymax")
    ))
  }
)

method(build_marks, GeomErrorbar) <- function(geom, scaled) {
  p <- scaled$params
  half <- (p$width / 2) / scaled$xspan
  unname(unlist(lapply(seq_along(scaled$x), function(i) {
    x <- scaled$x[[i]]
    col <- scaled$color[[i]]
    a <- scaled$alpha[[i]]
    list(
      mk_line(c(x, x), c(scaled$ymin[[i]], scaled$ymax[[i]]),
              stroke = col, width = p$linewidth, alpha = a),
      mk_line(c(x - half, x + half), rep(scaled$ymax[[i]], 2),
              stroke = col, width = p$linewidth, alpha = a),
      mk_line(c(x - half, x + half), rep(scaled$ymin[[i]], 2),
              stroke = col, width = p$linewidth, alpha = a)
    )
  }), recursive = FALSE))
}

#' Error bar layer (`ymin` to `ymax` with caps)
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param linewidth Stroke width.
#' @param width Cap width in x data/slot units.
#' @return A [Layer].
#' @export
geom_errorbar <- function(mapping = NULL, data = NULL, color = NULL,
                          alpha = NULL, linewidth = NULL, width = NULL) {
  Layer(
    geom = GeomErrorbar(), stat = stat_identity(), mapping = mapping,
    data = data,
    params = drop_null(list(color = color, alpha = alpha,
                            linewidth = linewidth, width = width))
  )
}

#' GeomPointrange: vertical interval with a midpoint dot
#' @noRd
GeomPointrange <- new_class("GeomPointrange", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "pointrange",
      default_params = list(color = "#000000", linewidth = 1.2, alpha = 1,
                            size = 3.5),
      required_aes = c("x", "y", "ymin", "ymax")
    ))
  }
)

method(build_marks, GeomPointrange) <- function(geom, scaled) {
  p <- scaled$params
  unname(unlist(lapply(seq_along(scaled$x), function(i) {
    x <- scaled$x[[i]]
    col <- scaled$color[[i]]
    a <- scaled$alpha[[i]]
    list(
      mk_line(c(x, x), c(scaled$ymin[[i]], scaled$ymax[[i]]),
              stroke = col, width = p$linewidth, alpha = a),
      mk_circle(x, scaled$y[[i]], r = scaled$size[[i]], fill = col, alpha = a)
    )
  }), recursive = FALSE))
}

#' Point-range layer (interval plus midpoint; forest-plot building block)
#'
#' A horizontal forest plot is this geom with the categorical variable on x
#' — a dedicated `geom_forest()` with flipped coordinates is a milestone.
#'
#' @param mapping,data,color,size,alpha As in [geom_point()].
#' @param linewidth Stroke width.
#' @return A [Layer].
#' @export
geom_pointrange <- function(mapping = NULL, data = NULL, color = NULL,
                            size = NULL, alpha = NULL, linewidth = NULL) {
  Layer(
    geom = GeomPointrange(), stat = stat_identity(), mapping = mapping,
    data = data,
    params = drop_null(list(color = color, size = size, alpha = alpha,
                            linewidth = linewidth))
  )
}

# --- dumbbell ----------------------------------------------------------------

#' GeomDumbbell: before/after comparison — two dots joined by a line
#' @noRd
GeomDumbbell <- new_class("GeomDumbbell", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "dumbbell",
      default_params = list(color = "#B8B8C4", linewidth = 2, alpha = 1,
                            size = 4, color_start = "#4A6DB5",
                            color_end = "#D8573B"),
      required_aes = c("x", "xend", "y")
    ))
  }
)

method(build_marks, GeomDumbbell) <- function(geom, scaled) {
  p <- scaled$params
  unname(unlist(lapply(seq_along(scaled$x), function(i) {
    y <- scaled$y[[i]]
    a <- scaled$alpha[[i]]
    list(
      mk_line(c(scaled$x[[i]], scaled$xend[[i]]), c(y, y),
              stroke = scaled$color[[i]], width = p$linewidth, alpha = a),
      mk_circle(scaled$x[[i]], y, r = scaled$size[[i]],
                fill = col_to_hex(p$color_start), alpha = a),
      mk_circle(scaled$xend[[i]], y, r = scaled$size[[i]],
                fill = col_to_hex(p$color_end), alpha = a)
    )
  }), recursive = FALSE))
}

#' Dumbbell layer (before/after per category)
#'
#' Map `x` (start value), `xend` (end value), and `y` (category).
#'
#' @param mapping,data,color,size,alpha As in [geom_point()]; `color` styles
#'   the connector.
#' @param linewidth Connector width.
#' @param color_start,color_end Endpoint dot colors.
#' @return A [Layer].
#' @export
geom_dumbbell <- function(mapping = NULL, data = NULL, color = NULL,
                          size = NULL, alpha = NULL, linewidth = NULL,
                          color_start = NULL, color_end = NULL) {
  Layer(
    geom = GeomDumbbell(), stat = stat_identity(), mapping = mapping,
    data = data,
    params = drop_null(list(color = color, size = size, alpha = alpha,
                            linewidth = linewidth, color_start = color_start,
                            color_end = color_end))
  )
}

# --- waterfall ---------------------------------------------------------------

#' GeomWaterfall: running-total bars with connectors
#' @noRd
GeomWaterfall <- new_class("GeomWaterfall", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "waterfall",
      default_params = list(alpha = 1, linewidth = 1,
                            color_pos = "#4CAF7D", color_neg = "#D95F5F"),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomWaterfall) <- function(geom, scaled) {
  p <- scaled$params
  ord <- order(scaled$x)
  marks <- list()
  for (k in seq_along(ord)) {
    i <- ord[k]
    fill <- col_to_hex(if (scaled$sign[[i]] == "pos") p$color_pos else p$color_neg)
    marks[[length(marks) + 1]] <- mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]],
      scaled$ymin[[i]], scaled$ymax[[i]],
      fill = fill, alpha = scaled$alpha[[i]]
    )
    # Connector from this bar's running total across to the next bar.
    if (k < length(ord)) {
      nxt <- ord[k + 1]
      marks[[length(marks) + 1]] <- mk_line(
        c(scaled$xmax[[i]], scaled$xmin[[nxt]]),
        rep(scaled$yend[[i]], 2),
        stroke = "#8A8A94", width = p$linewidth, dash = "3,2"
      )
    }
  }
  marks
}

#' Waterfall chart layer (running total of signed changes)
#'
#' Categories plot in level order; pass
#' `x = factor(x, levels = unique(x))` to keep them in data order.
#'
#' @param mapping,data,alpha As in [geom_point()].
#' @param width Bar width in x slot units.
#' @param color_pos,color_neg Bar colors for increases/decreases.
#' @return A [Layer].
#' @export
geom_waterfall <- function(mapping = NULL, data = NULL, alpha = NULL,
                           width = 0.8, color_pos = NULL, color_neg = NULL) {
  Layer(
    geom = GeomWaterfall(), stat = stat_waterfall(width = width),
    mapping = mapping, data = data,
    params = drop_null(list(alpha = alpha, color_pos = color_pos,
                            color_neg = color_neg))
  )
}

# --- Kaplan-Meier ------------------------------------------------------------

#' GeomKM: Kaplan-Meier survival curve with censoring ticks
#' @noRd
GeomKM <- new_class("GeomKM", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "km",
      default_params = list(color = "#4A6DB5", linewidth = 1.8, alpha = 1),
      required_aes = c("time", "status")
    ))
  }
)

method(build_marks, GeomKM) <- function(geom, scaled) {
  p <- scaled$params
  unname(unlist(lapply(group_rows(scaled), function(idx) {
    col <- scaled$color[idx][1]
    curve <- idx[scaled$role[idx] == "curve"]
    censor <- idx[scaled$role[idx] == "censor"]
    marks <- list(mk_line(
      scaled$x[curve], scaled$y[curve],
      stroke = col, width = p$linewidth, alpha = scaled$alpha[idx][1]
    ))
    # Censoring ticks: short vertical dashes on the curve.
    for (ci in censor) {
      marks[[length(marks) + 1]] <- mk_line(
        rep(scaled$x[[ci]], 2),
        c(scaled$y[[ci]] - 0.012, scaled$y[[ci]] + 0.012),
        stroke = col, width = p$linewidth
      )
    }
    marks
  }), recursive = FALSE))
}

#' Kaplan-Meier survival curve layer
#'
#' Map `time` and `status` (1/TRUE = event, 0/FALSE = censored); map
#' `color` to compare groups. The at-risk table is a milestone.
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param linewidth Curve width.
#' @return A [Layer].
#' @export
geom_km <- function(mapping = NULL, data = NULL, color = NULL,
                    alpha = NULL, linewidth = NULL) {
  Layer(
    geom = GeomKM(), stat = stat_km(), mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, linewidth = linewidth))
  )
}

# --- ROC ---------------------------------------------------------------------

#' GeomROC: ROC curve with the chance diagonal
#' @noRd
GeomROC <- new_class("GeomROC", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "roc",
      default_params = list(color = "#4A6DB5", linewidth = 1.8, alpha = 1),
      required_aes = c("truth", "score")
    ))
  }
)

method(build_marks, GeomROC) <- function(geom, scaled) {
  p <- scaled$params
  groups <- group_rows(scaled)
  marks <- list()
  # Chance diagonal drawn once, under the curves.
  ref <- which(scaled$role == "ref")[1:2]
  marks[[1]] <- mk_line(
    scaled$x[ref], scaled$y[ref],
    stroke = "#8A8A94", width = 1, dash = "4,3"
  )
  for (idx in groups) {
    curve <- idx[scaled$role[idx] == "curve"]
    marks[[length(marks) + 1]] <- mk_line(
      scaled$x[curve], scaled$y[curve],
      stroke = scaled$color[idx][1], width = p$linewidth,
      alpha = scaled$alpha[idx][1]
    )
  }
  marks
}

#' ROC curve layer
#'
#' Map `truth` (actual class; for factors the second level is the positive
#' class) and `score` (predicted score); map `color` to compare models.
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param linewidth Curve width.
#' @return A [Layer].
#' @export
geom_roc <- function(mapping = NULL, data = NULL, color = NULL,
                     alpha = NULL, linewidth = NULL) {
  Layer(
    geom = GeomROC(), stat = stat_roc(), mapping = mapping, data = data,
    params = drop_null(list(color = color, alpha = alpha, linewidth = linewidth))
  )
}
