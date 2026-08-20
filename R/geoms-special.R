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
    marks <- list()
    # Confidence ribbon first, so the curve line draws on top of it. Built
    # from the same step-interpolated x/y as the curve, so the band's
    # edges follow the staircase shape rather than cutting corners.
    if (isTRUE(p$conf_int) && !is.null(scaled$ymin)) {
      band <- curve[is.finite(scaled$ymin[curve]) & is.finite(scaled$ymax[curve])]
      if (length(band) > 1) {
        marks[[length(marks) + 1]] <- mk_polygon(
          c(scaled$x[band], rev(scaled$x[band])),
          c(scaled$ymax[band], rev(scaled$ymin[band])),
          fill = col, alpha = 0.18
        )
      }
    }
    marks[[length(marks) + 1]] <- mk_line(
      scaled$x[curve], scaled$y[curve],
      stroke = col, width = p$linewidth, alpha = scaled$alpha[idx][1]
    )
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
#' `color` to compare groups. Pair with [geom_km_risktable()] for the
#' classic number-at-risk strip.
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param linewidth Curve width.
#' @param conf_int Draw a 95% confidence band (Greenwood's formula, with a
#'   log-log transform that keeps the bounds inside \[0, 1\]).
#' @return A [Layer].
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   t = c(rexp(40, 0.1), rexp(40, 0.07)),
#'   ev = rbinom(80, 1, 0.7),
#'   arm = rep(c("placebo", "drug"), each = 40)
#' )
#' ggnext(d, aes(time = t, status = ev, color = arm)) +
#'   geom_km(conf_int = TRUE)
#' @export
geom_km <- function(mapping = NULL, data = NULL, color = NULL,
                    alpha = NULL, linewidth = NULL, conf_int = FALSE) {
  Layer(
    geom = GeomKM(), stat = stat_km(conf_int = conf_int), mapping = mapping,
    data = data,
    params = drop_null(list(color = color, alpha = alpha,
                            linewidth = linewidth, conf_int = conf_int))
  )
}

# --- Nelson-Aalen --------------------------------------------------------

#' GeomNelsonAalen: cumulative hazard step curve
#' @noRd
GeomNelsonAalen <- new_class("GeomNelsonAalen", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "nelson_aalen",
      default_params = list(color = "#4A6DB5", linewidth = 1.8, alpha = 1),
      required_aes = c("time", "status")
    ))
  }
)

method(build_marks, GeomNelsonAalen) <- function(geom, scaled) {
  p <- scaled$params
  unname(lapply(group_rows(scaled), function(idx) {
    ord <- idx[order(scaled$x[idx])]
    mk_line(
      scaled$x[ord], scaled$y[ord], stroke = scaled$color[[ord[1]]],
      width = p$linewidth, alpha = scaled$alpha[[ord[1]]]
    )
  }))
}

#' Nelson-Aalen cumulative hazard curve layer
#'
#' Map `time` and `status` (1/TRUE = event, 0/FALSE = censored) exactly as
#' for [geom_km()]; map `color` to compare groups. The cumulative hazard
#' H(t) is the additive counterpart of the Kaplan-Meier survival curve —
#' useful when the *rate* of events, not the surviving fraction, is what
#' matters.
#'
#' @param mapping,data,color,alpha As in [geom_point()].
#' @param linewidth Curve width.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(t = rexp(80, 0.08), ev = rbinom(80, 1, 0.75))
#' ggnext(d, aes(time = t, status = ev)) +
#'   geom_nelson_aalen() +
#'   labs(title = "Cumulative hazard", x = "Time", y = "H(t)")
#' @export
geom_nelson_aalen <- function(mapping = NULL, data = NULL, color = NULL,
                              alpha = NULL, linewidth = NULL) {
  layer_new(GeomNelsonAalen(), StatNelsonAalen(), mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth))
}

# --- KM risk table -----------------------------------------------------------

#' GeomKMRiskTable: number-at-risk table drawn in the lower panel strip
#'
#' `build_marks()` reads the precomputed group/tick table off `scaled$table`
#' (see `StatKMRiskTable`) and converts tick times into normalized x
#' through `scaled$xdomain` — the panel's real, already-expanded x
#' domain — exactly like `geom_abline()` does. That is what lets the
#' table's columns line up with a `geom_km()` curve sharing the same
#' panel, expansion and all, without the table needing its own x scale.
#'
#' @noRd
GeomKMRiskTable <- new_class("GeomKMRiskTable", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "km_risktable",
      default_params = list(label_size = 11, band_height = 0.18),
      required_aes = c("time", "status")
    ))
  }
)

method(build_marks, GeomKMRiskTable) <- function(geom, scaled) {
  tb <- scaled$table
  if (is.null(tb) || length(tb$groups) == 0) {
    return(list())
  }
  p <- scaled$params
  xd <- scaled$xdomain
  span <- xd[2] - xd[1]
  if (!is.finite(span) || span <= 0) span <- 1

  n <- length(tb$groups)
  band <- p$band_height %||% 0.18
  row_h <- band / (n + 1)
  size <- p$label_size %||% 11

  marks <- list(mk_text(
    0.02, band - row_h * 0.4, "Number at risk", size = size,
    color = "#4A5568", alpha = 1, anchor = "start"
  ))
  for (i in seq_len(n)) {
    y <- band - (i + 0.6) * row_h
    col <- if (!is.null(tb$colors)) tb$colors[[i]] else "#1A2233"
    marks[[length(marks) + 1]] <- mk_text(
      0.02, y, tb$groups[[i]], size = size, color = col, alpha = 1,
      anchor = "start"
    )
    counts <- tb$counts[[i]]
    for (j in seq_along(tb$breaks)) {
      xt <- (tb$breaks[[j]] - xd[1]) / span
      marks[[length(marks) + 1]] <- mk_text(
        xt, y, as.character(counts[[j]]), size = size, color = col,
        alpha = 1, anchor = "middle"
      )
    }
  }
  marks
}

#' Number-at-risk table for a Kaplan-Meier curve
#'
#' Draws the classic "number at risk" strip beneath a survival curve: for
#' each group, the count of subjects still at risk (`time >= tick`) at a
#' handful of tick times, laid out as an overlay in the bottom ~18% of the
#' panel. It is a single layer, not a separate sub-panel — add it
#' alongside [geom_km()] on the same plot.
#'
#' This layer must share the same `time`/`status`/grouping mapping and
#' data as the [geom_km()] layer it accompanies, so the two are trained on
#' the same time domain and its tick columns line up with the curve above
#' them.
#'
#' @param mapping,data Standard layer overrides. Map `time`, `status`, and
#'   (optionally) `color`/`group` exactly as for [geom_km()].
#' @param breaks Tick times to report counts at; `NULL` (default) picks
#'   about six round numbers spanning the data with `pretty()`.
#' @param label_size Text size in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   t = c(rexp(40, 0.1), rexp(40, 0.07)),
#'   ev = rbinom(80, 1, 0.7),
#'   arm = rep(c("placebo", "drug"), each = 40)
#' )
#' ggnext(d, aes(time = t, status = ev, color = arm)) +
#'   geom_km() +
#'   geom_km_risktable()
#' @export
geom_km_risktable <- function(mapping = NULL, data = NULL, breaks = NULL,
                              label_size = NULL) {
  layer_new(GeomKMRiskTable(), StatKMRiskTable(breaks = breaks), mapping, data,
            list(label_size = label_size))
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
