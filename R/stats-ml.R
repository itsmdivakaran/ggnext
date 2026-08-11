# Machine-learning stats -------------------------------------------------------
#
# The computations behind the ML diagnostic geoms: beeswarm packing,
# PDP/ICE averaging, confusion-matrix normalization, probability
# calibration binning, lift/gain accumulation, residual trends, silhouette
# ordering, and decision-grid cells.

#' @include classes.R stats.R
NULL

# --- Beeswarm (SHAP) ---------------------------------------------------------

#' StatBeeswarm: nudge overlapping points off a shared row
#' @noRd
StatBeeswarm <- new_class("StatBeeswarm", parent = Stat,
  constructor = function() new_object(Stat(name = "beeswarm"))
)

method(compute_stat, StatBeeswarm) <- function(stat, values) {
  # Points sharing a row are offset vertically in alternating directions so
  # dense regions fan out symmetrically instead of overplotting.
  rows <- values$y
  y <- as.numeric(rows)
  for (r in unique(rows)) {
    idx <- which(rows == r)
    xr <- values$x[idx]
    # Bin along x; within a bin, spread points across the row's height.
    span <- diff(range(xr, na.rm = TRUE))
    bin_w <- if (span == 0) 1 else span / 40
    bins <- if (span == 0) rep(1, length(xr)) else floor((xr - min(xr)) / bin_w)
    for (b in unique(bins)) {
      hit <- idx[bins == b]
      k <- length(hit)
      if (k == 1) next
      # Alternate above/below the row center: 0, +1, -1, +2, -2, ...
      offs <- (seq_len(k) %/% 2) * ifelse(seq_len(k) %% 2 == 0, 1, -1)
      y[hit] <- as.numeric(r) + offs / (k + 1) * 0.7
    }
  }
  values$y <- y
  values
}

# --- Partial dependence ------------------------------------------------------

#' StatPDP: append an average curve to the ICE curves
#' @noRd
StatPDP <- new_class("StatPDP", parent = Stat,
  constructor = function() new_object(Stat(name = "pdp"))
)

method(compute_stat, StatPDP) <- function(stat, values) {
  xs <- sort(unique(values$x))
  avg <- vapply(xs, function(v) mean(values$y[values$x == v], na.rm = TRUE),
                numeric(1))
  n <- length(values$x)
  out <- list(
    x = c(values$x, xs),
    y = c(values$y, avg),
    group = c(as.character(values$group), rep(".average", length(xs)))
  )
  if (!is.null(values$color)) {
    out$color <- c(values$color, rep(values$color[[1]], length(xs)))
  }
  with_labels(out, y = "prediction")
}

# --- Confusion matrix --------------------------------------------------------

#' StatConfusion: cross-tabulate and normalize
#'
#' @param normalize `"row"`, `"col"`, `"all"`, or `"none"`.
#' @noRd
StatConfusion <- new_class("StatConfusion", parent = Stat,
  properties = list(normalize = class_character),
  constructor = function(normalize = "row") {
    new_object(Stat(name = "confusion"), normalize = normalize)
  }
)

method(compute_stat, StatConfusion) <- function(stat, values) {
  # x and y arrive as discrete slot integers; counts come either from a
  # mapped `size` or from counting rows.
  key <- paste(values$x, values$y, sep = "\r")
  counts <- if (!is.null(values$size)) {
    tapply(values$size, key, sum)
  } else {
    table(key)
  }
  keys <- names(counts)
  parts <- do.call(rbind, strsplit(keys, "\r", fixed = TRUE))
  px <- as.numeric(parts[, 1])
  py <- as.numeric(parts[, 2])
  n <- as.numeric(counts)

  denom <- switch(stat@normalize,
    row = vapply(py, function(v) sum(n[py == v]), numeric(1)),
    col = vapply(px, function(v) sum(n[px == v]), numeric(1)),
    all = rep(sum(n), length(n)),
    none = rep(max(n), length(n)),
    stop("`normalize` must be one of: row, col, all, none.")
  )
  rate <- ifelse(denom > 0, n / denom, 0)

  out <- list(
    x = px, y = py,
    xmin = px - 0.5, xmax = px + 0.5,
    ymin = py - 0.5, ymax = py + 0.5,
    # Shade by rate; the count is the annotation.
    color = rate,
    label = as.character(n),
    group = as.character(seq_along(n))
  )
  with_labels(out, x = "predicted", y = "actual")
}

# --- Calibration -------------------------------------------------------------

#' StatCalibration: bin predictions and compute observed rates
#'
#' @param bins Number of equal-width probability bins.
#' @noRd
StatCalibration <- new_class("StatCalibration", parent = Stat,
  properties = list(bins = class_numeric),
  constructor = function(bins = 10) {
    new_object(Stat(name = "calibration"), bins = bins)
  }
)

method(compute_stat, StatCalibration) <- function(stat, values) {
  breaks <- seq(0, 1, length.out = stat@bins + 1)
  idx <- pmin(findInterval(values$x, breaks, rightmost.closed = TRUE),
              stat@bins)
  keep <- sort(unique(idx))
  out <- list(
    x = vapply(keep, function(b) mean(values$x[idx == b]), numeric(1)),
    y = vapply(keep, function(b) mean(values$y[idx == b]), numeric(1))
  )
  out$group <- rep("all", length(out$x))
  with_labels(out, x = "predicted probability", y = "observed rate")
}

# --- Lift / gain -------------------------------------------------------------

#' StatLiftGain: cumulative capture by score rank
#'
#' @param type `"gain"` or `"lift"`.
#' @noRd
StatLiftGain <- new_class("StatLiftGain", parent = Stat,
  properties = list(type = class_character),
  constructor = function(type = "gain") {
    new_object(Stat(name = "lift_gain"), type = type)
  }
)

method(compute_stat, StatLiftGain) <- function(stat, values) {
  ord <- order(values$score, decreasing = TRUE)
  y <- values$truth[ord]
  n <- length(y)
  pop <- seq_len(n) / n
  captured <- cumsum(y) / max(sum(y), 1)
  out <- if (stat@type == "gain") {
    list(x = c(0, pop), y = c(0, captured))
  } else {
    # Lift is capture rate relative to random targeting.
    list(x = pop, y = captured / pop)
  }
  out$group <- rep("all", length(out$x))
  with_labels(
    out,
    x = "proportion targeted",
    y = if (stat@type == "gain") "proportion of positives" else "lift"
  )
}

# --- Residuals ---------------------------------------------------------------

#' StatResidual: pass residuals through and add a loess trend
#'
#' @param smooth Append a trend series.
#' @noRd
StatResidual <- new_class("StatResidual", parent = Stat,
  properties = list(smooth = class_logical),
  constructor = function(smooth = TRUE) {
    new_object(Stat(name = "residual"), smooth = smooth)
  }
)

method(compute_stat, StatResidual) <- function(stat, values) {
  out <- list(
    x = values$x, y = values$y,
    group = rep("points", length(values$x)),
    # Carried so the geom can draw the y = 0 reference in data units.
    yzero = rep(0, length(values$x))
  )
  if (!is.null(values$color)) out$color <- values$color
  if (isTRUE(stat@smooth) && length(values$x) > 4) {
    gx <- seq(min(values$x), max(values$x), length.out = 60)
    fit <- stats::loess(y ~ x, data.frame(x = values$x, y = values$y),
                        span = 0.75)
    gy <- stats::predict(fit, newdata = data.frame(x = gx))
    out$x <- c(out$x, gx)
    out$y <- c(out$y, gy)
    out$group <- c(out$group, rep(".trend", length(gx)))
    out$yzero <- c(out$yzero, rep(0, length(gx)))
    if (!is.null(out$color)) {
      out$color <- c(out$color, rep(values$color[[1]], length(gx)))
    }
  }
  with_labels(out, x = "fitted", y = "residual")
}

# --- Silhouette --------------------------------------------------------------

#' StatSilhouette: order widths within cluster and lay out bars
#' @noRd
StatSilhouette <- new_class("StatSilhouette", parent = Stat,
  constructor = function() new_object(Stat(name = "silhouette"))
)

method(compute_stat, StatSilhouette) <- function(stat, values) {
  # y holds the cluster slot; sort each cluster's widths descending and
  # give every observation an equal share of its cluster's row height.
  out_x <- numeric(0)
  out_ymin <- numeric(0)
  out_ymax <- numeric(0)
  out_col <- character(0)
  out_grp <- character(0)
  for (s in sort(unique(values$y))) {
    idx <- which(values$y == s)
    ord <- idx[order(values$x[idx], decreasing = TRUE)]
    k <- length(ord)
    step <- 0.8 / k
    for (j in seq_len(k)) {
      i <- ord[j]
      base <- s - 0.4 + (j - 1) * step
      out_x <- c(out_x, values$x[i])
      out_ymin <- c(out_ymin, base)
      out_ymax <- c(out_ymax, base + step * 0.9)
      out_col <- c(out_col, if (is.null(values$color)) NA else values$color[i])
      out_grp <- c(out_grp, as.character(s))
    }
  }
  out <- list(
    x = out_x, y = (out_ymin + out_ymax) / 2,
    xmin = rep(0, length(out_x)), xmax = out_x,
    ymin = out_ymin, ymax = out_ymax, group = out_grp
  )
  if (!all(is.na(out_col))) out$color <- out_col
  with_labels(out, x = "silhouette width")
}

# --- Decision boundary -------------------------------------------------------

#' StatDecisionBoundary: expand grid points into cells
#' @noRd
StatDecisionBoundary <- new_class("StatDecisionBoundary", parent = Stat,
  constructor = function() new_object(Stat(name = "decision_boundary"))
)

method(compute_stat, StatDecisionBoundary) <- function(stat, values) {
  # Half a grid step in each direction turns the prediction grid into
  # touching cells that tile the panel.
  hx <- resolution(values$x) / 2
  hy <- resolution(values$y) / 2
  values$xmin <- values$x - hx
  values$xmax <- values$x + hx
  values$ymin <- values$y - hy
  values$ymax <- values$y + hy
  values
}
