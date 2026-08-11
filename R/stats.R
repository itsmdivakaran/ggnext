# Statistical transformations --------------------------------------------------
#
# Every stat is an S7 subclass of Stat with a compute_stat() method. A stat
# receives the evaluated (and, for categorical positions, already
# slot-indexed) aesthetic values as a named list of equal-length vectors and
# returns a new named list — possibly with a different number of rows and
# new columns (ymin/ymax, lower/upper, roles). Group-wise stats split on
# `values$group` and carry `group` and `color` through so downstream color
# mapping still works. All statistical math uses base R's stats package,
# written out explicitly — no ggplot2 code to fall back on.

#' @include classes.R
NULL

# --- helpers -----------------------------------------------------------------

# Apply `fn` to each group of rows and concatenate the resulting columns.
# `fn` receives the subset value-list and must return a named list of
# equal-length vectors (including any passthrough columns it wants to keep).
stat_by_group <- function(values, fn) {
  groups <- unique(values$group)
  parts <- lapply(groups, function(g) {
    idx <- which(values$group == g)
    sub <- lapply(values, function(col) {
      if (length(col) == length(values$group)) col[idx] else col
    })
    out <- fn(sub)
    n_out <- length(out[[1]])
    out$group <- rep(g, n_out)
    # Carry one representative color per group (pre-mapping raw value).
    if (!is.null(sub$color)) out$color <- rep(sub$color[[1]], n_out)
    out
  })
  nms <- unique(unlist(lapply(parts, names)))
  result <- lapply(nms, function(nm) {
    do.call(c, lapply(parts, function(p) {
      if (is.null(p[[nm]])) rep(NA, length(p[[1]])) else p[[nm]]
    }))
  })
  names(result) <- nms
  result
}

# Smallest gap between consecutive unique values; 1 for a single value
# (matches the slot width of a discrete axis).
resolution <- function(v) {
  u <- sort(unique(v))
  if (length(u) < 2) 1 else min(diff(u))
}

# Attach axis-title hints for stat-computed columns (histogram counts etc.).
with_labels <- function(values, ...) {
  attr(values, "computed_labels") <- list(...)
  values
}

# --- stat_count (bars) -------------------------------------------------------

#' StatCount: one count per distinct x (bar charts)
#'
#' @param width Bar width as a fraction of the x resolution.
#' @noRd
StatCount <- new_class("StatCount", parent = Stat,
  properties = list(width = class_numeric),
  constructor = function(width = 0.8) {
    new_object(Stat(name = "count"), width = width)
  }
)

method(compute_stat, StatCount) <- function(stat, values) {
  res <- resolution(values$x)
  out <- stat_by_group(values, function(sub) {
    tab <- table(sub$x)
    list(
      x = as.numeric(names(tab)),
      y = as.numeric(tab),
      xwidth = rep(stat@width * res, length(tab))
    )
  })
  with_labels(out, y = "count")
}

#' Count observations at each x
#' @return A `StatCount` object, to pass as a layer's `stat`.
#' @param width Bar width as a fraction of the x resolution.
#' @export
stat_count <- function(width = 0.8) StatCount(width = width)

# --- stat_col (bars from data values) ----------------------------------------

#' StatCol: pass y through and attach a bar width (column charts)
#'
#' @param width Bar width as a fraction of the x resolution.
#' @noRd
StatCol <- new_class("StatCol", parent = Stat,
  properties = list(width = class_numeric),
  constructor = function(width = 0.8) {
    new_object(Stat(name = "col"), width = width)
  }
)

method(compute_stat, StatCol) <- function(stat, values) {
  values$xwidth <- rep(stat@width * resolution(values$x), length(values$x))
  values
}

# --- stat_bin (histograms) ---------------------------------------------------

#' StatBin: bin x values and count per bin (histograms)
#'
#' Breaks are computed over the full x range (all groups share bins).
#'
#' @param bins Number of bins (ignored when `binwidth` is given).
#' @param binwidth Bin width in data units.
#' @noRd
StatBin <- new_class("StatBin", parent = Stat,
  properties = list(bins = class_numeric, binwidth = class_any),
  constructor = function(bins = 30, binwidth = NULL) {
    new_object(Stat(name = "bin"), bins = bins, binwidth = binwidth)
  }
)

method(compute_stat, StatBin) <- function(stat, values) {
  rng <- range(values$x, na.rm = TRUE)
  bw <- stat@binwidth
  # Half-open bins [b_i, b_{i+1}), left edge aligned to the data minimum,
  # with the final bin closed so the maximum lands inside it rather than
  # in a spurious bin of its own. `bins = n` therefore yields exactly n
  # bins; an explicit `binwidth` yields as many as the range needs.
  breaks <- if (is.null(bw)) {
    if (rng[2] == rng[1]) {
      bw <- 1 # all-equal x: one unit-wide bin
      c(rng[1], rng[1] + bw)
    } else {
      bw <- (rng[2] - rng[1]) / stat@bins
      seq(rng[1], rng[2], length.out = stat@bins + 1)
    }
  } else {
    seq(rng[1], rng[2] + bw, by = bw)
  }
  out <- stat_by_group(values, function(sub) {
    bin_idx <- findInterval(sub$x, breaks, rightmost.closed = TRUE)
    tab <- tabulate(bin_idx, nbins = length(breaks) - 1)
    keep <- which(tab > 0)
    list(
      x = breaks[keep] + bw / 2,
      y = as.numeric(tab[keep]),
      xwidth = rep(bw, length(keep))
    )
  })
  with_labels(out, y = "count")
}

#' Bin observations (histogram counts)
#' @param bins Number of bins (ignored when `binwidth` is given).
#' @param binwidth Bin width in data units.
#' @return A `StatBin` object, to pass as a layer's `stat`.
#' @export
stat_bin <- function(bins = 30, binwidth = NULL) {
  StatBin(bins = bins, binwidth = binwidth)
}

# --- stat_density (density curves) -------------------------------------------

#' StatDensity: kernel density estimate of x
#'
#' @param n Number of evaluation points.
#' @param adjust Bandwidth multiplier (as in `stats::density`).
#' @noRd
StatDensity <- new_class("StatDensity", parent = Stat,
  properties = list(n = class_numeric, adjust = class_numeric),
  constructor = function(n = 256, adjust = 1) {
    new_object(Stat(name = "density"), n = n, adjust = adjust)
  }
)

method(compute_stat, StatDensity) <- function(stat, values) {
  out <- stat_by_group(values, function(sub) {
    d <- stats::density(sub$x, n = stat@n, adjust = stat@adjust)
    # ymin = 0 keeps the baseline in the trained y domain so the filled
    # area under the curve always has somewhere to sit.
    list(x = d$x, y = d$y, ymin = rep(0, length(d$x)))
  })
  with_labels(out, y = "density")
}

#' Kernel density estimate
#' @param n Number of evaluation points.
#' @param adjust Bandwidth multiplier.
#' @return A `StatDensity` object, to pass as a layer's `stat`.
#' @export
stat_density <- function(n = 256, adjust = 1) StatDensity(n = n, adjust = adjust)

# --- stat_area (identity + zero baseline) ------------------------------------

#' StatArea: identity plus a zero baseline for area charts
#'
#' @noRd
StatArea <- new_class("StatArea", parent = Stat,
  constructor = function() new_object(Stat(name = "area"))
)

method(compute_stat, StatArea) <- function(stat, values) {
  values$ymin <- rep(0, length(values$y))
  values
}

# --- stat_boxplot ------------------------------------------------------------

#' StatBoxplot: five-number summary per x slot
#'
#' Tukey boxplot: box at the quartiles, whiskers to the furthest point
#' within `coef` * IQR of the box, outliers beyond as separate rows with
#' `role = "outlier"`.
#'
#' @param width Box width in x slot units.
#' @param coef Whisker length multiplier.
#' @noRd
StatBoxplot <- new_class("StatBoxplot", parent = Stat,
  properties = list(width = class_numeric, coef = class_numeric),
  constructor = function(width = 0.6, coef = 1.5) {
    new_object(Stat(name = "boxplot"), width = width, coef = coef)
  }
)

method(compute_stat, StatBoxplot) <- function(stat, values) {
  # A box per distinct x *and* group (so grouped boxplots work).
  values$group <- paste(values$group, values$x, sep = "\r")
  out <- stat_by_group(values, function(sub) {
    q <- stats::quantile(sub$y, c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE)
    iqr <- q[3] - q[1]
    in_lo <- sub$y[sub$y >= q[1] - stat@coef * iqr]
    in_hi <- sub$y[sub$y <= q[3] + stat@coef * iqr]
    whisker_lo <- min(in_lo)
    whisker_hi <- max(in_hi)
    outliers <- sub$y[sub$y < whisker_lo | sub$y > whisker_hi]
    x0 <- sub$x[[1]]
    n_out <- length(outliers)
    list(
      x = c(x0, rep(x0, n_out)),
      y = c(NA, outliers),
      lower = c(q[1], rep(NA, n_out)),
      middle = c(q[2], rep(NA, n_out)),
      upper = c(q[3], rep(NA, n_out)),
      ymin = c(whisker_lo, rep(NA, n_out)),
      ymax = c(whisker_hi, rep(NA, n_out)),
      xwidth = c(stat@width * resolution(values$x), rep(NA, n_out)),
      role = c("box", rep("outlier", n_out))
    )
  })
  out
}

#' Five-number summary for boxplots
#' @param width Box width in x slot units.
#' @param coef Whisker length multiplier.
#' @return A `StatBoxplot` object, to pass as a layer's `stat`.
#' @export
stat_boxplot <- function(width = 0.6, coef = 1.5) {
  StatBoxplot(width = width, coef = coef)
}

# --- stat_ydensity (violins) -------------------------------------------------

#' StatYdensity: mirrored density of y per x slot (violin plots)
#'
#' @param width Maximum violin width in x slot units.
#' @param n Number of density evaluation points.
#' @noRd
StatYdensity <- new_class("StatYdensity", parent = Stat,
  properties = list(width = class_numeric, n = class_numeric),
  constructor = function(width = 0.9, n = 101) {
    new_object(Stat(name = "ydensity"), width = width, n = n)
  }
)

method(compute_stat, StatYdensity) <- function(stat, values) {
  values$group <- paste(values$group, values$x, sep = "\r")
  stat_by_group(values, function(sub) {
    d <- stats::density(sub$y, n = stat@n)
    half <- (d$y / max(d$y)) * (stat@width / 2)
    x0 <- sub$x[[1]]
    list(
      x = rep(x0, length(d$x)),
      y = d$x, # density evaluated along the y axis
      xmin = x0 - half,
      xmax = x0 + half
    )
  })
}

#' Mirrored y-density for violins
#' @param width Maximum violin width in x slot units.
#' @param n Number of density evaluation points.
#' @return A `StatYdensity` object, to pass as a layer's `stat`.
#' @export
stat_ydensity <- function(width = 0.9, n = 101) StatYdensity(width = width, n = n)

# --- stat_smooth -------------------------------------------------------------

#' StatSmooth: fitted trend with a confidence band
#'
#' `method = "lm"` fits a straight line with an analytic confidence
#' interval; `method = "loess"` fits a local regression with a normal-
#' approximation band from the standard errors.
#'
#' @param method `"loess"` (default) or `"lm"`.
#' @param se Include the confidence band.
#' @param n Number of grid points for the fitted curve.
#' @param level Confidence level.
#' @noRd
StatSmooth <- new_class("StatSmooth", parent = Stat,
  properties = list(
    method = class_character, se = class_logical,
    n = class_numeric, level = class_numeric
  ),
  constructor = function(method = "loess", se = TRUE, n = 80, level = 0.95) {
    new_object(Stat(name = "smooth"), method = method, se = se, n = n, level = level)
  }
)

method(compute_stat, StatSmooth) <- function(stat, values) {
  stat_by_group(values, function(sub) {
    df <- data.frame(x = sub$x, y = sub$y)
    grid <- data.frame(x = seq(min(df$x), max(df$x), length.out = stat@n))
    if (stat@method == "lm") {
      fit <- stats::lm(y ~ x, data = df)
      pr <- stats::predict(fit, grid, interval = "confidence", level = stat@level)
      out <- list(x = grid$x, y = unname(pr[, "fit"]))
      if (stat@se) {
        out$ymin <- unname(pr[, "lwr"])
        out$ymax <- unname(pr[, "upr"])
      }
    } else if (stat@method == "loess") {
      fit <- stats::loess(y ~ x, data = df)
      pr <- stats::predict(fit, grid, se = stat@se)
      if (stat@se) {
        z <- stats::qnorm((1 + stat@level) / 2)
        out <- list(
          x = grid$x, y = as.numeric(pr$fit),
          ymin = as.numeric(pr$fit - z * pr$se.fit),
          ymax = as.numeric(pr$fit + z * pr$se.fit)
        )
      } else {
        out <- list(x = grid$x, y = as.numeric(pr))
      }
    } else {
      stop("Unknown smooth method: ", stat@method, " (use \"loess\" or \"lm\")")
    }
    out
  })
}

#' Fitted trend line with confidence band
#' @param method `"loess"` (default) or `"lm"`.
#' @param se Include the confidence band.
#' @param n Number of grid points.
#' @param level Confidence level.
#' @return A `StatSmooth` object, to pass as a layer's `stat`.
#' @export
stat_smooth <- function(method = "loess", se = TRUE, n = 80, level = 0.95) {
  StatSmooth(method = method, se = se, n = n, level = level)
}

# --- stat_jitter -------------------------------------------------------------

#' StatJitter: add uniform noise to positions (strip charts)
#'
#' Deterministic for a given seed; the global RNG state is left untouched.
#'
#' @param width Horizontal jitter half-range in data units; defaults to 40%
#'   of the x resolution.
#' @param height Vertical jitter half-range; defaults to 0.
#' @param seed RNG seed, fixed so renders are reproducible.
#' @noRd
StatJitter <- new_class("StatJitter", parent = Stat,
  properties = list(width = class_any, height = class_any, seed = class_numeric),
  constructor = function(width = NULL, height = NULL, seed = 42) {
    new_object(Stat(name = "jitter"), width = width, height = height, seed = seed)
  }
)

method(compute_stat, StatJitter) <- function(stat, values) {
  w <- stat@width %||% (0.4 * resolution(values$x))
  h <- stat@height %||% 0
  n <- length(values$x)
  # Local RNG: jitter must not perturb the user's random stream.
  noise <- with_seed(stat@seed, list(
    x = stats::runif(n, -w, w),
    y = if (h > 0) stats::runif(n, -h, h) else NULL
  ))
  values$x <- values$x + noise$x
  if (h > 0) values$y <- values$y + noise$y
  values
}

#' Jitter positions for strip charts
#' @param width Horizontal jitter half-range in data units.
#' @param height Vertical jitter half-range.
#' @param seed RNG seed (fixed for reproducible renders).
#' @return A `StatJitter` object, to pass as a layer's `stat`.
#' @export
stat_jitter <- function(width = NULL, height = NULL, seed = 42) {
  StatJitter(width = width, height = height, seed = seed)
}
