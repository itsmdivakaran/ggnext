# Specialty statistics: survival, classifier diagnostics, running totals -----
#
# Same contract as R/stats.R. The estimators are written out longhand — the
# Kaplan-Meier product-limit estimator and the empirical ROC curve are a few
# lines each when you own the whole pipeline.

#' @include classes.R stats.R
NULL

# --- Kaplan-Meier ------------------------------------------------------------

#' StatKM: Kaplan-Meier product-limit survival estimate
#'
#' Consumes `time` (follow-up time) and `status` (1/TRUE = event,
#' 0/FALSE = censored) aesthetics and emits the stepped survival curve as
#' `x`/`y` rows (`role = "curve"`), plus one row per censoring time
#' (`role = "censor"`) for tick marks. When `conf_int` is `TRUE`, `ymin`/
#' `ymax` columns carry a log-log-transformed 95% confidence band derived
#' from the Greenwood variance estimate (`NA` on censor rows).
#'
#' @param conf_int Compute a 95% confidence band via Greenwood's formula.
#' @noRd
StatKM <- new_class("StatKM", parent = Stat,
  properties = list(conf_int = class_logical),
  constructor = function(conf_int = FALSE) {
    new_object(Stat(name = "km", provides = c("x", "y")), conf_int = conf_int)
  }
)

method(compute_stat, StatKM) <- function(stat, values) {
  if (is.null(values$time) || is.null(values$status)) {
    stop("stat_km() requires aes(time = , status = ) (status: 1 = event, 0 = censored).")
  }
  out <- stat_by_group(values, function(sub) {
    ord <- order(sub$time)
    time <- sub$time[ord]
    event <- as.logical(sub$status[ord])

    # Product-limit estimator: at each distinct event time t_i with d_i
    # events out of n_i still at risk, survival multiplies by (1 - d_i/n_i).
    # In parallel, `gw` accumulates the Greenwood sum term
    # sum(d_i / (n_i * (n_i - d_i))); Var(S(t)) = S(t)^2 * gw(t).
    event_times <- unique(time[event])
    s <- 1
    gw <- 0
    xs <- 0
    ys <- 1
    gws <- 0
    for (t in event_times) {
      at_risk <- sum(time >= t)
      d <- sum(time == t & event)
      s_new <- s * (1 - d / at_risk)
      gw_new <- gw
      if (at_risk > d) {
        # A term is undefined once the risk set is exhausted (n_i == d_i,
        # so S(t) drops straight to 0 and the log-log CI degenerates
        # anyway); skip it rather than divide by zero.
        gw_new <- gw + d / (at_risk * (at_risk - d))
      }
      # Horizontal run to t at the old level, then the vertical drop.
      xs <- c(xs, t, t)
      ys <- c(ys, s, s_new)
      gws <- c(gws, gw, gw_new)
      s <- s_new
      gw <- gw_new
    }
    # Extend the final level to the last observed time (censored tail).
    if (max(time) > max(xs)) {
      xs <- c(xs, max(time))
      ys <- c(ys, s)
      gws <- c(gws, gw)
    }

    # Censoring ticks at the survival level in effect at that time.
    cens_times <- time[!event]
    cens_surv <- vapply(cens_times, function(tc) {
      lvl <- ys[max(which(xs <= tc))]
      lvl
    }, numeric(1))

    out <- list(
      x = c(xs, cens_times),
      y = c(ys, cens_surv),
      role = c(rep("curve", length(xs)), rep("censor", length(cens_times)))
    )
    if (isTRUE(stat@conf_int)) {
      ci <- km_greenwood_ci(ys, gws)
      # Censor rows carry no band of their own (drop_non_finite() leaves
      # NA intervals alone, same convention as geom_forecast_band()).
      out$ymin <- c(ci$lower, rep(NA, length(cens_times)))
      out$ymax <- c(ci$upper, rep(NA, length(cens_times)))
    }
    out
  })
  with_labels(out, x = "time", y = "survival probability")
}

# Greenwood's log-log-transformed 95% CI for a Kaplan-Meier curve.
#
#   Var(S(t)) = S(t)^2 * sum(t_i <= t) d_i / (n_i * (n_i - d_i))   [Greenwood]
#   C = exp(z * sqrt(Var(S(t))) / (S(t) * log(S(t))))
#   S_lower = S(t)^(1/C),  S_upper = S(t)^C
#
# which keeps both bounds inside [0, 1] (unlike a symmetric S(t) +/- z*SE
# band). `gw_sum` is the raw Greenwood sum term (not yet multiplied by
# S(t)^2); S(t) == 0 or 1 has no uncertainty to show, so the bound there
# is just S(t) itself.
km_greenwood_ci <- function(surv, gw_sum, z = stats::qnorm(0.975)) {
  lower <- surv
  upper <- surv
  ok <- is.finite(surv) & surv > 0 & surv < 1 & is.finite(gw_sum) & gw_sum > 0
  if (any(ok)) {
    theta <- z * sqrt(gw_sum[ok]) / log(surv[ok])
    cc <- exp(theta)
    lower[ok] <- surv[ok]^(1 / cc)
    upper[ok] <- surv[ok]^cc
  }
  lower <- pmin(pmax(lower, 0), 1)
  upper <- pmin(pmax(upper, 0), 1)
  list(lower = pmin(lower, upper), upper = pmax(lower, upper))
}

#' Kaplan-Meier survival estimate
#' @param conf_int Compute a 95% confidence band (Greenwood's formula).
#' @return A `StatKM` object, to pass as a layer's `stat`.
#' @export
stat_km <- function(conf_int = FALSE) StatKM(conf_int = conf_int)

# --- Nelson-Aalen --------------------------------------------------------

#' StatNelsonAalen: Nelson-Aalen cumulative hazard estimate
#'
#' Consumes the same `time`/`status` aesthetics as [stat_km()] and emits
#' the stepped cumulative hazard H(t) = sum(t_i <= t) d_i / n_i, the
#' additive counterpart of the multiplicative Kaplan-Meier estimator.
#'
#' @noRd
StatNelsonAalen <- new_class("StatNelsonAalen", parent = Stat,
  constructor = function() {
    new_object(Stat(name = "nelson_aalen", provides = c("x", "y")))
  }
)

method(compute_stat, StatNelsonAalen) <- function(stat, values) {
  if (is.null(values$time) || is.null(values$status)) {
    stop("stat_nelson_aalen() requires aes(time = , status = ) (status: 1 = event, 0 = censored).")
  }
  out <- stat_by_group(values, function(sub) {
    ord <- order(sub$time)
    time <- sub$time[ord]
    event <- as.logical(sub$status[ord])

    # Same event-time loop as StatKM, but the hazard accumulates
    # additively (d_i / n_i) instead of multiplying survival down.
    event_times <- unique(time[event])
    h <- 0
    xs <- 0
    ys <- 0
    for (t in event_times) {
      at_risk <- sum(time >= t)
      d <- sum(time == t & event)
      h_new <- h + d / at_risk
      xs <- c(xs, t, t)
      ys <- c(ys, h, h_new)
      h <- h_new
    }
    if (max(time) > max(xs)) {
      xs <- c(xs, max(time))
      ys <- c(ys, h)
    }
    list(x = xs, y = ys)
  })
  with_labels(out, x = "time", y = "cumulative hazard")
}

#' Nelson-Aalen cumulative hazard estimate
#' @return A `StatNelsonAalen` object, to pass as a layer's `stat`.
#' @export
stat_nelson_aalen <- function() StatNelsonAalen()

# --- KM risk table ---------------------------------------------------------

#' StatKMRiskTable: number-at-risk counts per group at fixed time ticks
#'
#' Consumes `time`, `status`, and the color/group aesthetic that
#' [stat_km()] would use to split arms, and precomputes a small
#' group-by-tick table of counts still at risk (`time >= tick`). The
#' table itself travels as an opaque `table` element (groups/breaks/
#' counts/colors); `GeomKMRiskTable` turns it into text marks once the
#' panel's real x domain is known (see `build_marks()`), the same way
#' `geom_abline()` waits for `scaled$xdomain`.
#'
#' @param breaks Explicit tick times, or `NULL` for `pretty()`.
#' @noRd
StatKMRiskTable <- new_class("StatKMRiskTable", parent = Stat,
  properties = list(breaks = class_any),
  constructor = function(breaks = NULL) {
    new_object(Stat(name = "km_risktable"), breaks = breaks)
  }
)

method(compute_stat, StatKMRiskTable) <- function(stat, values) {
  if (is.null(values$time) || is.null(values$status)) {
    stop("stat_km_risktable() requires aes(time = , status = ).", call. = FALSE)
  }
  time <- values$time
  groups <- values$group %||% rep("all", length(time))
  ug <- unique(groups)
  max_t <- max(time, na.rm = TRUE)

  breaks <- stat@breaks %||% pretty(c(0, max_t), n = 6)
  breaks <- sort(unique(breaks[breaks >= 0 & breaks <= max_t]))
  if (length(breaks) == 0) breaks <- 0

  counts <- lapply(ug, function(g) {
    tt <- time[groups == g]
    vapply(breaks, function(t) sum(tt >= t), numeric(1))
  })
  colors <- if (!is.null(values$color)) {
    vapply(ug, function(g) as.character(values$color[groups == g][1]), character(1))
  } else {
    NULL
  }

  list(
    # A 2-point placeholder spanning the observed time range: when this
    # layer trains the x scale by itself (no geom_km() present, see
    # NON_TRAINING_GEOMS's fallback), that is exactly the domain it
    # should get; paired with geom_km(), its own real event times win
    # and this placeholder is excluded from training entirely.
    x = c(0, max_t), y = c(0, 1),
    group = rep("__km_risktable__", 2),
    table = list(groups = ug, breaks = breaks, counts = counts, colors = colors)
  )
}

#' Number-at-risk table for a Kaplan-Meier curve
#' @param breaks Explicit tick times; `NULL` (default) uses `pretty()`.
#' @return A `StatKMRiskTable` object, to pass as a layer's `stat`.
#' @export
stat_km_risktable <- function(breaks = NULL) StatKMRiskTable(breaks = breaks)

# --- ROC ---------------------------------------------------------------------

#' StatROC: empirical ROC curve
#'
#' Consumes `truth` (actual class: logical, 0/1, or a two-level factor whose
#' *second* level is the positive class) and `score` (predicted score,
#' higher = more positive) aesthetics; emits FPR (`x`) / TPR (`y`) rows
#' (`role = "curve"`) plus two `role = "ref"` rows for the chance diagonal.
#'
#' @noRd
StatROC <- new_class("StatROC", parent = Stat,
  constructor = function() new_object(Stat(name = "roc", provides = c("x", "y")))
)

method(compute_stat, StatROC) <- function(stat, values) {
  if (is.null(values$truth) || is.null(values$score)) {
    stop("stat_roc() requires aes(truth = , score = ).")
  }
  out <- stat_by_group(values, function(sub) {
    truth <- sub$truth
    if (is.factor(truth)) truth <- truth == levels(truth)[2]
    truth <- as.logical(truth)
    ord <- order(sub$score, decreasing = TRUE)
    truth <- truth[ord]
    n_pos <- sum(truth)
    n_neg <- sum(!truth)
    if (n_pos == 0 || n_neg == 0) {
      stop("stat_roc() needs both positive and negative cases in `truth`.")
    }
    # Sweeping the threshold from +Inf downward admits one observation at a
    # time; TPR/FPR are the cumulative positive/negative rates.
    tpr <- c(0, cumsum(truth) / n_pos)
    fpr <- c(0, cumsum(!truth) / n_neg)
    list(
      x = c(fpr, 0, 1),
      y = c(tpr, 0, 1),
      role = c(rep("curve", length(fpr)), "ref", "ref")
    )
  })
  with_labels(out, x = "false positive rate", y = "true positive rate")
}

#' Empirical ROC curve
#' @return A `StatROC` object, to pass as a layer's `stat`.
#' @export
stat_roc <- function() StatROC()

# --- Waterfall ---------------------------------------------------------------

#' StatWaterfall: running-total bars from signed changes
#'
#' Consumes `x` (category, in appearance order when passed as a factor) and
#' `y` (signed change); emits one bar per category running from the previous
#' cumulative total to the new one, with a `sign` column (`"pos"`/`"neg"`)
#' for coloring.
#'
#' @param width Bar width in x slot units.
#' @noRd
StatWaterfall <- new_class("StatWaterfall", parent = Stat,
  properties = list(width = class_numeric),
  constructor = function(width = 0.8) {
    new_object(Stat(name = "waterfall"), width = width)
  }
)

method(compute_stat, StatWaterfall) <- function(stat, values) {
  ord <- order(values$x)
  x <- values$x[ord]
  change <- values$y[ord]
  totals <- cumsum(change)
  start <- c(0, totals[-length(totals)])
  half <- stat@width / 2
  out <- list(
    x = x,
    xmin = x - half,
    xmax = x + half,
    ymin = pmin(start, totals),
    ymax = pmax(start, totals),
    # Running-total endpoints (in bar order) for the connector lines.
    yend = totals,
    sign = ifelse(change >= 0, "pos", "neg"),
    group = values$group[ord]
  )
  if (!is.null(values$color)) out$color <- values$color[ord]
  with_labels(out, y = "running total")
}

#' Running totals for waterfall charts
#' @param width Bar width in x slot units.
#' @return A `StatWaterfall` object, to pass as a layer's `stat`.
#' @export
stat_waterfall <- function(width = 0.8) StatWaterfall(width = width)
