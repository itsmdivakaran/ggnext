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
#' (`role = "censor"`) for tick marks.
#'
#' @noRd
StatKM <- new_class("StatKM", parent = Stat,
  constructor = function() new_object(Stat(name = "km", provides = c("x", "y")))
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
    event_times <- unique(time[event])
    n_total <- length(time)
    s <- 1
    xs <- 0
    ys <- 1
    for (t in event_times) {
      at_risk <- sum(time >= t)
      d <- sum(time == t & event)
      s_new <- s * (1 - d / at_risk)
      # Horizontal run to t at the old level, then the vertical drop.
      xs <- c(xs, t, t)
      ys <- c(ys, s, s_new)
      s <- s_new
    }
    # Extend the final level to the last observed time (censored tail).
    if (max(time) > max(xs)) {
      xs <- c(xs, max(time))
      ys <- c(ys, s)
    }

    # Censoring ticks at the survival level in effect at that time.
    cens_times <- time[!event]
    cens_surv <- vapply(cens_times, function(tc) {
      lvl <- ys[max(which(xs <= tc))]
      lvl
    }, numeric(1))

    list(
      x = c(xs, cens_times),
      y = c(ys, cens_surv),
      role = c(rep("curve", length(xs)), rep("censor", length(cens_times)))
    )
  })
  with_labels(out, x = "time", y = "survival probability")
}

#' Kaplan-Meier survival estimate
#' @return A `StatKM` object, to pass as a layer's `stat`.
#' @export
stat_km <- function() StatKM()

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
