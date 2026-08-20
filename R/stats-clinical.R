# Biostatistics stats ----------------------------------------------------------
#
# The estimators behind the clinical geoms, written from scratch:
# Aalen-Johansen cumulative incidence, Bland-Altman limits of agreement,
# four-parameter log-logistic dose-response fitting, and the layout maths
# for forest, swimmer, waterfall, spaghetti, AE-heatmap, and CONSORT.

#' @include classes.R stats.R marks.R
NULL

# --- Forest ------------------------------------------------------------------

#' StatForest: swap interval aesthetics onto the x axis
#'
#' @param ref Reference-line position in data units.
#' @noRd
StatForest <- new_class("StatForest", parent = Stat,
  properties = list(ref = class_any),
  constructor = function(ref = 1) {
    new_object(Stat(name = "forest"), ref = ref)
  }
)

method(compute_stat, StatForest) <- function(stat, values) {
  # A forest plot's interval runs along x, but users naturally write
  # ymin/ymax; move them onto the x family so scale training sees them.
  if (!is.null(values$ymin)) {
    values$xmin <- values$ymin
    values$xmax <- values$ymax
    values$ymin <- NULL
    values$ymax <- NULL
  }
  if (is.null(values$xmin)) {
    values$xmin <- values$x
    values$xmax <- values$x
  }
  if (!is.na(stat@ref)) {
    values$xref <- rep(stat@ref, length(values$x))
  }
  values
}

# --- Cox proportional hazards -------------------------------------------------

# Newton-Raphson fit of a Cox proportional-hazards model with Breslow
# handling of tied event times (the simplification that treats every event
# at a tied time as contributing to one combined risk-set term, rather
# than Efron's exact partial-likelihood correction — the standard
# trade-off for keeping this dependency-free and correct for the common,
# lightly-tied case).
#
# For each distinct event time t_j with risk set R_j (n_j subjects with
# time >= t_j) and d_j events tied at t_j:
#   score      U(beta) += sum_{i in D_j} x_i - d_j * S1_j / S0_j
#   information I(beta) += d_j * (S2_j / S0_j - outer(S1_j / S0_j, S1_j / S0_j))
# where S0_j = sum_{k in R_j} w_k, S1_j = sum_{k in R_j} w_k * x_k,
# S2_j = sum_{k in R_j} w_k * x_k %*% t(x_k), and w_k = exp(x_k' beta).
cox_newton_raphson <- function(time, event, X, max_iter = 50, tol = 1e-9) {
  p <- ncol(X)
  beta <- rep(0, p)
  event_times <- sort(unique(time[event]))
  info <- matrix(0, p, p)
  for (iter in seq_len(max_iter)) {
    U <- rep(0, p)
    info <- matrix(0, p, p)
    for (t in event_times) {
      risk <- which(time >= t)
      d_idx <- which(time == t & event)
      d <- length(d_idx)
      Xr <- X[risk, , drop = FALSE]
      w <- exp(as.vector(Xr %*% beta))
      S0 <- sum(w)
      Xw <- Xr * w
      S1 <- colSums(Xw)
      S2 <- crossprod(Xw, Xr)
      U <- U + colSums(X[d_idx, , drop = FALSE]) - d * S1 / S0
      info <- info + d * (S2 / S0 - outer(S1, S1) / S0^2)
    }
    step <- tryCatch(solve(info, U), error = function(e) NULL)
    if (is.null(step)) {
      stop(
        "stat_coxph(): the information matrix is singular (likely complete ",
        "separation or a group with no events); the model did not converge.",
        call. = FALSE
      )
    }
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  list(beta = beta, info = info)
}

#' StatCoxph: hazard ratios from a from-scratch Cox regression
#'
#' Consumes `time`, `status` (1/TRUE = event, 0/FALSE = censored), and a
#' categorical `group` (treatment arm); fits a Cox proportional-hazards
#' model by Newton-Raphson maximisation of the (Breslow) partial
#' likelihood and emits one row per non-reference level: `x` = hazard
#' ratio, `xmin`/`xmax` = Wald 95% CI, `y` = level label. `y` is declared
#' via `discrete_provides` (see [Stat]) since it is entirely computed
#' here - the build would otherwise have no pre-stat `y` to detect a
#' categorical axis from.
#'
#' @param ref_level Reference level for the hazard ratio; `NULL` uses the
#'   first factor level (or first-observed value for a character/numeric
#'   grouping variable).
#' @noRd
StatCoxph <- new_class("StatCoxph", parent = Stat,
  properties = list(ref_level = class_any),
  constructor = function(ref_level = NULL) {
    new_object(
      Stat(
        name = "coxph", provides = c("x", "y", "xmin", "xmax"),
        discrete_provides = "y"
      ),
      ref_level = ref_level
    )
  }
)

method(compute_stat, StatCoxph) <- function(stat, values) {
  if (is.null(values$time) || is.null(values$status) || is.null(values$group)) {
    stop(
      "stat_coxph() requires aes(time = , status = , group = ) ",
      "(or color =, which doubles as group).", call. = FALSE
    )
  }
  time <- values$time
  event <- as.logical(values$status)
  arm <- as.character(values$group)

  levs <- unique(arm) # first-appearance order
  ref <- as.character(stat@ref_level %||% levs[1])
  if (!ref %in% levs) {
    stop(
      "stat_coxph(): `ref_level` (\"", ref, "\") is not one of the ",
      "observed groups: ", paste(levs, collapse = ", "), ".", call. = FALSE
    )
  }
  other <- setdiff(levs, ref)
  if (length(other) == 0) {
    stop(
      "stat_coxph() needs at least two groups (found only \"", ref, "\").",
      call. = FALSE
    )
  }

  X <- vapply(other, function(lv) as.numeric(arm == lv), numeric(length(arm)))
  dim(X) <- c(length(arm), length(other))
  fit <- cox_newton_raphson(time, event, X)
  se <- sqrt(diag(solve(fit$info)))
  z <- stats::qnorm(0.975)
  hr <- exp(fit$beta)

  out <- list(
    x = hr,
    xmin = exp(fit$beta - z * se),
    xmax = exp(fit$beta + z * se),
    y = other,
    xref = rep(1, length(other)),
    group = other
  )
  with_labels(out, x = paste0("hazard ratio vs ", ref))
}

#' Hazard ratios from a Cox proportional-hazards model
#' @param ref_level Reference level; `NULL` uses the first observed value.
#' @return A `StatCoxph` object, to pass as a layer's `stat`.
#' @export
stat_coxph <- function(ref_level = NULL) StatCoxph(ref_level = ref_level)

# --- Swimmer -----------------------------------------------------------------

#' StatSwimmer: bars from zero to each subject's duration
#' @noRd
StatSwimmer <- new_class("StatSwimmer", parent = Stat,
  constructor = function() new_object(Stat(name = "swimmer"))
)

method(compute_stat, StatSwimmer) <- function(stat, values) {
  values$xmin <- rep(0, length(values$x))
  values$xmax <- values$x
  # `label` doubles as the "still ongoing" flag for arrowheads.
  if (!is.null(values$label)) {
    values$ongoing <- as.logical(values$label)
    values$label <- NULL
  }
  values
}

# --- RECIST waterfall --------------------------------------------------------

#' StatWaterfallResponse: order subjects by best response
#' @noRd
StatWaterfallResponse <- new_class("StatWaterfallResponse", parent = Stat,
  constructor = function() new_object(Stat(name = "waterfall_response"))
)

method(compute_stat, StatWaterfallResponse) <- function(stat, values) {
  # Bars run left to right from best (most negative) to worst response —
  # the ordering is the point of the display, so x is replaced by rank.
  ord <- order(values$y, decreasing = TRUE)
  n <- length(ord)
  out <- list(
    x = seq_len(n),
    xmin = seq_len(n) - 0.42,
    xmax = seq_len(n) + 0.42,
    y = values$y[ord],
    ymin = rep(0, n),
    ymax = values$y[ord],
    group = as.character(seq_len(n)),
    # RECIST thresholds: +20% progression, -30% partial response.
    ythresh = c(20, -30)
  )
  if (!is.null(values$color)) {
    out$color <- values$color[ord]
  } else {
    # Default shading by RECIST category when no color is mapped.
    out$color <- ifelse(out$y <= -30, "#2E7D5B",
                        ifelse(out$y >= 20, "#C1462F", "#D9A441"))
  }
  with_labels(out, x = "subject (ordered)", y = "% change from baseline")
}

# --- Spider response ---------------------------------------------------------

#' StatSpiderResponse: carry the RECIST reference levels
#' @noRd
StatSpiderResponse <- new_class("StatSpiderResponse", parent = Stat,
  constructor = function() new_object(Stat(name = "spider_response"))
)

method(compute_stat, StatSpiderResponse) <- function(stat, values) {
  # RECIST thresholds: +20% progression, -30% partial response. They join
  # the y family so scale training keeps them inside the panel.
  values$ythresh <- c(20, -30)
  values
}

# --- Bland-Altman ------------------------------------------------------------

#' StatBlandAltman: mean vs difference, plus bias and limits of agreement
#' @noRd
StatBlandAltman <- new_class("StatBlandAltman", parent = Stat,
  constructor = function() new_object(Stat(name = "bland_altman"))
)

method(compute_stat, StatBlandAltman) <- function(stat, values) {
  m <- (values$x + values$y) / 2
  d <- values$x - values$y
  bias <- mean(d, na.rm = TRUE)
  s <- stats::sd(d, na.rm = TRUE)
  # 95% limits of agreement: bias +/- 1.96 SD of the differences.
  out <- list(
    x = m, y = d,
    yref = c(bias, bias + 1.96 * s, bias - 1.96 * s),
    group = rep("all", length(m))
  )
  # The reference lines must be inside the trained y domain.
  out$ymin <- rep(min(c(d, out$yref), na.rm = TRUE), length(m))
  out$ymax <- rep(max(c(d, out$yref), na.rm = TRUE), length(m))
  if (!is.null(values$color)) out$color <- values$color
  with_labels(out, x = "mean of methods", y = "difference")
}

# --- Cumulative incidence ----------------------------------------------------

#' StatCuminc: Aalen-Johansen cumulative incidence with competing risks
#' @noRd
StatCuminc <- new_class("StatCuminc", parent = Stat,
  constructor = function() new_object(Stat(name = "cuminc", provides = c("x", "y")))
)

method(compute_stat, StatCuminc) <- function(stat, values) {
  time <- values$time
  status <- values$status
  arms <- values$group
  out <- list(x = numeric(), y = numeric(), group = character(),
              color = character())
  keep_color <- !is.null(values$color)

  for (arm in unique(arms)) {
    sel <- which(arms == arm)
    tt <- time[sel]
    ss <- status[sel]
    ord <- order(tt)
    tt <- tt[ord]
    ss <- ss[ord]
    n <- length(tt)
    event_types <- sort(unique(ss[ss != 0]))

    # Overall survival by Kaplan-Meier across all event types; each event
    # type's incidence then accumulates S(t-) * (d_k / n_at_risk). Using
    # the overall survivor function is what makes this Aalen-Johansen
    # rather than the upward-biased 1 - KM per cause.
    utimes <- sort(unique(tt[ss != 0]))
    surv <- 1
    cif <- stats::setNames(numeric(length(event_types)),
                           as.character(event_types))
    curves <- lapply(event_types, function(k) list(x = 0, y = 0))
    names(curves) <- as.character(event_types)

    for (ti in utimes) {
      at_risk <- sum(tt >= ti)
      if (at_risk == 0) next
      d_all <- sum(tt == ti & ss != 0)
      for (k in event_types) {
        dk <- sum(tt == ti & ss == k)
        if (dk > 0) {
          cif[[as.character(k)]] <- cif[[as.character(k)]] +
            surv * dk / at_risk
        }
        curves[[as.character(k)]]$x <- c(curves[[as.character(k)]]$x, ti)
        curves[[as.character(k)]]$y <- c(curves[[as.character(k)]]$y,
                                         cif[[as.character(k)]])
      }
      surv <- surv * (1 - d_all / at_risk)
    }

    for (k in event_types) {
      cv <- curves[[as.character(k)]]
      label <- if (length(unique(arms)) > 1) {
        paste0(arm, ": event ", k)
      } else {
        paste0("event ", k)
      }
      out$x <- c(out$x, cv$x)
      out$y <- c(out$y, cv$y)
      out$group <- c(out$group, rep(label, length(cv$x)))
      if (keep_color) {
        out$color <- c(out$color, rep(values$color[sel][1], length(cv$x)))
      }
    }
  }
  if (!keep_color) {
    # Color by event type so the curves are distinguishable by default.
    out$color <- out$group
  }
  with_labels(out, x = "time", y = "cumulative incidence")
}

# --- Spaghetti ---------------------------------------------------------------

#' StatSpaghetti: append a mean trajectory
#'
#' @param mean_line Whether to append the group mean series.
#' @noRd
StatSpaghetti <- new_class("StatSpaghetti", parent = Stat,
  properties = list(mean_line = class_logical),
  constructor = function(mean_line = TRUE) {
    new_object(Stat(name = "spaghetti"), mean_line = mean_line)
  }
)

method(compute_stat, StatSpaghetti) <- function(stat, values) {
  if (!isTRUE(stat@mean_line)) {
    return(values)
  }
  xs <- sort(unique(values$x))
  mu <- vapply(xs, function(v) mean(values$y[values$x == v], na.rm = TRUE),
               numeric(1))
  out <- list(
    x = c(values$x, xs),
    y = c(values$y, mu),
    group = c(as.character(values$group), rep(".mean", length(xs)))
  )
  if (!is.null(values$color)) {
    out$color <- c(values$color, rep(values$color[[1]], length(xs)))
  }
  out
}

# --- AE heatmap --------------------------------------------------------------

#' StatAEHeatmap: tile cells shaded by incidence
#' @noRd
StatAEHeatmap <- new_class("StatAEHeatmap", parent = Stat,
  constructor = function() new_object(Stat(name = "ae_heatmap"))
)

method(compute_stat, StatAEHeatmap) <- function(stat, values) {
  v <- values$size %||% rep(1, length(values$x))
  out <- list(
    x = values$x, y = values$y,
    xmin = values$x - 0.5, xmax = values$x + 0.5,
    ymin = values$y - 0.5, ymax = values$y + 0.5,
    color = v,
    label = format(v, trim = TRUE),
    group = as.character(seq_along(values$x))
  )
  out
}

# --- Dose-response -----------------------------------------------------------

#' StatDoseResponse: four-parameter log-logistic fit
#' @noRd
StatDoseResponse <- new_class("StatDoseResponse", parent = Stat,
  constructor = function() new_object(Stat(name = "dose_response"))
)

method(compute_stat, StatDoseResponse) <- function(stat, values) {
  x <- values$x
  y <- values$y
  # Four-parameter log-logistic:
  #   y = bottom + (top - bottom) / (1 + (EC50 / dose)^hill)
  # Fitted by nonlinear least squares on log10(dose), with a
  # method-of-moments start so nls() converges without user hints.
  lx <- log10(pmax(x, .Machine$double.eps))
  bottom0 <- min(y)
  top0 <- max(y)
  mid <- (bottom0 + top0) / 2
  ec0 <- lx[which.min(abs(y - mid))]
  fit <- tryCatch(
    stats::nls(
      y ~ bottom + (top - bottom) / (1 + 10^((ec - lx) * hill)),
      data = data.frame(y = y, lx = lx),
      start = list(bottom = bottom0, top = top0, ec = ec0, hill = 1),
      control = stats::nls.control(warnOnly = TRUE)
    ),
    error = function(e) NULL
  )
  gx <- seq(min(lx), max(lx), length.out = 80)
  if (is.null(fit)) {
    # Fall back to a monotone interpolation rather than failing the plot.
    gy <- stats::approx(lx, y, xout = gx, rule = 2)$y
    ec50 <- 10^ec0
  } else {
    co <- stats::coef(fit)
    gy <- stats::predict(fit, newdata = data.frame(lx = gx))
    ec50 <- 10^co[["ec"]]
  }
  n_obs <- length(x)
  out <- list(
    x = c(x, 10^gx),
    y = c(y, gy),
    group = c(rep(".obs", n_obs), rep(".fit", length(gx))),
    xec50 = rep(ec50, n_obs + length(gx))
  )
  with_labels(out, x = "dose", y = "response")
}

# --- CONSORT -----------------------------------------------------------------

#' StatConsort: stack flow boxes and join them with arrows
#' @noRd
StatConsort <- new_class("StatConsort", parent = Stat,
  constructor = function() new_object(Stat(name = "consort", provides = c("x", "y")))
)

method(compute_stat, StatConsort) <- function(stat, values) {
  labels <- as.character(values$label)
  counts <- values$size
  n <- length(labels)
  # Boxes are stacked top to bottom with a fixed gap; arrows connect
  # consecutive boxes down the middle.
  prm <- values$params %||% list()
  box_h <- min(0.16, 0.86 / n)
  gap <- (0.9 - box_h * n) / max(1, n - 1)
  marks <- list()
  centers <- numeric(n)
  for (i in seq_len(n)) {
    top <- 0.95 - (i - 1) * (box_h + gap)
    bot <- top - box_h
    centers[i] <- (top + bot) / 2
    marks <- c(marks, list(mk_rect(
      0.2, 0.8, bot, top, fill = prm$box_fill %||% "#EDF1F7", alpha = 1,
      stroke = "#8595AC", stroke_width = 1
    )))
    txt <- if (is.null(counts)) {
      labels[i]
    } else {
      paste0(labels[i], "  (n = ", format(counts[i], big.mark = ","), ")")
    }
    marks <- c(marks, list(mk_text(0.5, centers[i], txt,
                                   size = prm$label_size %||% 12,
                                   color = "#1A2233")))
    if (i > 1) {
      prev_bot <- centers[i - 1] - box_h / 2
      marks <- c(marks, list(mk_line(
        c(0.5, 0.5), c(prev_bot, top), stroke = "#8595AC", width = 1.5
      )))
      # Arrowhead into the top of this box.
      marks <- c(marks, list(mk_polygon(
        c(0.485, 0.5, 0.515), c(top + 0.012, top, top + 0.012),
        fill = "#8595AC", alpha = 1
      )))
    }
  }
  out <- list(
    x = c(0, 1), y = c(0, 1), group = c("a", "b"),
    marks_precomputed = marks
  )
  panel_span(out)
}
