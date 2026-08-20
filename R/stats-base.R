# Stats for the additional core geoms ------------------------------------------
#
# Supporting computations for geom_abline(), geom_spoke(), geom_count(),
# and the distribution diagnostics geom_qq(), geom_qq_line(),
# geom_dotplot() and geom_quantile().

#' @include classes.R stats.R
NULL

# --- spoke -------------------------------------------------------------------

#' StatSpoke: turn angle and radius into a segment endpoint
#' @noRd
StatSpoke <- new_class("StatSpoke", parent = Stat,
  constructor = function() new_object(Stat(name = "spoke"))
)

method(compute_stat, StatSpoke) <- function(stat, values) {
  # The user maps angle to `xend` and radius to `yend`; convert them into
  # the actual endpoint the segment runs to.
  angle <- values$xend
  radius <- values$yend
  values$xend <- values$x + radius * cos(angle)
  values$yend <- values$y + radius * sin(angle)
  values
}

# --- count -------------------------------------------------------------------

#' StatSum: collapse duplicate positions and size by count
#' @noRd
StatSum <- new_class("StatSum", parent = Stat,
  constructor = function() new_object(Stat(name = "sum"))
)

method(compute_stat, StatSum) <- function(stat, values) {
  key <- paste(values$x, values$y, sep = "\r")
  keep <- !duplicated(key)
  n <- as.numeric(table(key)[key[keep]])
  out <- list(
    x = values$x[keep], y = values$y[keep],
    # `size` is mapped, so the build scales it into a radius by area.
    size = n,
    group = values$group[keep]
  )
  if (!is.null(values$color)) out$color <- values$color[keep]
  out
}

# --- quantile-quantile -------------------------------------------------------

#' StatQQ: theoretical against sample quantiles
#'
#' @param distribution Quantile function of the reference distribution.
#' @param line Return the reference line instead of the points.
#' @noRd
StatQQ <- new_class("StatQQ", parent = Stat,
  properties = list(distribution = class_any, line = class_logical),
  constructor = function(distribution = stats::qnorm, line = FALSE) {
    new_object(Stat(name = "qq", provides = c("x", "y")),
               distribution = distribution, line = line)
  }
)

method(compute_stat, StatQQ) <- function(stat, values) {
  out <- stat_by_group(values, function(sub) {
    s <- sort(sub$sample %||% sub$x)
    n <- length(s)
    if (n < 2) {
      stop("geom_qq(): need at least 2 observations.", call. = FALSE)
    }
    # Hazen plotting positions ((i - 0.5) / n), as used by stats::ppoints
    # for n > 10; they avoid 0 and 1, which have infinite quantiles.
    theoretical <- stat@distribution(stats::ppoints(n))
    if (!stat@line) {
      return(list(x = theoretical, y = s))
    }
    # The reference line passes through the first and third quartiles of
    # both the sample and the reference distribution.
    sq <- stats::quantile(s, c(0.25, 0.75), names = FALSE)
    tq <- stat@distribution(c(0.25, 0.75))
    slope <- diff(sq) / diff(tq)
    intercept <- sq[1] - slope * tq[1]
    ends <- range(theoretical)
    list(x = ends, y = intercept + slope * ends)
  })
  with_labels(out, x = "theoretical quantiles", y = "sample quantiles")
}

#' Quantile-quantile plot
#'
#' Sample quantiles against the quantiles of a reference distribution;
#' points on a straight line mean the sample follows it. Add
#' [geom_qq_line()] for the reference.
#'
#' @param mapping,data Standard layer overrides. Map the values to `x`.
#' @param distribution Quantile function of the reference distribution
#'   (default [stats::qnorm]).
#' @param color,size,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(v = rnorm(100))
#' ggnext(d, aes(v)) + geom_qq() + geom_qq_line()
#' @export
geom_qq <- function(mapping = NULL, data = NULL, distribution = stats::qnorm,
                    color = NULL, size = NULL, alpha = NULL) {
  layer_new(GeomPoint(), StatQQ(distribution = distribution, line = FALSE),
            mapping, data, list(color = color, size = size, alpha = alpha))
}

#' @rdname geom_qq
#' @param linewidth Line width for the reference line.
#' @export
geom_qq_line <- function(mapping = NULL, data = NULL,
                         distribution = stats::qnorm, color = NULL,
                         linewidth = NULL, alpha = NULL) {
  layer_new(GeomLine(), StatQQ(distribution = distribution, line = TRUE),
            mapping, data,
            list(color = color %||% "#C1462F", linewidth = linewidth,
                 alpha = alpha))
}

# --- dot plot ----------------------------------------------------------------

#' StatDotplot: bin values and stack a dot per observation
#'
#' @param binwidth Bin width in data units; `NULL` uses range / 30.
#' @noRd
StatDotplot <- new_class("StatDotplot", parent = Stat,
  properties = list(binwidth = class_any),
  constructor = function(binwidth = NULL) {
    new_object(Stat(name = "dotplot", provides = "y"), binwidth = binwidth)
  }
)

method(compute_stat, StatDotplot) <- function(stat, values) {
  out <- stat_by_group(values, function(sub) {
    rng <- range(sub$x, na.rm = TRUE)
    bw <- stat@binwidth %||% {
      if (diff(rng) == 0) 1 else diff(rng) / 30
    }
    # One dot per observation, stacked in its bin; the dot sits at the bin
    # centre so a column of dots reads as a histogram bar made of points.
    bin <- floor((sub$x - rng[1]) / bw)
    centre <- rng[1] + (bin + 0.5) * bw
    stack <- ave_seq(bin)
    list(x = centre, y = stack, binwidth = rep(bw, length(bin)))
  })
  with_labels(out, y = "count")
}

# Position of each element within its own bin: 1, 2, 3, ... per bin.
ave_seq <- function(bin) {
  out <- integer(length(bin))
  seen <- integer(0)
  for (i in seq_along(bin)) {
    key <- as.character(bin[i])
    seen[key] <- (if (is.na(seen[key])) 0L else seen[key]) + 1L
    out[i] <- seen[key]
  }
  out
}

#' Dot plot
#'
#' A histogram built from one dot per observation, stacked within its bin.
#' Better than bars for small samples, where it shows every data point.
#'
#' @param mapping,data Standard layer overrides. Map the values to `x`.
#' @param binwidth Bin width in data units.
#' @param color,size,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(cars, aes(speed)) + geom_dotplot(binwidth = 2)
#' @export
geom_dotplot <- function(mapping = NULL, data = NULL, binwidth = NULL,
                         color = NULL, size = NULL, alpha = NULL) {
  layer_new(GeomPoint(), StatDotplot(binwidth = binwidth), mapping, data,
            list(color = color, size = size %||% 4, alpha = alpha))
}

# --- quantile regression -----------------------------------------------------

#' StatQuantile: linear quantile regression fits
#'
#' @param quantiles Quantiles to fit.
#' @param n Points per fitted line.
#' @noRd
StatQuantile <- new_class("StatQuantile", parent = Stat,
  properties = list(quantiles = class_numeric, n = class_numeric),
  constructor = function(quantiles = c(0.25, 0.5, 0.75), n = 80) {
    new_object(Stat(name = "quantile"), quantiles = quantiles, n = n)
  }
)

method(compute_stat, StatQuantile) <- function(stat, values) {
  if (length(values$x) < 3) {
    stop("geom_quantile(): need at least 3 observations.", call. = FALSE)
  }
  grid <- seq(min(values$x), max(values$x), length.out = stat@n)
  out <- list(x = numeric(), y = numeric(), group = character())
  for (tau in stat@quantiles) {
    co <- rq_fit(values$x, values$y, tau)
    out$x <- c(out$x, grid)
    out$y <- c(out$y, co[1] + co[2] * grid)
    out$group <- c(out$group, rep(paste0("tau=", tau), length(grid)))
  }
  out$color <- out$group
  out
}

# Linear quantile regression by minimising the check loss
#   rho_tau(u) = u * (tau - (u < 0))
# with Nelder-Mead. The objective is convex, so a derivative-free search
# from the least-squares fit is reliable and keeps the package free of a
# linear-programming dependency.
rq_fit <- function(x, y, tau) {
  start <- stats::coef(stats::lm(y ~ x))
  loss <- function(par) {
    resid <- y - (par[1] + par[2] * x)
    sum(resid * (tau - (resid < 0)))
  }
  stats::optim(start, loss, method = "Nelder-Mead",
               control = list(reltol = 1e-10, maxit = 2000))$par
}

# --- correlation matrix -------------------------------------------------------

#' StatCor: expand a precomputed correlation cell into tile corners
#'
#' The correlation matrix itself is computed in [geom_cor()]'s constructor
#' (it needs the whole wide data frame at once, not row-wise aesthetic
#' vectors); this stat only does the generic "cell center -> tile
#' corners" expansion, the same job `StatAEHeatmap` does.
#'
#' @noRd
StatCor <- new_class("StatCor", parent = Stat,
  constructor = function() new_object(Stat(name = "cor"))
)

method(compute_stat, StatCor) <- function(stat, values) {
  list(
    x = values$x, y = values$y,
    xmin = values$x - 0.5, xmax = values$x + 0.5,
    ymin = values$y - 0.5, ymax = values$y + 0.5,
    fill = values$fill,
    label = values$label,
    group = as.character(seq_along(values$x))
  )
}

# --- missing-data pattern -------------------------------------------------

#' StatMissingPattern: expand a precomputed pattern cell into tile corners
#'
#' Same "cell center -> tile corners" expansion as `StatCor`; the
#' missingness pattern grid itself is built in [geom_missing_pattern()]'s
#' constructor from the raw wide data frame.
#'
#' @noRd
StatMissingPattern <- new_class("StatMissingPattern", parent = Stat,
  constructor = function() new_object(Stat(name = "missing_pattern"))
)

method(compute_stat, StatMissingPattern) <- function(stat, values) {
  list(
    x = values$x, y = values$y,
    xmin = values$x - 0.5, xmax = values$x + 0.5,
    ymin = values$y - 0.5, ymax = values$y + 0.5,
    fill = values$fill,
    label = values$label,
    group = as.character(seq_along(values$x))
  )
}

#' Quantile regression lines
#'
#' Fits a linear model to chosen conditional quantiles, showing how the
#' spread of `y` changes with `x` — not just its mean, as [geom_smooth()]
#' does.
#'
#' @param mapping,data Standard layer overrides.
#' @param quantiles Quantiles to fit (default the quartiles).
#' @param linewidth,alpha Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(cars, aes(speed, dist)) +
#'   geom_point(alpha = 0.5) +
#'   geom_quantile()
#' @export
geom_quantile <- function(mapping = NULL, data = NULL,
                          quantiles = c(0.25, 0.5, 0.75), linewidth = NULL,
                          alpha = NULL) {
  layer_new(GeomLine(), StatQuantile(quantiles = quantiles), mapping, data,
            list(linewidth = linewidth %||% 1.5, alpha = alpha))
}
