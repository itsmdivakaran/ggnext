# From-scratch scale math -----------------------------------------------------
#
# The `scales` package is off-limits, so the numeric heart of the package is
# written out longhand here and commented for future debugging. Three jobs:
#
#   1. expand_domain(): pad a data range so marks don't sit on the panel edge.
#   2. nice_ticks():    choose human-friendly tick locations (Heckbert's
#                       "nice numbers" algorithm from Graphics Gems, 1990).
#   3. normalize():     the domain -> [0, 1] linear map every scale performs.

#' Expand a numeric domain by a multiplicative margin
#'
#' Mirrors the familiar behaviour of padding each end of the data range by
#' `mult` (default 5%) of the span, so points at the extremes are not drawn
#' on the panel border. A degenerate domain (zero span, e.g. a single unique
#' value) is expanded by +/- 1 unit, or +/- 5% of the value's magnitude if
#' that is larger, so downstream math never divides by zero.
#'
#' @param domain Numeric length-2 vector, `c(min, max)`.
#' @param mult Fraction of the span to add on each side.
#' @return Numeric length-2 vector, the expanded domain.
#' @noRd
expand_domain <- function(domain, mult = 0.05) {
  lo <- domain[1]
  hi <- domain[2]
  span <- hi - lo
  if (span == 0) {
    pad <- max(1, abs(lo) * 0.05)
    return(c(lo - pad, hi + pad))
  }
  c(lo - span * mult, hi + span * mult)
}

# Round x to a "nice" value 1, 2, or 5 times a power of ten.
# If round = TRUE, pick the nearest nice value; otherwise the smallest nice
# value >= x (used for the overall range so ticks cover the data).
nice_number <- function(x, round) {
  exponent <- floor(log10(x))
  fraction <- x / 10^exponent # in [1, 10)
  nice_fraction <- if (round) {
    if (fraction < 1.5) 1 else if (fraction < 3) 2 else if (fraction < 7) 5 else 10
  } else {
    if (fraction <= 1) 1 else if (fraction <= 2) 2 else if (fraction <= 5) 5 else 10
  }
  nice_fraction * 10^exponent
}

#' Compute nice tick locations covering a range
#'
#' Heckbert-style "nice numbers": choose a nice step size (1/2/5 x 10^k)
#' close to `span / (n - 1)`, then emit multiples of that step across the
#' range. Unlike Heckbert's original, the raw span is not itself rounded up
#' to a nice number first — double-nicing inflates the step and leaves too
#' few ticks once out-of-domain ticks are clipped away. Callers typically
#' pass the *unexpanded* data domain and then drop ticks that fall outside
#' the expanded domain.
#'
#' @param lo,hi Range to cover (`lo < hi`).
#' @param n Target number of ticks (result may have a few more or fewer).
#' @return Numeric vector of tick values, ascending, step-aligned.
#' @noRd
nice_ticks <- function(lo, hi, n = 5) {
  if (lo == hi) {
    # Degenerate range: a single centered tick is the only honest answer.
    return(lo)
  }
  step <- nice_number((hi - lo) / (n - 1), round = TRUE)
  # Align the first/last tick to multiples of the step.
  tick_lo <- floor(lo / step) * step
  tick_hi <- ceiling(hi / step) * step
  ticks <- seq(tick_lo, tick_hi, by = step)
  # seq() accumulates float error (e.g. 0.30000000000000004). Snap each tick
  # onto an exact multiple of the step, then round at the step's decimal
  # precision — multiplying alone can reintroduce binary noise (3 * 0.2 is
  # not the double literal 0.6).
  ticks <- round(ticks / step) * step
  round(ticks, digits = max(0, -floor(log10(step))))
}

#' Linearly map values from a domain onto `[0, 1]`
#'
#' The single transform every continuous scale performs:
#' `t = (x - domain_min) / (domain_max - domain_min)`.
#' Values outside the domain map outside `[0, 1]`; clipping is a renderer
#' decision (the panel clip path / canvas clip region), not a scale decision.
#'
#' @param x Numeric vector.
#' @param domain Numeric length-2 vector with distinct endpoints.
#' @return Numeric vector of normalized positions.
#' @noRd
normalize <- function(x, domain) {
  span <- domain[2] - domain[1]
  stopifnot("normalize() requires a non-degenerate domain" = span != 0)
  (x - domain[1]) / span
}

# Format tick values for display: trimmed, no scientific notation for the
# magnitudes a spike plot produces, common formatting across the vector so
# labels align visually (e.g. "0.5", "1.0", "1.5").
format_ticks <- function(values) {
  format(values, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
}

# --- Axis transforms ---------------------------------------------------------
#
# A transform is a monotone reshaping of the data axis. Scales train and map
# in *transformed* space, so expansion, normalization, and tick placement
# stay plain linear math; only these two functions know about the curve.

# Data space -> transformed space.
trans_apply <- function(trans, x) {
  switch(trans,
    identity = x,
    log10 = {
      if (any(x <= 0, na.rm = TRUE)) {
        stop("A log10 scale requires strictly positive values.")
      }
      log10(x)
    },
    sqrt = {
      if (any(x < 0, na.rm = TRUE)) {
        stop("A sqrt scale requires non-negative values.")
      }
      sqrt(x)
    },
    reverse = -x,
    stop("Unknown transform: ", trans)
  )
}

# Transformed space -> data space (used to label ticks in original units).
trans_invert <- function(trans, x) {
  switch(trans,
    identity = x,
    log10 = 10^x,
    sqrt = x^2,
    reverse = -x,
    stop("Unknown transform: ", trans)
  )
}
