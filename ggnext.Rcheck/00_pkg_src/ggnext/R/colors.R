# From-scratch color module ---------------------------------------------------
#
# ggnext may not use the `scales` or `munsell` packages, so all color
# handling lives here: normalization of user colors to "#RRGGBB" hex, a
# colorblind-safe discrete palette, and linear interpolation for continuous
# color mapping. Only grDevices::col2rgb (shipped with base R) is used, to
# accept the standard R color names.

# Default palette for discrete color mapping: Okabe-Ito, the well-known
# colorblind-safe 8-color palette (public-domain values).
GGNEXT_DISCRETE_PALETTE <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

# Endpoints for continuous color mapping (dark navy -> soft teal-green).
GGNEXT_GRADIENT_LOW <- "#1A2E59"
GGNEXT_GRADIENT_HIGH <- "#5FD0A5"

#' Normalize any R color spec to "#RRGGBB" hex
#'
#' Accepts R color names ("steelblue"), short or long hex. Alpha channels are
#' intentionally not encoded here; opacity is carried separately as the
#' `alpha` mark attribute so both render targets treat it uniformly.
#'
#' @param col Character vector of colors.
#' @return Character vector of "#RRGGBB" strings.
#' @noRd
col_to_hex <- function(col) {
  rgb <- grDevices::col2rgb(col)
  # col2rgb returns a 3 x n integer matrix of [0, 255] channels.
  vapply(
    seq_len(ncol(rgb)),
    function(i) sprintf("#%02X%02X%02X", rgb[1, i], rgb[2, i], rgb[3, i]),
    character(1)
  )
}

#' Map a discrete vector to palette colors
#'
#' Levels are assigned palette entries in factor-level order (or sorted
#' unique order for character input), so the same level always receives the
#' same color within a plot.
#'
#' @param x Character or factor vector.
#' @return List with `colors` (hex per observation) and `levels`/`palette`
#'   (the legend key).
#' @noRd
map_color_discrete <- function(x, palette = GGNEXT_DISCRETE_PALETTE) {
  levs <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  # More levels than palette entries: recycle rather than fail, so large
  # categorical sets (network communities, AE terms) still render.
  pal <- rep_len(palette, max(length(levs), 1))[seq_along(levs)]
  list(
    colors = pal[match(as.character(x), levs)],
    levels = levs,
    palette = pal
  )
}

#' Map a numeric vector to an interpolated gradient
#'
#' Linear interpolation channel-by-channel in sRGB between the two gradient
#' endpoints. `t` is the position of each value in its own [min, max] range.
#'
#' @param x Numeric vector.
#' @return Character vector of hex colors, one per observation.
#' @noRd
map_color_continuous <- function(x, low = GGNEXT_GRADIENT_LOW,
                                 high = GGNEXT_GRADIENT_HIGH) {
  rng <- range(x, na.rm = TRUE)
  span <- rng[2] - rng[1]
  # A constant column maps everything to the gradient midpoint.
  t <- if (span == 0) rep(0.5, length(x)) else (x - rng[1]) / span
  lerp_hex(low, high, t)
}

# TRUE when a mapped color vector is a single repeated value that is itself
# a valid color spec (name or hex) — i.e. aes(color = "blue"), which the
# user means literally, not as a discrete category.
is_color_constant <- function(v) {
  v <- as.character(v)
  if (length(unique(v)) != 1L) {
    return(FALSE)
  }
  tryCatch(
    {
      grDevices::col2rgb(v[[1]])
      TRUE
    },
    error = function(e) FALSE
  )
}

# Interpolate between two hex colors at positions t in [0, 1].
lerp_hex <- function(from, to, t) {
  f <- grDevices::col2rgb(from)[, 1]
  g <- grDevices::col2rgb(to)[, 1]
  vapply(
    t,
    function(ti) {
      ch <- round(f + (g - f) * ti)
      sprintf("#%02X%02X%02X", ch[1], ch[2], ch[3])
    },
    character(1)
  )
}
