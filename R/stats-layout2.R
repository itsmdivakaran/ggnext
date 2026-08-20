# Additional layout stats ------------------------------------------------------
#
# A second layout-stats file (R/stats-layout.R itself is mid-edit for an
# unrelated CRAN submission and off limits here) holding StatAlluvial,
# which reuses StatSankey's node-stacking and ribbon-path machinery for
# ordered, repeated-measures categorical data.

#' @include classes.R stats.R stats-layout.R geoms-layout.R
NULL

# --- Alluvial ------------------------------------------------------------

#' StatAlluvial: tally consecutive-stage transitions, then delegate to Sankey
#'
#' Consumes one row per subject per stage (`x` = stage, `y` = category,
#' `group` = subject id), tallies every consecutive-stage transition into
#' an edge list (node names prefixed by stage, so the same category label
#' at different stages never collides), and hands that straight to
#' `StatSankey`'s `compute_stat()` for the actual node-stacking and
#' ribbon-path layout — an alluvial diagram is exactly a multi-stage
#' Sankey once the repeated-measures data has been reshaped into edges.
#'
#' @noRd
StatAlluvial <- new_class("StatAlluvial", parent = Stat,
  constructor = function() new_object(Stat(name = "alluvial", provides = c("x", "y")))
)

method(compute_stat, StatAlluvial) <- function(stat, values) {
  if (is.null(values$x) || is.null(values$y) || is.null(values$group)) {
    stop(
      "geom_alluvial() requires aes(x = stage, y = category, group = subject id).",
      call. = FALSE
    )
  }
  stages <- if (is.factor(values$x)) levels(values$x) else sort(unique(as.character(values$x)))
  if (length(stages) < 2) {
    stop("geom_alluvial() needs at least two distinct stages on `x`.", call. = FALSE)
  }
  d <- data.frame(
    id = as.character(values$group),
    stage = factor(as.character(values$x), levels = stages),
    category = as.character(values$y),
    stringsAsFactors = FALSE
  )

  from <- character(0)
  to <- character(0)
  for (i in seq_along(stages)[-length(stages)]) {
    a <- d[d$stage == stages[i], c("id", "category")]
    b <- d[d$stage == stages[i + 1], c("id", "category")]
    m <- merge(a, b, by = "id", suffixes = c(".from", ".to"))
    if (nrow(m) == 0) next
    # Prefix node names with their stage so "A" at stage 1 and "A" at
    # stage 2 become distinct Sankey nodes instead of merging into one.
    from <- c(from, paste0(stages[i], ": ", m$category.from))
    to <- c(to, paste0(stages[i + 1], ": ", m$category.to))
  }
  if (length(from) == 0) {
    stop(
      "geom_alluvial() found no subject with data at two consecutive ",
      "stages; check that `group` identifies the same subject across `x`.",
      call. = FALSE
    )
  }

  key <- paste(from, to, sep = "\r")
  tab <- table(key)
  parts <- do.call(rbind, strsplit(names(tab), "\r", fixed = TRUE))
  edge_values <- list(
    x = parts[, 1], xend = parts[, 2], y = as.numeric(tab),
    params = values$params
  )
  compute_stat(StatSankey(), edge_values)
}
