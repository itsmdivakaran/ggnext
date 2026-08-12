# Layout stats -----------------------------------------------------------------
#
# The layout algorithms behind the layout geoms, all written from scratch:
# squarified treemaps, Fruchterman-Reingold force layout, Sankey node
# stacking, chord arc allocation, streamgraph wiggle baselines, and UpSet
# intersection counting.
#
# Layouts that own the whole panel emit positions already in [0, 1] and set
# a `computed_labels` attribute so the (hidden) axes still have names.

#' @include classes.R marks.R
NULL

# Mark a values list as owning the whole panel. The layout has already
# placed everything in [0, 1], so the build must give it plain continuous
# axes over exactly [0, 1] with no expansion — otherwise the raw
# categorical inputs (node names, set labels) would train a discrete axis
# and shift the layout. The flag also blanks the axis titles, since the
# panel coordinates are an implementation detail, not data.
panel_span <- function(values) {
  attr(values, "computed_labels") <- list(x = "", y = "")
  attr(values, "panel_span") <- TRUE
  values
}

# --- Ridgeline ---------------------------------------------------------------

#' StatRidgeline: per-group density curves offset by group index
#'
#' @param scale Ridge height as a multiple of row spacing.
#' @param bw Density bandwidth (`NULL` for Silverman's rule).
#' @noRd
StatRidgeline <- new_class("StatRidgeline", parent = Stat,
  properties = list(scale = class_numeric, bw = class_any),
  constructor = function(scale = 1.6, bw = NULL) {
    new_object(Stat(name = "ridgeline"), scale = scale, bw = bw)
  }
)

method(compute_stat, StatRidgeline) <- function(stat, values) {
  # y holds the group slot (already an integer from the discrete scale);
  # x holds the values whose distribution we estimate.
  slots <- values$y
  out <- list(x = numeric(), ymin = numeric(), ymax = numeric(),
              group = character(), color = NULL)
  keep_color <- !is.null(values$color)
  colors <- character()
  for (s in sort(unique(slots))) {
    idx <- which(slots == s)
    xs <- check_density_group(values$x[idx], "geom_ridgeline", as.character(s))
    d <- if (is.null(stat@bw)) {
      stats::density(xs, n = 128)
    } else {
      stats::density(xs, bw = stat@bw, n = 128)
    }
    # Normalize each ridge to unit height, then scale by the row spacing.
    h <- d$y / max(d$y) * stat@scale
    out$x <- c(out$x, d$x)
    out$ymin <- c(out$ymin, rep(s, length(d$x)))
    out$ymax <- c(out$ymax, s + h)
    out$group <- c(out$group, rep(as.character(s), length(d$x)))
    if (keep_color) {
      colors <- c(colors, rep(values$color[idx[1]], length(d$x)))
    }
  }
  out$y <- out$ymin
  if (keep_color) out$color <- colors else out$color <- NULL
  out[!vapply(out, is.null, logical(1))]
}

# --- Radar --------------------------------------------------------------------

#' StatRadar: keep axis order and anchor the radial axis at zero
#' @noRd
StatRadar <- new_class("StatRadar", parent = Stat,
  constructor = function() new_object(Stat(name = "radar"))
)

method(compute_stat, StatRadar) <- function(stat, values) {
  # Remember the angular slot before coord_polar() replaces x with a
  # cartesian projection, so build_marks() can walk vertices around the
  # circle in axis order rather than data order.
  values$theta_order <- values$x
  # A radar's radial axis conventionally starts at 0; anchoring it there
  # keeps area differences honest. ymin joins the y-domain training.
  values$ymin <- rep(0, length(values$y))
  values
}

# --- Treemap -----------------------------------------------------------------

#' StatTreemap: squarified rectangle layout
#' @noRd
StatTreemap <- new_class("StatTreemap", parent = Stat,
  constructor = function() new_object(Stat(name = "treemap", provides = c("x", "y")))
)

method(compute_stat, StatTreemap) <- function(stat, values) {
  areas <- values$size
  if (is.null(areas)) {
    stop("geom_treemap() requires the `size` aesthetic (the area value).")
  }
  ord <- order(areas, decreasing = TRUE)
  rects <- squarify(areas[ord] / sum(areas), 0, 0, 1, 1)
  out <- list(
    xmin = vapply(rects, function(r) r$x0, numeric(1)),
    xmax = vapply(rects, function(r) r$x1, numeric(1)),
    ymin = vapply(rects, function(r) r$y0, numeric(1)),
    ymax = vapply(rects, function(r) r$y1, numeric(1))
  )
  out$x <- (out$xmin + out$xmax) / 2
  out$y <- (out$ymin + out$ymax) / 2
  if (!is.null(values$label)) out$label <- as.character(values$label)[ord]
  if (!is.null(values$color)) out$color <- values$color[ord]
  out$group <- as.character(seq_along(out$x))
  panel_span(out)
}

# Squarified treemap (Bruls, Huizing & van Wijk 2000). Repeatedly fill the
# shorter side of the remaining rectangle with a row of tiles, adding tiles
# to the row while doing so improves (lowers) the worst aspect ratio.
squarify <- function(areas, x0, y0, x1, y1) {
  out <- list()
  i <- 1
  while (i <= length(areas)) {
    w <- x1 - x0
    h <- y1 - y0
    if (w <= 0 || h <= 0) break
    short <- min(w, h)
    remaining <- sum(areas[i:length(areas)])
    row <- areas[i]
    j <- i
    # Grow the row while the aspect ratio keeps improving.
    while (j < length(areas)) {
      cand <- c(areas[i:j], areas[j + 1])
      if (worst_ratio(cand, short, remaining, w, h) >
          worst_ratio(areas[i:j], short, remaining, w, h)) {
        break
      }
      j <- j + 1
      row <- sum(areas[i:j])
    }
    row_areas <- areas[i:j]
    row_sum <- sum(row_areas)
    # Lay the row along the shorter side, splitting proportionally.
    if (w >= h) {
      band_w <- if (remaining > 0) row_sum / remaining * w else 0
      pos <- y0
      for (a in row_areas) {
        seg <- if (row_sum > 0) a / row_sum * h else 0
        out <- c(out, list(list(x0 = x0, x1 = x0 + band_w,
                                y0 = pos, y1 = pos + seg)))
        pos <- pos + seg
      }
      x0 <- x0 + band_w
    } else {
      band_h <- if (remaining > 0) row_sum / remaining * h else 0
      pos <- x0
      for (a in row_areas) {
        seg <- if (row_sum > 0) a / row_sum * w else 0
        out <- c(out, list(list(x0 = pos, x1 = pos + seg,
                                y0 = y0, y1 = y0 + band_h)))
        pos <- pos + seg
      }
      y0 <- y0 + band_h
    }
    i <- j + 1
  }
  out
}

# Worst (largest) aspect ratio in a candidate row.
worst_ratio <- function(row, short, remaining, w, h) {
  if (remaining <= 0 || short <= 0) {
    return(Inf)
  }
  total_area <- w * h
  s <- sum(row) / remaining * total_area
  if (s <= 0) {
    return(Inf)
  }
  scaled <- row / remaining * total_area
  max(max(scaled) * short^2 / s^2, s^2 / (min(scaled) * short^2))
}

# --- Network -----------------------------------------------------------------

#' StatNetwork: Fruchterman-Reingold force-directed layout
#'
#' @param iterations Simulation steps.
#' @param seed Random seed for the initial placement.
#' @noRd
StatNetwork <- new_class("StatNetwork", parent = Stat,
  properties = list(iterations = class_numeric, seed = class_numeric),
  constructor = function(iterations = 200, seed = 1) {
    new_object(Stat(name = "network", provides = c("x", "y")), iterations = iterations, seed = seed)
  }
)

method(compute_stat, StatNetwork) <- function(stat, values) {
  from <- as.character(values$x)
  to <- as.character(values$xend)
  if (is.null(values$xend)) {
    stop("geom_network() requires `x` (source) and `xend` (target).")
  }
  nodes <- unique(c(from, to))
  n <- length(nodes)
  fi <- match(from, nodes)
  ti <- match(to, nodes)

  # Deterministic initial placement on a circle plus a seeded jitter: a
  # circle avoids the degenerate all-forces-cancel start that a uniform
  # random cloud can produce for small graphs. with_seed() keeps the
  # caller's random stream untouched.
  ang <- 2 * pi * (seq_len(n) - 1) / n
  nudge <- with_seed(stat@seed, list(
    x = stats::runif(n, -0.02, 0.02),
    y = stats::runif(n, -0.02, 0.02)
  ))
  px <- 0.5 + 0.3 * cos(ang) + nudge$x
  py <- 0.5 + 0.3 * sin(ang) + nudge$y

  # Fruchterman-Reingold: repulsion k^2/d between all pairs, attraction
  # d^2/k along edges, with a linearly cooling max displacement.
  k <- 0.7 / sqrt(n)
  temp <- 0.1
  for (iter in seq_len(stat@iterations)) {
    dx <- numeric(n)
    dy <- numeric(n)
    for (i in seq_len(n)) {
      ddx <- px[i] - px
      ddy <- py[i] - py
      dist <- sqrt(ddx^2 + ddy^2)
      dist[i] <- Inf # no self-force
      dist[dist < 1e-4] <- 1e-4
      rep_f <- k^2 / dist
      dx[i] <- sum(ddx / dist * rep_f)
      dy[i] <- sum(ddy / dist * rep_f)
    }
    for (e in seq_along(fi)) {
      a <- fi[e]
      b <- ti[e]
      if (a == b) next
      ddx <- px[a] - px[b]
      ddy <- py[a] - py[b]
      dist <- max(sqrt(ddx^2 + ddy^2), 1e-4)
      att <- dist^2 / k
      ux <- ddx / dist * att
      uy <- ddy / dist * att
      dx[a] <- dx[a] - ux
      dy[a] <- dy[a] - uy
      dx[b] <- dx[b] + ux
      dy[b] <- dy[b] + uy
    }
    disp <- sqrt(dx^2 + dy^2)
    disp[disp < 1e-9] <- 1e-9
    step <- pmin(disp, temp) / disp
    px <- px + dx * step
    py <- py + dy * step
    # Keep nodes inside the panel with margin for labels.
    px <- pmin(pmax(px, 0.05), 0.95)
    py <- pmin(pmax(py, 0.05), 0.92)
    temp <- temp * (1 - iter / stat@iterations)
  }

  node_colors <- rep("#4A6DB5", n)
  if (!is.null(values$color)) {
    # Color follows the source node when the user mapped it.
    for (e in seq_along(fi)) node_colors[fi[e]] <- values$color[e]
  }

  # Emit finished marks rather than per-row positions: nodes are not
  # one-per-row, and the layout is already in panel coordinates.
  prm <- values$params %||% list()
  marks <- list()
  for (e in seq_along(fi)) {
    marks <- c(marks, list(mk_line(
      c(px[fi[e]], px[ti[e]]), c(py[fi[e]], py[ti[e]]),
      stroke = prm$edge_color %||% "#B8B8C4",
      width = prm$edge_width %||% 1, alpha = 0.8
    )))
  }
  node_r <- prm$node_size %||% 6
  for (i in seq_len(n)) {
    marks <- c(marks, list(mk_circle(
      px[i], py[i], node_r, node_colors[i], alpha = prm$alpha %||% 1
    )))
  }
  if (isTRUE(prm$label %||% TRUE)) {
    for (i in seq_len(n)) {
      marks <- c(marks, list(mk_text(
        px[i], py[i] + 0.038, nodes[i],
        size = prm$label_size %||% 11, color = "#3A3A3A"
      )))
    }
  }
  out <- list(
    x = c(0, 1), y = c(0, 1), group = c("a", "b"),
    marks_precomputed = marks
  )
  panel_span(out)
}

# --- Sankey ------------------------------------------------------------------

#' StatSankey: node stacking and ribbon paths
#' @noRd
StatSankey <- new_class("StatSankey", parent = Stat,
  constructor = function() new_object(Stat(name = "sankey", provides = c("x", "y")))
)

method(compute_stat, StatSankey) <- function(stat, values) {
  from <- as.character(values$x)
  to <- as.character(values$xend)
  val <- values$y
  if (is.null(values$xend) || is.null(val)) {
    stop("geom_sankey() requires `x` (source), `xend` (target), and `y` (value).")
  }

  # Assign each node to a stage: sources with no inbound edge are stage 0,
  # every other node is one past the deepest stage that feeds it.
  nodes <- unique(c(from, to))
  stage <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
  stage[setdiff(from, to)] <- 0L
  for (pass in seq_along(nodes)) {
    changed <- FALSE
    for (e in seq_along(from)) {
      s <- stage[[from[e]]]
      if (!is.na(s) && (is.na(stage[[to[e]]]) || stage[[to[e]]] < s + 1L)) {
        stage[[to[e]]] <- s + 1L
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  stage[is.na(stage)] <- 0L
  n_stage <- max(stage) + 1

  # Node totals: a node's height is the larger of what flows in and out.
  out_tot <- tapply(val, from, sum)
  in_tot <- tapply(val, to, sum)
  totals <- vapply(nodes, function(nd) {
    max(c(out_tot[nd], in_tot[nd]), na.rm = TRUE)
  }, numeric(1))
  totals[!is.finite(totals)] <- 0

  # Stack nodes within each stage, largest first, with gaps between them.
  #
  # One value-to-height scale is shared by every stage, set by the busiest
  # stage. Scaling each stage to fill the panel independently would make a
  # single flow change thickness between its two ends, which misreads as
  # the quantity changing.
  gap <- 0.03
  stage_totals <- vapply(unique(stage), function(s) {
    sum(totals[names(stage)[stage == s]])
  }, numeric(1))
  busiest <- max(stage_totals)
  max_nodes <- max(vapply(unique(stage), function(s) sum(stage == s), integer(1)))
  unit <- (1 - gap * max(0, max_nodes - 1)) / busiest

  ypos <- stats::setNames(numeric(length(nodes)), nodes)
  ytop <- ypos
  for (s in unique(stage)) {
    in_stage <- names(stage)[stage == s]
    in_stage <- in_stage[order(totals[in_stage], decreasing = TRUE)]
    # Center each stage's stack vertically so short stages sit in the middle.
    used <- sum(totals[in_stage]) * unit + gap * max(0, length(in_stage) - 1)
    cursor <- 1 - (1 - used) / 2
    for (nd in in_stage) {
      h <- totals[[nd]] * unit
      ytop[[nd]] <- cursor
      ypos[[nd]] <- cursor - h
      cursor <- cursor - h - gap
    }
  }

  # Inset the node columns so the first and last stages' labels have room
  # inside the panel instead of running off the edge.
  prm <- values$params %||% list()
  node_w <- prm$node_width %||% 0.035
  pad <- 0.02
  span <- 1 - node_w - 2 * pad
  xpos <- if (n_stage > 1) {
    stats::setNames(pad + (stage / (n_stage - 1)) * span, nodes)
  } else {
    stats::setNames(rep(pad, length(nodes)), nodes)
  }

  marks <- list()
  pal <- GGNEXT_DISCRETE_PALETTE
  node_color <- stats::setNames(
    rep_len(pal, length(nodes))[seq_along(nodes)], nodes
  )
  if (!is.null(values$color)) {
    for (e in seq_along(from)) node_color[[from[e]]] <- values$color[e]
  }

  # Ribbons: track how much of each node's height is already consumed, so
  # flows leave and enter in a consistent order and never overlap.
  used_out <- stats::setNames(numeric(length(nodes)), nodes)
  used_in <- stats::setNames(numeric(length(nodes)), nodes)
  ord <- order(match(from, nodes), -val)
  for (e in ord) {
    f <- from[e]
    t <- to[e]
    fh <- if (totals[[f]] > 0) val[e] / totals[[f]] * (ytop[[f]] - ypos[[f]]) else 0
    th <- if (totals[[t]] > 0) val[e] / totals[[t]] * (ytop[[t]] - ypos[[t]]) else 0
    y0a <- ytop[[f]] - used_out[[f]]
    y0b <- y0a - fh
    y1a <- ytop[[t]] - used_in[[t]]
    y1b <- y1a - th
    used_out[[f]] <- used_out[[f]] + fh
    used_in[[t]] <- used_in[[t]] + th

    x0 <- xpos[[f]] + node_w
    x1 <- xpos[[t]]
    curve_top <- sigmoid_between(x0, x1, y0a, y1a)
    curve_bot <- sigmoid_between(x0, x1, y0b, y1b)
    marks <- c(marks, list(mk_polygon(
      c(curve_top$x, rev(curve_bot$x)),
      c(curve_top$y, rev(curve_bot$y)),
      fill = node_color[[f]], alpha = prm$alpha %||% 0.55
    )))
  }
  # Node bars and labels on top of the ribbons.
  for (nd in nodes) {
    marks <- c(marks, list(mk_rect(
      xpos[[nd]], xpos[[nd]] + node_w, ypos[[nd]], ytop[[nd]],
      fill = node_color[[nd]], alpha = 1
    )))
    if (isTRUE(prm$label %||% TRUE)) {
      anchor <- if (stage[[nd]] == max(stage)) "end" else "start"
      lx <- if (anchor == "end") xpos[[nd]] - 0.01 else xpos[[nd]] + node_w + 0.01
      marks <- c(marks, list(mk_text(
        lx, (ypos[[nd]] + ytop[[nd]]) / 2, nd, size = 11,
        color = "#3A3A3A", anchor = anchor
      )))
    }
  }

  out <- list(
    x = c(0, 1), y = c(0, 1), group = c("a", "b"),
    marks_precomputed = marks
  )
  panel_span(out)
}

# A smooth S-curve between two points, used for Sankey ribbon edges.
sigmoid_between <- function(x0, x1, y0, y1, n = 40) {
  t <- seq(0, 1, length.out = n)
  w <- 3 * t^2 - 2 * t^3
  list(x = x0 + (x1 - x0) * t, y = y0 + (y1 - y0) * w)
}

# --- Chord -------------------------------------------------------------------

#' StatChord: circular arc allocation and ribbon curves
#' @noRd
StatChord <- new_class("StatChord", parent = Stat,
  constructor = function() new_object(Stat(name = "chord", provides = c("x", "y")))
)

method(compute_stat, StatChord) <- function(stat, values) {
  from <- as.character(values$x)
  to <- as.character(values$xend)
  val <- values$y
  if (is.null(values$xend) || is.null(val)) {
    stop("geom_chord() requires `x`, `xend`, and `y`.")
  }
  entities <- sort(unique(c(from, to)))
  n <- length(entities)
  # Each entity's arc is proportional to its total flow (in + out).
  totals <- vapply(entities, function(e) {
    sum(val[from == e]) + sum(val[to == e])
  }, numeric(1))
  gap <- 0.02 * 2 * pi
  avail <- 2 * pi - gap * n
  widths <- totals / sum(totals) * avail
  starts <- cumsum(c(0, widths[-n])) + gap * (seq_len(n) - 1)
  names(starts) <- entities
  names(widths) <- entities

  prm <- values$params %||% list()
  pal <- rep_len(GGNEXT_DISCRETE_PALETTE, n)
  ecol <- stats::setNames(pal[seq_len(n)], entities)
  R <- 0.42

  marks <- list()
  # Entity arcs around the rim.
  for (i in seq_len(n)) {
    e <- entities[i]
    a <- seq(starts[[e]], starts[[e]] + widths[[e]], length.out = 40)
    marks <- c(marks, list(mk_polygon(
      c(0.5 + R * cos(a), rev(0.5 + (R + 0.04) * cos(a))),
      c(0.5 + R * sin(a), rev(0.5 + (R + 0.04) * sin(a))),
      fill = ecol[[e]], alpha = 1
    )))
    if (isTRUE(prm$label %||% TRUE)) {
      mid <- starts[[e]] + widths[[e]] / 2
      marks <- c(marks, list(mk_text(
        0.5 + (R + 0.09) * cos(mid), 0.5 + (R + 0.09) * sin(mid), e,
        size = prm$label_size %||% 11, color = "#3A3A3A"
      )))
    }
  }
  # Ribbons: quadratic curves through the circle center.
  used <- stats::setNames(numeric(n), entities)
  for (e in seq_along(from)) {
    f <- from[e]
    t <- to[e]
    fw <- val[e] / totals[[f]] * widths[[f]]
    tw <- val[e] / totals[[t]] * widths[[t]]
    fa <- starts[[f]] + used[[f]]
    ta <- starts[[t]] + used[[t]]
    used[[f]] <- used[[f]] + fw
    used[[t]] <- used[[t]] + tw
    a1 <- seq(fa, fa + fw, length.out = 12)
    a2 <- seq(ta, ta + tw, length.out = 12)
    p1x <- 0.5 + R * cos(a1)
    p1y <- 0.5 + R * sin(a1)
    p2x <- 0.5 + R * cos(a2)
    p2y <- 0.5 + R * sin(a2)
    b1 <- quad_bezier(p1x[length(p1x)], p1y[length(p1y)], 0.5, 0.5, p2x[1], p2y[1])
    b2 <- quad_bezier(p2x[length(p2x)], p2y[length(p2y)], 0.5, 0.5, p1x[1], p1y[1])
    marks <- c(marks, list(mk_polygon(
      c(p1x, b1$x, p2x, b2$x), c(p1y, b1$y, p2y, b2$y),
      fill = ecol[[f]], alpha = prm$alpha %||% 0.45
    )))
  }
  out <- list(
    x = c(0, 1), y = c(0, 1), group = c("a", "b"),
    marks_precomputed = marks
  )
  panel_span(out)
}

# Quadratic Bezier from (x0,y0) to (x2,y2) with control point (x1,y1).
quad_bezier <- function(x0, y0, x1, y1, x2, y2, n = 24) {
  t <- seq(0, 1, length.out = n)
  list(
    x = (1 - t)^2 * x0 + 2 * (1 - t) * t * x1 + t^2 * x2,
    y = (1 - t)^2 * y0 + 2 * (1 - t) * t * y1 + t^2 * y2
  )
}

# --- Parallel coordinates ----------------------------------------------------

#' StatParallel: rescale each axis to a common \[0, 1\] range
#' @noRd
StatParallel <- new_class("StatParallel", parent = Stat,
  constructor = function() new_object(Stat(name = "parallel"))
)

method(compute_stat, StatParallel) <- function(stat, values) {
  # Each vertical axis is independently rescaled so variables measured in
  # different units share the panel.
  y <- values$y
  for (ax in unique(values$x)) {
    idx <- which(values$x == ax)
    rng <- range(y[idx], na.rm = TRUE)
    span <- rng[2] - rng[1]
    y[idx] <- if (span == 0) 0.5 else (y[idx] - rng[1]) / span
  }
  values$y <- y
  values
}

# --- Funnel ------------------------------------------------------------------

#' StatFunnel: centered bar widths proportional to value
#' @noRd
StatFunnel <- new_class("StatFunnel", parent = Stat,
  constructor = function() new_object(Stat(name = "funnel", provides = c("x", "y")))
)

method(compute_stat, StatFunnel) <- function(stat, values) {
  v <- values$y
  n <- length(v)
  w <- v / max(v)
  # Stages run top to bottom in the order given.
  slot <- seq_len(n)
  out <- list(
    x = rep(0.5, n),
    xmin = 0.5 - w / 2,
    xmax = 0.5 + w / 2,
    y = (n - slot + 0.5) / n,
    ymin = (n - slot + 0.1) / n,
    ymax = (n - slot + 0.9) / n,
    group = as.character(slot)
  )
  labs <- if (!is.null(values$label)) as.character(values$label) else {
    as.character(values$x)
  }
  out$label <- paste0(labs, "  ", format(v, big.mark = ",", trim = TRUE))
  if (!is.null(values$color)) out$color <- values$color
  panel_span(out)
}

# --- Stream ------------------------------------------------------------------

#' StatStream: stacked areas on a wiggle baseline
#' @noRd
StatStream <- new_class("StatStream", parent = Stat,
  constructor = function() new_object(Stat(name = "stream"))
)

method(compute_stat, StatStream) <- function(stat, values) {
  xs <- sort(unique(values$x))
  groups <- unique(values$group)
  # Wiggle baseline (Byron & Wattenberg): center each x column's stack on
  # minus half its total, so the band nearest the middle stays steadiest.
  ymin <- numeric(length(values$x))
  ymax <- numeric(length(values$x))
  for (xv in xs) {
    idx <- which(values$x == xv)
    idx <- idx[order(match(values$group[idx], groups))]
    total <- sum(values$y[idx])
    cursor <- -total / 2
    for (i in idx) {
      ymin[i] <- cursor
      ymax[i] <- cursor + values$y[i]
      cursor <- ymax[i]
    }
  }
  values$ymin <- ymin
  values$ymax <- ymax
  values$y <- (ymin + ymax) / 2
  values
}

# --- UpSet -------------------------------------------------------------------

#' StatUpset: intersection sizes and a membership matrix
#' @noRd
StatUpset <- new_class("StatUpset", parent = Stat,
  constructor = function() new_object(Stat(name = "upset", provides = c("x", "y")))
)

method(compute_stat, StatUpset) <- function(stat, values) {
  memb <- as.character(values$label %||% values$x)
  if (is.null(memb)) {
    stop("geom_upset() requires `label` (set membership, e.g. \"A&B\").")
  }
  counts <- sort(table(memb), decreasing = TRUE)
  combos <- names(counts)
  sets <- sort(unique(unlist(strsplit(combos, "&", fixed = TRUE))))
  n_c <- length(combos)
  n_s <- length(sets)

  # Top 60% of the panel: intersection-size bars. Bottom 40%: dot matrix.
  prm <- values$params %||% list()
  split_y <- 0.42
  bar_w <- 0.8 / n_c
  dot_r <- prm$dot_size %||% 5
  marks <- list()
  maxc <- max(counts)
  for (i in seq_len(n_c)) {
    cx <- 0.12 + (i - 0.5) / n_c * 0.86
    h <- counts[[i]] / maxc * (1 - split_y - 0.08)
    marks <- c(marks, list(
      mk_rect(cx - bar_w / 2.4, cx + bar_w / 2.4, split_y, split_y + h,
              fill = "#4A6DB5", alpha = prm$alpha %||% 1),
      mk_text(cx, split_y + h + 0.035, as.character(counts[[i]]),
              size = 10, color = "#3A3A3A")
    ))
    members <- strsplit(combos[i], "&", fixed = TRUE)[[1]]
    prev <- NULL
    for (j in seq_len(n_s)) {
      sy <- split_y - 0.06 - (j - 0.5) / n_s * (split_y - 0.06)
      on <- sets[j] %in% members
      marks <- c(marks, list(mk_circle(
        cx, sy, dot_r, if (on) "#2A2A33" else "#D8D8DE"
      )))
      if (on) {
        if (!is.null(prev)) {
          marks <- c(marks, list(mk_line(c(cx, cx), c(prev, sy),
                                         stroke = "#2A2A33", width = 2)))
        }
        prev <- sy
      }
    }
  }
  for (j in seq_len(n_s)) {
    sy <- split_y - 0.06 - (j - 0.5) / n_s * (split_y - 0.06)
    marks <- c(marks, list(mk_text(0.1, sy, sets[j], size = 11,
                                   color = "#3A3A3A", anchor = "end")))
  }
  out <- list(
    x = c(0, 1), y = c(0, 1), group = c("a", "b"),
    marks_precomputed = marks
  )
  panel_span(out)
}
