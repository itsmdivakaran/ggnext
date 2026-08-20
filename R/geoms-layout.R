# Layout geoms -----------------------------------------------------------------
#
# Geoms whose positions come from a layout algorithm rather than directly
# from the data: radar, ridgeline, sankey/alluvial, treemap, network,
# chord, parallel coordinates, bump, funnel, stream, upset.
#
# Every layout is computed here in normalized panel space and emitted as the
# same five mark primitives, so neither renderer needs to know these geoms
# exist. Layouts that own their whole panel (treemap, network, sankey,
# chord, upset) run in [0, 1] directly and pair with a hidden axis.

#' @include classes.R marks.R
NULL

# --- Radar / spider ----------------------------------------------------------

#' GeomRadar: closed polygon per group around a polar panel
#'
#' @noRd
GeomRadar <- new_class("GeomRadar", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "radar",
      default_params = list(
        color = "#2B6BE0", alpha = 0.25, linewidth = 2, fill_alpha = 0.25,
        points = TRUE
      )
    ))
  }
)

method(build_marks, GeomRadar) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    if (length(idx) < 2) next
    # Vertices must run around the circle in axis order, not in data-row
    # order — otherwise the polygon zigzags across the center. The angular
    # position is already encoded in the pre-coord x, kept as `theta_order`.
    idx <- idx[order(scaled$theta_order[idx])]
    col <- scaled$color[[idx[1]]]
    # Close the ring by repeating the first vertex.
    xs <- c(scaled$x[idx], scaled$x[idx[1]])
    ys <- c(scaled$y[idx], scaled$y[idx[1]])
    marks <- c(marks, list(
      mk_polygon(xs, ys, fill = col, alpha = scaled$params$fill_alpha),
      mk_line(xs, ys, stroke = col, width = scaled$params$linewidth)
    ))
    if (isTRUE(scaled$params$points)) {
      for (i in idx) {
        marks <- c(marks, list(mk_circle(scaled$x[[i]], scaled$y[[i]], 3, col)))
      }
    }
  }
  marks
}

#' Radar (spider / star) chart
#'
#' Draws one closed polygon per group across a set of categorical axes.
#' Pair with [coord_polar()] for the familiar circular form; without it the
#' same layer reads as a parallel-coordinates profile.
#'
#' @param mapping,data Standard layer overrides. Map the axes to `x`, the
#'   values to `y`, and the series to `color` (or `group`).
#' @param color Line/fill color when `color` is not mapped.
#' @param alpha Fill opacity of the polygon interior.
#' @param linewidth Outline width in px.
#' @param points Draw a marker at each vertex.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   axis = rep(c("Speed", "Power", "Range", "Cost", "Safety"), 2),
#'   value = c(8, 6, 7, 4, 9, 5, 9, 4, 8, 6),
#'   model = rep(c("A", "B"), each = 5)
#' )
#' ggnext(d, aes(axis, value, color = model)) +
#'   geom_radar() +
#'   coord_polar() +
#'   theme_minimal()
#' @export
geom_radar <- function(mapping = NULL, data = NULL, color = NULL,
                       alpha = NULL, linewidth = NULL, points = TRUE) {
  layer_new(GeomRadar(), StatRadar(), mapping, data, list(
    color = color, fill_alpha = alpha, linewidth = linewidth, points = points
  ))
}

# --- Ridgeline ---------------------------------------------------------------

#' GeomRidgeline: stacked density ridges (joyplot)
#'
#' @noRd
GeomRidgeline <- new_class("GeomRidgeline", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "ridgeline",
      default_params = list(alpha = 0.8, linewidth = 1, color = "#4A6DB5")
    ))
  }
)

method(build_marks, GeomRidgeline) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    if (length(idx) < 2) next
    ord <- idx[order(scaled$x[idx])]
    col <- scaled$color[[ord[1]]]
    base <- scaled$ymin[ord]
    top <- scaled$ymax[ord]
    marks <- c(marks, list(
      mk_polygon(
        c(scaled$x[ord], rev(scaled$x[ord])), c(top, rev(base)),
        fill = col, alpha = scaled$params$alpha
      ),
      mk_line(scaled$x[ord], top, stroke = col,
              width = scaled$params$linewidth)
    ))
  }
  marks
}

#' Ridgeline plot (joyplot)
#'
#' One density curve per group, offset vertically so distributions can be
#' compared at a glance. Groups are ordered by their factor levels, first
#' level at the bottom.
#'
#' @param mapping,data Standard layer overrides. Map the value to `x` and
#'   the group to `y` (or `color`).
#' @param scale Height of each ridge as a multiple of the row spacing;
#'   values above 1 make ridges overlap.
#' @param color,alpha,linewidth Appearance.
#' @param bw Density bandwidth; `NULL` uses a Silverman rule of thumb.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext(iris, aes(Sepal.Length, Species)) + geom_ridgeline()
#' @export
geom_ridgeline <- function(mapping = NULL, data = NULL, scale = 1.6,
                           color = NULL, alpha = NULL, linewidth = NULL,
                           bw = NULL) {
  layer_new(GeomRidgeline(), StatRidgeline(scale = scale, bw = bw),
            mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth))
}

# --- Sankey / alluvial -------------------------------------------------------

#' GeomSankey: node bars plus curved flow ribbons
#'
#' @noRd
GeomSankey <- new_class("GeomSankey", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "sankey",
      default_params = list(alpha = 0.55, node_width = 0.04, label = TRUE),
      # The user maps source/target/value; x/y come from the layout stat.
      required_aes = c("x", "xend", "y")
    ))
  }
)

method(build_marks, GeomSankey) <- function(geom, scaled) {
  scaled$marks_precomputed %||% list()
}

#' Sankey / alluvial flow diagram
#'
#' Shows how quantities move between stages. Nodes are drawn as bars at
#' each stage and flows as curved ribbons whose thickness is the value.
#'
#' The layout (node positions, ribbon paths) is computed from scratch:
#' nodes are stacked within their stage in decreasing size, and ribbons
#' leave and enter nodes in the same order, which is what keeps the
#' crossings readable.
#'
#' @param mapping,data Standard layer overrides. Requires `x` (source
#'   node), `xend` (target node), and `y` (flow value).
#' @param alpha Ribbon opacity.
#' @param node_width Node bar width as a fraction of the panel.
#' @param label Draw node labels.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   from = c("Visited", "Visited", "Signed up", "Signed up"),
#'   to = c("Signed up", "Left", "Purchased", "Churned"),
#'   n = c(400, 600, 150, 250)
#' )
#' ggnext(d, aes(x = from, xend = to, y = n)) +
#'   geom_sankey() +
#'   theme_void()
#' @export
geom_sankey <- function(mapping = NULL, data = NULL, alpha = NULL,
                        node_width = NULL, label = TRUE) {
  layer_new(GeomSankey(), StatSankey(), mapping, data,
            list(alpha = alpha, node_width = node_width, label = label))
}

#' GeomAlluvial: node bars plus flow ribbons, for ordered repeated-measures data
#'
#' Same rendering as `GeomSankey` (`build_marks()` just replays
#' `marks_precomputed`); a separate class so `required_aes` matches the
#' alluvial input shape (`x`/`y`/`group`, not `x`/`xend`/`y`) and so error
#' messages name the right function.
#'
#' @noRd
GeomAlluvial <- new_class("GeomAlluvial", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "alluvial",
      default_params = list(alpha = 0.55, node_width = 0.04, label = TRUE),
      required_aes = c("x", "y", "group")
    ))
  }
)

method(build_marks, GeomAlluvial) <- function(geom, scaled) {
  scaled$marks_precomputed %||% list()
}

#' Alluvial diagram (ordered, repeated-measures categorical flow)
#'
#' Tracks the same subjects through an ordered sequence of stages (e.g.
#' visit 1 category -> visit 2 category -> visit 3 category), unlike
#' [geom_sankey()], which draws a general directed flow graph from an
#' edge list. Internally it tallies every consecutive-stage transition
#' and reuses `geom_sankey()`'s node-stacking and ribbon-path layout.
#'
#' @param mapping,data Standard layer overrides. Map the stage to `x`
#'   (an ordered factor keeps its declared stage order), the category at
#'   that stage to `y`, and the subject id to `group`.
#' @param alpha Ribbon opacity.
#' @param node_width Node bar width as a fraction of the panel.
#' @param label Draw node labels.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   subject = rep(1:8, each = 3),
#'   visit = rep(c("Baseline", "Week 4", "Week 8"), 8),
#'   response = c(
#'     "SD", "SD", "PR", "SD", "PR", "PR", "SD", "SD", "SD",
#'     "PR", "PR", "CR", "SD", "PR", "PR", "SD", "SD", "PD",
#'     "PR", "CR", "CR", "SD", "PD", "PD"
#'   )
#' )
#' d$visit <- factor(d$visit, levels = c("Baseline", "Week 4", "Week 8"))
#' ggnext(d, aes(x = visit, y = response, group = subject)) +
#'   geom_alluvial() +
#'   theme_void()
#' @export
geom_alluvial <- function(mapping = NULL, data = NULL, alpha = NULL,
                          node_width = NULL, label = TRUE) {
  layer_new(GeomAlluvial(), StatAlluvial(), mapping, data,
            list(alpha = alpha, node_width = node_width, label = label))
}

# --- Treemap -----------------------------------------------------------------

#' GeomTreemap: squarified nested rectangles
#'
#' @noRd
GeomTreemap <- new_class("GeomTreemap", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "treemap",
      default_params = list(alpha = 1, label = TRUE, label_size = 12),
      # Positions are produced by the squarify layout, not mapped.
      required_aes = "size"
    ))
  }
)

method(build_marks, GeomTreemap) <- function(geom, scaled) {
  # Rectangles come from the squarified layout in StatTreemap; here we only
  # turn them into marks and add labels that fit.
  marks <- list()
  for (i in seq_along(scaled$xmin)) {
    marks <- c(marks, list(mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha,
      stroke = "#FFFFFF", stroke_width = 2
    )))
  }
  if (isTRUE(scaled$params$label) && !is.null(scaled$label)) {
    for (i in seq_along(scaled$xmin)) {
      w <- abs(scaled$xmax[[i]] - scaled$xmin[[i]])
      h <- abs(scaled$ymax[[i]] - scaled$ymin[[i]])
      # Only label tiles with room for the text.
      if (w < 0.06 || h < 0.05) next
      marks <- c(marks, list(mk_text(
        (scaled$xmin[[i]] + scaled$xmax[[i]]) / 2,
        (scaled$ymin[[i]] + scaled$ymax[[i]]) / 2,
        scaled$label[[i]], size = scaled$params$label_size,
        color = contrast_text(scaled$color[[i]])
      )))
    }
  }
  marks
}

#' Treemap
#'
#' Area-proportional nested rectangles, laid out with the squarified
#' algorithm (Bruls, Huizing & van Wijk 2000) so tiles stay close to square
#' and stay comparable by area.
#'
#' @param mapping,data Standard layer overrides. Requires `size` (the area
#'   value) and `label` (the tile name); map `color` to shade by category.
#' @param alpha Tile opacity.
#' @param label Draw tile labels where they fit.
#' @param label_size Label size in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   region = c("North", "South", "East", "West", "Central"),
#'   revenue = c(52, 38, 27, 19, 11)
#' )
#' ggnext(d, aes(size = revenue, label = region, color = region)) +
#'   geom_treemap() +
#'   theme_void()
#' @export
geom_treemap <- function(mapping = NULL, data = NULL, alpha = NULL,
                         label = TRUE, label_size = NULL) {
  layer_new(GeomTreemap(), StatTreemap(), mapping, data,
            list(alpha = alpha, label = label, label_size = label_size))
}

# --- Network -----------------------------------------------------------------

#' GeomNetwork: force-directed node-link diagram
#'
#' @noRd
GeomNetwork <- new_class("GeomNetwork", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "network",
      default_params = list(
        alpha = 1, node_size = 6, edge_color = "#B8B8C4", edge_width = 1,
        label = TRUE, label_size = 11
      ),
      # Nodes are named by x/xend; the force layout supplies coordinates.
      required_aes = c("x", "xend")
    ))
  }
)

method(build_marks, GeomNetwork) <- function(geom, scaled) {
  # The force layout in StatNetwork emits finished marks: node positions are
  # not one-per-row, so they cannot ride through the per-row scaling step.
  scaled$marks_precomputed %||% list()
}

#' Network (node-link) diagram
#'
#' Lays out a graph with a from-scratch Fruchterman-Reingold force
#' simulation (repulsion between all nodes, attraction along edges, with a
#' cooling schedule) and draws nodes and edges in one unified grammar —
#' node and edge aesthetics come from the same `aes()`.
#'
#' @param mapping,data Standard layer overrides. Requires `x` (source node)
#'   and `xend` (target node); optional `size` weights the edges.
#' @param node_size Node radius in px.
#' @param edge_color,edge_width Edge appearance.
#' @param label,label_size Node labels.
#' @param alpha Node opacity.
#' @param iterations Force-simulation steps; more is slower but tidier.
#' @param seed Random seed for the initial layout, so plots reproduce.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   from = c("A", "A", "B", "C", "D", "E"),
#'   to = c("B", "C", "C", "D", "E", "A")
#' )
#' ggnext(d, aes(x = from, xend = to)) + geom_network() + theme_void()
#' @export
geom_network <- function(mapping = NULL, data = NULL, node_size = NULL,
                         edge_color = NULL, edge_width = NULL, label = TRUE,
                         label_size = NULL, alpha = NULL, iterations = 200,
                         seed = 1) {
  layer_new(GeomNetwork(), StatNetwork(iterations = iterations, seed = seed),
            mapping, data,
            list(node_size = node_size, edge_color = edge_color,
                 edge_width = edge_width, label = label,
                 label_size = label_size, alpha = alpha))
}

# --- Chord -------------------------------------------------------------------

#' GeomChord: circular relationship diagram
#'
#' @noRd
GeomChord <- new_class("GeomChord", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "chord",
      default_params = list(alpha = 0.5, label = TRUE, label_size = 11),
      required_aes = c("x", "xend", "y")
    ))
  }
)

method(build_marks, GeomChord) <- function(geom, scaled) {
  scaled$marks_precomputed %||% list()
}

#' Chord diagram
#'
#' Entities sit on a circle; each relationship is a ribbon whose ends are
#' arcs proportional to the flow. Ribbon interiors are quadratic curves
#' pulled toward the circle center.
#'
#' @param mapping,data Standard layer overrides. Requires `x` (source),
#'   `xend` (target), and `y` (flow value).
#' @param alpha Ribbon opacity.
#' @param label,label_size Entity labels around the rim.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   from = c("A", "A", "B", "C"), to = c("B", "C", "C", "A"),
#'   n = c(5, 3, 7, 2)
#' )
#' ggnext(d, aes(x = from, xend = to, y = n)) + geom_chord() + theme_void()
#' @export
geom_chord <- function(mapping = NULL, data = NULL, alpha = NULL,
                       label = TRUE, label_size = NULL) {
  layer_new(GeomChord(), StatChord(), mapping, data,
            list(alpha = alpha, label = label, label_size = label_size))
}

# --- Parallel coordinates ----------------------------------------------------

#' GeomParallel: one polyline per observation across axes
#'
#' @noRd
GeomParallel <- new_class("GeomParallel", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "parallel",
      default_params = list(alpha = 0.5, linewidth = 1.2)
    ))
  }
)

method(build_marks, GeomParallel) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    if (length(idx) < 2) next
    ord <- idx[order(scaled$x[idx])]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = scaled$color[[ord[1]]],
      width = scaled$params$linewidth, alpha = scaled$params$alpha
    )))
  }
  marks
}

#' Parallel coordinates plot
#'
#' Each observation is a line crossing one vertical axis per variable —
#' the standard way to eyeball high-dimensional structure and clusters.
#' Each axis is independently rescaled to \[0, 1\] by the stat, so
#' variables in different units are comparable.
#'
#' @param mapping,data Standard layer overrides. Map the variable to `x`,
#'   the value to `y`, and the observation id to `group`.
#' @param color,alpha,linewidth Appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   id = rep(1:3, each = 3),
#'   var = rep(c("a", "b", "c"), 3),
#'   val = c(1, 9, 4, 3, 5, 8, 7, 2, 6)
#' )
#' ggnext(d, aes(var, val, group = id, color = factor(id))) + geom_parallel()
#' @export
geom_parallel <- function(mapping = NULL, data = NULL, color = NULL,
                          alpha = NULL, linewidth = NULL) {
  layer_new(GeomParallel(), StatParallel(), mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth))
}

# --- Bump --------------------------------------------------------------------

#' GeomBump: smoothed rank-over-time lines
#'
#' @noRd
GeomBump <- new_class("GeomBump", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "bump",
      default_params = list(alpha = 1, linewidth = 2.5, points = TRUE)
    ))
  }
)

method(build_marks, GeomBump) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    if (length(idx) < 2) next
    ord <- idx[order(scaled$x[idx])]
    col <- scaled$color[[ord[1]]]
    sm <- sigmoid_path(scaled$x[ord], scaled$y[ord])
    marks <- c(marks, list(mk_line(
      sm$x, sm$y, stroke = col, width = scaled$params$linewidth,
      alpha = scaled$params$alpha
    )))
    if (isTRUE(scaled$params$points)) {
      for (i in ord) {
        marks <- c(marks, list(mk_circle(scaled$x[[i]], scaled$y[[i]], 4, col)))
      }
    }
  }
  marks
}

#' Bump chart (rank over time)
#'
#' Rank trajectories drawn with sigmoid interpolation between periods, so
#' crossings read cleanly instead of as sharp zigzags.
#'
#' @param mapping,data Standard layer overrides. Map time to `x`, rank to
#'   `y`, and the series to `color`.
#' @param color,alpha,linewidth Appearance.
#' @param points Draw a marker at each period.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   year = rep(2021:2023, 3),
#'   rank = c(1, 2, 3, 2, 1, 1, 3, 3, 2),
#'   team = rep(c("A", "B", "C"), each = 3)
#' )
#' ggnext(d, aes(year, rank, color = team)) +
#'   geom_bump() + scale_y_reverse()
#' @export
geom_bump <- function(mapping = NULL, data = NULL, color = NULL,
                      alpha = NULL, linewidth = NULL, points = TRUE) {
  layer_new(GeomBump(), StatIdentity(), mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth,
                 points = points))
}

# Sigmoid interpolation between consecutive points: an S-curve on each
# segment, which is what makes bump charts readable where lines cross.
sigmoid_path <- function(x, y, n = 30) {
  xs <- numeric(0)
  ys <- numeric(0)
  for (i in seq_len(length(x) - 1)) {
    t <- seq(0, 1, length.out = n)
    # Smoothstep: 3t^2 - 2t^3 has zero slope at both ends.
    w <- 3 * t^2 - 2 * t^3
    xs <- c(xs, x[i] + (x[i + 1] - x[i]) * t)
    ys <- c(ys, y[i] + (y[i + 1] - y[i]) * w)
  }
  list(x = xs, y = ys)
}

# --- Funnel ------------------------------------------------------------------

#' GeomFunnel: centered tapering stage bars
#'
#' @noRd
GeomFunnel <- new_class("GeomFunnel", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "funnel",
      default_params = list(alpha = 0.9, label = TRUE, label_size = 12)
    ))
  }
)

method(build_marks, GeomFunnel) <- function(geom, scaled) {
  marks <- list()
  n <- length(scaled$y)
  for (i in seq_len(n)) {
    marks <- c(marks, list(mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha
    )))
  }
  if (isTRUE(scaled$params$label) && !is.null(scaled$label)) {
    for (i in seq_len(n)) {
      marks <- c(marks, list(mk_text(
        0.5, (scaled$ymin[[i]] + scaled$ymax[[i]]) / 2, scaled$label[[i]],
        size = scaled$params$label_size,
        color = contrast_text(scaled$color[[i]])
      )))
    }
  }
  marks
}

#' Conversion funnel
#'
#' Horizontally centered bars, one per stage, whose widths are proportional
#' to the value — the standard conversion/drop-off view.
#'
#' @param mapping,data Standard layer overrides. Map the stage to `x`
#'   (ordered top to bottom) and the count to `y`.
#' @param alpha Bar opacity.
#' @param label Draw the stage label and value inside each bar.
#' @param label_size Label size in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   stage = factor(c("Visits", "Signups", "Trials", "Paid"),
#'                  levels = c("Visits", "Signups", "Trials", "Paid")),
#'   n = c(10000, 3200, 1100, 420)
#' )
#' ggnext(d, aes(stage, n, color = stage)) + geom_funnel() + theme_void()
#' @export
geom_funnel <- function(mapping = NULL, data = NULL, alpha = NULL,
                        label = TRUE, label_size = NULL) {
  layer_new(GeomFunnel(), StatFunnel(), mapping, data,
            list(alpha = alpha, label = label, label_size = label_size))
}

# --- Stream ------------------------------------------------------------------

#' GeomStream: wiggle-centered stacked areas
#'
#' @noRd
GeomStream <- new_class("GeomStream", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "stream",
      default_params = list(alpha = 0.9)
    ))
  }
)

method(build_marks, GeomStream) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    if (length(idx) < 2) next
    ord <- idx[order(scaled$x[idx])]
    marks <- c(marks, list(mk_polygon(
      c(scaled$x[ord], rev(scaled$x[ord])),
      c(scaled$ymax[ord], rev(scaled$ymin[ord])),
      fill = scaled$color[[ord[1]]], alpha = scaled$params$alpha
    )))
  }
  marks
}

#' Streamgraph
#'
#' Stacked areas centered on a wiggle baseline rather than zero, which
#' keeps every band's thickness readable as it changes over time.
#'
#' @param mapping,data Standard layer overrides. Map time to `x`, value to
#'   `y`, and the series to `color`.
#' @param alpha Band opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   t = rep(1:6, 3),
#'   v = c(2, 4, 6, 5, 3, 2, 1, 3, 5, 8, 6, 4, 5, 4, 3, 4, 6, 7),
#'   grp = rep(c("a", "b", "c"), each = 6)
#' )
#' ggnext(d, aes(t, v, color = grp)) + geom_stream() + theme_minimal()
#' @export
geom_stream <- function(mapping = NULL, data = NULL, alpha = NULL) {
  layer_new(GeomStream(), StatStream(), mapping, data, list(alpha = alpha))
}

# --- UpSet -------------------------------------------------------------------

#' GeomUpset: set-intersection bars with a membership matrix
#'
#' @noRd
GeomUpset <- new_class("GeomUpset", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "upset",
      default_params = list(alpha = 1, dot_size = 6),
      # Membership strings only; the stat counts intersections.
      required_aes = "label"
    ))
  }
)

method(build_marks, GeomUpset) <- function(geom, scaled) {
  scaled$marks_precomputed %||% list()
}

#' UpSet plot (set intersections)
#'
#' Replaces unreadable 4+ way Venn diagrams: a bar chart of intersection
#' sizes above a dot matrix showing which sets each bar belongs to.
#'
#' @param mapping,data Standard layer overrides. Requires `label` (the set
#'   membership of each observation, e.g. `"A&B"`).
#' @param alpha Bar opacity.
#' @param dot_size Membership-dot radius in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(sets = c("A", "A&B", "B", "A&B&C", "C", "A&B", "A"))
#' ggnext(d, aes(label = sets)) + geom_upset() + theme_void()
#' @export
geom_upset <- function(mapping = NULL, data = NULL, alpha = NULL,
                       dot_size = NULL) {
  layer_new(GeomUpset(), StatUpset(), mapping, data,
            list(alpha = alpha, dot_size = dot_size))
}

# --- helpers -----------------------------------------------------------------

# Pick black or white text for legibility on a colored tile, using the
# relative-luminance formula from WCAG 2.x.
contrast_text <- function(hex) {
  rgb <- grDevices::col2rgb(hex)[, 1] / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- sum(c(0.2126, 0.7152, 0.0722) * lin)
  if (lum > 0.45) "#1A1A22" else "#FFFFFF"
}
