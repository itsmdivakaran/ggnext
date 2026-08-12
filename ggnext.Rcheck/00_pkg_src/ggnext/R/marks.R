# Mark constructors ------------------------------------------------------------
#
# Marks are the render-target-agnostic drawing primitives that geoms emit and
# renderers consume. All positions are in normalized panel coordinates
# (x/y in [0, 1], y pointing up); stroke widths, radii, and font sizes are in
# device px. Every geom builds its output exclusively from these five
# constructors, so a new render target only ever needs five cases.

# A filled circle (points, outliers, dumbbell ends).
mk_circle <- function(x, y, r, fill, alpha = 1) {
  list(type = "circle", x = x, y = y, r = r, fill = fill, alpha = alpha)
}

# An open polyline through (xs[i], ys[i]) (lines, steps, whiskers, KM curves).
# `dash` is an SVG dash pattern string like "4,3"; "" means solid.
mk_line <- function(xs, ys, stroke, width = 1.5, alpha = 1, dash = "") {
  list(
    type = "line", xs = as.list(xs), ys = as.list(ys),
    stroke = stroke, width = width, alpha = alpha, dash = dash
  )
}

# An axis-aligned filled rectangle (bars, boxes, tiles).
# (x0, y0) and (x1, y1) are opposite corners in normalized coordinates.
mk_rect <- function(x0, x1, y0, y1, fill, alpha = 1, stroke = "", stroke_width = 0) {
  list(
    type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
    fill = fill, alpha = alpha, stroke = stroke, stroke_width = stroke_width
  )
}

# A closed filled polygon (areas, ribbons, violins, densities).
mk_polygon <- function(xs, ys, fill, alpha = 1, stroke = "", stroke_width = 0) {
  list(
    type = "polygon", xs = as.list(xs), ys = as.list(ys),
    fill = fill, alpha = alpha, stroke = stroke, stroke_width = stroke_width
  )
}

# A text label anchored at (x, y). `size` is the font size in px.
mk_text <- function(x, y, text, size = 11, color = "#000000", alpha = 1,
                    anchor = "middle") {
  list(
    type = "text", x = x, y = y, text = as.character(text), size = size,
    color = color, alpha = alpha, anchor = anchor
  )
}
