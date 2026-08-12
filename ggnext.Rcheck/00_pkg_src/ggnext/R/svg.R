# Static render target: minimal hand-rolled SVG writer ------------------------
#
# No grid, no svglite, no gridSVG: SVG is assembled as strings from the
# computed-geometry buffer. Device coordinates are standard SVG (origin at
# top-left, y growing downward); the buffer's normalized y (which grows
# upward, like the data) is flipped exactly once, in norm_to_px().
#
# All styling comes from buffer$theme — this file holds no colors of its own.

# Convert a normalized panel position to device pixels.
# `flip = TRUE` (the y axis) maps norm 0 -> panel bottom, norm 1 -> panel top.
norm_to_px <- function(norm, offset, extent, flip = FALSE) {
  t <- if (flip) 1 - norm else norm
  offset + t * extent
}

# Format a device coordinate: 2 decimal places is sub-pixel accurate and
# keeps snapshots byte-stable across platforms.
px <- function(x) {
  formatC(x, format = "f", digits = 2, drop0trailing = TRUE)
}

# One SVG element as a string, from a tag name and attribute list.
# Attribute values are escaped: a theme font stack or colour containing a
# double quote would otherwise close the attribute early and produce an
# unparseable document.
svg_tag <- function(tag, attrs, content = NULL) {
  attr_str <- paste0(
    names(attrs), "=\"", xml_escape_attr(unlist(attrs)), "\"",
    collapse = " "
  )
  if (is.null(content)) {
    paste0("<", tag, " ", attr_str, "/>")
  } else {
    paste0("<", tag, " ", attr_str, ">", xml_escape(content), "</", tag, ">")
  }
}

# Attribute values additionally need the quote characters escaped.
xml_escape_attr <- function(s) {
  s <- xml_escape(as.character(s))
  s <- gsub("\"", "&quot;", s, fixed = TRUE)
  gsub("'", "&apos;", s, fixed = TRUE)
}

xml_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

#' Render a geometry buffer to an SVG string
#'
#' @param buffer Output of build_geometry().
#' @return Length-1 character vector: a complete standalone SVG document.
#' @noRd
render_svg <- function(buffer) {
  th <- buffer$theme
  out <- character()

  out <- c(out, sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" ",
      "viewBox=\"0 0 %d %d\" font-family=\"%s\">"
    ),
    buffer$width, buffer$height, buffer$width, buffer$height,
    xml_escape_attr(th$font)
  ))
  out <- c(out, svg_tag("rect", list(
    x = 0, y = 0, width = buffer$width, height = buffer$height,
    fill = th$background
  )))

  # Title block above the panels.
  out <- c(out, svg_title_block(buffer, th))

  # Each panel draws its own chrome and marks; a non-faceted plot has one.
  for (i in seq_along(buffer$panels)) {
    out <- c(out, svg_panel(buffer$panels[[i]], th, i))
  }

  out <- c(out, svg_legend(buffer, th))
  out <- c(out, "</svg>")
  paste(out, collapse = "\n")
}

# Plot title, subtitle, tag, and caption.
svg_title_block <- function(buffer, th) {
  lb <- buffer$labels
  out <- character()
  y <- 8
  if (!is.null(lb$tag)) {
    out <- c(out, svg_tag("text", list(
      x = 10, y = y + th$plot_title_size, `text-anchor` = "start",
      `font-family` = th$title_font, `font-size` = th$plot_title_size,
      `font-weight` = "bold", fill = th$title_color
    ), lb$tag))
  }
  tag_indent <- if (is.null(lb$tag)) 10 else 34
  if (!is.null(lb$title)) {
    y <- y + th$plot_title_size
    out <- c(out, svg_tag("text", list(
      x = tag_indent, y = y, `text-anchor` = "start",
      `font-family` = th$title_font, `font-size` = th$plot_title_size,
      `font-weight` = th$title_face, fill = th$title_color
    ), lb$title))
    y <- y + 6
  }
  if (!is.null(lb$subtitle)) {
    y <- y + th$plot_subtitle_size
    out <- c(out, svg_tag("text", list(
      x = tag_indent, y = y, `text-anchor` = "start",
      `font-size` = th$plot_subtitle_size, fill = th$subtitle_color
    ), lb$subtitle))
  }
  if (!is.null(lb$caption)) {
    out <- c(out, svg_tag("text", list(
      x = buffer$width - 10, y = buffer$height - 8, `text-anchor` = "end",
      `font-size` = th$caption_size, fill = th$subtitle_color
    ), lb$caption))
  }
  out
}

# One panel: background, grid, marks, axes, strip label.
svg_panel <- function(panel, th, index) {
  p <- panel$rect
  out <- character()

  out <- c(out, svg_tag("rect", list(
    x = px(p$x), y = px(p$y), width = px(p$w), height = px(p$h),
    fill = th$panel_fill
  )))

  # Facet strip above the panel.
  if (!is.null(panel$strip)) {
    sh <- th$strip_font_size + 12
    out <- c(out, svg_tag("rect", list(
      x = px(p$x), y = px(p$y - sh), width = px(p$w), height = px(sh),
      fill = th$strip_fill
    )))
    out <- c(out, svg_tag("text", list(
      x = px(p$x + p$w / 2), y = px(p$y - sh / 2 + th$strip_font_size * 0.35),
      `text-anchor` = "middle", `font-size` = th$strip_font_size,
      fill = th$strip_color
    ), panel$strip))
  }

  out <- c(out, if (is.null(panel$polar)) {
    svg_cartesian_grid(panel, th)
  } else {
    svg_polar_grid(panel, th)
  })

  # Data marks, clipped to the panel so out-of-limits points don't spill.
  clip_id <- paste0("panel", index)
  out <- c(out, sprintf(
    "<clipPath id=\"%s\"><rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"/></clipPath>",
    clip_id, px(p$x), px(p$y), px(p$w), px(p$h)
  ))
  out <- c(out, sprintf("<g clip-path=\"url(#%s)\">", clip_id))
  for (layer in panel$layers) {
    for (mark in layer$marks) {
      out <- c(out, render_svg_mark(mark, p))
    }
  }
  out <- c(out, "</g>")

  if (is.null(panel$polar)) {
    out <- c(out, svg_cartesian_axes(panel, th))
  }
  out
}

svg_cartesian_grid <- function(panel, th) {
  p <- panel$rect
  out <- character()
  if (nzchar(th$grid_color) && isTRUE(th$grid_major_x)) {
    for (n in panel$x$ticks$norm) {
      gx <- norm_to_px(n, p$x, p$w)
      out <- c(out, svg_tag("line", list(
        x1 = px(gx), y1 = px(p$y), x2 = px(gx), y2 = px(p$y + p$h),
        stroke = th$grid_color, `stroke-width` = 1
      )))
    }
  }
  if (nzchar(th$grid_color) && isTRUE(th$grid_major_y)) {
    for (n in panel$y$ticks$norm) {
      gy <- norm_to_px(n, p$y, p$h, flip = TRUE)
      out <- c(out, svg_tag("line", list(
        x1 = px(p$x), y1 = px(gy), x2 = px(p$x + p$w), y2 = px(gy),
        stroke = th$grid_color, `stroke-width` = 1
      )))
    }
  }
  # Minor gridlines, under the majors, when the theme asks for them.
  if (nzchar(th$grid_color_minor)) {
    if (isTRUE(th$grid_major_x)) {
      for (n in panel$x$ticks$minor) {
        gx <- norm_to_px(n, p$x, p$w)
        out <- c(out, svg_tag("line", list(
          x1 = px(gx), y1 = px(p$y), x2 = px(gx), y2 = px(p$y + p$h),
          stroke = th$grid_color_minor, `stroke-width` = 0.5
        )))
      }
    }
    if (isTRUE(th$grid_major_y)) {
      for (n in panel$y$ticks$minor) {
        gy <- norm_to_px(n, p$y, p$h, flip = TRUE)
        out <- c(out, svg_tag("line", list(
          x1 = px(p$x), y1 = px(gy), x2 = px(p$x + p$w), y2 = px(gy),
          stroke = th$grid_color_minor, `stroke-width` = 0.5
        )))
      }
    }
  }

  if (nzchar(th$panel_border)) {
    out <- c(out, svg_tag("rect", list(
      x = px(p$x), y = px(p$y), width = px(p$w), height = px(p$h),
      fill = "none", stroke = th$panel_border, `stroke-width` = 1
    )))
  }
  out
}

# Polar grid: concentric rings for the radial axis, spokes for the angular
# axis, and category labels around the outside.
svg_polar_grid <- function(panel, th) {
  p <- panel$rect
  pol <- panel$polar
  cx <- p$x + p$w / 2
  cy <- p$y + p$h / 2
  out <- character()
  if (!nzchar(th$grid_color)) {
    return(out)
  }
  for (i in seq_along(pol$rings)) {
    r_norm <- pol$rings[[i]]
    rr <- (pol$inner + (1 - pol$inner) * r_norm) * 0.5
    out <- c(out, svg_tag("ellipse", list(
      cx = px(cx), cy = px(cy), rx = px(rr * p$w), ry = px(rr * p$h),
      fill = "none", stroke = th$grid_color, `stroke-width` = 1
    )))
    out <- c(out, svg_tag("text", list(
      x = px(cx + 3), y = px(cy - rr * p$h - 2), `text-anchor` = "start",
      `font-size` = th$tick_font_size, fill = th$label_color
    ), pol$ring_labels[[i]]))
  }
  for (i in seq_along(pol$spokes)) {
    ang <- pol$start + pol$direction * 2 * pi * pol$spokes[[i]]
    ex <- cx + 0.5 * p$w * sin(ang)
    ey <- cy - 0.5 * p$h * cos(ang)
    out <- c(out, svg_tag("line", list(
      x1 = px(cx), y1 = px(cy), x2 = px(ex), y2 = px(ey),
      stroke = th$grid_color, `stroke-width` = 1
    )))
    lx <- cx + 0.56 * p$w * sin(ang)
    ly <- cy - 0.56 * p$h * cos(ang)
    anchor <- if (abs(sin(ang)) < 0.2) "middle" else if (sin(ang) > 0) "start" else "end"
    out <- c(out, svg_tag("text", list(
      x = px(lx), y = px(ly + th$tick_font_size * 0.35),
      `text-anchor` = anchor, `font-size` = th$tick_font_size,
      fill = th$label_color
    ), pol$spoke_labels[[i]]))
  }
  out
}

svg_cartesian_axes <- function(panel, th) {
  p <- panel$rect
  out <- character()

  if (isTRUE(th$axis_line_x)) {
    out <- c(out, svg_tag("line", list(
      x1 = px(p$x), y1 = px(p$y + p$h), x2 = px(p$x + p$w), y2 = px(p$y + p$h),
      stroke = th$axis_color, `stroke-width` = 1
    )))
  }
  if (isTRUE(th$axis_line_y)) {
    out <- c(out, svg_tag("line", list(
      x1 = px(p$x), y1 = px(p$y), x2 = px(p$x), y2 = px(p$y + p$h),
      stroke = th$axis_color, `stroke-width` = 1
    )))
  }

  if (isTRUE(panel$show_x_labels) && isTRUE(th$axis_text_x)) {
    for (i in seq_along(panel$x$ticks$norm)) {
      tx <- norm_to_px(panel$x$ticks$norm[[i]], p$x, p$w)
      y0 <- p$y + p$h
      if (isTRUE(th$ticks_x)) {
        out <- c(out, svg_tag("line", list(
          x1 = px(tx), y1 = px(y0), x2 = px(tx), y2 = px(y0 + th$tick_len),
          stroke = th$axis_color, `stroke-width` = 1
        )))
      }
      out <- c(out, svg_tag("text", list(
        x = px(tx), y = px(y0 + th$tick_len + th$tick_font_size),
        `text-anchor` = "middle", `font-size` = th$tick_font_size,
        fill = th$label_color
      ), panel$x$ticks$labels[[i]]))
    }
  }
  if (isTRUE(panel$show_y_labels) && isTRUE(th$axis_text_y)) {
    for (i in seq_along(panel$y$ticks$norm)) {
      ty <- norm_to_px(panel$y$ticks$norm[[i]], p$y, p$h, flip = TRUE)
      if (isTRUE(th$ticks_y)) {
        out <- c(out, svg_tag("line", list(
          x1 = px(p$x - th$tick_len), y1 = px(ty), x2 = px(p$x), y2 = px(ty),
          stroke = th$axis_color, `stroke-width` = 1
        )))
      }
      out <- c(out, svg_tag("text", list(
        x = px(p$x - th$tick_len - 3), y = px(ty + th$tick_font_size * 0.35),
        `text-anchor` = "end", `font-size` = th$tick_font_size,
        fill = th$label_color
      ), panel$y$ticks$labels[[i]]))
    }
  }
  out
}

# Axis titles are drawn once for the whole figure, not per panel.
svg_axis_titles <- function(buffer, th) {
  first <- buffer$panels[[1]]
  last <- buffer$panels[[length(buffer$panels)]]
  x_mid <- (first$rect$x + last$rect$x + last$rect$w) / 2
  y_mid <- (first$rect$y + last$rect$y + last$rect$h) / 2
  caption_pad <- if (is.null(buffer$labels$caption)) 0 else th$caption_size + 8
  legend_pad <- if (identical(buffer$legend_position, "bottom")) 34 else 0
  out <- character()
  if (isTRUE(th$axis_title_x) && nzchar(first$x$title)) {
    out <- c(out, svg_tag("text", list(
      x = px(x_mid), y = px(buffer$height - 10 - caption_pad - legend_pad),
      `text-anchor` = "middle", `font-size` = th$title_font_size,
      fill = th$label_color
    ), first$x$title))
  }
  if (isTRUE(th$axis_title_y) && nzchar(first$y$title)) {
    out <- c(out, svg_tag("text", list(
      x = 15, y = px(y_mid),
      transform = sprintf("rotate(-90 15 %s)", px(y_mid)),
      `text-anchor` = "middle", `font-size` = th$title_font_size,
      fill = th$label_color
    ), first$y$title))
  }
  out
}

# Legend: discrete swatches or a continuous gradient bar.
svg_legend <- function(buffer, th) {
  out <- svg_axis_titles(buffer, th)
  lg <- buffer$legend
  if (is.null(lg)) {
    return(out)
  }
  first <- buffer$panels[[1]]
  last <- buffer$panels[[length(buffer$panels)]]

  if (identical(buffer$legend_position, "bottom")) {
    # Horizontal strip of swatches centered under the panels.
    y <- buffer$height - 18 -
      (if (is.null(buffer$labels$caption)) 0 else th$caption_size + 8)
    n <- length(lg$labels)
    item_w <- 90
    x0 <- (buffer$width - n * item_w) / 2
    for (i in seq_len(n)) {
      xi <- x0 + (i - 1) * item_w
      out <- c(out, svg_tag("rect", list(
        x = px(xi), y = px(y - 9), width = 12, height = 12,
        fill = if (lg$type == "gradient") lg$high else lg$colors[[i]]
      )))
      out <- c(out, svg_tag("text", list(
        x = px(xi + 18), y = px(y), `text-anchor` = "start",
        `font-size` = th$legend_font_size, fill = th$legend_text_color
      ), lg$labels[[i]]))
    }
    return(out)
  }

  lx <- last$rect$x + last$rect$w + 14
  ly <- first$rect$y + 8
  if (!is.null(lg$title)) {
    out <- c(out, svg_tag("text", list(
      x = px(lx), y = px(ly), `text-anchor` = "start",
      `font-size` = th$title_font_size, fill = th$label_color
    ), lg$title))
  }
  if (lg$type == "gradient") {
    # A vertical gradient bar with labels from high (top) to low (bottom).
    bar_h <- 120
    out <- c(out, sprintf(
      paste0(
        "<defs><linearGradient id=\"ggnext-grad\" x1=\"0\" y1=\"1\" ",
        "x2=\"0\" y2=\"0\"><stop offset=\"0\" stop-color=\"%s\"/>",
        "<stop offset=\"1\" stop-color=\"%s\"/></linearGradient></defs>"
      ),
      xml_escape_attr(lg$low), xml_escape_attr(lg$high)
    ))
    out <- c(out, svg_tag("rect", list(
      x = px(lx), y = px(ly + 10), width = 14, height = bar_h,
      fill = "url(#ggnext-grad)"
    )))
    n <- length(lg$labels)
    for (i in seq_len(n)) {
      # labels run low -> high, the bar runs bottom -> top.
      yy <- ly + 10 + bar_h - (i - 1) / max(1, n - 1) * bar_h
      out <- c(out, svg_tag("text", list(
        x = px(lx + 20), y = px(yy + th$tick_font_size * 0.35),
        `text-anchor` = "start", `font-size` = th$legend_font_size,
        fill = th$legend_text_color
      ), lg$labels[[i]]))
    }
    return(out)
  }
  for (i in seq_along(lg$labels)) {
    sy <- ly + 10 + (i - 1) * 20
    out <- c(out, svg_tag("rect", list(
      x = px(lx), y = px(sy), width = 12, height = 12, fill = lg$colors[[i]]
    )))
    out <- c(out, svg_tag("text", list(
      x = px(lx + 18), y = px(sy + 10), `text-anchor` = "start",
      `font-size` = th$legend_font_size, fill = th$legend_text_color
    ), lg$labels[[i]]))
  }
  out
}

# Render one mark to SVG. This switch is the complete mark vocabulary; a new
# primitive gets a case here (and one in the canvas template) and every geom
# can then use it. Geoms themselves never touch this file.
render_svg_mark <- function(mark, panel) {
  dx <- function(v) norm_to_px(v, panel$x, panel$w)
  dy <- function(v) norm_to_px(v, panel$y, panel$h, flip = TRUE)

  switch(mark$type,
    circle = svg_tag("circle", list(
      cx = px(dx(mark$x)),
      cy = px(dy(mark$y)),
      r = px(mark$r),
      fill = mark$fill,
      `fill-opacity` = mark$alpha
    )),
    line = {
      pts <- paste(
        px(dx(unlist(mark$xs))), px(dy(unlist(mark$ys))),
        sep = ",", collapse = " "
      )
      attrs <- list(
        points = pts, fill = "none",
        stroke = mark$stroke, `stroke-width` = mark$width,
        `stroke-opacity` = mark$alpha,
        `stroke-linejoin` = "round", `stroke-linecap` = "round"
      )
      if (nzchar(mark$dash)) attrs$`stroke-dasharray` <- mark$dash
      svg_tag("polyline", attrs)
    },
    rect = {
      xs <- sort(c(dx(mark$x0), dx(mark$x1)))
      ys <- sort(c(dy(mark$y0), dy(mark$y1)))
      attrs <- list(
        x = px(xs[1]), y = px(ys[1]),
        width = px(xs[2] - xs[1]), height = px(ys[2] - ys[1]),
        fill = mark$fill, `fill-opacity` = mark$alpha
      )
      if (nzchar(mark$stroke)) {
        attrs$stroke <- mark$stroke
        attrs$`stroke-width` <- mark$stroke_width
      }
      svg_tag("rect", attrs)
    },
    polygon = {
      pts <- paste(
        px(dx(unlist(mark$xs))), px(dy(unlist(mark$ys))),
        sep = ",", collapse = " "
      )
      attrs <- list(
        points = pts, fill = mark$fill, `fill-opacity` = mark$alpha
      )
      if (nzchar(mark$stroke)) {
        attrs$stroke <- mark$stroke
        attrs$`stroke-width` <- mark$stroke_width
      }
      svg_tag("polygon", attrs)
    },
    text = svg_tag("text", list(
      x = px(dx(mark$x)), y = px(dy(mark$y)),
      dy = "0.35em", `text-anchor` = mark$anchor,
      `font-size` = mark$size, fill = mark$color,
      `fill-opacity` = mark$alpha
    ), mark$text),
    stop("Unknown mark type: ", mark$type)
  )
}
