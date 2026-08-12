# Plot build: specification -> computed-geometry buffer -----------------------
#
# build_geometry() is the single geometry-computation step shared by every
# render target. It walks the grammar pipeline once:
#
#   layer data -> aes evaluation -> grouping -> facet split ->
#   discretize categorical positions -> stat -> position adjustment ->
#   scale training -> scale mapping (normalize to [0, 1]) ->
#   coord transform -> geom marks
#
# and returns a plain-list "geometry buffer" holding a list of *panels*
# (a non-faceted plot is the one-panel case). Renderers only ever read this
# buffer; they never see the original data or the S7 objects. That is the
# load-bearing guarantee behind "one object, many render targets, zero
# duplicated geometry code".

# Default device geometry. Margins leave room for tick labels and axis
# titles; the build grows them for legends, titles, and facet strips.
GGPLOT3_DEVICE <- list(
  width = 640,
  height = 480,
  margin = list(top = 20, right = 16, bottom = 48, left = 60)
)

# Which computed columns are positions on each axis. Anything listed here is
# trained into the axis domain and normalized through the axis scale;
# everything else passes through to the geom untouched.
X_POS_COLS <- c("x", "xend", "xmin", "xmax", "xref", "xec50")
Y_POS_COLS <- c(
  "y", "yend", "ymin", "ymax", "lower", "middle", "upper",
  "yzero", "yref", "ythresh"
)

# Geoms whose stat performs its own layout across the whole panel. Their
# output is already in [0, 1], so the build gives them bare unit axes and
# hides the axis chrome (see panel_span() in R/stats-layout.R).
PANEL_SPAN_GEOMS <- c(
  "treemap", "network", "sankey", "chord", "upset", "funnel", "consort"
)

# Geoms that span the panel from their own parameters rather than from
# data. Their placeholder rows must not widen the axis domain - a
# reference line should never change the scales it is drawn against.
NON_TRAINING_GEOMS <- "abline"

# Rows whose position cannot be drawn. Numeric positions must be finite
# (NA, NaN, Inf all fail); a categorical position must simply be present.
# Interval columns such as ymin/ymax are deliberately NOT checked: geoms
# like geom_forecast_band() use NA there to mean "no interval on this row".
non_finite_rows <- function(v) {
  if (is.numeric(v)) !is.finite(v) else is.na(v)
}

# Drop rows the renderer could not place, once, with a count - silently
# emitting `cx="NA"` into the SVG would produce an invalid document and put
# the affected points at the panel origin.
drop_non_finite <- function(values, geom_name) {
  pos <- intersect(c("x", "y"), names(values))
  if (length(pos) == 0) {
    return(values)
  }
  n <- length(values[[pos[1]]])
  bad <- Reduce(`|`, lapply(pos, function(k) non_finite_rows(values[[k]])))
  if (!any(bad)) {
    return(values)
  }
  warning(
    "Removed ", sum(bad), " row", if (sum(bad) > 1) "s" else "",
    " containing non-finite values (geom_", geom_name, "()).",
    call. = FALSE
  )
  lapply(values, function(v) if (length(v) == n) v[!bad] else v)
}

# TRUE when any layer's geom does its own whole-panel layout.
has_panel_span <- function(plot) {
  any(vapply(plot@layers, function(l) l@geom@name %in% PANEL_SPAN_GEOMS,
             logical(1)))
}

#' Compute the geometry buffer for a plot
#'
#' @param plot A [Ggplot3Plot].
#' @return A named list: device size, theme, title block, legend, and a list
#'   of panels (each with its own rect, axes, strip label, and marks in
#'   normalized panel coordinates).
#' @noRd
build_geometry <- function(plot) {
  if (length(plot@layers) == 0) {
    stop("Cannot render a plot with no layers; add e.g. geom_point().")
  }
  th <- plot@theme %||% theme_ggplot3()
  palette <- resolve_palette(plot, th)

  # Stages 1-4 of the pipeline (aes -> group -> facet -> stat -> position)
  # are shared with plot_data(), which needs exactly these computed values
  # in data units.
  staged <- stage_values(plot)
  panel_specs <- staged
  grid_shape <- attr(staged, "grid_shape")
  scale_types <- attr(staged, "scale_types")
  facet <- plot@facet
  free_x <- facet_is_free(facet, "x")
  free_y <- facet_is_free(facet, "y")
  all_layers <- unlist(lapply(panel_specs, function(ps) ps$layers),
                       recursive = FALSE)

  # -- 5. Train continuous domains (pooled, or per panel when free). ---------
  shared_scales <- train_scales(scale_types, all_layers)
  panel_scales <- lapply(panel_specs, function(ps) {
    if (!free_x && !free_y) {
      return(shared_scales)
    }
    own <- train_scales(scale_types, ps$layers)
    list(
      x = if (free_x) own$x else shared_scales$x,
      y = if (free_y) own$y else shared_scales$y
    )
  })

  # A panel-spanning layout owns its coordinates; showing [0, 1] ticks and
  # axis titles for them would be noise, so the chrome is switched off here
  # rather than making every such geom depend on theme_void().
  if (has_panel_span(plot)) {
    th <- theme(
      base = th, grid_color = "", grid_color_minor = "",
      axis_line_x = FALSE, axis_line_y = FALSE,
      ticks_x = FALSE, ticks_y = FALSE,
      axis_text_x = FALSE, axis_text_y = FALSE,
      axis_title_x = FALSE, axis_title_y = FALSE,
      panel_fill = th@background
    )
  }

  build_panels(plot, panel_specs, panel_scales, grid_shape, all_layers,
               th, palette, facet, free_x, free_y)
}

# Stages 1-4: evaluate aesthetics, derive groups, split into facet panels,
# discretize categorical positions, apply stats and position adjustments.
# Returns a list of panel specs, carrying the resolved scale types and grid
# shape as attributes.
stage_values <- function(plot) {
  # -- 1. Evaluate aesthetics and derive grouping, layer by layer. -----------
  layer_values <- lapply(plot@layers, function(layer) {
    data <- layer@data %||% plot@data
    if (is.null(data)) {
      stop("Layer has no data: supply data to ggplot3() or to the layer.")
    }
    mapping <- if (layer@inherit) {
      merge_aes(plot@mapping, layer@mapping)
    } else {
      layer@mapping
    }
    values <- if (is.null(mapping)) list() else eval_aes(mapping, data)

    # Required aesthetics are what the *user* must map. A stat may compute
    # some of them (stat_bin() supplies `y`), so those count as present.
    missing_aes <- setdiff(
      layer@geom@required_aes,
      c(names(values), layer@stat@provides)
    )
    if (length(missing_aes) > 0) {
      stop(
        "geom_", layer@geom@name, "() requires the aesthetic(s): ",
        paste(missing_aes, collapse = ", "),
        ". Map them in aes(), or use a stat that computes them.",
        call. = FALSE
      )
    }

    # Grouping: explicit `group` wins; otherwise a discrete color mapping
    # partitions the data; otherwise everything is one group.
    n_rows <- length(values[[1]] %||% character())
    if (is.null(values$group)) {
      values$group <- if (!is.null(values$color) && !is.numeric(values$color) &&
                          !is_color_constant(values$color)) {
        as.character(values$color)
      } else {
        rep("all", n_rows)
      }
    } else {
      values$group <- as.character(values$group)
    }

    keep_before <- length(values[[1]] %||% character())
    values <- drop_non_finite(values, layer@geom@name)
    if (!is.null(values[[1]]) && length(values[[1]]) < keep_before) {
      data <- data[seq_len(nrow(data)) %in% seq_along(values[[1]]), ,
                   drop = FALSE]
    }

    list(values = values, mapping = mapping, layer = layer, data = data)
  })

  # -- 2. Split into facet panels. -------------------------------------------
  # Each panel gets its own copy of every layer's evaluated values, subset to
  # that panel's rows. Layers whose data lacks the faceting variable (e.g. a
  # reference line) are repeated unchanged in every panel.
  facet <- plot@facet
  if (is.null(facet)) {
    panel_specs <- list(list(key = NULL, layers = layer_values))
    grid_shape <- list(ncol = 1, nrow = 1)
  } else {
    base_data <- plot@data
    if (is.null(base_data)) {
      stop("Faceting requires plot-level data passed to ggplot3().")
    }
    splits <- facet_split(facet, base_data)
    grid_shape <- facet_layout(facet, length(splits), base_data)
    panel_specs <- lapply(splits, function(sp) {
      sub_layers <- lapply(layer_values, function(lv) {
        if (!all(facet@vars %in% names(lv$data))) {
          return(lv)
        }
        keep <- if (identical(lv$data, base_data)) {
          sp$rows
        } else {
          facet_rows_for_key(facet, lv$data, sp$key)
        }
        lv$values <- lapply(lv$values, function(v) v[keep])
        lv$data <- lv$data[keep, , drop = FALSE]
        lv
      })
      list(key = sp$key, layers = sub_layers)
    })
  }

  # -- 3. Resolve scale types (shared across panels). ------------------------
  all_layers <- unlist(lapply(panel_specs, function(ps) ps$layers), recursive = FALSE)
  # A panel-spanning layout (treemap, network, sankey, chord, upset, funnel)
  # has already positioned everything in [0, 1]; it needs plain unexpanded
  # continuous axes, never a discrete axis trained on its raw node names.
  spanning <- has_panel_span(plot)
  scale_types <- list()
  for (aes_name in c("x", "y")) {
    if (spanning) {
      scale_types[[aes_name]] <- ScaleContinuous(
        aesthetic = aes_name, limits = c(0, 1), name = "", expand = 0
      )
      next
    }
    scale_types[[aes_name]] <- plot@scales[[aes_name]] %||% {
      is_discrete <- any(vapply(all_layers, function(lv) {
        v <- lv$values[[aes_name]]
        !is.null(v) && (is.character(v) || is.factor(v) || is.logical(v))
      }, logical(1)))
      if (is_discrete) ScaleDiscrete(aesthetic = aes_name) else {
        ScaleContinuous(aesthetic = aes_name)
      }
    }
    # Discrete levels are always pooled across panels: a category axis must
    # mean the same thing in every panel, even under free scales.
    if (S7_inherits(scale_types[[aes_name]], ScaleDiscrete)) {
      pooled <- do.call(c, lapply(all_layers, function(lv) {
        v <- lv$values[[aes_name]]
        if (is.null(v) || is.numeric(v)) NULL else v
      }))
      if (is.null(pooled)) {
        stop("No categorical data found for the discrete `", aes_name, "` scale.")
      }
      scale_types[[aes_name]] <- scale_train(scale_types[[aes_name]], pooled)
    }
  }

  # -- 4. Discretize + stat + position, per panel. ---------------------------
  panel_specs <- lapply(panel_specs, function(ps) {
    ps$layers <- lapply(ps$layers, function(lv) {
      values <- lv$values
      # Tooltip source values, captured before categories become integers so
      # tooltips read "class: suv", not "class: 6".
      lv$tipsrc <- list(x = values$x, y = values$y)
      for (aes_name in c("x", "y")) {
        sc <- scale_types[[aes_name]]
        if (S7_inherits(sc, ScaleDiscrete) && !is.null(values[[aes_name]]) &&
            !is.numeric(values[[aes_name]])) {
          values[[aes_name]] <- scale_discrete_index(sc, values[[aes_name]])
        }
      }
      # A few layouts (sankey, chord, network, upset, consort) build their
      # marks inside the stat, because their output is not one-row-per-
      # observation. Those still have to honour the layer's styling
      # arguments, so the merged params travel with the values.
      values$params <- utils::modifyList(
        lv$layer@geom@default_params, lv$layer@params
      )
      values <- compute_stat(lv$layer@stat, values)
      pos <- lv$layer@params$position
      if (!is.null(pos)) values <- position_apply(pos, values)
      lv$values <- values
      lv
    })
    ps
  })

  names(panel_specs) <- vapply(
    panel_specs, function(ps) ps$key %||% "panel", character(1)
  )
  attr(panel_specs, "grid_shape") <- grid_shape
  attr(panel_specs, "scale_types") <- scale_types
  panel_specs
}

# Stages 5-8: axis layout, chrome, device layout, and mark building.
build_panels <- function(plot, panel_specs, panel_scales, grid_shape,
                         all_layers, th, palette, facet, free_x, free_y) {
  # -- 6. Legend and title block (shared chrome). ----------------------------
  labels <- plot@labels
  legend <- build_legend(all_layers, plot, palette)
  if (identical(th@legend_position, "none")) legend <- NULL
  title_block <- list(
    title = labels_get(labels, "title"),
    subtitle = labels_get(labels, "subtitle"),
    caption = labels_get(labels, "caption"),
    tag = labels_get(labels, "tag")
  )

  # -- 7. Device layout: margins grow for chrome, panels tile the rest. ------
  dev <- GGPLOT3_DEVICE
  if (!is.null(plot@size)) {
    dev$width <- plot@size[[1]]
    dev$height <- plot@size[[2]]
  }
  top_extra <- 0
  if (!is.null(title_block$title)) top_extra <- top_extra + th@plot_title_size + 10
  if (!is.null(title_block$subtitle)) top_extra <- top_extra + th@plot_subtitle_size + 6
  if (!is.null(title_block$tag)) top_extra <- max(top_extra, th@plot_title_size + 6)
  dev$margin$top <- dev$margin$top + top_extra
  if (!is.null(title_block$caption)) {
    dev$margin$bottom <- dev$margin$bottom + th@caption_size + 8
  }
  legend_side <- if (is.null(legend)) "none" else th@legend_position
  if (legend_side == "right") {
    label_chars <- max(nchar(c(unlist(legend$labels), legend$title %||% "")))
    dev$margin$right <- 40 + round(label_chars * 6.5)
  } else if (legend_side == "bottom") {
    dev$margin$bottom <- dev$margin$bottom + 34
  }
  strip_h <- if (is.null(facet)) 0 else th@strip_font_size + 12

  plot_area <- list(
    x = dev$margin$left,
    y = dev$margin$top,
    w = dev$width - dev$margin$left - dev$margin$right,
    h = dev$height - dev$margin$top - dev$margin$bottom
  )
  n_panels <- length(panel_specs)
  gap <- if (n_panels > 1) 26 else 0
  cell_w <- (plot_area$w - gap * (grid_shape$ncol - 1)) / grid_shape$ncol
  cell_h <- (plot_area$h - gap * (grid_shape$nrow - 1)) / grid_shape$nrow

  # -- 8. Build each panel: axes, marks, tooltips. ---------------------------
  panels <- lapply(seq_along(panel_specs), function(i) {
    ps <- panel_specs[[i]]
    scales <- panel_scales[[i]]
    row <- (i - 1) %/% grid_shape$ncol
    col <- (i - 1) %% grid_shape$ncol
    rect <- list(
      x = plot_area$x + col * (cell_w + gap),
      y = plot_area$y + row * (cell_h + gap) + strip_h,
      w = cell_w,
      h = cell_h - strip_h
    )

    axes <- build_axes(scales, ps$layers, plot, labels)
    # Inner panels of a fixed-scale facet drop redundant tick labels.
    show_x_labels <- free_x || is.null(facet) ||
      row == grid_shape$nrow - 1 || i > n_panels - grid_shape$ncol
    show_y_labels <- free_y || is.null(facet) || col == 0

    layers <- lapply(ps$layers, function(lv) {
      build_layer_marks(lv, scales, axes, plot, palette, th)
    })

    # coord_flip() swaps the mark coordinates (in flip_positions()), so the
    # axes have to swap with them or the plot is mislabelled: the category
    # axis must end up wherever the categories were drawn. Marks are built
    # against the *unflipped* axes above, because that is the space the
    # scales were trained in — only the rendered guides swap.
    out_axes <- axes
    if (S7_inherits(plot@coord, CoordCartesian) && plot@coord@flip) {
      out_axes <- list(x = axes$y, y = axes$x)
      swap <- show_x_labels
      show_x_labels <- show_y_labels
      show_y_labels <- swap
    }

    list(
      rect = rect,
      x = out_axes$x,
      y = out_axes$y,
      layers = layers,
      strip = ps$key,
      show_x_labels = show_x_labels,
      show_y_labels = show_y_labels,
      polar = polar_guides(plot@coord, axes)
    )
  })

  inter <- plot@interaction
  interaction <- list(
    tooltip = if (is.null(inter)) TRUE else !isFALSE(inter@tooltip),
    zoom = if (is.null(inter)) TRUE else inter@zoom,
    brush = if (is.null(inter)) TRUE else inter@brush
  )

  list(
    width = dev$width,
    height = dev$height,
    panels = panels,
    labels = title_block,
    legend = legend,
    legend_position = legend_side,
    interaction = interaction,
    theme = theme_to_list(th)
  )
}

# --- Build helpers -----------------------------------------------------------

# Which rows of a non-primary layer's data belong to a given panel key.
facet_rows_for_key <- function(facet, data, key) {
  keys <- do.call(paste, c(lapply(facet@vars, function(v) {
    as.character(data[[v]])
  }), sep = " | "))
  which(keys == key)
}

facet_is_free <- function(facet, axis) {
  if (is.null(facet)) {
    return(FALSE)
  }
  facet@scales %in% c("free", paste0("free_", axis))
}

# Train continuous scales over every positional column of the given layers.
train_scales <- function(scale_types, layers) {
  out <- scale_types
  for (aes_name in c("x", "y")) {
    sc <- scale_types[[aes_name]]
    if (!S7_inherits(sc, ScaleContinuous)) next
    cols <- if (aes_name == "x") X_POS_COLS else Y_POS_COLS
    trainable <- Filter(
      function(lv) !(lv$layer@geom@name %in% NON_TRAINING_GEOMS), layers
    )
    # If every layer is a reference line there is nothing else to train on,
    # so fall back to all of them rather than failing.
    if (length(trainable) == 0) trainable <- layers
    pooled <- unlist(lapply(trainable, function(lv) {
      unlist(lv$values[intersect(cols, names(lv$values))])
    }))
    if (is.null(pooled) || length(pooled) == 0) {
      stop("No data found to train the `", aes_name, "` scale.")
    }
    out[[aes_name]] <- scale_train(sc, as.numeric(pooled))
  }
  out
}

# Expanded domain, tick positions, tick labels, and title for both axes.
build_axes <- function(scales, layers, plot, labels) {
  # Which axis (if any) is the polar angular axis — it wraps rather than pads.
  theta_axis <- S7_inherits(plot@coord, CoordPolar)
  theta_which <- if (theta_axis) plot@coord@theta else ""
  lapply(c(x = "x", y = "y"), function(aes_name) {
    scale <- scales[[aes_name]]
    if (S7_inherits(scale, ScaleDiscrete)) {
      n_lev <- length(scale@levels)
      # On a polar angular axis the categories must wrap evenly around the
      # full turn: slot 1 sits at angle 0 and slot n+1 would land back on
      # it, so the domain runs [1, n+1] with no expansion. A cartesian
      # category axis instead pads half a slot on each side.
      expanded <- if (isTRUE(theta_axis) && aes_name == theta_which) {
        c(1, n_lev + 1)
      } else {
        c(1 - 0.6, n_lev + 0.6)
      }
      tick_values <- seq_len(n_lev)
      tick_labels <- scale@levels
    } else {
      expanded <- expand_domain(scale@domain, mult = scale@expand)
      tick_values <- scale_breaks(scale, n = 5)
      tick_values <- tick_values[tick_values >= expanded[1] &
                                   tick_values <= expanded[2]]
      # Ticks live in transformed space; labels show original data units.
      data_values <- trans_invert(scale@trans, tick_values)
      tick_labels <- if (is.function(scale@labels)) {
        as.character(scale@labels(data_values))
      } else if (!is.null(scale@labels)) {
        if (length(scale@labels) != length(tick_values)) {
          stop(
            "`labels` has ", length(scale@labels), " value",
            if (length(scale@labels) == 1) "" else "s",
            " but there ", if (length(tick_values) == 1) "is " else "are ",
            length(tick_values), " break",
            if (length(tick_values) == 1) "" else "s",
            " on the ", aes_name, " axis. Supply one label per break, or a ",
            "function of the break values.",
            call. = FALSE
          )
        }
        as.character(scale@labels)
      } else {
        format_ticks(data_values)
      }
    }
    # Axis title: labs() wins, then the scale's name, then the mapping.
    title <- labels_get(labels, aes_name) %||% scale@name %||%
      axis_title_default(layers, aes_name)
    # Minor breaks sit midway between majors. They are computed here (not
    # in the renderers) so both targets draw the same guides.
    minor <- if (length(tick_values) > 1) {
      mids <- (utils::head(tick_values, -1) + utils::tail(tick_values, -1)) / 2
      mids <- mids[mids >= expanded[1] & mids <= expanded[2]]
      as.list(normalize(mids, expanded))
    } else {
      list()
    }

    list(
      domain = expanded,
      ticks = list(
        values = tick_values,
        norm = normalize(tick_values, expanded),
        labels = as.list(as.character(tick_labels)),
        minor = minor
      ),
      title = title
    )
  })
}

# Polar plots need circular gridlines instead of straight ones; precompute
# the guide radii/angles here so renderers stay presentation-only.
polar_guides <- function(coord, axes) {
  if (!S7_inherits(coord, CoordPolar)) {
    return(NULL)
  }
  theta_axis <- if (coord@theta == "x") axes$x else axes$y
  r_axis <- if (coord@theta == "x") axes$y else axes$x
  list(
    theta = coord@theta,
    start = coord@start,
    direction = coord@direction,
    inner = coord@inner,
    spokes = as.list(theta_axis$ticks$norm),
    spoke_labels = theta_axis$ticks$labels,
    rings = as.list(r_axis$ticks$norm),
    ring_labels = r_axis$ticks$labels
  )
}

# The discrete or continuous color key for the legend, if any.
build_legend <- function(layers, plot, palette) {
  cs <- plot@color_scale
  for (lv in layers) {
    v <- lv$values$color
    if (is.null(v)) next
    title <- labels_get(plot@labels, "color") %||%
      (if (!is.null(cs)) cs@name else NULL) %||%
      deparse(lv$mapping$exprs$color)
    if (is.numeric(v)) {
      grad <- gradient_endpoints(plot, NULL)
      rng <- range(v, na.rm = TRUE)
      return(list(
        type = "gradient",
        title = title,
        low = grad[1], high = grad[2],
        labels = as.list(format_ticks(seq(rng[1], rng[2], length.out = 5)))
      ))
    }
    if (!is_color_constant(v)) {
      key <- map_color_discrete(v, palette)
      return(list(
        type = "discrete",
        title = title,
        labels = as.list(key$levels),
        colors = as.list(key$palette[seq_along(key$levels)])
      ))
    }
  }
  NULL
}

# Discrete palette precedence: scale_color_manual() > theme > package default.
resolve_palette <- function(plot, th) {
  cs <- plot@color_scale
  if (!is.null(cs) && cs@type == "discrete") {
    return(cs@palette)
  }
  if (length(th@point_palette) > 0) {
    return(col_to_hex(th@point_palette))
  }
  GGPLOT3_DISCRETE_PALETTE
}

# Continuous gradient precedence: scale_color_gradient() > theme > default.
gradient_endpoints <- function(plot, th) {
  cs <- plot@color_scale
  if (!is.null(cs) && cs@type == "continuous") {
    return(cs@palette)
  }
  th <- th %||% plot@theme %||% theme_ggplot3()
  # Either endpoint may be set on its own; the other keeps its default.
  low <- if (nzchar(th@gradient_low)) {
    col_to_hex(th@gradient_low)
  } else {
    GGPLOT3_GRADIENT_LOW
  }
  high <- if (nzchar(th@gradient_high)) {
    col_to_hex(th@gradient_high)
  } else {
    GGPLOT3_GRADIENT_HIGH
  }
  c(low, high)
}

labels_get <- function(labels, field) {
  if (is.null(labels)) NULL else prop(labels, field)
}

# Scale one layer's positions, resolve its visual aesthetics, and hand the
# result to the geom's build_marks() method.
build_layer_marks <- function(lv, scales, axes, plot, palette, th) {
  values <- lv$values
  layer <- lv$layer
  defaults <- layer@geom@default_params
  pos_cols <- intersect(names(values), c(X_POS_COLS, Y_POS_COLS))
  n <- length(values[[pos_cols[1]]])

  scaled <- values
  for (col in intersect(X_POS_COLS, names(values))) {
    scaled[[col]] <- scale_map(scales$x, values[[col]], expand = axes$x$domain)
  }
  for (col in intersect(Y_POS_COLS, names(values))) {
    scaled[[col]] <- scale_map(scales$y, values[[col]], expand = axes$y$domain)
  }
  # coord_flip() swaps the axes once, here — geoms and renderers never see it.
  if (S7_inherits(plot@coord, CoordCartesian) && plot@coord@flip) {
    scaled <- flip_positions(scaled)
  }
  if (!is.null(scaled$x) && !is.null(scaled$y)) {
    tp <- coord_transform(plot@coord, scaled$x, scaled$y)
    scaled$x <- tp$x
    scaled$y <- tp$y
  }

  scaled$color <- if (!is.null(values$color)) {
    v <- values$color
    if (is.numeric(v)) {
      grad <- gradient_endpoints(plot, th)
      map_color_continuous(v, grad[1], grad[2])
    } else if (is_color_constant(v)) {
      # aes(color = "blue"): a constant naming a real color is honored
      # literally instead of being treated as a one-level category.
      rep(col_to_hex(v[[1]]), n)
    } else {
      map_color_discrete(v, palette)$colors
    }
  } else {
    rep(col_to_hex(layer@params$color %||% defaults$color %||% "#000000"), n)
  }
  scaled$size <- if (!is.null(values$size) && is.numeric(values$size)) {
    map_size(values$size)
  } else {
    rep(layer@params$size %||% defaults$size %||% 3, n)
  }
  scaled$alpha <- rep(layer@params$alpha %||% defaults$alpha %||% 1, n)
  scaled$params <- utils::modifyList(defaults, layer@params)
  scaled$xspan <- axes$x$domain[2] - axes$x$domain[1]
  scaled$yspan <- axes$y$domain[2] - axes$y$domain[1]
  # Full axis domains in data units, for geoms that must span the panel
  # (geom_abline() evaluates its line at the panel's own x limits).
  scaled$xdomain <- axes$x$domain
  scaled$ydomain <- axes$y$domain
  scaled$theme <- th

  list(
    geom = layer@geom@name,
    marks = build_marks(layer@geom, scaled),
    tips = as.list(build_tips(lv, axes, plot, n))
  )
}

# Swap the x and y families of positional columns for coord_flip().
flip_positions <- function(scaled) {
  swap <- list(
    x = "y", y = "x", xend = "yend", yend = "xend",
    xmin = "ymin", xmax = "ymax", ymin = "xmin", ymax = "xmax"
  )
  out <- scaled
  for (from in names(swap)) {
    to <- swap[[from]]
    if (!is.null(scaled[[from]])) out[[to]] <- scaled[[from]]
  }
  out
}

# Tooltip strings, formatted in R so the JS side stays presentation-only.
build_tips <- function(lv, axes, plot, n) {
  inter <- plot@interaction
  if (!is.null(inter) && is.character(inter@tooltip)) {
    missing_cols <- setdiff(inter@tooltip, names(lv$data))
    if (length(missing_cols) > 0) {
      stop(
        "interact(tooltip = ) column(s) not found in the layer data: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    if (nrow(lv$data) == n) {
      lines <- lapply(inter@tooltip, function(cn) {
        sprintf("%s: %s", cn, format_tip(lv$data[[cn]]))
      })
      return(do.call(paste, c(lines, sep = "\n")))
    }
  }
  if (!is.null(lv$tipsrc$x) && !is.null(lv$tipsrc$y) &&
      length(lv$tipsrc$x) == n) {
    return(sprintf(
      "%s: %s\n%s: %s",
      axes$x$title, format_tip(lv$tipsrc$x),
      axes$y$title, format_tip(lv$tipsrc$y)
    ))
  }
  rep("", n)
}

# Format a value for a tooltip: element-wise, so each row shows its own
# value rather than a column-width-padded one (format() pads to a common
# width across the whole vector, which looks wrong in a per-point tooltip).
format_tip <- function(v) {
  vapply(v, function(x) format(x, trim = TRUE, digits = 6), character(1))
}

# Default axis title: deparse the first mapped expression for the aesthetic;
# stat-computed columns (histogram counts, densities, survival) get named by
# the stat through its `computed_labels` attribute.
axis_title_default <- function(layers, aes_name) {
  for (lv in layers) {
    e <- lv$mapping$exprs[[aes_name]]
    if (!is.null(e)) {
      return(deparse(e))
    }
  }
  for (lv in layers) {
    lab <- attr(lv$values, "computed_labels")[[aes_name]]
    if (!is.null(lab)) {
      return(lab)
    }
  }
  aes_name
}

# Map a numeric size aesthetic onto point radii in [2, 7] px, scaling the
# *area* (radius ~ sqrt of value position) so twice the value reads as twice
# the ink, not four times.
map_size <- function(values) {
  rng <- range(values, na.rm = TRUE)
  span <- rng[2] - rng[1]
  t <- if (span == 0) rep(0.5, length(values)) else (values - rng[1]) / span
  r_min <- 2
  r_max <- 7
  sqrt(r_min^2 + t * (r_max^2 - r_min^2))
}

# --- Position adjustments ----------------------------------------------------
#
# Applied in data space, after the stat and before scale training, so the
# axis domain covers e.g. stacked bar totals. Bar-like stats emit
# x / y / xwidth / group; adjustments convert those into the xmin / xmax /
# ymin / ymax corners the rect-based geoms draw.

position_apply <- function(position, values) {
  switch(position,
    identity = position_identity(values),
    stack = position_stack(values),
    dodge = position_dodge(values),
    fill = position_fill(values),
    stop("Unknown position adjustment: ", position)
  )
}

# Bars from a common baseline at 0, groups overplotted where they collide.
position_identity <- function(values) {
  values$ymin <- rep(0, length(values$y))
  values$ymax <- values$y
  bar_corners(values)
}

# Groups stack on top of one another at each x.
position_stack <- function(values) {
  ymin <- numeric(length(values$y))
  ymax <- numeric(length(values$y))
  for (xv in unique(values$x)) {
    idx <- which(values$x == xv)
    tops <- cumsum(values$y[idx])
    ymax[idx] <- tops
    ymin[idx] <- tops - values$y[idx]
  }
  values$ymin <- ymin
  values$ymax <- ymax
  bar_corners(values)
}

# Like stack, but every x column is rescaled to sum to 1 (proportions).
position_fill <- function(values) {
  out <- position_stack(values)
  for (xv in unique(values$x)) {
    idx <- which(values$x == xv)
    total <- max(out$ymax[idx])
    if (total > 0) {
      out$ymax[idx] <- out$ymax[idx] / total
      out$ymin[idx] <- out$ymin[idx] / total
    }
  }
  out
}

# Groups sit side by side within each x slot.
position_dodge <- function(values) {
  groups <- unique(values$group)
  n_g <- length(groups)
  gi <- match(values$group, groups)
  sub_w <- values$xwidth / n_g
  values$x <- values$x + (gi - (n_g + 1) / 2) * sub_w
  values$xwidth <- sub_w
  values$ymin <- rep(0, length(values$y))
  values$ymax <- values$y
  bar_corners(values)
}

# Convert center-x + width into explicit rect corners and drop the width, so
# scale training sees the true horizontal extent of every bar.
bar_corners <- function(values) {
  values$xmin <- values$x - values$xwidth / 2
  values$xmax <- values$x + values$xwidth / 2
  values$xwidth <- NULL
  values
}

# Tiny null-coalescing helper (base R has %||% only from 4.4.0).
`%||%` <- function(a, b) if (is.null(a)) b else a
