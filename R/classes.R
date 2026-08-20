# S7 class hierarchy ----------------------------------------------------------
#
# A fresh Grammar-of-Graphics object model. This is *not* ggproto and not a
# port of ggplot2's classes: each grammar concept is an ordinary S7 class,
# behaviour lives in S7 generics, and extension means "add a subclass plus a
# method", never "copy and mutate a prototype object".
#
#   GgnextPlot  = data + mapping + [Layer] + [Scale] + Coord
#   Layer        = data override + mapping override + Geom + Stat + params
#   Geom         = how computed values become drawing primitives ("marks")
#   Stat         = how raw layer data becomes computed values
#   Scale        = domain training + domain -> [0, 1] mapping + tick breaks
#   Coord        = how normalized (x, y) positions map onto the panel
#
# Extension recipe: subclass Geom (or Stat) and implement build_marks()
# (or compute_stat()); nothing in the renderers needs to change.

# --- Stat --------------------------------------------------------------------

#' Stat: statistical transformation applied to layer data
#'
#' Base class for statistical transformations. A stat receives the evaluated
#' aesthetic values and returns (possibly new) values for the geom to draw.
#'
#' @param name Human-readable stat name.
#' @param provides Aesthetics this stat computes itself. A geom's required
#'   aesthetics are validated against the user's mapping *plus* these, so
#'   e.g. `geom_point(stat = stat_bin())` is accepted even though the user
#'   maps only `x` - the stat supplies `y`.
#' @param discrete_provides Positional aesthetics (`"x"` and/or `"y"`) that
#'   this stat always computes as *categorical* values, even when nothing
#'   maps that aesthetic before the stat runs (e.g. a Cox-model geom whose
#'   `y` is a set of treatment-arm labels the stat itself derives). The
#'   build's scale-type detection normally infers continuous vs. discrete
#'   from the raw, pre-stat mapping - the one signal it cannot see for an
#'   aesthetic that is *entirely* stat-computed; listing it here is that
#'   missing signal. When set, the discrete scale's levels are trained
#'   from the layer's `group` aesthetic (present pre-stat for every layer)
#'   rather than from the (absent) raw `x`/`y`.
#' @return An S7 object of class `Stat`, the base class for statistical transformations.
#' @export
Stat <- new_class("Stat", properties = list(
  name = class_character,
  provides = new_property(class_character, default = character()),
  discrete_provides = new_property(class_character, default = character())
))

#' StatIdentity: pass data through unchanged
#'
#' @noRd
StatIdentity <- new_class("StatIdentity", parent = Stat,
  constructor = function() new_object(Stat(name = "identity"))
)

#' Apply a stat's transformation to evaluated aesthetic values
#'
#' Methods receive `values`, a named list of evaluated aesthetic vectors,
#' and return a named list of (possibly transformed) aesthetic vectors.
#'
#' @param stat A [Stat] subclass instance.
#' @param ... Method arguments; all methods take `values` (named list of
#'   evaluated aesthetic vectors).
#' @return Named list of (possibly transformed) aesthetic vectors.
#' @export
compute_stat <- new_generic("compute_stat", dispatch_args = "stat")

method(compute_stat, StatIdentity) <- function(stat, values) {
  values
}

# --- Geom --------------------------------------------------------------------

#' Geom: geometric representation of computed values
#'
#' Base class for geoms. A geom turns scaled (normalized-to-\[0, 1\])
#' aesthetic values into a list of render-target-agnostic drawing primitives
#' ("marks"); see [build_marks()].
#'
#' @param name Human-readable geom name.
#' @param default_params Named list of default visual parameters, used when
#'   neither an aesthetic mapping nor a literal layer parameter supplies a
#'   value.
#' @param required_aes Aesthetics that must be present (post-stat) for this
#'   geom to draw; defaults to `x` and `y`.
#' @return An S7 object of class `Geom`, the base class for geometric layers.
#' @export
Geom <- new_class("Geom", properties = list(
  name = class_character,
  default_params = class_list,
  required_aes = new_property(class_character, default = c("x", "y"))
))

#' GeomPoint: one mark (a circle) per observation
#'
#' @noRd
GeomPoint <- new_class("GeomPoint", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "point",
      default_params = list(color = "#000000", size = 3, alpha = 1)
    ))
  }
)

#' Build drawing primitives ("marks") from scaled aesthetic values
#'
#' The single seam between the grammar and the renderers. Each geom method
#' returns a list of marks; a mark is a plain named list with a `type` field
#' (currently `"circle"`; the full catalog will add `"rect"`, `"line"`,
#' `"path"`, `"polygon"`, `"text"`) plus type-specific fields in *normalized
#' panel coordinates* (x/y in \[0, 1\], y pointing up). Because both render
#' targets consume marks, adding a geom never touches renderer code, and
#' adding a renderer never touches geom code.
#'
#' @param geom A [Geom] subclass instance.
#' @param ... Method arguments; all methods take `scaled`, a named list of
#'   scaled aesthetic vectors (`x`, `y` in \[0, 1\]; `color` as hex;
#'   `size` as radius in px; `alpha` in \[0, 1\]).
#' @return List of marks.
#' @export
build_marks <- new_generic("build_marks", dispatch_args = "geom")

method(build_marks, GeomPoint) <- function(geom, scaled) {
  lapply(seq_along(scaled$x), function(i) {
    list(
      type = "circle",
      x = scaled$x[[i]],
      y = scaled$y[[i]],
      r = scaled$size[[i]],
      fill = scaled$color[[i]],
      alpha = scaled$alpha[[i]]
    )
  })
}

# --- Scale -------------------------------------------------------------------

#' Scale: map data values onto a normalized visual range
#'
#' Base class for scales. A scale owns one positional aesthetic, learns its
#' domain from data ("training"), and maps data values onto \[0, 1\].
#'
#' @param aesthetic Which aesthetic this scale governs (`"x"` or `"y"`).
#' @param name Axis title; `NULL` means "use the mapped expression's label".
#' @param breaks Explicit tick positions, or `NULL` for automatic breaks.
#' @param labels Explicit tick labels (same length as `breaks`), a function
#'   applied to the break values, or `NULL` for automatic formatting.
#' @return An S7 object of class `Scale`, the base class for scales.
#' @export
Scale <- new_class("Scale", properties = list(
  aesthetic = class_character,
  name = class_any,
  breaks = class_any,
  labels = class_any
))

#' ScaleContinuous: linear continuous positional scale
#'
#' @param aesthetic `"x"` or `"y"`.
#' @param limits Optional numeric length-2 vector fixing the domain; `NULL`
#'   (default) trains the domain from the data.
#' @param name Optional axis title.
#' @param breaks Explicit tick positions in data units, or `NULL` for
#'   automatic breaks.
#' @param labels Tick labels: a character vector, a function applied to the
#'   break values, or `NULL` for automatic formatting.
#' @param trans Axis transform: `"identity"`, `"log10"`, `"sqrt"`, or
#'   `"reverse"`.
#' @param expand Fraction of the data span padded onto each end of the axis.
#'
#' The `domain` property (the trained data range) is filled in during the
#' plot build, not at construction.
#' @return An S7 object of class `ScaleContinuous`; usually built by [scale_x_continuous()] rather than called directly.
#' @export
ScaleContinuous <- new_class("ScaleContinuous", parent = Scale,
  properties = list(
    limits = class_any,
    domain = class_any,
    trans = class_character,
    expand = class_numeric
  ),
  constructor = function(aesthetic = "x", limits = NULL, name = NULL,
                         breaks = NULL, labels = NULL, trans = "identity",
                         expand = 0.05) {
    if (!is.null(limits) &&
        (!is.numeric(limits) || length(limits) != 2 || limits[1] >= limits[2])) {
      stop("`limits` must be NULL or an increasing numeric vector of length 2.")
    }
    if (!trans %in% c("identity", "log10", "sqrt", "reverse")) {
      stop("`trans` must be one of: identity, log10, sqrt, reverse.")
    }
    new_object(
      Scale(aesthetic = aesthetic, name = name, breaks = breaks, labels = labels),
      limits = limits, domain = NULL, trans = trans, expand = expand
    )
  }
)

#' Train a scale on data values
#'
#' Learns the scale's domain (its trained data range). User-supplied
#' `limits` win over the data range. Returns an updated scale — S7 objects
#' are values, so training is a functional update, not a mutation.
#'
#' @param scale A [Scale] subclass instance.
#' @param ... Method arguments; all methods take `values`, the raw data
#'   vector mapped to this scale's aesthetic.
#' @return The scale, with `domain` filled in.
#' @export
scale_train <- new_generic("scale_train", dispatch_args = "scale")

method(scale_train, ScaleContinuous) <- function(scale, values) {
  if (!is.numeric(values)) {
    stop(
      "Aesthetic `", scale@aesthetic, "` must be numeric for a continuous ",
      "scale; got ", class(values)[[1]], "."
    )
  }
  raw <- if (is.null(scale@limits)) values else scale@limits
  # The domain lives in *transformed* space, so all downstream math
  # (expansion, normalization, tick placement) stays plain linear algebra.
  scale@domain <- range(trans_apply(scale@trans, raw), na.rm = TRUE)
  scale
}

#' Map data values through a trained scale onto \[0, 1\]
#'
#' @param scale A trained [Scale] subclass instance.
#' @param ... Method arguments; all methods take `values` (numeric data
#'   vector) and optionally `expand`, a padded (expanded) domain to map
#'   against instead of the trained domain, as produced during the plot
#'   build.
#' @return Numeric vector of normalized positions.
#' @export
scale_map <- new_generic("scale_map", dispatch_args = "scale")

method(scale_map, ScaleContinuous) <- function(scale, values, expand = NULL) {
  domain <- if (is.null(expand)) scale@domain else expand
  if (is.null(domain)) {
    stop("Scale for `", scale@aesthetic, "` has not been trained.")
  }
  normalize(trans_apply(scale@trans, values), domain)
}

#' Compute tick breaks for a trained scale
#'
#' @param scale A trained [Scale] subclass instance.
#' @param ... Method arguments; all methods take `n`, the target tick count.
#' @return Numeric vector of tick values in data units.
#' @export
scale_breaks <- new_generic("scale_breaks", dispatch_args = "scale")

method(scale_breaks, ScaleContinuous) <- function(scale, n = 5) {
  if (is.null(scale@domain)) {
    stop("Scale for `", scale@aesthetic, "` has not been trained.")
  }
  # User-supplied breaks are given in *data* space; everything the build
  # does with breaks happens in transformed space, so convert here.
  if (!is.null(scale@breaks)) {
    return(trans_apply(scale@trans, scale@breaks))
  }
  if (scale@trans == "log10") {
    # Decade ticks read far better than linear ticks on a log axis.
    lo <- floor(scale@domain[1])
    hi <- ceiling(scale@domain[2])
    return(seq(lo, hi, by = max(1, round((hi - lo) / n))))
  }
  nice_ticks(scale@domain[1], scale@domain[2], n = n)
}

# --- Coord -------------------------------------------------------------------

#' Coord: position transformation for normalized coordinates
#'
#' Base class for coordinate systems. A coord receives positions already
#' normalized to \[0, 1\] by the scales and returns (possibly transformed)
#' normalized positions. Cartesian is the identity; polar / flipped / 3D
#' coords will override this seam without touching scales or geoms.
#'
#' @param name Human-readable coord name.
#' @return An S7 object of class `Coord`, the base class for coordinate systems.
#' @export
Coord <- new_class("Coord", properties = list(
  name = class_character
))

#' CoordCartesian: the identity coordinate system
#'
#' @param flip Swap x and y (see [coord_flip()]).
#' @return An S7 object of class `CoordCartesian`; usually built by [coord_cartesian()] or [coord_flip()].
#' @export
CoordCartesian <- new_class("CoordCartesian", parent = Coord,
  properties = list(flip = new_property(class_logical, default = FALSE)),
  constructor = function(flip = FALSE) {
    new_object(Coord(name = if (flip) "flip" else "cartesian"), flip = flip)
  }
)

#' CoordPolar: polar coordinate system
#'
#' Maps the normalized x axis onto angle and the normalized y axis onto
#' radius, so lines, areas, and points become circular. This is the engine
#' behind radar/spider charts, pie-like wedges, and circular bar charts.
#'
#' @param theta Which axis becomes the angle: `"x"` (default) or `"y"`.
#' @param start Angle in radians for normalized position 0 (default: 12
#'   o'clock).
#' @param direction `1` for clockwise, `-1` for counter-clockwise.
#' @param inner Inner radius as a fraction of the outer radius (a donut
#'   hole); `0` fills to the center.
#' @return An S7 object of class `CoordPolar`; usually built by [coord_polar()].
#' @export
CoordPolar <- new_class("CoordPolar", parent = Coord,
  properties = list(
    theta = class_character,
    start = class_numeric,
    direction = class_numeric,
    inner = class_numeric
  ),
  constructor = function(theta = "x", start = 0, direction = 1, inner = 0) {
    if (!theta %in% c("x", "y")) stop("`theta` must be \"x\" or \"y\".")
    new_object(
      Coord(name = "polar"),
      theta = theta, start = start, direction = direction, inner = inner
    )
  }
)

#' Transform normalized positions through a coordinate system
#'
#' @param coord A [Coord] subclass instance.
#' @param ... Method arguments; all methods take `x` and `y`, numeric
#'   vectors of normalized positions.
#' @return List with transformed `x` and `y`.
#' @export
coord_transform <- new_generic("coord_transform", dispatch_args = "coord")

method(coord_transform, CoordCartesian) <- function(coord, x, y) {
  # Cartesian coordinates are the identity in normalized space. The flip to
  # SVG/canvas device space (y grows downward) is a renderer concern; the
  # x/y *axis* swap of coord_flip() happens once in the build, so geoms and
  # renderers never see it.
  list(x = x, y = y)
}

method(coord_transform, CoordPolar) <- function(coord, x, y) {
  # Normalized (x, y) -> polar -> back into the normalized unit square.
  #
  #   angle  = start + direction * 2*pi * theta_position
  #   radius = inner + (1 - inner) * radial_position, halved because the
  #            unit square's center-to-edge distance is 0.5
  #
  # Points are then placed around the panel center (0.5, 0.5). Emitting
  # normalized coordinates again means every downstream stage (marks,
  # renderers) is unchanged — polar is purely a coordinate concern.
  theta_pos <- if (coord@theta == "x") x else y
  r_pos <- if (coord@theta == "x") y else x
  angle <- coord@start + coord@direction * 2 * pi * theta_pos
  radius <- (coord@inner + (1 - coord@inner) * r_pos) * 0.5
  list(
    x = 0.5 + radius * sin(angle),
    y = 0.5 + radius * cos(angle)
  )
}

# --- Layer and plot ----------------------------------------------------------

#' Layer: one geom + stat + data/mapping overrides
#'
#' @param geom A [Geom] subclass instance.
#' @param stat A [Stat] subclass instance.
#' @param mapping Layer-level aesthetic mapping (or `NULL` to inherit).
#' @param data Layer-level data (or `NULL` to inherit the plot data).
#' @param params Named list of literal visual parameters (e.g. `size = 4`).
#' @param inherit Whether the layer merges the plot-level mapping into its
#'   own (`TRUE` for data layers; reference-line layers that synthesize
#'   their own data set `FALSE`).
#' @return An S7 object of class `Layer`, as returned by the `geom_*()` constructors.
#' @export
Layer <- new_class("Layer", properties = list(
  geom = Geom,
  stat = Stat,
  mapping = class_any,
  data = class_any,
  params = class_list,
  inherit = new_property(class_logical, default = TRUE)
))

# The Interact class itself; documented with its interact() constructor
# (separate topics would collide as Interact.Rd vs interact.Rd on
# case-insensitive filesystems).

#' @rdname interact
#' @export
Interact <- new_class("Interact", properties = list(
  tooltip = class_any,
  zoom = class_logical,
  brush = class_logical
))

#' ColorScale: palette override for the color aesthetic
#'
#' Created by [scale_color_manual()] / [scale_color_gradient()].
#'
#' @param palette Character vector of hex colors (two, for a gradient).
#' @param name Optional legend title.
#' @param type `"discrete"` or `"continuous"`.
#' @return An S7 object of class `ColorScale`; usually built by [scale_color_manual()] or [scale_color_gradient()].
#' @export
ColorScale <- new_class("ColorScale", properties = list(
  palette = class_character,
  name = class_any,
  type = class_character
))

#' Facet: split a plot into panels by data values
#'
#' Base class for faceting. Created by [facet_wrap()] / [facet_grid()].
#'
#' @param vars Character vector of faceting variable names.
#' @param ncol,nrow Panel grid shape (`facet_wrap` only; `NULL` auto-sizes).
#' @param scales `"fixed"` (shared axes, the default) or `"free"`,
#'   `"free_x"`, `"free_y"` (per-panel axes).
#' @param type `"wrap"` or `"grid"`.
#' @return An S7 object of class `Facet`; usually built by [facet_wrap()] or [facet_grid()].
#' @export
Facet <- new_class("Facet", properties = list(
  vars = class_character,
  ncol = class_any,
  nrow = class_any,
  scales = class_character,
  type = class_character
))

#' Animation: a transition spec bound to a data variable
#'
#' Created by [animate()]. Frames are computed from the same geometry
#' pipeline as static output — one buffer per frame value — and the
#' interactive target plays them with a scrubber.
#'
#' @param var Name of the transition variable.
#' @param duration Milliseconds per frame.
#' @param easing Easing curve: `"linear"`, `"cubic"`, `"cubic-in-out"`.
#' @param loop Restart automatically at the end.
#' @return An S7 object of class `Animation`; usually built by [animate()].
#' @export
Animation <- new_class("Animation", properties = list(
  var = class_character,
  duration = class_numeric,
  easing = class_character,
  loop = class_logical
))

#' GgnextPlot: the complete plot specification
#'
#' Built by [ggnext()] and grown with `+`. Holds everything needed to
#' compute geometry; rendering never reaches back past the computed buffer.
#'
#' @param data Default data frame for all layers.
#' @param mapping Default aesthetic mapping from [aes()].
#' @param layers List of [Layer] objects.
#' @param scales Named list of [Scale] objects, keyed by aesthetic.
#' @param coord A [Coord] subclass instance.
#' @param interaction An [Interact] spec, or `NULL` for a static plot.
#' @param theme A `Theme` object controlling the plot chrome.
#' @param labels A `Labels` object with the title block and axis titles.
#' @param color_scale A [ColorScale] palette override, or `NULL`.
#' @param facet A [Facet] spec, or `NULL` for a single panel.
#' @param animation An [Animation] spec, or `NULL`.
#' @param size Device size in pixels, `c(width, height)`.
#' @return An S7 object of class `GgnextPlot`, as returned by [ggnext()].
#' @export
GgnextPlot <- new_class("GgnextPlot", properties = list(
  data = class_any,
  mapping = class_any,
  layers = class_list,
  scales = class_list,
  coord = Coord,
  interaction = class_any,
  theme = class_any,
  labels = class_any,
  color_scale = class_any,
  facet = class_any,
  animation = class_any,
  size = class_any
))
