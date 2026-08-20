# ggnext 0.1.0

The first feature-complete release of the core grammar. Everything is built
on the same computed-geometry buffer, so static and interactive output can
never disagree.

## Additional core geoms

Nineteen further layers, closing most of the gap with the geoms an R user
expects to find: `geom_rect()`, `geom_polygon()`, `geom_raster()`,
`geom_abline()`, `geom_blank()`, `geom_label()`, `geom_linerange()`,
`geom_crossbar()`, `geom_errorbarh()`, `geom_rug()`, `geom_curve()`,
`geom_spoke()`, `geom_count()`, `geom_freqpoly()`, `geom_function()`,
`geom_qq()`, `geom_qq_line()`, `geom_dotplot()` and `geom_quantile()`.

Quantile regression is fitted by minimising the check loss directly, so no
linear-programming dependency is needed. 2D contouring/binning and spatial
layers remain unimplemented.

## Correctness fixes

* `coord_flip()` swapped the marks but not the axes, so every flipped plot
  was mislabelled.
* `geom_sankey()`, `geom_chord()`, `geom_network()`, `geom_upset()` and
  `geom_consort()` ignored all of their styling arguments, because those
  geoms build their marks inside the stat, which never saw the layer's
  parameters.
* Non-finite values (`NA`, `NaN`, `Inf`) reached the renderer and produced
  invalid SVG such as `cx="NA"`. They are now dropped with a warning
  reporting the row count, matching what the histogram path already did.
  Interval columns keep their `NA`s, which `geom_forecast_band()` relies on.
* `geom_smooth()` with too few observations wrote `NaN` coordinates; it now
  refuses with a message naming the minimum for the chosen method.
* Density-based geoms surfaced raw `stats::density()` errors; they now name
  the geom and the offending group.
* SVG attribute values were not escaped, so a quote in a theme font or
  colour produced an unparseable document.
* `stat_bin(bins = n)` dropped empty bins, so `n` was only honoured when
  every bin was occupied.
* A stat may now supply an aesthetic the geom requires, so
  `geom_point(stat = stat_bin())` works.
* `geom_spider_response(thresholds =)`, `geom_smooth(alpha =)` and
  `theme(grid_color_minor =)` had no effect; gradient endpoints only
  applied when both were set.
* Invalid enum values were accepted silently, and a `labels` vector of the
  wrong length was recycled instead of reported.
* `to_json()` emitted a bare `Inf`, which is not valid JSON, and left C0
  control characters unescaped.

## Performance

* Building the reference page reparsed the entire Rd database once per
  documented function, making the site build quadratic in the size of the
  manual. The database is now parsed once and cached, taking a full site
  build from 3.6s to 0.9s.
* `build_site(gallery = FALSE)` rewrites the pages without redrawing the
  figures, for quick iteration on prose. Figures an earlier build wrote are
  kept rather than dropped.

## Grammar

* `facet_wrap()` and `facet_grid()`, with `scales = "fixed"` (default),
  `"free"`, `"free_x"`, and `"free_y"`. The geometry buffer now carries a
  list of panels; a non-faceted plot is the one-panel case.
* `coord_polar()` — the engine behind radar charts, circular bar charts,
  and pie/donut wedges — and `coord_flip()` for horizontal layouts.
* `animate()` is implemented: it builds one geometry buffer per level of
  the transition variable, with axes fixed across frames, and the
  interactive target plays them with a scrubber and play/pause.
  (It previously warned and did nothing.)
* `interact()` gains `brush = TRUE` (drag a rectangle to zoom) and accepts
  a character vector of data columns for `tooltip`.

## Pipe sugar

* Every `geom_*()`, `theme_*()`, `scale_*()`, `coord_*()`, `facet_*()`,
  `labs()`, `interact()`, `animate()` and `plot_size()` that takes at least
  one argument now also accepts a plot as its first pipe argument:
  `p |> geom_point() |> theme_minimal()` runs the same as
  `p + geom_point() + theme_minimal()`. `+` remains the grammar's one true
  composition operator — reusable `Theme`/`Scale` bundles built independently
  of any plot, and `Reduce(`+`, layers, p)`, both still work exactly as
  before; the pipe form is an additive call-site convenience, not a
  replacement. Non-standard evaluation is preserved, so
  `p |> facet_wrap(cyl)` resolves `cyl` against the data exactly as
  `p + facet_wrap(cyl)` always has. A handful of genuinely zero-argument
  constructors (`theme_minimal()`, `theme_classic()`, `theme_modern()`,
  `theme_dark()`, `theme_void()`, `theme_ggnext()`, `coord_flip()`) have no
  parameter to route a piped plot through and stay `+`-only.

## Plot validation

* `validate_plot()` checks a plot for common statistical-graphics mistakes —
  an empty plot, a continuous geom (`geom_point()`, `geom_line()`, ...)
  mapped to a categorical `y`, a mapped aesthetic that is mostly missing, a
  discrete color/fill scale with more levels than a legend can usefully
  show, and a `sqrt`-transformed scale fed negative values — and returns
  the findings.
* `plot_check()` prints that report and returns the plot unchanged, so it
  sits inside a pipeline: `p |> plot_check() |> render()`.

## Titles, axes, and scales

* `labs()`, `ggtitle()`, `xlab()`, `ylab()` for the title block (title,
  subtitle, caption, tag) and axis titles.
* `xlim()`, `ylim()`, `lims()` for quick limit setting.
* `scale_x_continuous()` / `scale_y_continuous()` gain `breaks`, `labels`
  (a vector or a function), `expand`, and `trans`.
* Axis transforms: `scale_x_log10()`, `scale_x_sqrt()`, `scale_x_reverse()`
  and the `y` equivalents.
* `scale_x_discrete()` / `scale_y_discrete()` for explicit level ordering.
* `scale_color_manual()` and `scale_color_gradient()` for palette control.
* `plot_size()` and `ggnext(width =, height =)` for output dimensions.

## Themes

* Six presets: `theme_ggnext()`, `theme_minimal()`, `theme_classic()`,
  `theme_modern()`, `theme_dark()`, `theme_void()`.
* `theme()` now takes a `base` argument, so any preset can be restyled,
  and exposes 35 settings covering colors, typography, grid and axis
  visibility, legend position, and palettes.
* Discrete color legends and continuous gradient legends, positionable
  right or bottom.

## Data export

* `plot_data()` returns exactly the values a plot draws — post-stat,
  post-position, post-facet, in data units, with internal scratch columns
  and constant grouping columns removed.
* `write_plot_data()` writes that table to CSV.

## New geoms

* **Layout**: `geom_radar()`, `geom_ridgeline()`, `geom_sankey()` /
  `geom_alluvial()`, `geom_treemap()`, `geom_network()`, `geom_chord()`,
  `geom_parallel()`, `geom_bump()`, `geom_funnel()`, `geom_stream()`,
  `geom_upset()`.
* **Machine learning**: `geom_shap()`, `geom_partial_dependence()`,
  `geom_confusion_matrix()`, `geom_calibration()`, `geom_lift_gain()`,
  `geom_residual()`, `geom_learning_curve()`, `geom_silhouette()`,
  `geom_embedding()`, `geom_decision_boundary()`, `geom_forecast_band()`.
* **Clinical / biostatistics**: `geom_forest()`, `geom_swimmer()`,
  `geom_spider_response()`, `geom_waterfall_response()`,
  `geom_bland_altman()`, `geom_cuminc()`, `geom_spaghetti()`,
  `geom_ae_heatmap()`, `geom_dose_response()`, `geom_shift()`,
  `geom_consort()`.

Layout algorithms are implemented from scratch: squarified treemaps
(Bruls, Huizing & van Wijk), Fruchterman-Reingold force layout, Sankey
node stacking with a single shared value scale, Aalen-Johansen cumulative
incidence, and four-parameter log-logistic dose-response fitting.

## Rendering

* Five mark primitives (`circle`, `line`, `rect`, `polygon`, `text`) are
  now supported by both render targets; a new geom needs no renderer
  changes.
* The canvas target zooms per panel, so faceted plots stay independently
  explorable.

## Documentation

* `build_site()` generates the documentation site, rendering every gallery
  figure with ggnext itself so the gallery cannot drift from the code.
* `ggnext_logo()` draws the hex sticker with the package's own SVG writer.

## Other fixes

* `aes(color = "blue")` is honored as a literal color rather than being
  treated as a single-level category and assigned the first palette entry.
* A mapped aesthetic that resolves to a function instead of a data column
  (e.g. `aes(color = class)` when `class` is not a column) now raises an
  error naming the problem and listing the available columns, instead of
  failing later with "attempt to replicate an object of type 'builtin'".
* Plain S3 `print` methods are re-registered in `.onLoad()`; S7's own
  method registration was dropping them, so `render()` printed the full
  document source instead of a summary.

# ggnext 0.0.0.9000

Initial de-risking spike: S7 class hierarchy, `geom_point()`, one
continuous linear scale, Cartesian coordinates, `stat_identity()`, the
hand-rolled SVG writer, and the interactive canvas target.
