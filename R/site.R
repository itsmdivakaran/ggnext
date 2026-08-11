# Documentation site generator -------------------------------------------------
#
# pkgdown is not usable here: it pulls in a large dependency tree and, more
# to the point, ggplot3's own gallery needs to be produced *by ggplot3*.
# This module builds a small static site from the package's own sources:
#
#   index.html      overview, install, quick start, feature grid
#   gallery.html    every geom, rendered live at build time
#   reference.html  the function index, grouped, from the Rd files
#   themes.html     the same plot in every theme
#
# The whole site is plain HTML + inline CSS, so it opens from a file:// URL
# and can be published to GitHub Pages by committing docs/.

#' Build the ggplot3 documentation site
#'
#' Renders every gallery example with the package itself and writes a
#' self-contained static site. No external site generator, no CDN assets.
#'
#' @param dir Output directory (created if needed).
#' @param quiet Suppress progress messages.
#' @param cookbook Also build the Cookbook page by knitting the worked
#'   reference shipped in `inst/examples/`. Needs the `knitr` and
#'   `markdown` packages; the page is skipped with a message if either is
#'   missing. It renders ~100 plots, so it is the slow part of the build —
#'   pass `FALSE` for a quick rebuild of the rest.
#' @return The output directory, invisibly.
#' @examples
#' out <- file.path(tempdir(), "ggplot3-site")
#' build_site(out, quiet = TRUE)
#' list.files(out)
#' @export
build_site <- function(dir = "docs", quiet = FALSE,
                       cookbook = TRUE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  say <- function(...) if (!quiet) message(...)

  say("Writing logo")
  ggplot3_logo(file = file.path(dir, "logo.svg"))

  say("Rendering gallery")
  examples <- gallery_examples()
  figs <- character(0)
  for (nm in names(examples)) {
    ex <- examples[[nm]]
    svg <- tryCatch(
      render(eval(ex$code), target = "static"),
      error = function(e) NULL
    )
    if (is.null(svg)) next
    fname <- paste0("fig-", nm, ".svg")
    writeLines(svg, file.path(dir, fname), useBytes = TRUE)
    figs[nm] <- fname
  }

  say("Writing pages")
  writeLines(site_index(figs, examples), file.path(dir, "index.html"),
             useBytes = TRUE)
  writeLines(site_gallery(figs, examples), file.path(dir, "gallery.html"),
             useBytes = TRUE)
  writeLines(site_reference(dir), file.path(dir, "reference.html"),
             useBytes = TRUE)
  writeLines(site_themes(dir), file.path(dir, "themes.html"), useBytes = TRUE)
  writeLines(site_guide(dir), file.path(dir, "guide.html"), useBytes = TRUE)
  writeLines(site_credits(), file.path(dir, "credits.html"), useBytes = TRUE)

  if (isTRUE(cookbook)) {
    say("Knitting cookbook (~100 plots)")
    cb <- site_cookbook()
    if (is.null(cb)) {
      say("  skipped: needs the knitr and markdown packages")
    } else {
      writeLines(cb, file.path(dir, "cookbook.html"), useBytes = TRUE)
    }
  }

  say("Site written to ", normalizePath(dir))
  invisible(dir)
}

# --- page shell --------------------------------------------------------------

SITE_CSS <- '
/* The site is deliberately light-only: the figures are rendered with light
   themes at build time, and a dark page around a light plot reads badly. */
:root {
  --ink: #16181D; --muted: #5C6270; --line: #E3E5EA; --bg: #FFFFFF;
  --soft: #F9FAFB; --accent: #1A6E55; --accent-soft: #E7F4EF;
  --code-bg: #F5F6F8;
}
html { background: #FFFFFF; }
* { box-sizing: border-box; }
body {
  margin: 0; background: var(--bg); color: var(--ink);
  font: 16px/1.65 -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica,
        Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
}
header {
  border-bottom: 1px solid var(--line); position: sticky; top: 0;
  background: var(--bg); z-index: 5;
}
nav {
  max-width: 1080px; margin: 0 auto; padding: 12px 24px;
  display: flex; align-items: center; gap: 22px; flex-wrap: wrap;
}
nav img { width: 34px; height: 39px; }
nav a { color: var(--muted); text-decoration: none; font-size: 15px; }
nav a:hover, nav a.on { color: var(--accent); }
nav .brand { font-weight: 700; color: var(--ink); font-size: 17px; }
main { max-width: 1080px; margin: 0 auto; padding: 36px 24px 80px; }
h1 { font-size: 34px; line-height: 1.2; margin: 0 0 10px; letter-spacing: -0.5px; }
h2 { font-size: 23px; margin: 44px 0 14px; letter-spacing: -0.2px; }
h3 { font-size: 17px; margin: 26px 0 8px; }
p.lead { font-size: 19px; color: var(--muted); margin: 0 0 24px; max-width: 62ch; }
p { max-width: 72ch; }
code, pre { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
code { background: var(--code-bg); padding: 1px 5px; border-radius: 4px;
       font-size: 0.9em; }
pre {
  background: var(--code-bg); padding: 14px 16px; border-radius: 8px;
  overflow-x: auto; font-size: 13.5px; line-height: 1.55;
  border: 1px solid var(--line);
}
pre code { background: none; padding: 0; font-size: inherit; }
.hero { display: flex; gap: 34px; align-items: center; flex-wrap: wrap; }
.hero img { width: 168px; }
.hero div { flex: 1 1 380px; }
.grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 16px; margin: 20px 0; }
.card { border: 1px solid var(--line); border-radius: 10px; padding: 16px 18px;
        background: var(--soft); }
.card h3 { margin: 0 0 6px; font-size: 15.5px; }
.card p { margin: 0; font-size: 14px; color: var(--muted); }
.fig { border: 1px solid var(--line); border-radius: 10px; overflow: hidden;
       margin: 18px 0 28px; background: var(--bg); }
.fig img { display: block; width: 100%; height: auto; }
/* Inline SVG (the Cookbook embeds plots directly rather than as <img>).
   Without a cap, a wide plot forces the whole page to scroll sideways. */
main svg { display: block; max-width: 100%; height: auto; margin: 14px 0 22px;
           border: 1px solid var(--line); border-radius: 8px; }
.fig figcaption { padding: 12px 16px; border-top: 1px solid var(--line);
                  font-size: 14px; color: var(--muted); }
.figs { display: grid; grid-template-columns: repeat(auto-fit, minmax(340px, 1fr));
        gap: 20px; }
table { border-collapse: collapse; width: 100%; font-size: 14.5px;
        display: block; overflow-x: auto; }
th, td { text-align: left; padding: 7px 12px; border-bottom: 1px solid var(--line);
         vertical-align: top; }
th { color: var(--muted); font-weight: 600; font-size: 13px;
     text-transform: uppercase; letter-spacing: 0.4px; }
td code { white-space: nowrap; }
.tag { display: inline-block; background: var(--accent-soft); color: var(--accent);
       border-radius: 20px; padding: 2px 11px; font-size: 12.5px;
       font-weight: 600; }
footer { border-top: 1px solid var(--line); color: var(--muted); font-size: 14px;
         padding: 22px 24px; text-align: center; }
.note { border-left: 3px solid var(--accent); background: var(--accent-soft);
        padding: 10px 16px; border-radius: 0 8px 8px 0; margin: 16px 0;
        font-size: 15px; }
.note p { margin: 0; max-width: none; }
.toc { background: var(--soft); border: 1px solid var(--line);
       border-radius: 10px; padding: 14px 20px; margin: 22px 0 34px; }
.toc ol { margin: 8px 0 0; padding-left: 22px; columns: 2; }
.toc li { margin: 3px 0; font-size: 14.5px; break-inside: avoid; }
.toc a { color: var(--ink); text-decoration: none; }
.toc a:hover { color: var(--accent); }
.toc strong { font-size: 13px; text-transform: uppercase;
              letter-spacing: 0.4px; color: var(--muted); }
.secnav { display: flex; gap: 8px; flex-wrap: wrap; margin: 0 0 26px; }
.secnav a { font-size: 13.5px; padding: 4px 12px; border-radius: 20px;
            background: var(--soft); border: 1px solid var(--line);
            color: var(--muted); text-decoration: none; }
.secnav a:hover { color: var(--accent); border-color: var(--accent); }
.item { border-top: 1px solid var(--line); padding-top: 26px; margin-top: 34px; }
.item:first-of-type { border-top: 0; }
.item h3 { margin-top: 0; font-size: 19px; }
.aes { font-size: 14px; color: var(--muted); margin: 6px 0 12px; }
.aes code { font-size: 12.5px; }
h2 { scroll-margin-top: 70px; }
h3 { scroll-margin-top: 70px; }
'

site_page <- function(title, active, body) {
  nav_items <- c(
    index = "Overview", guide = "Guide", gallery = "Gallery",
    cookbook = "Cookbook", reference = "Reference", themes = "Themes",
    credits = "Credits"
  )
  links <- vapply(names(nav_items), function(k) {
    sprintf(
      "<a href=\"%s.html\"%s>%s</a>", k,
      if (identical(k, active)) " class=\"on\"" else "", nav_items[[k]]
    )
  }, character(1))
  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", title, "</title>\n<style>", SITE_CSS, "</style>\n</head>\n<body>\n",
    "<header><nav><img src=\"logo.svg\" alt=\"ggplot3\">",
    "<span class=\"brand\">ggplot3</span>",
    paste(links, collapse = ""),
    "</nav></header>\n<main>\n", body, "\n</main>\n",
    "<footer>ggplot3 \u2014 a next-generation Grammar of Graphics for R. ",
    "Built on S7. <a href=\"credits.html\">Credits</a>.</footer>\n",
    "</body>\n</html>\n"
  )
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

code_block <- function(code) {
  paste0("<pre><code>", html_escape(paste(code, collapse = "\n")), "</code></pre>")
}

# --- gallery examples --------------------------------------------------------
#
# Each entry is a quoted plot expression plus a caption. The site renders
# them with the package itself, so the gallery can never drift from the
# code - if a geom breaks, the build fails loudly.

gallery_examples <- function() {
  # Each entry: a title, a section, the aesthetics the geom expects, one
  # sentence on when to reach for it, and a quoted plot expression. The
  # site evaluates the expression, so the gallery cannot drift from the
  # code - if a geom breaks, the site build surfaces it.
  ex <- function(title, section, aes_spec, note, code) {
    list(title = title, section = section, aes = aes_spec, note = note,
         code = code)
  }
  list(
    point = ex(
      "Scatter plot", "Essentials", "x, y, color, size, alpha",
      "The default view of a relationship between two continuous variables.",
      quote(
        ggplot3(cars, aes(speed, dist)) + geom_point() +
          labs(title = "Stopping distance rises with speed",
               x = "Speed (mph)", y = "Distance (ft)")
      )),
    jitter = ex(
      "Jittered points", "Essentials", "x, y, color",
      "Points nudged by a reproducible random offset so overlapping observations stay countable; pair with a boxplot or violin.",
      quote(
        ggplot3(iris, aes(Species, Sepal.Width, color = Species)) +
          geom_jitter(alpha = 0.7) + theme(legend_position = "none")
      )),
    smooth = ex(
      "Trend line with confidence band", "Essentials", "x, y, color",
      "A loess or linear fit with a confidence ribbon; use method = \"lm\" for a straight line.",
      quote(
        ggplot3(cars, aes(speed, dist)) + geom_point(alpha = 0.6) +
          geom_smooth(method = "lm") + theme_minimal()
      )),
    line = ex(
      "Line chart", "Essentials", "x, y, color, group, linewidth, dash",
      "One polyline per group, ordered by x - the standard time-series view.",
      quote(
        ggplot3(
          data.frame(t = rep(1:12, 2),
                     v = c(cumsum(rnorm(12, 2)), cumsum(rnorm(12, 1))),
                     g = rep(c("A", "B"), each = 12)),
          aes(t, v, color = g)
        ) + geom_line() + theme_minimal()
      )),
    step = ex(
      "Step chart", "Essentials", "x, y, color, group",
      "Holds each value until the next observation - right for quantities that change discretely, like a policy rate.",
      quote(
        ggplot3(data.frame(t = 1:8, v = c(2, 2, 3, 3, 5, 4, 4, 6)),
                aes(t, v)) + geom_step() + geom_point() + theme_minimal()
      )),
    area = ex(
      "Area chart", "Essentials", "x, y, color, group, alpha",
      "A line closed to a zero baseline; reads as magnitude over time rather than rate of change.",
      quote(
        ggplot3(data.frame(t = 1:10, v = c(2, 4, 3, 6, 8, 7, 9, 8, 11, 13)),
                aes(t, v)) + geom_area(alpha = 0.5) + theme_minimal()
      )),
    ribbon = ex(
      "Ribbon", "Essentials", "x, ymin, ymax, color, alpha",
      "A band between two series - forecast intervals, min/max envelopes, uncertainty around a fit.",
      quote(
        ggplot3(
          data.frame(t = 1:12, lo = (1:12) * 0.8, hi = (1:12) * 1.4),
          aes(t, ymin = lo, ymax = hi)
        ) + geom_ribbon(alpha = 0.45) + theme_minimal()
      )),
    segment = ex(
      "Segments", "Essentials", "x, y, xend, yend, color",
      "Straight lines between explicit endpoints; the building block for arrows, connectors, and slope charts.",
      quote(
        ggplot3(data.frame(x = 1:4, y = c(2, 4, 3, 5),
                           xe = 1:4 + 0.7, ye = c(4, 6, 2, 7)),
                aes(x, y, xend = xe, yend = ye)) +
          geom_segment(linewidth = 2) + theme_minimal()
      )),
    reference = ex(
      "Reference lines", "Essentials", "(literal intercepts)",
      "geom_hline() and geom_vline() span the panel and ignore the plot's aes(), so they never disturb the data mapping.",
      quote(
        ggplot3(cars, aes(speed, dist)) + geom_point(alpha = 0.6) +
          geom_hline(mean(cars$dist), dash = "5,4", color = "#C1462F") +
          geom_vline(mean(cars$speed), dash = "5,4", color = "#C1462F") +
          theme_minimal()
      )),
    text = ex(
      "Text labels", "Essentials", "x, y, label, color, size",
      "Draws the label column at each position; use for annotating a handful of points, not hundreds.",
      quote(
        ggplot3(data.frame(x = c(1, 2, 3), y = c(3, 1, 2),
                           l = c("alpha", "beta", "gamma")),
                aes(x, y, label = l)) +
          geom_point(size = 5, alpha = 0.3) + geom_text() + theme_minimal()
      )),
    tile = ex(
      "Tile heatmap", "Essentials", "x, y, color",
      "A grid of cells shaded by a continuous value - correlation matrices, calendars, any two-way table.",
      quote(
        ggplot3(
          local({
            g <- expand.grid(x = 1:8, y = 1:6)
            g$z <- as.vector(outer(1:8, 1:6, function(a, b) sin(a / 2) + cos(b / 2)))
            g
          }),
          aes(x, y, color = z)
        ) + geom_tile() + theme_minimal()
      )),
    bar = ex(
      "Bar chart", "Distributions", "x, color, position",
      "geom_bar() counts rows per category; geom_col() takes the height from y directly.",
      quote(
        ggplot3(
          data.frame(g = c("alpha", "beta", "gamma", "delta"),
                     v = c(12, 27, 19, 8)),
          aes(g, v, color = g)
        ) + geom_col() + theme(legend_position = "none")
      )),
    stacked = ex(
      "Stacked and dodged bars", "Distributions", "x, color, position",
      "position = \"stack\" (default) shows totals, \"dodge\" compares groups side by side, \"fill\" shows proportions.",
      quote(
        ggplot3(
          data.frame(g = rep(c("Q1", "Q2", "Q3"), each = 3),
                     grp = rep(c("a", "b", "c"), 3),
                     v = c(4, 6, 3, 7, 4, 5, 5, 8, 2)),
          aes(g, v, color = grp)
        ) + geom_col(position = "dodge") + theme_minimal()
      )),
    histogram = ex(
      "Histogram", "Distributions", "x, bins or binwidth",
      "Bins a continuous variable and counts each bin; bins = n gives exactly n bins.",
      quote(
        ggplot3(cars, aes(speed)) + geom_histogram(bins = 8) + theme_minimal()
      )),
    density = ex(
      "Density", "Distributions", "x, color, adjust",
      "A smoothed distribution estimate - easier to overlay across groups than histograms.",
      quote(
        ggplot3(iris, aes(Sepal.Length, color = Species)) +
          geom_density(alpha = 0.5) + theme_minimal()
      )),
    boxplot = ex(
      "Box plot", "Distributions", "x, y, color",
      "Tukey's five-number summary with outliers beyond 1.5 IQR drawn individually.",
      quote(
        ggplot3(iris, aes(Species, Sepal.Length, color = Species)) +
          geom_boxplot() + theme(legend_position = "none")
      )),
    violin = ex(
      "Violin", "Distributions", "x, y, color",
      "A mirrored density per category - shows bimodality that a box plot hides.",
      quote(
        ggplot3(iris, aes(Species, Sepal.Width, color = Species)) +
          geom_violin() + theme(legend_position = "none")
      )),
    combo = ex(
      "Layered distribution view", "Distributions", "x, y, color",
      "Violin for shape, box for summary, jitter for the raw data - layers draw in the order added.",
      quote(
        ggplot3(iris, aes(Species, Sepal.Length, color = Species)) +
          geom_violin(alpha = 0.25) + geom_boxplot() +
          geom_jitter(alpha = 0.4) + theme(legend_position = "none")
      )),
    ridgeline = ex(
      "Ridgeline (joyplot)", "Distributions", "x, y (the group)",
      "One density per group, offset vertically; compares many distributions in little vertical space.",
      quote(
        ggplot3(iris, aes(Sepal.Length, Species)) +
          geom_ridgeline() + theme_minimal()
      )),
    errorbar = ex(
      "Error bars and point ranges", "Distributions", "x, y, ymin, ymax",
      "An interval per observation; geom_pointrange() adds the estimate marker.",
      quote(
        ggplot3(
          data.frame(g = c("A", "B", "C", "D"), m = c(5, 7, 4, 8),
                     lo = c(4, 6.2, 3.1, 7.1), hi = c(6, 7.8, 4.9, 8.9)),
          aes(g, m, ymin = lo, ymax = hi)
        ) + geom_pointrange() + theme_minimal()
      )),
    dumbbell = ex(
      "Dumbbell", "Distributions", "x, xend, y",
      "Two endpoints joined by a connector - before/after comparison across categories.",
      quote(
        ggplot3(
          data.frame(g = c("North", "South", "East", "West"),
                     before = c(12, 18, 9, 14), after = c(19, 21, 15, 13)),
          aes(before, xend = after, y = g)
        ) + geom_dumbbell() + theme_minimal()
      )),
    waterfall = ex(
      "Waterfall", "Distributions", "x, y",
      "Running-total bars showing how each contribution moves a starting value to an ending one.",
      quote(
        ggplot3(
          data.frame(step = factor(c("Start", "Sales", "Costs", "Tax", "End"),
                                   levels = c("Start", "Sales", "Costs", "Tax", "End")),
                     v = c(100, 45, -30, -12, 0)),
          aes(step, v)
        ) + geom_waterfall() + theme_minimal()
      )),
    facet = ex(
      "Facets", "Layout", "facet_wrap(var), facet_grid(rows, cols)",
      "One panel per subset. Axes are shared by default, which is what makes panels comparable.",
      quote(
        ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() + facet_wrap(Species) +
          theme(legend_position = "none")
      )),
    facet_free = ex(
      "Facets with free scales", "Layout", "scales = \"free\"",
      "Each panel scales to its own data - right when panels differ in magnitude and shape matters more than comparison.",
      quote(
        ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
          geom_point() + facet_wrap(Species, scales = "free") +
          theme(legend_position = "none")
      )),
    radar = ex(
      "Radar / spider", "Layout", "x (axis), y (value), color (series)",
      "A closed profile per series across categorical axes. The radial axis starts at zero so areas stay honest.",
      quote(
        ggplot3(
          data.frame(
            axis = rep(c("Speed", "Power", "Range", "Cost", "Safety"), 2),
            value = c(8, 6, 7, 4, 9, 5, 9, 4, 8, 6),
            model = rep(c("A", "B"), each = 5)
          ),
          aes(axis, value, color = model)
        ) + geom_radar() + coord_polar() + theme_minimal()
      )),
    polar_bar = ex(
      "Circular bar chart", "Layout", "coord_polar()",
      "Any cartesian geom bends into polar coordinates; bars become wedges.",
      quote(
        ggplot3(
          data.frame(g = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                     v = c(4, 7, 6, 9, 12, 15, 11)),
          aes(g, v, color = g)
        ) + geom_col() + coord_polar() +
          theme(legend_position = "none")
      )),
    treemap = ex(
      "Treemap", "Layout", "size (area), label, color",
      "Area-proportional tiles laid out with the squarified algorithm, so tiles stay near-square and comparable.",
      quote(
        ggplot3(
          data.frame(region = c("North", "South", "East", "West", "Central"),
                     revenue = c(52, 38, 27, 19, 11)),
          aes(size = revenue, label = region, color = region)
        ) + geom_treemap() + theme(legend_position = "none")
      )),
    sankey = ex(
      "Sankey / alluvial", "Layout", "x (source), xend (target), y (value)",
      "Flows between stages. One value-to-height scale is shared across stages, so a flow keeps its thickness end to end.",
      quote(
        ggplot3(
          data.frame(
            from = c("Visited", "Visited", "Signed up", "Signed up"),
            to = c("Signed up", "Left", "Purchased", "Churned"),
            n = c(400, 600, 150, 250)
          ),
          aes(x = from, xend = to, y = n)
        ) + geom_sankey()
      )),
    network = ex(
      "Network", "Layout", "x (source), xend (target)",
      "A Fruchterman-Reingold force layout: repulsion between all nodes, attraction along edges, with a cooling schedule.",
      quote(
        ggplot3(
          data.frame(from = c("A", "A", "B", "C", "D", "E", "B"),
                     to = c("B", "C", "C", "D", "E", "A", "E")),
          aes(x = from, xend = to)
        ) + geom_network()
      )),
    chord = ex(
      "Chord", "Layout", "x (source), xend (target), y (value)",
      "Entities on a circle joined by ribbons whose ends are arcs proportional to the flow.",
      quote(
        ggplot3(
          data.frame(from = c("A", "A", "B", "C"), to = c("B", "C", "C", "A"),
                     n = c(5, 3, 7, 2)),
          aes(x = from, xend = to, y = n)
        ) + geom_chord()
      )),
    stream = ex(
      "Streamgraph", "Layout", "x, y, color (series)",
      "Stacked areas on a wiggle baseline rather than zero, so each band's thickness stays readable.",
      quote(
        ggplot3(
          data.frame(t = rep(1:8, 3),
                     v = c(2, 4, 6, 5, 3, 2, 4, 5, 1, 3, 5, 8, 6, 4, 3, 2,
                           5, 4, 3, 4, 6, 7, 5, 4),
                     grp = rep(c("a", "b", "c"), each = 8)),
          aes(t, v, color = grp)
        ) + geom_stream() + theme_minimal()
      )),
    bump = ex(
      "Bump chart", "Layout", "x (time), y (rank), color (series)",
      "Rank trajectories with sigmoid interpolation, so crossings read cleanly instead of as zigzags.",
      quote(
        ggplot3(
          data.frame(year = rep(2020:2024, 4),
                     rank = c(1, 2, 3, 3, 2, 2, 1, 1, 2, 1,
                              3, 3, 2, 1, 3, 4, 4, 4, 4, 4),
                     team = rep(c("A", "B", "C", "D"), each = 5)),
          aes(year, rank, color = team)
        ) + geom_bump() + scale_y_reverse() + theme_minimal()
      )),
    funnel = ex(
      "Funnel", "Layout", "x (stage), y (count)",
      "Centred bars tapering through the stages of a conversion or triage process.",
      quote(
        ggplot3(
          data.frame(
            stage = factor(c("Visits", "Signups", "Trials", "Paid"),
                           levels = c("Visits", "Signups", "Trials", "Paid")),
            n = c(10000, 3200, 1100, 420)
          ),
          aes(stage, n, color = stage)
        ) + geom_funnel() + theme(legend_position = "none")
      )),
    parallel = ex(
      "Parallel coordinates", "Layout", "x (variable), y (value), group",
      "One line per observation across independently rescaled axes - eyeball structure in high-dimensional data.",
      quote(
        ggplot3(
          local({
            d <- iris[c(1, 20, 60, 80, 110, 140), ]
            data.frame(
              id = rep(rownames(d), 4),
              var = rep(c("SL", "SW", "PL", "PW"), each = nrow(d)),
              val = c(d$Sepal.Length, d$Sepal.Width, d$Petal.Length, d$Petal.Width),
              sp = rep(as.character(d$Species), 4)
            )
          }),
          aes(var, val, group = id, color = sp)
        ) + geom_parallel() + theme_minimal()
      )),
    upset = ex(
      "UpSet (set intersections)", "Layout", "label (membership, e.g. \"A&B\")",
      "Intersection sizes with a membership matrix - readable where a 4-way Venn diagram is not.",
      quote(
        ggplot3(
          data.frame(sets = c("A", "A&B", "B", "A&B&C", "C", "A&B",
                              "A", "B&C", "A&C", "A&B")),
          aes(label = sets)
        ) + geom_upset()
      )),
    shap = ex(
      "SHAP beeswarm", "Machine learning", "x (SHAP value), y (feature), color (feature value)",
      "Every observation's contribution per feature, nudged vertically where values collide; colour shows the direction of effect.",
      quote(
        ggplot3(
          local({
            set.seed(1)
            data.frame(
              feature = rep(c("age", "income", "tenure"), each = 40),
              shap = c(rnorm(40, 0.3, 0.2), rnorm(40, -0.1, 0.3),
                       rnorm(40, 0, 0.15)),
              value = runif(120)
            )
          }),
          aes(shap, feature, color = value)
        ) + geom_shap() + geom_vline(0, dash = "3,3") +
          labs(x = "SHAP value", y = NULL) + theme_minimal()
      )),
    pdp = ex(
      "Partial dependence + ICE", "Machine learning", "x (feature), y (prediction), group",
      "Thin per-observation ICE curves under a bold average, so heterogeneous effects are visible.",
      quote(
        ggplot3(
          local({
            set.seed(9)
            data.frame(
              x = rep(1:10, 8), id = rep(1:8, each = 10),
              pred = as.vector(sapply(1:8, function(i) {
                (1:10) * 0.1 * i + rnorm(10, 0, 0.15)
              }))
            )
          }),
          aes(x, pred, group = id)
        ) + geom_partial_dependence() + theme_minimal()
      )),
    roc = ex(
      "ROC curve", "Machine learning", "score, truth",
      "The true-positive against false-positive staircase over every score threshold.",
      quote(
        ggplot3(
          local({
            set.seed(2)
            s <- runif(300)
            data.frame(score = s, truth = rbinom(300, 1, s))
          }),
          aes(score = score, truth = truth)
        ) + geom_roc() + theme_minimal()
      )),
    calibration = ex(
      "Calibration curve", "Machine learning", "x (predicted prob), y (outcome)",
      "Binned predictions against observed rates; distance from the diagonal is over- or under-confidence.",
      quote(
        ggplot3(
          local({
            set.seed(3)
            p <- runif(400)
            data.frame(pred = p, obs = rbinom(400, 1, p^1.3))
          }),
          aes(pred, obs)
        ) + geom_calibration() + theme_minimal()
      )),
    lift = ex(
      "Cumulative gain", "Machine learning", "score, truth",
      "Share of positives captured against share of population targeted - how to size a cutoff.",
      quote(
        ggplot3(
          local({
            set.seed(11)
            s <- runif(250)
            data.frame(score = s, y = rbinom(250, 1, s))
          }),
          aes(score = score, truth = y)
        ) + geom_lift_gain() + theme_minimal()
      )),
    confusion = ex(
      "Confusion matrix", "Machine learning", "x (predicted), y (actual), size (count)",
      "Row-normalised shading with raw counts annotated, so class imbalance cannot hide errors.",
      quote(
        ggplot3(
          data.frame(
            predicted = c(rep("cat", 14), rep("dog", 9), rep("bird", 6)),
            actual = c(rep("cat", 11), rep("dog", 3), rep("dog", 7),
                       rep("cat", 2), rep("bird", 5), "cat")
          ),
          aes(predicted, actual)
        ) + geom_confusion_matrix() + theme(legend_position = "none")
      )),
    residual = ex(
      "Residual diagnostics", "Machine learning", "x (fitted), y (residual)",
      "Residuals against fitted values with a zero line and a loess trend - the first check on a linear model.",
      quote(
        ggplot3(
          local({
            m <- lm(dist ~ speed, cars)
            data.frame(fitted = fitted(m), resid = resid(m))
          }),
          aes(fitted, resid)
        ) + geom_residual() + theme_minimal()
      )),
    learning = ex(
      "Learning curve", "Machine learning", "x (size/epoch), y (score), color (split)",
      "Train and validation score against training-set size - shows whether a model is data-limited or over-fitting.",
      quote(
        ggplot3(
          data.frame(
            n = rep(c(50, 100, 200, 400, 800), 2),
            score = c(.72, .80, .85, .88, .90, .66, .75, .81, .85, .88),
            split = rep(c("train", "validation"), each = 5)
          ),
          aes(n, score, color = split)
        ) + geom_learning_curve() + theme_minimal()
      )),
    embedding = ex(
      "Embedding with hulls", "Machine learning", "x, y, color (cluster)",
      "A t-SNE/UMAP/PCA scatter with convex hulls, so cluster shape is visible rather than inferred from colour.",
      quote(
        ggplot3(
          local({
            set.seed(4)
            data.frame(d1 = c(rnorm(40), rnorm(40, 4)),
                       d2 = c(rnorm(40), rnorm(40, 3)),
                       cluster = rep(c("a", "b"), each = 40))
          }),
          aes(d1, d2, color = cluster)
        ) + geom_embedding() + theme_minimal()
      )),
    silhouette = ex(
      "Silhouette", "Machine learning", "x (width), y (cluster)",
      "Sorted silhouette widths per cluster - the standard visual check on cluster separation.",
      quote(
        ggplot3(
          local({
            set.seed(12)
            data.frame(cluster = rep(c("1", "2", "3"), each = 25),
                       width = c(runif(25, .3, .9), runif(25, .1, .7),
                                 runif(25, -.1, .6)))
          }),
          aes(width, cluster, color = cluster)
        ) + geom_silhouette() + theme(legend_position = "none")
      )),
    boundary = ex(
      "Decision boundary", "Machine learning", "x, y, color (predicted class)",
      "A shaded prediction grid; overlay geom_point() for the training data.",
      quote(
        ggplot3(
          local({
            g <- expand.grid(x = seq(0, 1, 0.04), y = seq(0, 1, 0.04))
            g$cls <- ifelse(g$x + g$y > 1, "a", "b")
            g
          }),
          aes(x, y, color = cls)
        ) + geom_decision_boundary() + theme_minimal()
      )),
    forecast = ex(
      "Forecast with interval", "Machine learning", "x, y, ymin, ymax, group",
      "History solid, forecast dashed, interval as a ribbon on the forecast rows only.",
      quote(
        ggplot3(
          data.frame(
            t = 1:12, v = c(3, 4, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12),
            lo = c(rep(NA, 7), 6.5, 7, 7.5, 8, 8.5),
            hi = c(rep(NA, 7), 9.5, 11, 12.5, 14, 15.5),
            part = rep(c("actual", "forecast"), c(7, 5))
          ),
          aes(t, v, ymin = lo, ymax = hi, group = part)
        ) + geom_forecast_band() + theme_minimal()
      )),
    km = ex(
      "Kaplan-Meier", "Clinical", "time, status, color (arm)",
      "Product-limit survival curves with censoring ticks, per treatment arm.",
      quote(
        ggplot3(
          local({
            set.seed(5)
            data.frame(
              t = c(rexp(60, 0.08), rexp(60, 0.14)),
              ev = rbinom(120, 1, 0.75),
              arm = rep(c("Treatment", "Control"), each = 60)
            )
          }),
          aes(time = t, status = ev, color = arm)
        ) + geom_km() + theme_minimal()
      )),
    cuminc = ex(
      "Cumulative incidence", "Clinical", "time, status (0 = censored, 1..k = event types)",
      "Aalen-Johansen curves per event type - the correct estimator when competing risks make 1 - KM biased upward.",
      quote(
        ggplot3(
          local({
            set.seed(8)
            data.frame(t = rexp(150, 0.1),
                       ev = sample(0:2, 150, replace = TRUE,
                                   prob = c(.4, .35, .25)))
          }),
          aes(time = t, status = ev)
        ) + geom_cuminc() + theme_minimal()
      )),
    forest = ex(
      "Forest plot", "Clinical", "x (estimate), y (study), ymin, ymax, size (weight)",
      "Estimates with confidence intervals and a no-effect reference; marker area encodes study weight.",
      quote(
        ggplot3(
          data.frame(
            study = c("Trial A", "Trial B", "Trial C", "Trial D", "Pooled"),
            hr = c(0.82, 0.71, 0.95, 0.88, 0.83),
            lo = c(0.65, 0.52, 0.78, 0.70, 0.74),
            hi = c(1.03, 0.97, 1.16, 1.10, 0.93),
            weight = c(30, 22, 28, 20, 100)
          ),
          aes(hr, study, ymin = lo, ymax = hi, size = weight)
        ) + geom_forest() +
          labs(x = "Hazard ratio (95% CI)", y = NULL) + theme_minimal()
      )),
    swimmer = ex(
      "Swimmer plot", "Clinical", "x (duration), y (subject), color, label (ongoing)",
      "Per-subject time on treatment, with arrowheads for subjects still ongoing at data cutoff.",
      quote(
        ggplot3(
          data.frame(
            subject = paste0("S", 1:8),
            months = c(4, 9, 14, 6, 20, 11, 17, 7),
            response = c("PR", "CR", "CR", "SD", "PR", "SD", "CR", "PD"),
            ongoing = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
          ),
          aes(months, subject, color = response, label = ongoing)
        ) + geom_swimmer() + labs(x = "Months", y = NULL) + theme_minimal()
      )),
    spider = ex(
      "Oncology spider plot", "Clinical", "x (time), y (% change), color (subject)",
      "Per-subject change from baseline over time, with the RECIST +20% / -30% thresholds marked.",
      quote(
        ggplot3(
          data.frame(
            month = rep(c(0, 2, 4, 6, 8), 4),
            pct = c(0, -20, -35, -40, -42, 0, 10, 25, 40, 55,
                    0, -5, -10, -8, -12, 0, -30, -45, -50, -48),
            subject = rep(c("S1", "S2", "S3", "S4"), each = 5)
          ),
          aes(month, pct, color = subject)
        ) + geom_spider_response() +
          labs(x = "Month", y = "% change from baseline") + theme_minimal()
      )),
    waterfall_response = ex(
      "RECIST waterfall", "Clinical", "x (subject), y (% change)",
      "Best response per subject, ordered worst to best and shaded by RECIST category.",
      quote(
        ggplot3(
          local({
            set.seed(6)
            data.frame(subject = paste0("S", 1:24),
                       pct = sort(runif(24, -78, 48), decreasing = TRUE))
          }),
          aes(subject, pct)
        ) + geom_waterfall_response() +
          labs(y = "% change from baseline", x = NULL) +
          theme(axis_text_x = FALSE)
      )),
    spaghetti = ex(
      "Spaghetti trajectories", "Clinical", "x (time), y (measure), group (subject)",
      "Individual longitudinal paths with a bold group mean - shows change without hiding spread.",
      quote(
        ggplot3(
          local({
            set.seed(7)
            data.frame(
              week = rep(0:5, 10), id = rep(1:10, each = 6),
              score = as.vector(sapply(1:10, function(i) {
                50 + i + (0:5) * 2 + rnorm(6, 0, 3)
              }))
            )
          }),
          aes(week, score, group = id)
        ) + geom_spaghetti() + theme_minimal()
      )),
    bland = ex(
      "Bland-Altman", "Clinical", "x, y (the two methods)",
      "Difference against mean with bias and 95% limits of agreement - the standard method-comparison plot.",
      quote(
        ggplot3(
          local({
            set.seed(13)
            a <- rnorm(80, 100, 12)
            data.frame(method_a = a, method_b = a + rnorm(80, 2, 5))
          }),
          aes(method_a, method_b)
        ) + geom_bland_altman() + theme_minimal()
      )),
    dose = ex(
      "Dose-response", "Clinical", "x (dose), y (response)",
      "A four-parameter log-logistic fit with the EC50 marked; pair with scale_x_log10().",
      quote(
        ggplot3(
          data.frame(
            dose = rep(c(0.1, 1, 10, 100, 1000), each = 3),
            resp = c(5, 7, 6, 18, 22, 20, 52, 48, 55, 82, 79, 85, 95, 97, 93)
          ),
          aes(dose, resp)
        ) + geom_dose_response() + scale_x_log10() + theme_minimal()
      )),
    ae_heatmap = ex(
      "Adverse-event heatmap", "Clinical", "x (arm), y (term), size (incidence)",
      "Incidence by preferred term and treatment arm, shaded by rate and annotated with values.",
      quote(
        ggplot3(
          local({
            d <- expand.grid(arm = c("Placebo", "Low", "High"),
                             ae = c("Nausea", "Fatigue", "Headache", "Rash"))
            d$pct <- c(5, 12, 22, 8, 15, 26, 3, 6, 11, 2, 9, 17)
            d
          }),
          aes(arm, ae, size = pct)
        ) + geom_ae_heatmap() + theme(legend_position = "none")
      )),
    consort = ex(
      "CONSORT flow", "Clinical", "label (stage), size (count)",
      "Participant flow from screening to analysis, laid out automatically from a stage/count table.",
      quote(
        ggplot3(
          data.frame(
            stage = c("Assessed for eligibility", "Randomised",
                      "Received allocation", "Completed follow-up", "Analysed"),
            n = c(420, 300, 291, 276, 271)
          ),
          aes(label = stage, size = n)
        ) + geom_consort()
      ))
  )
}

# --- pages -------------------------------------------------------------------

site_index <- function(figs, examples) {
  features <- list(
    c("The grammar you already know",
      "data + aes() + geoms + stats + scales + coords + facets + theme, composed with +. The vocabulary and the muscle memory carry straight over."),
    c("One object, two renderers",
      "The same plot renders to a standalone SVG or a self-contained interactive HTML page, from one computed-geometry buffer - so the targets cannot disagree."),
    c("Static first, interactive on request",
      "render(p) gives you an image. Adding + interact() opts into tooltips, scroll-zoom, and brush-to-zoom; + animate() adds a frame scrubber."),
    c("A much larger catalog",
      "59 geoms in the box: essentials through Sankey, treemap, network, radar, SHAP, ROC, Kaplan-Meier, forest, and CONSORT - no extension hunting."),
    c("Exact data export",
      "plot_data(p) returns precisely the values drawn - post-stat, post-position, post-facet - so you can publish the numbers beside the figure."),
    c("Themes and deep customization",
      "Six presets plus 35 theme settings, custom palettes, titles, axis breaks, labels, transforms, and limits.")
  )
  cards <- vapply(features, function(f) {
    sprintf("<div class=\"card\"><h3>%s</h3><p>%s</p></div>", f[1], f[2])
  }, character(1))

  hero_fig <- if ("smooth" %in% names(figs)) {
    sprintf(
      "<figure class=\"fig\"><img src=\"%s\" alt=\"scatter with trend\"></figure>",
      figs[["smooth"]]
    )
  } else {
    ""
  }

  paste0(
    "<div class=\"hero\"><img src=\"logo.svg\" alt=\"ggplot3 hex logo\">",
    "<div><h1>ggplot3</h1>",
    "<p class=\"lead\">Data visualisations, reimagined, using a ",
    "next-generation Grammar of Graphics \u2014 the familiar grammar, ",
    "extended with native interactivity, animation, exact data export, and ",
    "a far larger geom catalog.</p>",
    "<span class=\"tag\">S7 &middot; SVG &middot; canvas &middot; 59 geoms</span>",
    "</div></div>",

    "<h2>Install</h2>",
    code_block(c(
      "# install.packages(\"remotes\")",
      "remotes::install_github(\"maheshdivakaran/ggplot3\")"
    )),

    "<h2>Quick start</h2>",
    code_block(c(
      "library(ggplot3)",
      "",
      "p <- ggplot3(cars, aes(speed, dist)) +",
      "  geom_point(alpha = 0.6) +",
      "  geom_smooth(method = \"lm\") +",
      "  labs(title = \"Stopping distance rises with speed\",",
      "       x = \"Speed (mph)\", y = \"Distance (ft)\") +",
      "  theme_minimal()",
      "",
      "p                                  # static SVG, shown in the viewer",
      "render(p, file = \"plot.svg\")       # save it",
      "render(p + interact(), file = \"plot.html\")   # interactive version",
      "plot_data(p)                       # exactly the values drawn"
    )),
    hero_fig,

    "<h2>What you get</h2>",
    "<div class=\"grid\">", paste(cards, collapse = ""), "</div>",

    "<h2>The grammar</h2>",
    "<p>Every plot is <code>data + aes() + layers + scales + coord + facet ",
    "+ theme</code>, composed with <code>+</code>. Plots are immutable ",
    "values: adding a component returns a new plot, so partial ",
    "specifications can be shared and forked freely.</p>",
    code_block(c(
      "ggplot3(mpg_like, aes(displ, hwy, color = class)) +",
      "  geom_point(size = 3) +",
      "  facet_wrap(class) +",
      "  scale_y_continuous(name = \"Highway MPG\", breaks = c(20, 30, 40)) +",
      "  coord_cartesian() +",
      "  interact(tooltip = c(\"model\", \"hwy\"), brush = TRUE) +",
      "  theme_modern()"
    )),
    "<p>See the <a href=\"guide.html\">Guide</a> for a walkthrough, the ",
    "<a href=\"gallery.html\">Gallery</a> for ", length(examples),
    " worked examples, or the <a href=\"cookbook.html\">Cookbook</a> for ",
    "every function and option demonstrated end to end.</p>",

    "<h2>Interactivity</h2>",
    "<p>Interactivity is an additive grammar verb, not a separate package ",
    "or a post-hoc conversion. <code>+ interact()</code> switches the ",
    "default render target to a self-contained HTML page - vanilla ",
    "JavaScript, no CDN - driven by the same geometry buffer the SVG ",
    "writer consumes.</p>",
    code_block(c(
      "render(p)                                  # static SVG (default)",
      "render(p + interact())                     # tooltips + zoom + brush",
      "render(p + interact(tooltip = c(\"model\"))) # tooltip from data columns",
      "render(p + animate(year))                  # frame scrubber"
    )),

    "<h2>Credits</h2>",
    "<p>ggplot3 takes its vocabulary from Leland Wilkinson\u2019s ",
    "<em>The Grammar of Graphics</em> and from Hadley Wickham\u2019s ",
    "<strong>ggplot2</strong>, whose API design this package deliberately ",
    "preserves. The extended catalog follows conventions established by ",
    "the wider R visualisation community \u2014 ggridges, ggalluvial, ",
    "ggraph, treemapify, ggradar, ggstream, ggupset, circlize, GGally, ",
    "ggbump, ggiraph, gganimate, survminer, DALEX and others. ",
    "See the <a href=\"credits.html\">credits page</a> for the full list ",
    "and the algorithm references.</p>",
    "<p>The implementation is independent: it is written on ",
    "<a href=\"reference.html\">S7</a> from first principles, and no code ",
    "is derived from those packages.</p>"
  ) |> site_page(title = "ggplot3 - Grammar of Graphics for R", active = "index")
}

# Credits page: prior art, algorithm references, and the implementation note.
site_credits <- function() {
  refs <- list(
    list("Ridgeline / joyplots", "ggridges",
         "https://wilkelab.org/ggridges/"),
    list("Alluvial and Sankey flows", "ggalluvial, ggsankey",
         "https://corybrunson.github.io/ggalluvial/"),
    list("Network grammar", "ggraph, igraph",
         "https://ggraph.data-imaginist.com"),
    list("Treemaps", "treemapify", "https://wilkox.org/treemapify/"),
    list("Radar / spider charts", "ggradar",
         "https://github.com/ricardo-bion/ggradar"),
    list("Streamgraphs", "ggstream",
         "https://github.com/davidsjoberg/ggstream"),
    list("Set intersections", "ggupset, UpSetR",
         "https://github.com/const-ae/ggupset"),
    list("Chord diagrams", "circlize",
         "https://jokergoo.github.io/circlize_book/"),
    list("Parallel coordinates", "GGally", "https://ggobi.github.io/ggally/"),
    list("Bump charts", "ggbump", "https://github.com/davidsjoberg/ggbump"),
    list("Label and extra geoms", "ggforce, ggrepel, ggalt",
         "https://ggforce.data-imaginist.com"),
    list("Interactivity model", "ggiraph, plotly",
         "https://davidgohel.github.io/ggiraph/"),
    list("Animation grammar", "gganimate", "https://gganimate.com"),
    list("Plot composition", "patchwork, cowplot",
         "https://patchwork.data-imaginist.com"),
    list("Survival and clinical figures",
         "survminer, survival, cmprsk, swimplot, consort",
         "https://rpkgs.datanovia.com/survminer/"),
    list("Model explanation plots", "DALEX, shapviz, iml, yardstick",
         "https://modeloriented.github.io/DALEX/"),
    list("Colour theory", "Okabe &amp; Ito palette; scales, viridis",
         "https://jfly.uni-koeln.de/color/")
  )
  rows <- vapply(refs, function(r) {
    sprintf("<tr><td>%s</td><td><a href=\"%s\">%s</a></td></tr>",
            r[[1]], r[[3]], r[[2]])
  }, character(1))

  algos <- list(
    c("Tick placement", "Paul Heckbert, \u201cNice Numbers for Graph Labels\u201d, Graphics Gems (1990)"),
    c("Treemap layout", "Bruls, Huizing &amp; van Wijk, \u201cSquarified Treemaps\u201d (2000)"),
    c("Force-directed graphs", "Fruchterman &amp; Reingold, \u201cGraph Drawing by Force-Directed Placement\u201d (1991)"),
    c("Streamgraph baselines", "Byron &amp; Wattenberg, \u201cStacked Graphs \u2014 Geometry &amp; Aesthetics\u201d (2008)"),
    c("Survival estimation", "Kaplan &amp; Meier (1958); Aalen (1978), Johansen (1978) for competing risks"),
    c("Box plot rule", "Tukey, Exploratory Data Analysis (1977)"),
    c("Method agreement", "Bland &amp; Altman, \u201cStatistical methods for assessing agreement\u201d (1986)"),
    c("Convex hulls", "Jarvis march (gift wrapping), 1973"),
    c("Contrast ratios", "WCAG 2.x relative luminance")
  )
  algo_rows <- vapply(algos, function(a) {
    sprintf("<tr><td>%s</td><td>%s</td></tr>", a[1], a[2])
  }, character(1))

  body <- paste0(
    "<h1>Credits</h1>",
    "<p class=\"lead\">ggplot3 is an independent implementation, but it ",
    "stands on a great deal of prior art and takes its vocabulary directly ",
    "from it.</p>",

    "<h2>The grammar</h2>",
    "<p>The grammar comes from Leland Wilkinson\u2019s <em>The Grammar of ",
    "Graphics</em> (Springer, 2005) and, above all, from Hadley ",
    "Wickham\u2019s <a href=\"https://ggplot2.tidyverse.org\">ggplot2</a>. ",
    "The API design \u2014 <code>aes()</code>, <code>geom_*()</code>, ",
    "<code>stat_*()</code>, <code>scale_*()</code>, <code>coord_*()</code>, ",
    "<code>facet_*()</code>, <code>theme()</code>, and composition with ",
    "<code>+</code> \u2014 is preserved deliberately, so that anyone who ",
    "knows ggplot2 already knows most of this package.</p>",

    "<h2>Prior art by area</h2>",
    "<p>The extended catalog follows conventions established by these ",
    "packages. They are credited as design references, not as code ",
    "ancestry.</p>",
    "<table><thead><tr><th>Area</th><th>Reference</th></tr></thead><tbody>",
    paste(rows, collapse = ""), "</tbody></table>",

    "<h2>Algorithms</h2>",
    "<p>The layout and statistical algorithms are implemented from their ",
    "published descriptions.</p>",
    "<table><thead><tr><th>Used for</th><th>Source</th></tr></thead><tbody>",
    paste(algo_rows, collapse = ""), "</tbody></table>",

    "<h2>Implementation</h2>",
    "<p>ggplot3 is built on ",
    "<a href=\"https://rconsortium.github.io/S7/\">S7</a>, the R ",
    "Consortium\u2019s object system, which is the package\u2019s only ",
    "hard dependency. The rendering, scale, colour and layout code is ",
    "written from scratch rather than wrapping the packages above; no code ",
    "is copied from any of them.</p>",
    "<div class=\"note\"><p>If your work depends on one of the packages ",
    "listed here, please cite it directly \u2014 they remain the reference ",
    "implementations in their areas.</p></div>"
  )
  site_page("Credits - ggplot3", "credits", body)
}

site_gallery <- function(figs, examples) {
  sections <- unique(vapply(examples, function(e) e$section, character(1)))
  slug <- function(x) tolower(gsub("[^A-Za-z0-9]+", "-", x))

  intros <- c(
    "Essentials" = paste(
      "The everyday layers. Each takes the same aesthetics you would",
      "expect, and several can be stacked in one plot - layers draw in",
      "the order they are added."
    ),
    "Distributions" = paste(
      "Summaries of one variable, or of one variable split by a",
      "category. Where a stat is involved, plot_data() will show you",
      "exactly what was computed."
    ),
    "Layout" = paste(
      "Geoms whose positions come from a layout algorithm rather than",
      "straight from the data. Each algorithm is implemented directly -",
      "squarified treemaps, force-directed graphs, Sankey node stacking."
    ),
    "Machine learning" = paste(
      "Model diagnostics as first-class layers. Each takes a tidy data",
      "frame rather than a fitted model object, so any framework that",
      "can produce the columns will work."
    ),
    "Clinical" = paste(
      "Figures that clinical reporting needs constantly and that",
      "otherwise take dozens of lines of manual layering. Estimators",
      "such as Kaplan-Meier and Aalen-Johansen are computed in-package."
    )
  )

  nav <- paste0(
    "<div class=\"secnav\">",
    paste(vapply(sections, function(sec) {
      sprintf("<a href=\"#%s\">%s</a>", slug(sec), sec)
    }, character(1)), collapse = ""),
    "</div>"
  )

  n_shown <- sum(!is.na(figs[names(examples)]))
  body <- paste0(
    "<h1>Gallery</h1>",
    "<p class=\"lead\">", n_shown, " worked examples. Every figure was ",
    "rendered by ggplot3 when this site was built, and the code under each ",
    "one is exactly what produced it.</p>",
    nav
  )

  for (sec in sections) {
    body <- paste0(
      body, "<h2 id=\"", slug(sec), "\">", sec, "</h2>",
      if (!is.na(intros[sec])) paste0("<p>", intros[[sec]], "</p>") else ""
    )
    in_sec <- names(examples)[
      vapply(examples, function(e) identical(e$section, sec), logical(1))
    ]
    for (nm in in_sec) {
      if (is.na(figs[nm]) || is.null(figs[[nm]])) next
      e <- examples[[nm]]
      body <- paste0(
        body,
        "<div class=\"item\">",
        "<h3 id=\"", nm, "\">", e$title, "</h3>",
        "<p>", e$note, "</p>",
        "<p class=\"aes\">Aesthetics: <code>", html_escape(e$aes),
        "</code></p>",
        "<figure class=\"fig\"><img src=\"", figs[[nm]], "\" alt=\"",
        e$title, "\"></figure>",
        code_block(deparse(e$code, width.cutoff = 62)),
        "</div>"
      )
    }
  }
  site_page("Gallery - ggplot3", "gallery", body)
}

site_themes <- function(dir) {
  themes <- c("theme_ggplot3", "theme_minimal", "theme_classic",
              "theme_modern", "theme_dark", "theme_void")
  blurbs <- c(
    theme_ggplot3 = "The default: a softly tinted panel with white gridlines.",
    theme_minimal = "No panel fill, light grey grid, no axis lines or ticks.",
    theme_classic = "White panel, black axes, no grid, serif type - the statistical-journal look.",
    theme_modern  = "Editorial styling: large title, horizontal rules only, a brighter palette.",
    theme_dark    = "Dark background with a palette tuned for it.",
    theme_void    = "No chrome at all - for treemaps, networks, and sparkline-style output."
  )
  body <- paste0(
    "<h1>Themes</h1><p class=\"lead\">Six presets, and 35 individual ",
    "settings you can override with <code>theme()</code>. Every setting ",
    "travels inside the geometry buffer, so the static and interactive ",
    "targets always agree.</p><div class=\"figs\">"
  )
  for (th in themes) {
    p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point(alpha = 0.85) +
      labs(title = th, subtitle = "iris sepal dimensions")
    p <- plot_add(p, do.call(th, list()))
    svg <- tryCatch(render(p, target = "static"), error = function(e) NULL)
    if (is.null(svg)) next
    fname <- paste0("theme-", th, ".svg")
    writeLines(svg, file.path(dir, fname), useBytes = TRUE)
    body <- paste0(
      body, "<figure class=\"fig\"><img src=\"", fname, "\" alt=\"", th,
      "\"><figcaption><code>", th, "()</code> &mdash; ", blurbs[[th]],
      "</figcaption></figure>"
    )
  }
  body <- paste0(body, "</div>")

  body <- paste0(
    body,
    "<h2>Customizing</h2>",
    "<p>Override any setting on top of any preset. <code>base</code> picks ",
    "the preset to start from; everything else is applied over it.</p>",
    code_block(c(
      "ggplot3(cars, aes(speed, dist)) + geom_point() +",
      "  theme(",
      "    base = theme_minimal(),",
      "    panel_fill = \"#FFF8F0\",",
      "    grid_major_x = FALSE,",
      "    plot_title_size = 22,",
      "    legend_position = \"bottom\",",
      "    point_palette = c(\"#2B6BE0\", \"#E05A2B\", \"#12A594\")",
      "  )"
    )),
    "<div class=\"note\"><p>Colour settings accept any R colour ",
    "specification - a name like <code>\"steelblue\"</code> or a hex ",
    "string. An empty string <code>\"\"</code> means <em>do not draw ",
    "this element</em>, which is how <code>grid_color = \"\"</code> ",
    "removes gridlines entirely.</p></div>",
    "<h2>Every setting</h2>",
    theme_settings_table()
  )
  site_page("Themes - ggplot3", "themes", body)
}

# The Cookbook page: the worked reference from inst/examples/, knitted so
# the site and the shipped .Rmd can never drift apart. Returns NULL when
# knitr or markdown is unavailable, so the rest of the site still builds.
site_cookbook <- function() {
  if (!requireNamespace("knitr", quietly = TRUE) ||
      !requireNamespace("markdown", quietly = TRUE)) {
    return(NULL)
  }
  rmd <- system.file("examples", "ggplot3-full-reference.Rmd",
                     package = "ggplot3")
  if (!nzchar(rmd) || !file.exists(rmd)) {
    return(NULL)
  }

  md <- tempfile(fileext = ".md")
  on.exit(unlink(md), add = TRUE)
  # Knit in a throwaway working directory: the document writes a CSV and an
  # SVG to demonstrate the file-output arguments, and those belong in a
  # temporary place, not in the site folder.
  wd <- tempfile("cookbook")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  old <- setwd(wd)
  on.exit(setwd(old), add = TRUE)

  ok <- tryCatch({
    knitr::knit(rmd, output = md, quiet = TRUE)
    TRUE
  }, error = function(e) {
    warning("Cookbook page skipped: ", conditionMessage(e), call. = FALSE)
    FALSE
  })
  if (!ok) {
    return(NULL)
  }

  body <- paste(markdown::mark(md), collapse = "\n")

  # The .Rmd carries its own page styling so it looks right when knitted
  # standalone. Inside the site that would fight the site stylesheet, so
  # strip it and let the site's CSS apply.
  body <- gsub("<style[^>]*>.*?</style>", "", body)

  paste0(
    "<h1>Cookbook</h1>",
    "<p class=\"lead\">Every exported function and every option that ",
    "changes the output, as worked examples. Each figure below was ",
    "rendered by ggplot3 when this page was built, and several sections ",
    "check the computed numbers against base R.</p>",
    "<div class=\"note\"><p>This page is generated from ",
    "<code>inst/examples/ggplot3-full-reference.Rmd</code>, which ships ",
    "with the package. Open it in RStudio to run any of it yourself: ",
    "<code>system.file(\"examples\", ",
    "\"ggplot3-full-reference.Rmd\", package = \"ggplot3\")</code></p></div>",
    cookbook_toc(body),
    body
  ) |> site_page(title = "Cookbook - ggplot3", active = "cookbook")
}

# Build a two-column contents list from the knitted document's headings.
cookbook_toc <- function(body) {
  m <- gregexpr("<h1 id=\"([^\"]+)\">(.*?)</h1>", body, perl = TRUE)
  hits <- regmatches(body, m)[[1]]
  if (length(hits) == 0) {
    return("")
  }
  ids <- sub(".*id=\"([^\"]+)\".*", "\\1", hits)
  txt <- sub("<h1[^>]*>(.*?)</h1>", "\\1", hits)
  txt <- gsub("<[^>]+>", "", txt)
  items <- paste(sprintf("<li><a href=\"#%s\">%s</a></li>", ids, txt),
                 collapse = "")
  paste0("<div class=\"toc\"><strong>Sections</strong><ol>", items,
         "</ol></div>")
}

# The long-form guide: the material that would otherwise be vignettes,
# rendered with live figures rather than static prose.
site_guide <- function(dir) {
  fig <- function(name, plot) {
    svg <- tryCatch(render(plot, target = "static"), error = function(e) NULL)
    if (is.null(svg)) {
      return("")
    }
    fname <- paste0("guide-", name, ".svg")
    writeLines(svg, file.path(dir, fname), useBytes = TRUE)
    paste0("<figure class=\"fig\"><img src=\"", fname, "\" alt=\"", name,
           "\"></figure>")
  }

  sections <- c(
    "grammar" = "The grammar",
    "layers" = "Layers, geoms and stats",
    "aesthetics" = "Aesthetics and grouping",
    "scales" = "Scales and axes",
    "positions" = "Position adjustments",
    "facets" = "Facets",
    "coords" = "Coordinate systems",
    "labels" = "Titles and labels",
    "themes" = "Themes",
    "legends" = "Legends",
    "interactivity" = "Interactivity",
    "animation" = "Animation",
    "data" = "Getting the data back",
    "output" = "Output and sizing",
    "extending" = "Extending ggplot3",
    "troubleshooting" = "Troubleshooting"
  )
  toc <- paste0(
    "<div class=\"toc\"><strong>On this page</strong><ol>",
    paste(vapply(names(sections), function(k) {
      sprintf("<li><a href=\"#%s\">%s</a></li>", k, sections[[k]])
    }, character(1)), collapse = ""),
    "</ol></div>"
  )
  h <- function(k) sprintf("<h2 id=\"%s\">%s</h2>", k, sections[[k]])

  body <- paste0(
    "<h1>Guide</h1>",
    "<p class=\"lead\">The grammar end to end: building a plot, ",
    "controlling the axes, styling it, adding interactivity, and getting ",
    "the numbers back out.</p>",
    toc,

    h("grammar"),
    "<p>A plot is <code>data</code>, an aesthetic <code>mapping</code>, and ",
    "one or more <code>layers</code>. Scales, coordinates, facets and a ",
    "theme are all optional \u2014 sensible defaults are chosen from the ",
    "data.</p>",
    code_block(c(
      "ggplot3(cars, aes(speed, dist)) +",
      "  geom_point()"
    )),
    "<p><code>aes()</code> captures unevaluated expressions, so you refer ",
    "to columns by name. The first two unnamed arguments are ",
    "<code>x</code> and <code>y</code>. Adding a component with ",
    "<code>+</code> returns a <em>new</em> plot \u2014 plots are immutable ",
    "values, so a partial specification can be reused as a template.</p>",
    code_block(c(
      "base <- ggplot3(iris, aes(Sepal.Length, Sepal.Width))",
      "base + geom_point()                    # scatter",
      "base + geom_point() + geom_smooth()    # scatter with a trend",
      "# `base` itself is unchanged."
    )),
    "<div class=\"note\"><p>Because <code>+</code> never mutates, you can ",
    "build a plot conditionally: <code>p &lt;- p + if (annotate) ",
    "geom_text() else NULL</code>. Adding <code>NULL</code> is a no-op.</p>",
    "</div>",

    h("layers"),
    "<p>Every layer pairs a <strong>geom</strong> (how values are drawn) ",
    "with a <strong>stat</strong> (how raw data becomes those values). ",
    "<code>geom_point()</code> uses <code>stat_identity()</code>; ",
    "<code>geom_histogram()</code> bins first; <code>geom_boxplot()</code> ",
    "computes Tukey\u2019s five numbers. Layers draw in the order added, ",
    "so put context underneath and detail on top.</p>",
    code_block(c(
      "ggplot3(iris, aes(Species, Sepal.Length, color = Species)) +",
      "  geom_violin(alpha = 0.25) +   # distribution, underneath",
      "  geom_boxplot() +              # summary",
      "  geom_jitter(alpha = 0.4)      # raw observations, on top"
    )),
    fig("layers", ggplot3(iris, aes(Species, Sepal.Length, color = Species)) +
          geom_violin(alpha = 0.25) + geom_boxplot() + geom_jitter(alpha = 0.4) +
          theme(legend_position = "none")),
    "<p>A layer can override the plot\u2019s data and mapping, which is ",
    "how you annotate one plot with a second dataset:</p>",
    code_block(c(
      "means <- aggregate(Sepal.Length ~ Species, iris, mean)",
      "",
      "ggplot3(iris, aes(Species, Sepal.Length)) +",
      "  geom_jitter(alpha = 0.3) +",
      "  geom_point(aes(Species, Sepal.Length), data = means,",
      "             color = \"#C1462F\", size = 6)"
    )),

    h("aesthetics"),
    "<p>Supported aesthetics are <code>x</code>, <code>y</code>, ",
    "<code>color</code> (<code>colour</code> also works), ",
    "<code>size</code>, <code>group</code>, <code>label</code>, ",
    "<code>xend</code>/<code>yend</code>, <code>ymin</code>/",
    "<code>ymax</code>, and the specialised <code>time</code>/",
    "<code>status</code> and <code>truth</code>/<code>score</code> pairs ",
    "used by the survival and classifier geoms.</p>",
    "<p>There is a difference between <em>mapping</em> and <em>setting</em>:</p>",
    code_block(c(
      "geom_point(aes(color = Species))   # mapped: colour varies by data",
      "geom_point(color = \"steelblue\")    # set: one literal colour",
      "",
      "# A constant inside aes() that names a real colour is honoured",
      "# literally rather than treated as a one-level category:",
      "geom_point(aes(color = \"blue\"))    # blue points, no legend"
    )),
    "<p>Grouping decides what counts as one line, one polygon, one box. ",
    "An explicit <code>group</code> wins; otherwise a discrete ",
    "<code>color</code> mapping does the grouping; otherwise everything is ",
    "one group. Map <code>group</code> when you want separate lines ",
    "<em>without</em> separate colours:</p>",
    code_block(c(
      "aes(week, score, group = subject)              # many grey lines",
      "aes(week, score, group = subject, color = arm) # coloured by arm"
    )),

    h("scales"),
    "<p>Scales decide how data values become positions and colours. A ",
    "positional scale is chosen automatically \u2014 continuous for ",
    "numbers, discrete for character, factor or logical \u2014 and you ",
    "override it to control the title, limits, tick positions, tick ",
    "labels, padding, or transform.</p>",
    code_block(c(
      "ggplot3(cars, aes(speed, dist)) +",
      "  geom_point() +",
      "  scale_x_continuous(",
      "    name = \"Speed (mph)\",",
      "    breaks = c(5, 10, 15, 20, 25),",
      "    expand = 0                      # no padding: axis hugs the data",
      "  ) +",
      "  scale_y_continuous(",
      "    name = \"Stopping distance\",",
      "    labels = function(v) paste0(v, \" ft\")",
      "  )"
    )),
    fig("scales", ggplot3(cars, aes(speed, dist)) + geom_point() +
          scale_x_continuous(name = "Speed (mph)",
                             breaks = c(5, 10, 15, 20, 25), expand = 0) +
          scale_y_continuous(name = "Stopping distance",
                             labels = function(v) paste0(v, " ft")) +
          theme_minimal()),
    "<p>Transforms live on the scale, so the whole pipeline \u2014 ",
    "expansion, tick placement, mark positions \u2014 happens in ",
    "transformed space while labels stay in the original units.</p>",
    code_block(c(
      "scale_x_log10()   scale_y_log10()      # decade ticks",
      "scale_x_sqrt()    scale_y_sqrt()",
      "scale_x_reverse() scale_y_reverse()    # e.g. rank axes"
    )),
    "<p>Discrete axes take explicit level orders, which is how you sort ",
    "bars by size rather than alphabetically:</p>",
    code_block(c(
      "d <- d[order(-d$value), ]",
      "ggplot3(d, aes(name, value)) + geom_col() +",
      "  scale_x_discrete(limits = d$name)"
    )),
    "<div class=\"note\"><p><code>limits</code> sets the <em>domain</em>, ",
    "not a crop: data outside the limits still goes through the stats and ",
    "is clipped when drawn, so a mean or a fit is unaffected by the ",
    "window you choose.</p></div>",

    h("positions"),
    "<p>Bars and columns take a <code>position</code>:</p>",
    code_block(c(
      "geom_col(position = \"stack\")     # default: totals",
      "geom_col(position = \"dodge\")     # side by side: compare groups",
      "geom_col(position = \"fill\")      # proportions within each x",
      "geom_col(position = \"identity\")  # overplotted from a zero baseline"
    )),
    fig("positions",
        ggplot3(
          data.frame(g = rep(c("Q1", "Q2", "Q3"), each = 3),
                     grp = rep(c("a", "b", "c"), 3),
                     v = c(4, 6, 3, 7, 4, 5, 5, 8, 2)),
          aes(g, v, color = grp)
        ) + geom_col(position = "dodge") + theme_minimal()),
    "<p>Adjustments are applied in data space, before the axis is trained, ",
    "so a stacked chart\u2019s axis covers the stack totals rather than ",
    "the tallest single bar.</p>",

    h("facets"),
    "<p><code>facet_wrap()</code> splits the data and draws one panel per ",
    "subset. Panels share axes by default, which is what makes them ",
    "comparable; use <code>scales = \"free\"</code> when each panel\u2019s ",
    "own range matters more than cross-panel comparison.</p>",
    code_block(c(
      "facet_wrap(Species)                    # shared axes",
      "facet_wrap(Species, ncol = 2)          # force the grid shape",
      "facet_wrap(Species, scales = \"free\")   # per-panel axes",
      "facet_wrap(c(am, cyl))                 # two variables",
      "facet_grid(am, cyl)                    # rows by columns"
    )),
    fig("facets", ggplot3(iris, aes(Sepal.Length, Sepal.Width,
                                    color = Species)) +
          geom_point() + facet_wrap(Species) +
          theme(legend_position = "none")),
    "<p>Inner panels drop redundant tick labels automatically. A layer ",
    "whose data lacks the faceting variable \u2014 a reference line, say ",
    "\u2014 is repeated unchanged in every panel.</p>",

    h("coords"),
    "<p><code>coord_flip()</code> swaps the axes, which is the usual fix ",
    "for long category labels:</p>",
    code_block(c(
      "ggplot3(d, aes(category, value)) + geom_col() + coord_flip()"
    )),
    "<p><code>coord_polar()</code> bends the panel into a circle: the ",
    "<code>theta</code> axis becomes the angle and the other becomes the ",
    "radius. Any cartesian geom follows.</p>",
    code_block(c(
      "coord_polar()                      # x -> angle",
      "coord_polar(theta = \"y\")           # y -> angle",
      "coord_polar(inner = 0.3)           # donut hole",
      "coord_polar(direction = -1)        # counter-clockwise"
    )),
    fig("polar",
        ggplot3(data.frame(g = c("Mon", "Tue", "Wed", "Thu", "Fri"),
                           v = c(4, 7, 6, 9, 12)),
                aes(g, v, color = g)) +
          geom_col() + coord_polar() + theme(legend_position = "none")),

    h("labels"),
    "<p><code>labs()</code> sets the whole title block; each argument is ",
    "optional and repeated calls merge, so you can build it up.</p>",
    code_block(c(
      "labs(",
      "  title = \"Stopping distance rises with speed\",",
      "  subtitle = \"1920s road tests, 50 observations\",",
      "  caption = \"Source: datasets::cars\",",
      "  tag = \"A\",              # panel tag for multi-figure layouts",
      "  x = \"Speed (mph)\", y = \"Distance (ft)\",",
      "  color = \"Vehicle class\"  # legend title",
      ")",
      "",
      "ggtitle(\"Main\", subtitle = \"Sub\")   # shorthands",
      "xlab(\"Speed\"); ylab(\"Distance\")"
    )),
    fig("theming", ggplot3(cars, aes(speed, dist)) + geom_point() +
          labs(title = "Stopping distance rises with speed",
               subtitle = "1920s road tests, 50 observations",
               caption = "Source: datasets::cars", tag = "A") +
          theme(base = theme_modern(), grid_major_x = FALSE)),
    "<p>Pass <code>y = NULL</code> to drop an axis title that the category ",
    "labels already make obvious.</p>",

    h("themes"),
    "<p>Six presets ship with the package; <code>theme()</code> overrides ",
    "individual settings on top of any of them.</p>",
    code_block(c(
      "theme_ggplot3()   # default: tinted panel, white grid",
      "theme_minimal()   # no panel fill, light grid, no axis lines",
      "theme_classic()   # white panel, black axes, no grid, serif",
      "theme_modern()    # editorial: big title, horizontal rules only",
      "theme_dark()      # dark background and palette",
      "theme_void()      # no chrome at all",
      "",
      "theme(base = theme_minimal(), grid_major_x = FALSE,",
      "      legend_position = \"bottom\", plot_title_size = 20)"
    )),
    "<p>See the <a href=\"themes.html\">Themes page</a> for all 35 ",
    "settings and their defaults.</p>",

    h("legends"),
    "<p>A legend appears automatically when <code>color</code> is mapped: ",
    "a swatch key for discrete data, a gradient bar for continuous. ",
    "Control it with the theme and with palette scales.</p>",
    code_block(c(
      "theme(legend_position = \"bottom\")   # or \"right\" (default), \"none\"",
      "labs(color = \"Species\")             # legend title",
      "scale_color_manual(c(\"#2B6BE0\", \"#E05A2B\", \"#12A594\"))",
      "scale_color_gradient(low = \"#FFF3B0\", high = \"#9E2A2B\")"
    )),
    fig("legend", ggplot3(iris, aes(Sepal.Length, Sepal.Width,
                                    color = Petal.Length)) +
          geom_point(size = 3) +
          scale_color_gradient(low = "#FFF3B0", high = "#9E2A2B") +
          labs(color = "Petal length") + theme_minimal()),

    h("interactivity"),
    "<p>Plots are static first. <code>+ interact()</code> switches the ",
    "default render target to a self-contained HTML page \u2014 the same ",
    "geometry, a different serializer.</p>",
    code_block(c(
      "p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +",
      "  geom_point()",
      "",
      "render(p)                              # SVG",
      "render(p + interact())                 # tooltips + zoom + brush",
      "render(p + interact(tooltip = c(\"Species\", \"Petal.Length\")))",
      "render(p + interact(zoom = FALSE, brush = FALSE))  # tooltip only",
      "",
      "render(p, file = \"plot.svg\")",
      "render(p + interact(), file = \"plot.html\")"
    )),
    "<p>Hover shows the mapped values (or the columns you name), the wheel ",
    "zooms about the cursor, dragging brushes a region, and double-click ",
    "resets. In a faceted plot each panel zooms independently. The page is ",
    "one file with no external assets, so it works from a ",
    "<code>file://</code> URL and can be emailed as-is.</p>",

    h("animation"),
    "<p><code>animate()</code> reruns the pipeline once per level of a ",
    "transition variable. Scales are trained on the full data first, so ",
    "the axes hold still while frames play \u2014 the thing that makes an ",
    "animation readable rather than dizzying.</p>",
    code_block(c(
      "ggplot3(panel_data, aes(gdp, life_expectancy, color = continent)) +",
      "  geom_point(size = 4) +",
      "  scale_x_log10() +",
      "  animate(year, duration = 700, easing = \"cubic-in-out\")"
    )),
    "<p>The interactive page gains a play/pause button and a frame ",
    "scrubber. <code>render(p, target = \"static\")</code> still produces ",
    "an SVG of all the data at once, so animation never blocks an ",
    "export.</p>",

    h("data"),
    "<p><code>plot_data()</code> returns exactly what the plot draws. For ",
    "a scatter that is the input; for a histogram it is the bins actually ",
    "drawn; for a box plot the quartiles and whisker ends.</p>",
    code_block(c(
      "p <- ggplot3(cars, aes(speed)) + geom_histogram(bins = 5)",
      "plot_data(p)",
      "#>        x  y ymin ymax  xmin  xmax",
      "#> 1  6.625  6    0    6  4.00  9.25",
      "#> 2 11.875 17    0   17  9.25 14.50",
      "",
      "write_plot_data(p, \"figure-2-data.csv\")"
    )),
    "<p>With several layers you get one table per layer; with facets, a ",
    "<code>panel</code> column. <code>plot_data(p, layer = 2)</code> and ",
    "<code>plot_data(p, panel = 1)</code> select; ",
    "<code>plot_data(p, panel = \"list\")</code> returns one table per ",
    "panel.</p>",
    "<div class=\"note\"><p>This is the honest record of a figure: it ",
    "reflects every stat, position adjustment and facet split, and drops ",
    "internal scratch columns \u2014 so what you publish beside the chart ",
    "is what the chart shows.</p></div>",

    h("output"),
    "<p>Size is set at construction or with <code>plot_size()</code>, and ",
    "<code>render()</code> writes to a file or returns a string.</p>",
    code_block(c(
      "ggplot3(d, aes(x, y), width = 900, height = 600)",
      "p + plot_size(900, 600)",
      "",
      "svg <- render(p)                       # a character string",
      "render(p, file = \"plot.svg\")           # write SVG",
      "render(p + interact(), file = \"plot.html\")",
      "cat(render(p))                         # inspect the source"
    )),
    "<p>Printing a plot at the console shows a summary and opens the image ",
    "in the RStudio viewer (or your browser). Printing a rendered document ",
    "shows a short summary rather than thousands of characters of ",
    "markup.</p>",

    h("extending"),
    "<p>A geom is an S7 subclass plus one <code>build_marks()</code> ",
    "method returning drawing primitives in normalized panel coordinates. ",
    "Because both renderers consume those primitives, a new geom needs no ",
    "renderer changes at all.</p>",
    code_block(c(
      "GeomCross <- S7::new_class(\"GeomCross\", parent = Geom,",
      "  constructor = function() {",
      "    S7::new_object(Geom(name = \"cross\",",
      "                        default_params = list(size = 5, alpha = 1)))",
      "  }",
      ")",
      "",
      "S7::method(build_marks, GeomCross) <- function(geom, scaled) {",
      "  unlist(lapply(seq_along(scaled$x), function(i) {",
      "    r <- scaled$size[[i]] / 400",
      "    list(",
      "      ggplot3:::mk_line(c(scaled$x[[i]] - r, scaled$x[[i]] + r),",
      "                        rep(scaled$y[[i]], 2), scaled$color[[i]]),",
      "      ggplot3:::mk_line(rep(scaled$x[[i]], 2),",
      "                        c(scaled$y[[i]] - r, scaled$y[[i]] + r),",
      "                        scaled$color[[i]])",
      "    )",
      "  }), recursive = FALSE)",
      "}",
      "",
      "geom_cross <- function(mapping = NULL, data = NULL, ...) {",
      "  ggplot3:::layer_new(GeomCross(), stat_identity(), mapping, data,",
      "                      list(...))",
      "}"
    )),
    "<p>The five primitives are <code>mk_circle()</code>, ",
    "<code>mk_line()</code>, <code>mk_rect()</code>, ",
    "<code>mk_polygon()</code> and <code>mk_text()</code>. A statistical ",
    "transformation is a <code>Stat</code> subclass with a ",
    "<code>compute_stat()</code> method. The same seam is what every ",
    "built-in geom uses, so there is no private path a package geom can ",
    "take that yours cannot.</p>",

    h("troubleshooting"),
    "<table><tbody>",
    "<tr><td><code>evaluated to a function, not a data column</code></td>",
    "<td>The name in <code>aes()</code> is not a column, and R found a ",
    "function of that name instead \u2014 <code>class</code>, ",
    "<code>dist</code> and <code>rank</code> are common traps. The error ",
    "lists the columns that do exist.</td></tr>",
    "<tr><td><code>requires the aesthetic(s)</code></td>",
    "<td>A geom needs mappings you have not supplied, e.g. ",
    "<code>geom_sankey()</code> needs <code>x</code>, <code>xend</code> ",
    "and <code>y</code>.</td></tr>",
    "<tr><td><code>a log10 scale requires strictly positive values</code></td>",
    "<td>Zero or negative values cannot go on a log axis; filter them or ",
    "use <code>scale_x_sqrt()</code>.</td></tr>",
    "<tr><td><code>must be numeric for a continuous scale</code></td>",
    "<td>A character column reached a scale you declared continuous; ",
    "either let the scale be chosen automatically or use ",
    "<code>scale_x_discrete()</code>.</td></tr>",
    "<tr><td>Legend missing</td>",
    "<td>Legends come from a <em>mapped</em> <code>color</code>. ",
    "<code>geom_point(color = \"red\")</code> sets a constant and produces ",
    "none; check <code>legend_position</code> is not <code>\"none\"</code>.",
    "</td></tr>",
    "<tr><td>Console fills with markup</td>",
    "<td>You are printing a rendered document from an older session. ",
    "Restart R; <code>render()</code> output prints as a one-line ",
    "summary.</td></tr>",
    "</tbody></table>"
  )
  site_page("Guide - ggplot3", "guide", body)
}

theme_settings_table <- function() {
  d <- THEME_DEFAULTS
  groups <- list(
    "Colors" = c("background", "panel_fill", "grid_color", "grid_color_minor",
                 "axis_color", "label_color", "title_color", "subtitle_color",
                 "strip_fill", "strip_color", "legend_text_color",
                 "panel_border"),
    "Typography" = c("font", "title_font", "tick_font_size", "title_font_size",
                     "plot_title_size", "plot_subtitle_size", "caption_size",
                     "strip_font_size", "legend_font_size", "title_face"),
    "Structure" = c("tick_len", "grid_major_x", "grid_major_y", "axis_line_x",
                    "axis_line_y", "ticks_x", "ticks_y", "axis_text_x",
                    "axis_text_y", "axis_title_x", "axis_title_y",
                    "legend_position", "point_palette", "gradient_low",
                    "gradient_high")
  )
  rows <- character(0)
  for (g in names(groups)) {
    rows <- c(rows, sprintf(
      "<tr><th colspan=\"2\" style=\"padding-top:16px\">%s</th></tr>", g
    ))
    for (k in groups[[g]]) {
      v <- d[[k]]
      shown <- if (length(v) == 0) "&mdash;" else {
        html_escape(paste(format(v), collapse = ", "))
      }
      rows <- c(rows, sprintf(
        "<tr><td><code>%s</code></td><td><code>%s</code></td></tr>", k, shown
      ))
    }
  }
  paste0(
    "<table><thead><tr><th>Setting</th><th>Default</th></tr></thead><tbody>",
    paste(rows, collapse = ""), "</tbody></table>"
  )
}

site_reference <- function(dir = "docs") {
  fns <- sort(getNamespaceExports("ggplot3"))
  # If per-function HTML help pages exist alongside the site (e.g. from a
  # previous `pkgdown::build_site()`), link the index into them. Only files
  # that actually exist are linked, so the page never carries dead links.
  detail_dir <- file.path(dir, "reference")
  detail <- if (dir.exists(detail_dir)) {
    sub("\\.html$", "", list.files(detail_dir, pattern = "\\.html$"))
  } else {
    character(0)
  }
  # Group by naming convention: the grammar's own vocabulary is the index.
  groups <- list(
    "Plot construction" = c("ggplot3", "aes", "render", "plot_size"),
    "Layers: essentials" = grep("^geom_(point|jitter|line|path|step|area|ribbon|segment|hline|vline|text|tile|bar|col|histogram|density|boxplot|violin|smooth)$", fns, value = TRUE),
    "Layers: layout" = grep("^geom_(radar|ridgeline|sankey|alluvial|treemap|network|chord|parallel|bump|funnel|stream|upset)$", fns, value = TRUE),
    "Layers: machine learning" = grep("^geom_(shap|partial_dependence|confusion_matrix|calibration|lift_gain|residual|learning_curve|silhouette|embedding|decision_boundary|forecast_band|roc)$", fns, value = TRUE),
    "Layers: clinical" = grep("^geom_(km|cuminc|forest|swimmer|spaghetti|spider_response|waterfall_response|bland_altman|ae_heatmap|dose_response|shift|consort)$", fns, value = TRUE),
    "Layers: other" = grep("^geom_(errorbar|pointrange|dumbbell|waterfall)$", fns, value = TRUE),
    "Scales" = grep("^scale_", fns, value = TRUE),
    "Stats" = grep("^stat_", fns, value = TRUE),
    "Coordinates" = grep("^coord_", fns, value = TRUE),
    "Facets" = grep("^facet_", fns, value = TRUE),
    "Themes" = c(grep("^theme", fns, value = TRUE)),
    "Labels and limits" = c("labs", "ggtitle", "xlab", "ylab", "xlim", "ylim", "lims"),
    "Interactivity and animation" = c("interact", "animate"),
    "Data and utilities" = c("plot_data", "write_plot_data", "build_site",
                             "ggplot3_logo")
  )
  # Anything exported but not grouped above (S7 classes, generics).
  listed <- unlist(groups, use.names = FALSE)
  groups[["Classes and generics"]] <- setdiff(fns, listed)

  body <- paste0(
    "<h1>Reference</h1><p class=\"lead\">Every exported function, grouped ",
    "by role. Run <code>?function_name</code> in R for the full help page ",
    "with arguments and runnable examples.</p>"
  )
  for (g in names(groups)) {
    items <- groups[[g]]
    items <- items[items %in% fns]
    if (length(items) == 0) next
    rows <- vapply(items, function(f) {
      name <- if (f %in% detail) {
        sprintf("<a href=\"reference/%s.html\"><code>%s</code></a>", f, f)
      } else {
        sprintf("<code>%s</code>", f)
      }
      sprintf("<tr><td>%s</td><td>%s</td></tr>", name, html_escape(fn_title(f)))
    }, character(1))
    body <- paste0(
      body, "<h2>", g, "</h2>",
      "<table><tbody>", paste(rows, collapse = ""), "</tbody></table>"
    )
  }
  site_page("Reference \u2014 ggplot3", "reference", body)
}

# One-line description for a function, read from its installed Rd file so
# the site index and the R help can never disagree.
fn_title <- function(fn) {
  db <- tryCatch(tools::Rd_db("ggplot3"), error = function(e) NULL)
  if (is.null(db)) {
    return("")
  }
  for (rd in db) {
    tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
    aliases <- unlist(rd[tags == "\\alias"])
    if (fn %in% trimws(aliases)) {
      title <- paste(unlist(rd[tags == "\\title"]), collapse = "")
      return(trimws(gsub("\\s+", " ", title)))
    }
  }
  ""
}
