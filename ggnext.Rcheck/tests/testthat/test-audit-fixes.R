# Regressions for defects found in the correctness audit. Each test names
# the behaviour that was wrong, so a reintroduction fails loudly.

# --- coord_flip must swap the axes, not just the marks ----------------------

test_that("coord_flip() swaps axis titles, ticks and labels", {
  d <- data.frame(g = c("alpha", "beta", "gamma"), v = c(3, 7, 5))
  flipped <- panel_of(ggnext(d, aes(g, v)) + geom_col() + coord_flip())
  plain <- panel_of(ggnext(d, aes(g, v)) + geom_col())

  # The category axis has to end up where the categories are drawn.
  expect_equal(flipped$x$title, "v")
  expect_equal(flipped$y$title, "g")
  expect_equal(unlist(flipped$y$ticks$labels), c("alpha", "beta", "gamma"))
  # ...and the unflipped plot is untouched.
  expect_equal(plain$x$title, "g")
  expect_equal(plain$y$title, "v")
})

test_that("flipped bars vary along x and sit in equal category slots", {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 7, 5))
  m <- marks_of(ggnext(d, aes(g, v)) + geom_col() + coord_flip())
  widths <- vapply(m, function(k) abs(k$x1 - k$x0), numeric(1))
  heights <- vapply(m, function(k) abs(k$y1 - k$y0), numeric(1))
  expect_gt(length(unique(round(widths, 6))), 1)   # value axis
  expect_equal(length(unique(round(heights, 6))), 1) # category slots
})

# --- stat-built geoms must honour their styling arguments -------------------

test_that("geoms whose stat builds the marks still honour layer params", {
  sk <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"),
                   n = c(4, 3, 7))
  ribbon_alpha <- function(a) {
    m <- marks_of(ggnext(sk, aes(x = from, xend = to, y = n)) +
                    geom_sankey(alpha = a))
    unique(vapply(Filter(function(k) k$type == "polygon", m),
                  function(k) k$alpha, numeric(1)))
  }
  expect_true(0.05 %in% ribbon_alpha(0.05))
  expect_false(0.05 %in% ribbon_alpha(0.9))

  # label = FALSE really removes the node labels.
  labelled <- marks_of(ggnext(sk, aes(x = from, xend = to, y = n)) +
                         geom_sankey(label = TRUE))
  bare <- marks_of(ggnext(sk, aes(x = from, xend = to, y = n)) +
                     geom_sankey(label = FALSE))
  expect_gt(sum(vapply(labelled, function(k) k$type == "text", logical(1))), 0)
  expect_equal(sum(vapply(bare, function(k) k$type == "text", logical(1))), 0)

  net <- data.frame(f = c("A", "B", "C"), t = c("B", "C", "A"))
  node_r <- function(s) {
    m <- marks_of(ggnext(net, aes(x = f, xend = t)) +
                    geom_network(node_size = s))
    unique(vapply(Filter(function(k) k$type == "circle", m),
                  function(k) k$r, numeric(1)))
  }
  expect_equal(node_r(3), 3)
  expect_equal(node_r(12), 12)

  cs <- data.frame(stage = c("A", "B"), n = c(10, 5))
  fills <- marks_of(ggnext(cs, aes(label = stage, size = n)) +
                      geom_consort(box_fill = "#FF0000"))
  expect_true("#FF0000" %in% vapply(
    Filter(function(k) k$type == "rect", fills), function(k) k$fill,
    character(1)
  ))
})

# --- non-finite values ------------------------------------------------------

test_that("non-finite positions are dropped with a warning, not drawn", {
  d <- data.frame(x = c(1, NA, 3, 4), y = c(1, 2, NA, 4))
  expect_warning(
    svg <- render(ggnext(d, aes(x, y)) + geom_point()),
    "Removed 2 rows containing non-finite values"
  )
  expect_no_match(svg, "NA")
  expect_equal(length(gregexpr("<circle ", svg, fixed = TRUE)[[1]]), 2)
})

test_that("Inf is dropped rather than crashing the tick algorithm", {
  d <- data.frame(x = c(1, 2, Inf), y = 1:3)
  expect_warning(svg <- render(ggnext(d, aes(x, y)) + geom_point()),
                 "non-finite")
  expect_match(svg, "^<svg ")
  expect_no_match(svg, "NaN")
})

test_that("interval columns may still legitimately hold NA", {
  # geom_forecast_band() uses NA in ymin/ymax to mean "no interval here",
  # so the drop must not touch those columns.
  d <- data.frame(
    t = 1:6, v = 1:6,
    lo = c(NA, NA, NA, 3, 4, 5), hi = c(NA, NA, NA, 7, 8, 9),
    part = rep(c("a", "f"), each = 3)
  )
  expect_silent(
    svg <- render(ggnext(d, aes(t, v, ymin = lo, ymax = hi, group = part)) +
                    geom_forecast_band())
  )
  expect_match(svg, "^<svg ")
})

# --- small-sample guards ----------------------------------------------------

test_that("density geoms name the geom and the group when a group is too small", {
  d <- data.frame(g = c("a", "a", "a", "b"), y = c(1, 2, 3, 9))
  expect_error(render(ggnext(d, aes(y, color = g)) + geom_density()),
               "geom_density\\(\\).*group `b`.*at least 2")
  expect_error(render(ggnext(d, aes(g, y)) + geom_violin()),
               "geom_violin\\(\\).*at least 2")
  expect_error(render(ggnext(d, aes(y, g)) + geom_ridgeline()),
               "geom_ridgeline\\(\\).*at least 2")
})

test_that("geom_smooth refuses a fit it cannot compute", {
  expect_error(
    render(ggnext(data.frame(x = 1:2, y = c(2, 4)), aes(x, y)) +
             geom_smooth(method = "lm")),
    "too few for method = \"lm\""
  )
  expect_error(
    render(ggnext(data.frame(x = 1:2, y = c(2, 4)), aes(x, y)) +
             geom_smooth()),
    "too few for method = \"loess\""
  )
  # And a normal fit is unaffected, with no NaN in the band.
  svg <- render(ggnext(cars, aes(speed, dist)) + geom_smooth(method = "lm"))
  expect_no_match(svg, "NaN")
})

# --- histogram bin count ----------------------------------------------------

test_that("bins = n yields exactly n bins, including empty ones", {
  # Sparse data used to collapse to however many bins happened to be
  # occupied; a gap in a histogram is information.
  sparse <- data.frame(z = c(1, 1, 1, 2, 10))
  expect_equal(nrow(plot_data(ggnext(sparse, aes(z)) +
                                geom_histogram(bins = 10))), 10)
  set.seed(1)
  wide <- data.frame(z = stats::rnorm(500))
  for (b in c(5, 10, 30)) {
    d <- plot_data(ggnext(wide, aes(z)) + geom_histogram(bins = b))
    expect_equal(nrow(d), b)
    expect_equal(sum(d$y), 500)
  }
})

# --- escaping ---------------------------------------------------------------

test_that("SVG attribute values are escaped", {
  skip_if_not_installed("xml2")
  quoted <- render(ggnext(cars, aes(speed, dist)) + geom_point() +
                     theme(font = 'My "Font", sans'))
  expect_s3_class(xml2::read_xml(quoted), "xml_document")

  apos <- render(ggnext(cars, aes(speed, dist)) + geom_point() +
                   theme(font = "Foo's Sans"))
  expect_s3_class(xml2::read_xml(apos), "xml_document")

  cat_quote <- render(ggnext(data.frame(g = c('a"b', "c"), v = 1:2),
                              aes(g, v)) + geom_col())
  expect_s3_class(xml2::read_xml(cat_quote), "xml_document")
})

test_that("the JSON writer emits valid JSON for every numeric edge case", {
  expect_equal(ggnext:::to_json(Inf), "null")
  expect_equal(ggnext:::to_json(-Inf), "null")
  expect_equal(ggnext:::to_json(NaN), "null")
  # A tiny magnitude keeps scientific notation instead of 300 characters.
  expect_lt(nchar(ggnext:::to_json(1e-300)), 30)
  # C0 control characters become \uXXXX escapes.
  expect_equal(ggnext:::to_json("a\001b"), "\"a\\u0001b\"")
})

# --- arguments that used to do nothing --------------------------------------

test_that("geom_spider_response(thresholds =) draws the RECIST lines", {
  d <- data.frame(m = rep(c(0, 2, 4), 2), p = c(0, -20, -35, 0, 10, 25),
                  s = rep(c("A", "B"), each = 3))
  with_thresh <- marks_of(ggnext(d, aes(m, p, color = s)) +
                            geom_spider_response(thresholds = TRUE))
  without <- marks_of(ggnext(d, aes(m, p, color = s)) +
                        geom_spider_response(thresholds = FALSE))
  expect_equal(length(with_thresh) - length(without), 2)
})

test_that("geom_smooth(alpha =) sets the confidence band opacity", {
  band_alpha <- function(a) {
    marks_of(ggnext(cars, aes(speed, dist)) +
               geom_smooth(method = "lm", alpha = a))[[1]]$alpha
  }
  expect_equal(band_alpha(0.05), 0.05)
  expect_equal(band_alpha(0.8), 0.8)
})

test_that("theme(grid_color_minor =) draws minor gridlines", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  plain <- render(p)
  minor <- render(p + theme(grid_color_minor = "#00FF00"))
  expect_false(identical(as.character(plain), as.character(minor)))
  expect_match(minor, "#00FF00", fixed = TRUE)
  # Both targets learn about them from the same buffer.
  expect_match(render(p, target = "interactive"), "\"minor\":", fixed = TRUE)
})

test_that("gradient endpoints can be set independently", {
  p <- ggnext(cars, aes(speed, dist, color = dist)) + geom_point()
  expect_false(identical(
    as.character(render(p)),
    as.character(render(p + theme(gradient_low = "#000000")))
  ))
  expect_false(identical(
    as.character(render(p)),
    as.character(render(p + theme(gradient_high = "#FFFFFF")))
  ))
})

# --- validation -------------------------------------------------------------

test_that("enum arguments reject invalid values", {
  expect_error(geom_lift_gain(type = "zzz"), "should be one of")
  expect_error(geom_text(anchor = "zzz"), "should be one of")
})

test_that("a labels vector must match the number of breaks", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  expect_error(
    render(p + scale_x_continuous(breaks = c(1, 5, 9), labels = "one")),
    "`labels` has 1 value but there are"
  )
  expect_silent(
    render(p + scale_x_continuous(breaks = c(5, 15, 25),
                                  labels = c("a", "b", "c")))
  )
})

test_that("a stat may supply an aesthetic the geom requires", {
  # Validation used to run before the stat, so this was rejected even
  # though stat_bin() computes `y`.
  set.seed(1)
  d <- data.frame(z = stats::rnorm(200))
  expect_match(render(ggnext(d, aes(z)) + geom_point(stat = stat_bin(bins = 8))),
               "^<svg ")
  expect_match(
    render(ggnext(data.frame(g = c("a", "a", "b")), aes(g)) +
             geom_point(stat = stat_count())),
    "^<svg "
  )
  # A genuinely missing aesthetic is still caught, with advice.
  expect_error(render(ggnext(cars, aes(speed)) + geom_point()),
               "requires the aesthetic\\(s\\): y")
})
