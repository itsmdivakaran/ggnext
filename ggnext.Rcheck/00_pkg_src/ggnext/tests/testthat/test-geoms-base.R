# The additional core geoms that close the ggplot2 gap.

test_that("geom_rect draws one rectangle per row from explicit corners", {
  d <- data.frame(x1 = c(1, 3), x2 = c(2, 5), y1 = c(1, 2), y2 = c(4, 3))
  m <- marks_of(ggnext(d, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2)) +
                  geom_rect())
  expect_length(m, 2)
  expect_true(all(vapply(m, function(k) k$type, character(1)) == "rect"))
  # Corners follow the data: the second rectangle is the wider one.
  w <- vapply(m, function(k) abs(k$x1 - k$x0), numeric(1))
  expect_gt(w[2], w[1])
})

test_that("geom_polygon closes one shape per group", {
  d <- data.frame(x = c(1, 3, 2, 4, 6, 5), y = c(1, 1, 3, 1, 1, 3),
                  g = rep(c("a", "b"), each = 3))
  m <- marks_of(ggnext(d, aes(x, y, color = g)) + geom_polygon())
  expect_length(m, 2)
  expect_equal(unique(vapply(m, function(k) k$type, character(1))), "polygon")
  expect_length(m[[1]]$xs, 3)
})

test_that("geom_raster draws tiles without borders", {
  g <- expand.grid(x = 1:5, y = 1:5)
  g$z <- seq_len(25)
  m <- marks_of(ggnext(g, aes(x, y, color = z)) + geom_raster())
  expect_length(m, 25)
  expect_equal(unique(vapply(m, function(k) k$stroke_width, numeric(1))), 0)
})

test_that("geom_abline spans the panel and does not train the scales", {
  d <- data.frame(a = 1:10, b = 1:10)
  plain <- panel_of(ggnext(d, aes(a, b)) + geom_point())
  withab <- panel_of(ggnext(d, aes(a, b)) + geom_point() + geom_abline(0, 1))
  # A reference line must never widen the axes it is drawn against.
  expect_identical(plain$x$domain, withab$x$domain)
  expect_identical(plain$y$domain, withab$y$domain)
  # On 1:1 data the identity line runs corner to corner.
  line <- withab$layers[[2]]$marks[[1]]
  expect_equal(unlist(line$xs), c(0, 1))
  expect_equal(unlist(line$ys), c(0, 1))
})

test_that("geom_abline respects intercept and slope", {
  b <- panel_of(ggnext(cars, aes(speed, dist)) + geom_point() +
                  geom_abline(intercept = 10, slope = 2))
  line <- b$layers[[2]]$marks[[1]]
  expected <- ((10 + 2 * b$x$domain) - b$y$domain[1]) /
    diff(b$y$domain)
  expect_equal(unlist(line$ys), expected)
})

test_that("geom_blank draws nothing but still trains the scales", {
  b <- panel_of(ggnext(cars, aes(speed, dist)) + geom_blank())
  expect_length(b$layers[[1]]$marks, 0)
  expect_equal(b$x$domain, ggnext:::expand_domain(range(cars$speed)))
})

test_that("geom_label draws a plate behind every label", {
  d <- data.frame(x = c(1, 2), y = c(2, 1), l = c("alpha", "beta"))
  m <- marks_of(ggnext(d, aes(x, y, label = l)) + geom_label())
  types <- vapply(m, function(k) k$type, character(1))
  expect_equal(sum(types == "rect"), 2)
  expect_equal(sum(types == "text"), 2)
  # The plate is drawn first so the text sits on top.
  expect_equal(types[1], "rect")
})

test_that("geom_linerange and geom_crossbar draw their intervals", {
  d <- data.frame(g = c("a", "b"), m = c(2, 3), lo = c(1, 2), hi = c(4, 5))
  lr <- marks_of(ggnext(d, aes(g, ymin = lo, ymax = hi)) + geom_linerange())
  expect_length(lr, 2)
  expect_equal(unique(vapply(lr, function(k) k$type, character(1))), "line")

  cb <- marks_of(ggnext(d, aes(g, m, ymin = lo, ymax = hi)) + geom_crossbar())
  types <- vapply(cb, function(k) k$type, character(1))
  expect_equal(sum(types == "rect"), 2)   # the boxes
  expect_equal(sum(types == "line"), 2)   # the estimate lines
})

test_that("geom_errorbarh draws horizontal intervals with caps", {
  d <- data.frame(g = c("a", "b"), lo = c(1, 2), hi = c(4, 5))
  m <- marks_of(ggnext(d, aes(y = g, xmin = lo, xmax = hi)) +
                  geom_errorbarh())
  # Per row: the interval plus two caps.
  expect_length(m, 6)
})

test_that("geom_rug puts ticks on the requested sides only", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  bl <- marks_of(p + geom_rug(), layer = 2)
  b_only <- marks_of(p + geom_rug(sides = "b"), layer = 2)
  expect_equal(length(bl), 2 * nrow(cars))       # bottom + left
  expect_equal(length(b_only), nrow(cars))       # bottom only
})

test_that("geom_curve bows away from the straight chord", {
  d <- data.frame(x = 1, y = 1, xe = 3, ye = 3)
  curved <- marks_of(ggnext(d, aes(x, y, xend = xe, yend = ye)) +
                       geom_curve())[[1]]
  straight <- marks_of(ggnext(d, aes(x, y, xend = xe, yend = ye)) +
                         geom_segment())[[1]]
  expect_gt(length(curved$xs), length(straight$xs))
  # The midpoint of the curve is off the chord.
  mid <- length(curved$xs) %/% 2
  expect_false(isTRUE(all.equal(curved$xs[[mid]], curved$ys[[mid]])))
})

test_that("geom_spoke turns angle and radius into endpoints", {
  d <- data.frame(x = 0, y = 0, angle = 0, radius = 1)
  v <- compute_stat(ggnext:::StatSpoke(),
                    list(x = 0, y = 0, xend = 0, yend = 1,
                         group = "all"))
  # angle 0, radius 1 points straight along +x.
  expect_equal(v$xend, 1)
  expect_equal(v$yend, 0)
})

test_that("geom_count sizes points by how many share a position", {
  d <- data.frame(x = c(1, 1, 1, 2, 2, 3), y = c(1, 1, 2, 2, 2, 3))
  m <- marks_of(ggnext(d, aes(x, y)) + geom_count())
  expect_length(m, 4)  # (1,1)x2, (1,2), (2,2)x2, (3,3)
  radii <- vapply(m, function(k) k$r, numeric(1))
  expect_gt(max(radii), min(radii))
})

test_that("geom_freqpoly bins like a histogram but draws a line", {
  m <- marks_of(ggnext(cars, aes(speed)) + geom_freqpoly(bins = 8))
  expect_length(m, 1)
  expect_equal(m[[1]]$type, "line")
  expect_length(m[[1]]$xs, 8)
})

test_that("geom_function evaluates the function over its range", {
  m <- marks_of(ggnext(data.frame(x = c(-3, 3)), aes(x)) +
                  geom_function(dnorm, xlim = c(-3, 3), n = 51))
  expect_length(m[[1]]$xs, 51)
  # A normal density peaks in the middle.
  ys <- unlist(m[[1]]$ys)
  expect_equal(which.max(ys), 26)
})

# --- distribution diagnostics ------------------------------------------------

test_that("geom_qq plots sample against theoretical quantiles", {
  set.seed(1)
  d <- data.frame(v = stats::rnorm(100))
  vals <- compute_stat(ggnext:::StatQQ(),
                       list(x = d$v, group = rep("all", 100)))
  expect_equal(vals$x, stats::qnorm(stats::ppoints(100)))
  expect_equal(vals$y, sort(d$v))
})

test_that("geom_qq_line passes through both quartiles", {
  set.seed(2)
  s <- stats::rnorm(200)
  vals <- compute_stat(ggnext:::StatQQ(line = TRUE),
                       list(x = s, group = rep("all", 200)))
  sq <- stats::quantile(s, c(0.25, 0.75), names = FALSE)
  tq <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(sq) / diff(tq)
  intercept <- sq[1] - slope * tq[1]
  expect_equal(vals$y, intercept + slope * vals$x)
})

test_that("geom_dotplot stacks one dot per observation", {
  m <- marks_of(ggnext(cars, aes(speed)) + geom_dotplot(binwidth = 2))
  expect_length(m, nrow(cars))
  # Dots in the same bin stack to successive heights starting at 1.
  d <- plot_data(ggnext(cars, aes(speed)) + geom_dotplot(binwidth = 2))
  expect_equal(min(d$y), 1)
  expect_true(all(d$y == floor(d$y)))
})

test_that("quantile regression brackets the data and orders by tau", {
  set.seed(3)
  d <- data.frame(x = runif(200, 0, 10))
  d$y <- 2 * d$x + stats::rnorm(200, 0, 3)
  vals <- compute_stat(ggnext:::StatQuantile(quantiles = c(0.1, 0.5, 0.9)),
                       list(x = d$x, y = d$y, group = rep("all", nrow(d))))
  # At any x the fitted quantiles must be ordered.
  by_tau <- split(vals$y, vals$group)
  expect_true(all(by_tau[["tau=0.1"]] < by_tau[["tau=0.5"]]))
  expect_true(all(by_tau[["tau=0.5"]] < by_tau[["tau=0.9"]]))
  # The median fit should sit near the least-squares fit for symmetric noise.
  lm_fit <- stats::coef(stats::lm(y ~ x, d))
  rq_med <- ggnext:::rq_fit(d$x, d$y, 0.5)
  expect_equal(unname(rq_med[2]), lm_fit[[2]], tolerance = 0.15)
})

test_that("every new geom renders to both targets", {
  set.seed(1)
  plots <- list(
    rect = ggnext(data.frame(x1 = 1, x2 = 2, y1 = 1, y2 = 2),
                   aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2)) +
      geom_rect(),
    polygon = ggnext(data.frame(x = c(1, 3, 2), y = c(1, 1, 3)), aes(x, y)) +
      geom_polygon(),
    abline = ggnext(cars, aes(speed, dist)) + geom_point() + geom_abline(0, 3),
    blank = ggnext(cars, aes(speed, dist)) + geom_blank(),
    label = ggnext(data.frame(x = 1, y = 1, l = "hi"), aes(x, y, label = l)) +
      geom_label(),
    linerange = ggnext(data.frame(g = "a", lo = 1, hi = 2),
                        aes(g, ymin = lo, ymax = hi)) + geom_linerange(),
    crossbar = ggnext(data.frame(g = "a", m = 1.5, lo = 1, hi = 2),
                       aes(g, m, ymin = lo, ymax = hi)) + geom_crossbar(),
    errorbarh = ggnext(data.frame(g = "a", lo = 1, hi = 2),
                        aes(y = g, xmin = lo, xmax = hi)) + geom_errorbarh(),
    rug = ggnext(cars, aes(speed, dist)) + geom_point() + geom_rug(),
    curve = ggnext(data.frame(x = 1, y = 1, xe = 3, ye = 3),
                    aes(x, y, xend = xe, yend = ye)) + geom_curve(),
    count = ggnext(data.frame(x = c(1, 1, 2), y = c(1, 1, 2)), aes(x, y)) +
      geom_count(),
    freqpoly = ggnext(cars, aes(speed)) + geom_freqpoly(bins = 6),
    fn = ggnext(data.frame(x = c(-2, 2)), aes(x)) +
      geom_function(dnorm, xlim = c(-2, 2)),
    qq = ggnext(data.frame(v = stats::rnorm(50)), aes(v)) +
      geom_qq() + geom_qq_line(),
    dotplot = ggnext(cars, aes(speed)) + geom_dotplot(binwidth = 3),
    quantile = ggnext(cars, aes(speed, dist)) + geom_quantile(),
    raster = ggnext(local({
      g <- expand.grid(x = 1:5, y = 1:5); g$z <- seq_len(25); g
    }), aes(x, y, color = z)) + geom_raster()
  )
  for (nm in names(plots)) {
    expect_match(render(plots[[nm]]), "^<svg ", label = nm)
    expect_match(render(plots[[nm]], target = "interactive"),
                 "^<!DOCTYPE html>", label = nm)
  }
})
