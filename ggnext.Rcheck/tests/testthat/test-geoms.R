# The geom catalog: mark output and the statistical math behind it.

# marks_of() / panel_of() come from helper-buffer.R.
mark_types <- function(marks) vapply(marks, function(m) m$type, character(1))

test_that("line geoms emit one polyline per group", {
  d <- data.frame(x = rep(1:10, 2), y = rnorm(20), g = rep(c("a", "b"), each = 10))
  m <- marks_of(ggnext(d, aes(x, y, color = g)) + geom_line())
  expect_length(m, 2)
  expect_equal(mark_types(m), c("line", "line"))
  # Points are ordered by x within each group.
  expect_false(is.unsorted(unlist(m[[1]]$xs)))
})

test_that("geom_step interleaves horizontal-then-vertical corners", {
  d <- data.frame(x = c(1, 2, 3), y = c(1, 3, 2))
  m <- marks_of(ggnext(d, aes(x, y)) + geom_step())
  expect_length(m, 1)
  # 3 points -> 5 vertices (each new point contributes a corner + itself).
  expect_length(m[[1]]$xs, 5)
  # The corner between points 1 and 2 keeps y constant while x advances.
  expect_equal(m[[1]]$ys[[1]], m[[1]]$ys[[2]])
  expect_equal(m[[1]]$xs[[2]], m[[1]]$xs[[3]])
})

test_that("geom_area closes its polygon on the zero baseline", {
  d <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 4))
  buffer <- ggnext:::build_geometry(ggnext(d, aes(x, y)) + geom_area())
  m <- buffer$panels[[1]]$layers[[1]]$marks[[1]]
  expect_equal(m$type, "polygon")
  expect_length(m$xs, 10) # 5 top + 5 baseline vertices
  # The baseline sits at data-y 0, normalized through the y domain.
  norm0 <- ggnext:::normalize(0, buffer$panels[[1]]$y$domain)
  expect_equal(m$ys[[10]], norm0)
})

test_that("geom_segment and geom_dumbbell draw per-row primitives", {
  d <- data.frame(a = 1:3, b = 1:3, c = 2:4, d = 0:2)
  m <- marks_of(ggnext(d, aes(a, b, xend = c, yend = d)) + geom_segment())
  expect_equal(mark_types(m), rep("line", 3))

  dd <- data.frame(lo = c(1, 2), hi = c(3, 5), g = c("u", "v"))
  m2 <- marks_of(ggnext(dd, aes(lo, xend = hi, y = g)) + geom_dumbbell())
  # Per row: connector + two endpoint dots.
  expect_equal(sum(mark_types(m2) == "line"), 2)
  expect_equal(sum(mark_types(m2) == "circle"), 4)
})

test_that("reference lines span the panel and ignore plot aes", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point() + geom_hline(50)
  m <- marks_of(p, layer = 2)
  expect_length(m, 1)
  expect_equal(unlist(m[[1]]$xs), c(0, 1)) # full panel width, normalized
})

test_that("stat_count counts and stacked bars accumulate", {
  d <- data.frame(g = c("a", "a", "a", "b", "b"), grp = c("u", "u", "v", "u", "v"))
  # Ungrouped: bar heights are the category counts.
  values <- list(x = c(1, 1, 1, 2, 2), group = rep("all", 5))
  out <- compute_stat(stat_count(), values)
  expect_equal(out$y[order(out$x)], c(3, 2))

  # Grouped + stacked: the top of each stack is the total count.
  p <- ggnext(d, aes(g, color = grp)) + geom_bar()
  buffer <- ggnext:::build_geometry(p)
  m <- buffer$panels[[1]]$layers[[1]]$marks
  expect_equal(mark_types(m), rep("rect", 4))
  tops <- vapply(m, function(mk) max(mk$y0, mk$y1), numeric(1))
  # Denormalize the tallest stack top back to data units: must equal 3.
  expect_equal(
    max(tops) * (buffer$panels[[1]]$y$domain[2] - buffer$panels[[1]]$y$domain[1]) + buffer$panels[[1]]$y$domain[1],
    3
  )
})

test_that("position = 'dodge' narrows bars and keeps them side by side", {
  d <- data.frame(g = rep(c("a", "b"), each = 4), grp = rep(c("u", "v"), 4))
  p <- ggnext(d, aes(g, color = grp)) + geom_bar(position = "dodge")
  m <- marks_of(p)
  expect_length(m, 4)
  widths <- vapply(m, function(mk) abs(mk$x1 - mk$x0), numeric(1))
  # All dodged bars share one width; two bars per slot never overlap.
  expect_equal(length(unique(round(widths, 10))), 1)
})

test_that("stat_bin preserves the total count", {
  x <- rnorm(500)
  out <- compute_stat(stat_bin(bins = 17), list(x = x, group = rep("all", 500)))
  expect_equal(sum(out$y), 500)
  expect_true(all(diff(out$x) > 0))
})

test_that("stat_density integrates to ~1", {
  out <- compute_stat(
    stat_density(),
    list(x = rnorm(400), group = rep("all", 400))
  )
  dx <- diff(out$x[1:2])
  expect_equal(sum(out$y) * dx, 1, tolerance = 0.02)
})

test_that("stat_boxplot computes Tukey's five numbers and outliers", {
  y <- c(1:10, 100) # 100 is an outlier
  out <- compute_stat(
    stat_boxplot(),
    list(x = rep(1, 11), y = y, group = rep("all", 11))
  )
  box <- which(out$role == "box")
  expect_length(box, 1)
  expect_equal(out$lower[box], stats::quantile(y, 0.25, names = FALSE))
  expect_equal(out$middle[box], stats::quantile(y, 0.5, names = FALSE))
  expect_equal(out$upper[box], stats::quantile(y, 0.75, names = FALSE))
  expect_equal(out$y[out$role == "outlier"], 100)
  expect_equal(out$ymax[box], 10) # whisker stops at the furthest inlier
})

test_that("geom_boxplot renders box, whiskers, median, and outliers", {
  y <- c(1:10, 100)
  d <- data.frame(g = rep("a", 11), y = y)
  m <- marks_of(ggnext(d, aes(g, y)) + geom_boxplot())
  tab <- table(mark_types(m))
  expect_equal(unname(tab["rect"]), 1) # the box
  expect_equal(unname(tab["circle"]), 1) # the outlier
  expect_equal(unname(tab["line"]), 5) # 2 stems + 2 caps + median
})

test_that("geom_violin emits one mirrored polygon per slot", {
  m <- marks_of(ggnext(iris, aes(Species, Sepal.Width)) + geom_violin())
  expect_equal(mark_types(m), rep("polygon", 3))
  # Mirrored: polygon x range is symmetric about the slot center.
  poly <- m[[1]]
  xs <- unlist(poly$xs)
  n_half <- length(xs) / 2
  center <- mean(range(xs))
  expect_equal(xs[1:n_half] + rev(xs[(n_half + 1):(2 * n_half)]),
               rep(2 * center, n_half), tolerance = 1e-9)
})

test_that("stat_smooth lm matches lm() and bands bracket the fit", {
  out <- compute_stat(
    stat_smooth(method = "lm"),
    list(x = cars$speed, y = cars$dist, group = rep("all", nrow(cars)))
  )
  fit <- stats::lm(dist ~ speed, data = cars)
  expect_equal(
    out$y,
    unname(stats::predict(fit, data.frame(speed = out$x))),
    tolerance = 1e-8
  )
  expect_true(all(out$ymin < out$y & out$y < out$ymax))
})

test_that("stat_jitter is deterministic and restores the RNG state", {
  values <- list(x = rep(1:3, 5), y = rnorm(15), group = rep("all", 15))
  set.seed(999)
  before <- .Random.seed
  out1 <- compute_stat(stat_jitter(), values)
  expect_identical(.Random.seed, before) # global stream untouched
  out2 <- compute_stat(stat_jitter(), values)
  expect_identical(out1$x, out2$x) # same seed, same jitter
  expect_false(identical(out1$x, values$x))
})

test_that("stat_km reproduces the product-limit estimate by hand", {
  # 4 subjects: events at t=1,2,4; censored at t=3.
  # S(1) = 3/4; S(2) = 3/4 * 2/3 = 1/2; at t=4 one at risk: S(4) = 0.
  values <- list(
    time = c(1, 2, 3, 4), status = c(1, 1, 0, 1), group = rep("all", 4)
  )
  out <- compute_stat(stat_km(), values)
  curve <- which(out$role == "curve")
  # Survival level after each drop, read at the drop x positions.
  drops <- unique(out$x[curve][diff(c(1, out$y[curve])) < 0])
  s_after <- vapply(
    c(1, 2, 4),
    function(t) min(out$y[curve][out$x[curve] == t]),
    numeric(1)
  )
  expect_equal(s_after, c(0.75, 0.5, 0))
  # One censor tick, at the survival level in effect (0.5).
  cens <- which(out$role == "censor")
  expect_length(cens, 1)
  expect_equal(out$x[cens], 3)
  expect_equal(out$y[cens], 0.5)
})

test_that("stat_roc yields the exact staircase for a perfect classifier", {
  values <- list(
    truth = c(1, 1, 0, 0), score = c(0.9, 0.8, 0.2, 0.1),
    group = rep("all", 4)
  )
  out <- compute_stat(stat_roc(), values)
  curve <- which(out$role == "curve")
  # Perfect ranking: TPR reaches 1 while FPR is still 0.
  expect_equal(out$x[curve], c(0, 0, 0, 0.5, 1))
  expect_equal(out$y[curve], c(0, 0.5, 1, 1, 1))
})

test_that("stat_waterfall accumulates running totals with signs", {
  values <- list(
    x = 1:4, y = c(100, -30, 20, -10), group = rep("all", 4)
  )
  out <- compute_stat(stat_waterfall(), values)
  expect_equal(out$yend, c(100, 70, 90, 80))
  expect_equal(out$sign, c("pos", "neg", "pos", "neg"))
  expect_equal(out$ymin, c(0, 70, 70, 80))
  expect_equal(out$ymax, c(100, 100, 90, 90))
})

test_that("geom_waterfall draws bars plus connectors", {
  wf <- data.frame(
    step = factor(c("s", "a", "b"), levels = c("s", "a", "b")),
    delta = c(10, 5, -3)
  )
  m <- marks_of(ggnext(wf, aes(step, delta)) + geom_waterfall())
  tab <- table(mark_types(m))
  expect_equal(unname(tab["rect"]), 3)
  expect_equal(unname(tab["line"]), 2) # n - 1 connectors
})

test_that("discrete x scales train levels and label ticks with them", {
  d <- data.frame(g = c("b", "a", "b", "c"), v = 1:4)
  buffer <- ggnext:::build_geometry(ggnext(d, aes(g, v)) + geom_point())
  expect_equal(unlist(buffer$panels[[1]]$x$ticks$labels), c("a", "b", "c")) # sorted
  expect_equal(buffer$panels[[1]]$x$ticks$values, 1:3)
  expect_equal(buffer$panels[[1]]$x$domain, c(0.4, 3.6)) # 0.6-slot expansion

  # Factors keep their declared order.
  d$g <- factor(d$g, levels = c("c", "b", "a"))
  buffer2 <- ggnext:::build_geometry(ggnext(d, aes(g, v)) + geom_point())
  expect_equal(unlist(buffer2$panels[[1]]$x$ticks$labels), c("c", "b", "a"))
})

test_that("a discrete color mapping produces a legend in the buffer and SVG", {
  p <- ggnext(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point()
  buffer <- ggnext:::build_geometry(p)
  expect_equal(buffer$legend$title, "Species")
  expect_equal(unlist(buffer$legend$labels), levels(iris$Species))
  expect_length(buffer$legend$colors, 3)
  svg <- render(p)
  for (lev in levels(iris$Species)) {
    expect_match(svg, paste0(">", lev, "</text>"))
  }
  # No color mapping -> no legend, narrower right margin.
  b2 <- ggnext:::build_geometry(ggnext(cars, aes(speed, dist)) + geom_point())
  expect_null(b2$legend)
  expect_gt(b2$panels[[1]]$rect$w, buffer$panels[[1]]$rect$w)
})
