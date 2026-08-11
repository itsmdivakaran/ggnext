# The shared geometry-computation step (build_geometry) — the buffer both
# render targets consume.

test_that("build_geometry() produces one circle mark per observation", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  marks <- buffer$panels[[1]]$layers[[1]]$marks
  expect_length(marks, nrow(cars))
  expect_true(all(vapply(marks, function(m) m$type, character(1)) == "circle"))
})

test_that("normalized positions stay inside [0, 1] under default expansion", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  xs <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$x, numeric(1))
  ys <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$y, numeric(1))
  expect_true(all(xs >= 0 & xs <= 1))
  expect_true(all(ys >= 0 & ys <= 1))
  # The 5% expansion means extremes sit strictly inside the panel...
  expect_gt(min(xs), 0)
  expect_lt(max(xs), 1)
  # ...at exactly 5% in from each edge (0.05 / 1.10 of the expanded span).
  expect_equal(min(xs), 0.05 / 1.10, tolerance = 1e-10)
  expect_equal(max(xs), 1.05 / 1.10, tolerance = 1e-10)
})

test_that("positions are the linear map of data through the expanded domain", {
  d <- data.frame(x = c(0, 5, 10), y = c(0, 50, 100))
  p <- ggplot3(d, aes(x, y)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  # Expanded x domain is [-0.5, 10.5]; check the midpoint lands at 0.5.
  expect_equal(buffer$panels[[1]]$x$domain, c(-0.5, 10.5))
  xs <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$x, numeric(1))
  expect_equal(xs, (d$x + 0.5) / 11)
})

test_that("axis ticks carry values, normalized positions, and labels", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  ticks <- buffer$panels[[1]]$x$ticks
  expect_equal(ticks$values, seq(5, 25, by = 5)) # 0 clipped by expansion
  expect_equal(unlist(ticks$labels), c("5", "10", "15", "20", "25"))
  expect_equal(ticks$norm, ggplot3:::normalize(ticks$values, buffer$panels[[1]]$x$domain))
  expect_true(all(ticks$norm >= 0 & ticks$norm <= 1))
})

test_that("axis titles default to the mapped expressions", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  expect_equal(buffer$panels[[1]]$x$title, "speed")
  expect_equal(buffer$panels[[1]]$y$title, "dist")
  # An explicit scale name wins.
  p2 <- p + scale_x_continuous(name = "Speed (mph)")
  expect_equal(ggplot3:::build_geometry(p2)$panels[[1]]$x$title, "Speed (mph)")
})

test_that("a discrete color aesthetic maps levels consistently to the palette", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point()
  buffer <- ggplot3:::build_geometry(p)
  fills <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$fill, character(1))
  # Same species -> same color; three species -> first three palette entries.
  per_species <- vapply(
    split(fills, iris$Species),
    function(f) length(unique(f)),
    integer(1)
  )
  expect_equal(unname(per_species), c(1L, 1L, 1L))
  expect_setequal(unique(fills), ggplot3:::GGPLOT3_DISCRETE_PALETTE[1:3])
})

test_that("a continuous color aesthetic interpolates the gradient endpoints", {
  d <- data.frame(x = 1:3, y = 1:3, z = c(0, 5, 10))
  p <- ggplot3(d, aes(x, y, color = z)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  fills <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$fill, character(1))
  expect_equal(fills[1], ggplot3:::GGPLOT3_GRADIENT_LOW)
  expect_equal(fills[3], ggplot3:::GGPLOT3_GRADIENT_HIGH)
})

test_that("a mapped size aesthetic scales radii within [2, 7] px by area", {
  d <- data.frame(x = 1:3, y = 1:3, s = c(1, 2, 3))
  p <- ggplot3(d, aes(x, y, size = s)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  rs <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$r, numeric(1))
  expect_equal(rs[1], 2)
  expect_equal(rs[3], 7)
  # Area (r^2) is linear in the value, so the midpoint has the mean area.
  expect_equal(rs[2]^2, mean(c(2^2, 7^2)))
})

test_that("literal layer params override geom defaults", {
  d <- data.frame(x = 1, y = 1)
  p <- ggplot3(d, aes(x, y)) + geom_point(color = "red", size = 10, alpha = 0.5)
  m <- ggplot3:::build_geometry(p)$panels[[1]]$layers[[1]]$marks[[1]]
  expect_equal(m$fill, "#FF0000")
  expect_equal(m$r, 10)
  expect_equal(m$alpha, 0.5)
})

test_that("tooltip payloads carry the original data values", {
  d <- data.frame(a = c(1.5, 2.5), b = c(10, 20))
  p <- ggplot3(d, aes(a, b)) + geom_point()
  tips <- ggplot3:::build_geometry(p)$panels[[1]]$layers[[1]]$tips
  expect_equal(tips[[1]], "a: 1.5\nb: 10")
  expect_equal(tips[[2]], "a: 2.5\nb: 20")
})

test_that("build errors are clear for common misuse", {
  expect_error(
    ggplot3:::build_geometry(ggplot3(cars, aes(speed, dist))),
    "no layers"
  )
  expect_error(
    ggplot3:::build_geometry(ggplot3(cars, aes(speed)) + geom_point()),
    "requires the aesthetic"
  )
  expect_error(
    ggplot3:::build_geometry(ggplot3() + geom_point()),
    "no data"
  )
})
