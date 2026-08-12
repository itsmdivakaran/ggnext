# The grammar API: aes(), constructors, and `+` composition.

test_that("ggnext() builds an empty plot specification", {
  p <- ggnext(cars, aes(speed, dist))
  expect_true(S7::S7_inherits(p, GgnextPlot))
  expect_length(p@layers, 0)
  expect_equal(p@coord@name, "cartesian")
})

test_that("aes() takes positional x/y and named aesthetics", {
  m <- aes(speed, dist, color = gear)
  expect_named(m$exprs, c("x", "y", "color"))
  expect_equal(deparse(m$exprs$x), "speed")
  # British spelling is accepted as an alias.
  m2 <- aes(x = a, y = b, colour = c)
  expect_named(m2$exprs, c("x", "y", "color"))
})

test_that("aes() rejects unsupported and duplicated aesthetics", {
  expect_error(aes(x, y, shape = z), "Unsupported aesthetic")
  expect_error(aes(x, y, x = z), "Duplicated")
  expect_error(aes(a, b, c), "at most two unnamed")
})

test_that("`+` adds layers, replaces scales, and swaps coords", {
  p <- ggnext(cars, aes(speed, dist))
  p1 <- p + geom_point()
  expect_length(p1@layers, 1)
  p2 <- p1 + geom_point(color = "red")
  expect_length(p2@layers, 2)
  # Plots are immutable values: p1 is untouched by building p2.
  expect_length(p1@layers, 1)

  s <- scale_x_continuous(limits = c(0, 30))
  p3 <- p1 + s
  expect_equal(p3@scales$x@limits, c(0, 30))
  # Adding a second x scale replaces the first, keyed by aesthetic.
  p4 <- p3 + scale_x_continuous(limits = c(0, 99))
  expect_equal(p4@scales$x@limits, c(0, 99))
  expect_length(p4@scales, 1)

  p5 <- p1 + coord_cartesian()
  expect_equal(p5@coord@name, "cartesian")

  # NULL is a no-op, handy in conditional pipelines.
  expect_length((p1 + NULL)@layers, 1)
})

test_that("`+` rejects objects that are not grammar components", {
  p <- ggnext(cars, aes(speed, dist))
  expect_error(p + 1, "Cannot add")
  expect_error(p + "geom_point", "Cannot add")
})

test_that("layer-level aes and data override plot-level settings", {
  d_layer <- data.frame(u = c(1, 2), v = c(3, 4))
  p <- ggnext(cars, aes(speed, dist)) +
    geom_point(aes(u, v), data = d_layer)
  buffer <- ggnext:::build_geometry(p)
  expect_length(buffer$panels[[1]]$layers[[1]]$marks, 2)
  expect_equal(buffer$panels[[1]]$x$title, "u")
})

test_that("constructors validate their inputs", {
  expect_error(ggnext(data = 1:10), "data frame")
  expect_error(ggnext(cars, mapping = "aes"), "created with aes()")
})

test_that("one plot object renders to either target with identical geometry", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  svg <- render(p, target = "static")
  html <- render(p, target = "interactive")
  expect_match(svg, "^<svg ")
  expect_match(html, "^<!DOCTYPE html>")
  expect_error(render(p, target = "webgl"), "should be one of")
})

test_that("plots are static-first; interact() opts into the HTML target", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  # No interact() spec: the default render target is the static SVG.
  expect_match(render(p), "^<svg ")
  # Adding interact() flips the default; the plot object itself is enough.
  pi <- p + interact()
  expect_match(render(pi), "^<!DOCTYPE html>")
  # An explicit target always wins, in both directions.
  expect_match(render(pi, target = "static"), "^<svg ")
  expect_match(render(p, target = "interactive"), "^<!DOCTYPE html>")
})

test_that("interact() flags are carried into the buffer and the page", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  buffer <- ggnext:::build_geometry(p + interact(tooltip = TRUE, zoom = FALSE))
  expect_true(buffer$interaction$tooltip)
  expect_false(buffer$interaction$zoom)
  html <- render(p + interact(zoom = FALSE))
  expect_match(
    html,
    "\"interaction\":{\"tooltip\":true,\"zoom\":false,\"brush\":true}",
    fixed = TRUE
  )
  # Explicit interactive render of a plain plot enables everything.
  expect_match(
    render(p, target = "interactive"),
    "\"interaction\":{\"tooltip\":true,\"zoom\":true,\"brush\":true}",
    fixed = TRUE
  )
})

test_that("render output prints as a summary, not the raw document", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  out <- render(p)
  expect_s3_class(out, "ggnext_render")
  printed <- capture.output(print(out))
  expect_match(printed[1], "<ggnext static render:")
  expect_no_match(paste(printed, collapse = ""), "<circle")
})
