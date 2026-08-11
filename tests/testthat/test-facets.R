# Faceting: panel splitting, layout, shared vs free scales.

test_that("facet_wrap() makes one panel per level, in level order", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species)
  b <- buf(p)
  expect_length(b$panels, 3)
  expect_equal(
    vapply(b$panels, function(pn) pn$strip, character(1)),
    levels(iris$Species)
  )
  # Every observation is drawn exactly once, spread across the panels.
  total <- sum(vapply(b$panels, function(pn) length(pn$layers[[1]]$marks),
                      integer(1)))
  expect_equal(total, nrow(iris))
})

test_that("panels tile without overlapping and share the plot area", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species, ncol = 2)
  b <- buf(p)
  rects <- lapply(b$panels, function(pn) pn$rect)
  # 3 panels at ncol = 2 -> a 2 x 2 grid with the last cell empty.
  expect_equal(rects[[1]]$y, rects[[2]]$y)      # same row
  expect_lt(rects[[1]]$x, rects[[2]]$x)         # left then right
  expect_gt(rects[[3]]$y, rects[[1]]$y)         # second row lower
  # No horizontal overlap within a row.
  expect_lte(rects[[1]]$x + rects[[1]]$w, rects[[2]]$x)
})

test_that("fixed scales (the default) give every panel the same domain", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species)
  b <- buf(p)
  domains <- lapply(b$panels, function(pn) pn$x$domain)
  expect_true(all(vapply(domains, identical, logical(1), domains[[1]])))
})

test_that("scales = 'free' gives each panel its own domain", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species, scales = "free")
  b <- buf(p)
  domains <- lapply(b$panels, function(pn) pn$x$domain)
  expect_false(identical(domains[[1]], domains[[3]]))
  # Each panel's domain brackets only its own data.
  setosa <- iris$Sepal.Length[iris$Species == "setosa"]
  expect_lte(domains[[1]][1], min(setosa))
  expect_gte(domains[[1]][2], max(setosa))
})

test_that("scales = 'free_y' frees only the y axis", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species, scales = "free_y")
  b <- buf(p)
  xs <- lapply(b$panels, function(pn) pn$x$domain)
  ys <- lapply(b$panels, function(pn) pn$y$domain)
  expect_identical(xs[[1]], xs[[3]])
  expect_false(identical(ys[[1]], ys[[3]]))
})

test_that("inner panels drop redundant tick labels under fixed scales", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species, ncol = 2)
  b <- buf(p)
  # Left column keeps y labels; the right-hand panel does not.
  expect_true(b$panels[[1]]$show_y_labels)
  expect_false(b$panels[[2]]$show_y_labels)
})

test_that("facet_grid() lays panels out rows by columns", {
  d <- transform(mtcars, cyl = factor(cyl), am = factor(am))
  p <- ggplot3(d, aes(disp, mpg)) + geom_point() + facet_grid(am, cyl)
  b <- buf(p)
  # 2 transmission types x 3 cylinder counts.
  expect_length(b$panels, 6)
  expect_equal(b$panels[[1]]$rect$y, b$panels[[3]]$rect$y) # first row
  expect_gt(b$panels[[4]]$rect$y, b$panels[[1]]$rect$y)    # second row
})

test_that("faceting accepts strings and several variables", {
  p1 <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap("Species")
  expect_length(buf(p1)$panels, 3)

  d <- transform(mtcars, cyl = factor(cyl), am = factor(am))
  p2 <- ggplot3(d, aes(disp, mpg)) + geom_point() + facet_wrap(c(am, cyl))
  expect_length(buf(p2)$panels, 6)
})

test_that("a layer without the faceting variable repeats in every panel", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + geom_hline(3) + facet_wrap(Species)
  b <- buf(p)
  for (pn in b$panels) {
    expect_length(pn$layers[[2]]$marks, 1)
  }
})

test_that("faceting errors clearly on a missing variable", {
  expect_error(
    buf(ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
          geom_point() + facet_wrap(NotAColumn)),
    "not found in the data"
  )
})

test_that("the SVG draws one clip path and one strip per panel", {
  svg <- render(
    ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
      geom_point() + facet_wrap(Species)
  )
  expect_equal(length(gregexpr("<clipPath ", svg, fixed = TRUE)[[1]]), 3)
  for (lev in levels(iris$Species)) {
    expect_match(svg, paste0(">", lev, "</text>"))
  }
})
