# interact() extensions: brush, tooltip columns, literal colors, animate stub.

test_that("brush is on by default and serialized to the page", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  html <- render(p, target = "interactive")
  expect_match(html, "\"brush\":true", fixed = TRUE)
  expect_match(html, "brushActive", fixed = TRUE)
  html_off <- render(p + interact(brush = FALSE))
  expect_match(html_off, "\"brush\":false", fixed = TRUE)
})

test_that("interact(tooltip = columns) swaps in layer-data columns", {
  p <- ggnext(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
    interact(tooltip = c("Species", "Petal.Length"))
  html <- render(p)
  expect_match(html, "Species: setosa\\nPetal.Length: 1.4", fixed = TRUE)
  # Unknown columns fail loudly, not silently.
  expect_error(
    render(ggnext(cars, aes(speed, dist)) + geom_point() +
             interact(tooltip = "nope")),
    "not found in the layer data"
  )
  expect_error(interact(tooltip = 1), "TRUE, FALSE, or a character vector")
})

test_that("a constant color inside aes() is honored literally", {
  p <- ggnext(mtcars, aes(disp, hp, color = "blue")) + geom_point()
  buffer <- ggnext:::build_geometry(p)
  fills <- vapply(buffer$panels[[1]]$layers[[1]]$marks, function(m) m$fill, character(1))
  expect_true(all(fills == "#0000FF"))
  expect_null(buffer$legend) # literals produce no legend
})

test_that("aes() names the problem when a mapping hits a function", {
  expect_error(
    ggnext:::build_geometry(
      ggnext(mtcars, aes(disp, hp, color = class)) + geom_point()
    ),
    "evaluated to a function.*Columns present"
  )
})

test_that("animate() builds one frame per level of the transition variable", {
  d <- data.frame(
    x = rep(1:4, 3), y = c(1:4, (1:4) * 2, (1:4) * 3),
    step = rep(c(1, 2, 3), each = 4)
  )
  p <- ggnext(d, aes(x, y)) + geom_point() + animate(step)
  # An animated plot defaults to the interactive target and ships frames.
  html <- render(p)
  expect_match(html, "^<!DOCTYPE html>")
  expect_match(html, "\"animation\":", fixed = TRUE)
  expect_match(html, "ggnext-scrub", fixed = TRUE)

  buffer <- ggnext:::build_animation(p, ggnext:::build_geometry(p))
  expect_length(buffer$animation$frames, 3)
  expect_equal(unlist(buffer$animation$labels), c("1", "2", "3"))
  # Each frame holds the marks for that step only.
  expect_length(buffer$animation$frames[[1]][[1]][[1]]$marks, 4)
})

test_that("animate() keeps axes fixed across frames", {
  d <- data.frame(x = c(1, 2, 1, 2), y = c(1, 2, 10, 20), s = c(1, 1, 2, 2))
  p <- ggnext(d, aes(x, y)) + geom_point() + animate(s)
  buffer <- ggnext:::build_animation(p, ggnext:::build_geometry(p))
  # Frame 1 holds small values but must use the full-data domain, so its
  # marks sit low in the panel rather than filling it.
  ys <- vapply(buffer$animation$frames[[1]][[1]][[1]]$marks,
               function(m) m$y, numeric(1))
  expect_true(all(ys < 0.2))
})

test_that("an animated plot still renders statically", {
  d <- data.frame(x = 1:4, y = 1:4, s = c(1, 1, 2, 2))
  p <- ggnext(d, aes(x, y)) + geom_point() + animate(s)
  expect_match(render(p, target = "static"), "^<svg ")
})
