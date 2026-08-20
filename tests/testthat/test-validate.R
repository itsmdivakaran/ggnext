# validate_plot() / plot_check(): the statistical-graphics linter.

test_that("a clean plot has no findings", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  findings <- validate_plot(p)
  expect_s3_class(findings, "ggnext_check")
  expect_length(findings, 0)
})

test_that("an empty plot is flagged", {
  findings <- validate_plot(ggnext(cars, aes(speed, dist)))
  expect_length(findings, 1)
  expect_equal(findings[[1]]$rule, "empty_plot")
})

test_that("a continuous geom mapped to a categorical y is flagged", {
  p <- ggnext(mtcars, aes(mpg, as.character(cyl))) + geom_point()
  findings <- validate_plot(p)
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_true("categorical_y" %in% rules)
})

test_that("a categorical y is not flagged for geoms that expect one", {
  p <- ggnext(mtcars, aes(as.character(cyl), mpg)) + geom_boxplot()
  findings <- validate_plot(p)
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_false("categorical_y" %in% rules)
})

test_that("a heavily missing aesthetic is flagged", {
  d <- cars
  d$dist[1:10] <- NA
  findings <- validate_plot(ggnext(d, aes(speed, dist)) + geom_point())
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_true("missing_values" %in% rules)
})

test_that("a small fraction of missing values is not flagged", {
  d <- cars
  d$dist[1] <- NA
  findings <- validate_plot(ggnext(d, aes(speed, dist)) + geom_point())
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_false("missing_values" %in% rules)
})

test_that("a high-cardinality discrete color scale is flagged", {
  d <- data.frame(x = 1:30, y = 1:30, g = factor(1:30))
  findings <- validate_plot(ggnext(d, aes(x, y, color = g)) + geom_point())
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_true("high_cardinality_scale" %in% rules)
})

test_that("scale_x_sqrt() on negative data is flagged", {
  d <- data.frame(x = -5:5, y = -5:5)
  p <- ggnext(d, aes(x, y)) + geom_point() + scale_x_sqrt()
  findings <- validate_plot(p)
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_true("sqrt_negative" %in% rules)
})

test_that("scale_x_sqrt() on non-negative data is not flagged", {
  d <- data.frame(x = 0:10, y = 0:10)
  p <- ggnext(d, aes(x, y)) + geom_point() + scale_x_sqrt()
  findings <- validate_plot(p)
  rules <- vapply(findings, function(f) f$rule, character(1))
  expect_false("sqrt_negative" %in% rules)
})

test_that("validate_plot() rejects non-plot input", {
  expect_error(validate_plot(1), "expects a ggnext plot")
})

test_that("printing a clean check reports no issues", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  printed <- capture.output(print(validate_plot(p)))
  expect_match(printed[1], "No issues found")
})

test_that("printing findings shows one warning line per finding", {
  p <- ggnext(mtcars, aes(mpg, as.character(cyl))) + geom_point()
  printed <- capture.output(print(validate_plot(p)))
  expect_true(all(startsWith(printed, "⚠")))
})

test_that("plot_check() prints a report and returns the plot, unchanged", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  printed <- capture.output(result <- plot_check(p))
  expect_match(printed[1], "No issues found")
  expect_equal(result, p)
})

test_that("plot_check() composes inside a pipe", {
  result <- ggnext(cars, aes(speed, dist)) |>
    geom_point() |>
    plot_check() |>
    geom_smooth()
  expect_length(result@layers, 2)
})
