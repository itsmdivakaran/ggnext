# Titles, axis customization, coords, palettes, and the data export.

# --- labels ------------------------------------------------------------------

test_that("labs() sets the title block and axis titles", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() +
    labs(title = "T", subtitle = "S", caption = "C", tag = "A",
         x = "Speed", y = "Distance")
  b <- buf(p)
  expect_equal(b$labels$title, "T")
  expect_equal(b$labels$subtitle, "S")
  expect_equal(b$labels$caption, "C")
  expect_equal(b$labels$tag, "A")
  expect_equal(b$panels[[1]]$x$title, "Speed")
  expect_equal(b$panels[[1]]$y$title, "Distance")

  svg <- render(p)
  for (txt in c("T", "S", "C", "A", "Speed", "Distance")) {
    expect_match(svg, paste0(">", txt, "</text>"), fixed = TRUE)
  }
})

test_that("labs() merges across repeated additions", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() +
    labs(title = "first") + labs(subtitle = "second")
  b <- buf(p)
  expect_equal(b$labels$title, "first")
  expect_equal(b$labels$subtitle, "second")
  # A later labs() overrides only what it names.
  b2 <- buf(p + labs(title = "third"))
  expect_equal(b2$labels$title, "third")
  expect_equal(b2$labels$subtitle, "second")
})

test_that("ggtitle(), xlab(), and ylab() are shorthands for labs()", {
  b <- buf(ggplot3(cars, aes(speed, dist)) + geom_point() +
             ggtitle("Main", subtitle = "Sub") + xlab("X") + ylab("Y"))
  expect_equal(b$labels$title, "Main")
  expect_equal(b$labels$subtitle, "Sub")
  expect_equal(b$panels[[1]]$x$title, "X")
  expect_equal(b$panels[[1]]$y$title, "Y")
})

test_that("the title block grows the top margin", {
  plain <- buf(ggplot3(cars, aes(speed, dist)) + geom_point())
  titled <- buf(ggplot3(cars, aes(speed, dist)) + geom_point() +
                  labs(title = "T", subtitle = "S"))
  expect_gt(titled$panels[[1]]$rect$y, plain$panels[[1]]$rect$y)
})

# --- axis customization ------------------------------------------------------

test_that("scale breaks and labels override the automatic ones", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() +
    scale_x_continuous(breaks = c(5, 15, 25))
  b <- buf(p)
  expect_equal(b$panels[[1]]$x$ticks$values, c(5, 15, 25))
  expect_equal(unlist(b$panels[[1]]$x$ticks$labels), c("5", "15", "25"))
})

test_that("a labels function is applied to the break values", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() +
    scale_x_continuous(breaks = c(10, 20),
                       labels = function(v) paste0(v, " mph"))
  expect_equal(
    unlist(buf(p)$panels[[1]]$x$ticks$labels),
    c("10 mph", "20 mph")
  )
})

test_that("expand = 0 makes the domain exactly the data range", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() +
    scale_x_continuous(expand = 0)
  expect_equal(buf(p)$panels[[1]]$x$domain, range(cars$speed))
})

test_that("xlim()/ylim() fix the domain", {
  b <- buf(ggplot3(cars, aes(speed, dist)) + geom_point() +
             xlim(0, 30) + ylim(0, 150))
  # Limits are still expanded by the default 5%.
  expect_equal(b$panels[[1]]$x$domain, c(-1.5, 31.5))
  expect_equal(b$panels[[1]]$y$domain, c(-7.5, 157.5))
})

test_that("lims() adds both scales at once", {
  b <- buf(ggplot3(cars, aes(speed, dist)) + geom_point() +
             lims(x = c(0, 30), y = c(0, 150)))
  expect_equal(b$panels[[1]]$x$domain, c(-1.5, 31.5))
})

test_that("log10 scales train and label in the right spaces", {
  d <- data.frame(x = 10^(1:4), y = 1:4)
  b <- buf(ggplot3(d, aes(x, y)) + geom_point() + scale_x_log10())
  # The domain lives in log space...
  expect_equal(b$panels[[1]]$x$domain, c(1 - 0.15, 4 + 0.15))
  # ...but tick labels are in data units.
  expect_true("100" %in% unlist(b$panels[[1]]$x$ticks$labels))
  # Positions are the log of the data, normalized.
  xs <- vapply(b$panels[[1]]$layers[[1]]$marks, function(m) m$x, numeric(1))
  expect_equal(xs, ggplot3:::normalize(log10(d$x), b$panels[[1]]$x$domain))
})

test_that("sqrt and reverse transforms behave", {
  d <- data.frame(x = c(1, 4, 9), y = 1:3)
  xs <- vapply(marks_of(ggplot3(d, aes(x, y)) + geom_point() + scale_x_sqrt()),
               function(m) m$x, numeric(1))
  expect_true(is.unsorted(xs) == FALSE) # monotone increasing
  # Reverse flips the order of positions relative to the data.
  xr <- vapply(marks_of(ggplot3(d, aes(x, y)) + geom_point() + scale_x_reverse()),
               function(m) m$x, numeric(1))
  expect_true(is.unsorted(rev(xr)) == FALSE)
})

test_that("log10 refuses non-positive data with a clear message", {
  d <- data.frame(x = c(0, 1, 10), y = 1:3)
  expect_error(
    buf(ggplot3(d, aes(x, y)) + geom_point() + scale_x_log10()),
    "strictly positive"
  )
})

# --- coords ------------------------------------------------------------------

test_that("coord_flip() swaps the axes", {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 7, 5))
  flipped <- marks_of(ggplot3(d, aes(g, v)) + geom_col() + coord_flip())
  # Bars now run horizontally: their x extent varies, y extent is the slot.
  widths <- vapply(flipped, function(m) abs(m$x1 - m$x0), numeric(1))
  expect_gt(length(unique(round(widths, 6))), 1)
})

test_that("coord_polar() maps positions onto a circle", {
  d <- data.frame(g = c("a", "b", "c", "d"), v = c(1, 1, 1, 1))
  b <- buf(ggplot3(d, aes(g, v)) + geom_point() + coord_polar())
  m <- b$panels[[1]]$layers[[1]]$marks
  # Equal values -> equal radius from the panel center (0.5, 0.5).
  radii <- vapply(m, function(mk) {
    sqrt((mk$x - 0.5)^2 + (mk$y - 0.5)^2)
  }, numeric(1))
  expect_equal(length(unique(round(radii, 9))), 1)
  # The polar guide rings and spokes are precomputed for the renderers.
  expect_false(is.null(b$panels[[1]]$polar))
  expect_length(b$panels[[1]]$polar$spokes, 4)
})

test_that("a polar angular axis wraps evenly around the full turn", {
  d <- data.frame(g = c("a", "b", "c", "d"), v = c(1, 2, 3, 4))
  b <- buf(ggplot3(d, aes(g, v)) + geom_point() + coord_polar())
  # 4 categories -> domain [1, 5] so slot k sits at angle (k-1)/4 turns.
  expect_equal(b$panels[[1]]$x$domain, c(1, 5))
  expect_equal(unlist(b$panels[[1]]$polar$spokes), c(0, 0.25, 0.5, 0.75))
})

# --- palettes ----------------------------------------------------------------

test_that("scale_color_manual() overrides the discrete palette", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point() + scale_color_manual(c("#FF0000", "#00FF00", "#0000FF"))
  b <- buf(p)
  expect_setequal(unlist(b$legend$colors), c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("scale_color_gradient() overrides the continuous endpoints", {
  d <- data.frame(x = 1:3, y = 1:3, z = c(0, 5, 10))
  b <- buf(ggplot3(d, aes(x, y, color = z)) + geom_point() +
             scale_color_gradient(low = "#000000", high = "#FFFFFF"))
  fills <- vapply(b$panels[[1]]$layers[[1]]$marks, function(m) m$fill,
                  character(1))
  expect_equal(fills[1], "#000000")
  expect_equal(fills[3], "#FFFFFF")
  expect_equal(b$legend$type, "gradient")
})

test_that("a theme palette applies when no scale overrides it", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point() + theme_modern()
  b <- buf(p)
  expect_equal(unlist(b$legend$colors)[1], "#2B6BE0")
})

test_that("a continuous color mapping produces a gradient legend", {
  d <- data.frame(x = 1:5, y = 1:5, z = 1:5)
  b <- buf(ggplot3(d, aes(x, y, color = z)) + geom_point())
  expect_equal(b$legend$type, "gradient")
  expect_length(b$legend$labels, 5)
  svg <- render(ggplot3(d, aes(x, y, color = z)) + geom_point())
  expect_match(svg, "linearGradient", fixed = TRUE)
})

# --- plot size ---------------------------------------------------------------

test_that("plot size is configurable at construction and with plot_size()", {
  expect_equal(buf(ggplot3(cars, aes(speed, dist), width = 900,
                           height = 300) + geom_point())$width, 900)
  b <- buf(ggplot3(cars, aes(speed, dist)) + geom_point() + plot_size(800, 600))
  expect_equal(b$width, 800)
  expect_equal(b$height, 600)
})

# --- data export -------------------------------------------------------------

test_that("plot_data() returns exactly the plotted values", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  d <- plot_data(p)
  expect_s3_class(d, "data.frame")
  # Only the aesthetics the layer uses; no scratch columns, no constant group.
  expect_named(d, c("x", "y"))
  expect_equal(d$x, cars$speed)
  expect_equal(d$y, cars$dist)
})

test_that("plot_data() reflects the stat, not the input", {
  h <- ggplot3(cars, aes(speed)) + geom_histogram(bins = 5)
  d <- plot_data(h)
  expect_equal(nrow(d), 5)
  expect_true(all(c("x", "y", "xmin", "xmax") %in% names(d)))
  # Counts sum to the number of observations.
  expect_equal(sum(d$y), nrow(cars))

  b <- ggplot3(iris, aes(Species, Sepal.Length)) + geom_boxplot()
  db <- plot_data(b)
  expect_true(all(c("lower", "middle", "upper") %in% names(db)))
})

test_that("plot_data() adds a panel column when faceted", {
  p <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species)
  d <- plot_data(p)
  expect_true("panel" %in% names(d))
  expect_equal(nrow(d), nrow(iris))
  expect_setequal(unique(d$panel), levels(iris$Species))
})

test_that("plot_data() selects layers and panels", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() + geom_smooth()
  both <- plot_data(p)
  expect_length(both, 2)
  expect_equal(nrow(plot_data(p, layer = 1)), nrow(cars))
  expect_error(plot_data(p, layer = 5), "between 1 and 2")

  f <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() + facet_wrap(Species)
  expect_equal(nrow(plot_data(f, panel = 1)), 50)
  expect_length(plot_data(f, panel = "list"), 3)
})

test_that("write_plot_data() writes a CSV of the plotted values", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  write_plot_data(p, path)
  back <- utils::read.csv(path)
  expect_equal(back$x, cars$speed)
  expect_named(back, c("x", "y"))
})
