# The static (SVG) render target.

test_that("SVG output contains one <circle> per observation", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  svg <- render(p)
  expect_equal(
    length(gregexpr("<circle ", svg, fixed = TRUE)[[1]]),
    nrow(cars)
  )
})

test_that("SVG is well-formed XML with the SVG namespace", {
  skip_if_not_installed("xml2")
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  doc <- xml2::read_xml(render(p)) # read_xml() errors on malformed XML
  expect_equal(xml2::xml_name(doc), "svg")
  expect_equal(
    xml2::xml_attr(doc, "xmlns"),
    "http://www.w3.org/2000/svg"
  )
})

test_that("point positions in the SVG match hand-computed device pixels", {
  skip_if_not_installed("xml2")
  # Fixed limits make the whole transform chain predictable end to end:
  # limits [0, 10] expand to [-0.5, 10.5]; panel is x:[60, 624], y:[20, 432]
  # (640x480 device minus margins l=60 r=16 t=20 b=48).
  d <- data.frame(x = c(0, 5, 10), y = c(0, 5, 10))
  p <- ggnext(d, aes(x, y)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 10))
  doc <- xml2::read_xml(render(p))
  circles <- xml2::xml_find_all(doc, ".//*[local-name() = 'circle']")
  cx <- as.numeric(xml2::xml_attr(circles, "cx"))
  cy <- as.numeric(xml2::xml_attr(circles, "cy"))

  t <- (d$x + 0.5) / 11 # normalized positions
  expect_equal(cx, round(60 + t * 564, 2))
  expect_equal(cy, round(20 + (1 - t) * 412, 2)) # y flipped in device space
})

test_that("axes render with correct tick labels and titles", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  svg <- render(p)
  for (label in c("5", "10", "15", "20", "25")) {
    expect_match(svg, paste0(">", label, "</text>"))
  }
  for (label in c("0", "40", "80", "120")) {
    expect_match(svg, paste0(">", label, "</text>"))
  }
  expect_match(svg, ">speed</text>")
  expect_match(svg, ">dist</text>")
})

test_that("axis titles are XML-escaped", {
  d <- data.frame(x = 1:3, y = 1:3)
  p <- ggnext(d, aes(x, y)) +
    geom_point() +
    scale_x_continuous(name = "a < b & c")
  svg <- render(p)
  expect_match(svg, "a &lt; b &amp; c", fixed = TRUE)
})

test_that("render(file =) writes the SVG to disk", {
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path))
  render(p, file = path)
  expect_true(file.exists(path))
  content <- paste(readLines(path), collapse = "\n")
  expect_match(content, "^<svg ")
})

test_that("the acceptance plot matches its SVG snapshot", {
  # The full acceptance criterion in one artifact: correct axes, tick
  # labels, and point positions for a linear scale. The writer is fully
  # deterministic (fixed formatC() rounding, no fonts measured, no system
  # state), so a byte-level snapshot is stable across platforms.
  p <- ggnext(cars, aes(speed, dist)) + geom_point()
  path <- file.path(tempdir(), "cars-points.svg")
  render(p, file = path)
  expect_snapshot_file(path, "cars-points.svg")
})
