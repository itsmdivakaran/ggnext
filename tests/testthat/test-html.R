# The interactive (HTML/canvas) render target.

interactive_page <- function(p = NULL) {
  if (is.null(p)) p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  render(p, target = "interactive")
}

test_that("interactive output is a standalone HTML page with a canvas", {
  html <- interactive_page()
  expect_match(html, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(html, "<canvas id=\"ggplot3-canvas\">", fixed = TRUE)
  expect_match(html, "<div id=\"ggplot3-tooltip\">", fixed = TRUE)
})

test_that("interactive output embeds the geometry buffer as JSON", {
  html <- interactive_page()
  expect_match(html, "var spec = {", fixed = TRUE)
  # One circle mark per observation, same buffer as the static target.
  expect_equal(
    length(gregexpr("\"type\":\"circle\"", html, fixed = TRUE)[[1]]),
    nrow(cars)
  )
  # Axis domains ride along so the JS can relabel ticks while zooming,
  # and every panel carries its own rectangle.
  expect_match(html, "\"domain\":", fixed = TRUE)
  expect_match(html, "\"panels\":", fixed = TRUE)
  expect_match(html, "\"rect\":", fixed = TRUE)
})

test_that("interactive output contains the expected canvas draw calls", {
  html <- interactive_page()
  expect_match(html, "getContext(\"2d\")", fixed = TRUE)
  expect_match(html, "ctx.arc(", fixed = TRUE) # points
  expect_match(html, "ctx.fillText(", fixed = TRUE) # tick labels + titles
  expect_match(html, "ctx.clip()", fixed = TRUE) # panel clipping
})

test_that("interactive output wires up tooltip and zoom listeners", {
  html <- interactive_page()
  expect_match(html, "addEventListener(\"mousemove\"", fixed = TRUE)
  expect_match(html, "addEventListener(\"wheel\"", fixed = TRUE)
  expect_match(html, "addEventListener(\"dblclick\"", fixed = TRUE)
  expect_match(html, "tooltip.textContent", fixed = TRUE)
})

test_that("tooltip data carries the original x/y values", {
  d <- data.frame(a = c(1.5, 2.5), b = c(10, 20))
  html <- interactive_page(ggplot3(d, aes(a, b)) + geom_point())
  expect_match(html, "a: 1.5\\nb: 10", fixed = TRUE) # \n escaped in JSON
  expect_match(html, "a: 2.5\\nb: 20", fixed = TRUE)
})

test_that("interactive output is fully self-contained (no CDN, no assets)", {
  html <- interactive_page()
  expect_no_match(html, "https?://") # no external URL of any kind
  expect_no_match(html, "<script[^>]+src=")
  expect_no_match(html, "<link ")
  expect_no_match(html, "d3\\.|plotly|ggiraph")
})

test_that("static and interactive targets consume identical geometry", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  buffer <- ggplot3:::build_geometry(p)
  svg <- ggplot3:::render_svg(buffer)
  html <- ggplot3:::render_html(buffer)
  # Same buffer in, same mark count out of both serializers.
  n_svg <- length(gregexpr("<circle ", svg, fixed = TRUE)[[1]])
  n_html <- length(gregexpr("\"type\":\"circle\"", html, fixed = TRUE)[[1]])
  expect_equal(n_svg, nrow(cars))
  expect_equal(n_html, nrow(cars))
  # And the exact normalized coordinates appear in the JSON.
  x1 <- buffer$panels[[1]]$layers[[1]]$marks[[1]]$x
  expect_match(html, format(x1, digits = 15), fixed = TRUE)
})

test_that("JSON serializer handles the types the buffer uses", {
  expect_equal(ggplot3:::to_json(NULL), "null")
  expect_equal(ggplot3:::to_json(TRUE), "true")
  expect_equal(ggplot3:::to_json(1.5), "1.5")
  expect_equal(ggplot3:::to_json("a\"b"), "\"a\\\"b\"")
  expect_equal(ggplot3:::to_json(c(1, 2)), "[1,2]")
  expect_equal(ggplot3:::to_json(list(a = 1, b = "x")), "{\"a\":1,\"b\":\"x\"}")
  expect_equal(ggplot3:::to_json(list(1, "x")), "[1,\"x\"]")
  expect_equal(ggplot3:::to_json(list(a = list(b = c(TRUE, FALSE)))),
               "{\"a\":{\"b\":[true,false]}}")
  expect_equal(ggplot3:::to_json(NA), "null")
})
