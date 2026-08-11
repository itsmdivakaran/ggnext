# The theme system: presets, overrides, and threading through both targets.

test_that("theme presets restyle both render targets", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  svg_dark <- render(p + theme_dark())
  expect_match(svg_dark, "#1C1C22") # dark background
  expect_match(svg_dark, "#26262E") # dark panel
  html_dark <- render(p + theme_dark(), target = "interactive")
  expect_match(html_dark, "\"background\":\"#1C1C22\"", fixed = TRUE)

  svg_min <- render(p + theme_minimal())
  expect_match(svg_min, "#E4E4E8") # minimal gridlines
})

test_that("theme() overrides individual settings from the default", {
  p <- ggplot3(cars, aes(speed, dist)) + geom_point() +
    theme(panel_fill = "#FFF8F0", grid_color = "grey80")
  svg <- render(p)
  expect_match(svg, "#FFF8F0")
  expect_match(svg, "#CCCCCC") # grey80 normalized to hex
})

test_that("theme() rejects unknown or unnamed settings", {
  expect_error(theme(panel_colour = "red"), "Unknown theme setting")
  expect_error(theme("red"), "must be named")
})

test_that("the default look is unchanged without an explicit theme", {
  svg <- render(ggplot3(cars, aes(speed, dist)) + geom_point())
  expect_match(svg, "#F4F4F6") # default panel fill
})

test_that("renderers contain no hard-coded chrome colors of their own", {
  # Rendering the same plot under two themes must not share chrome colors
  # (proof the theme is truly consumed from the buffer, not baked in).
  p <- ggplot3(cars, aes(speed, dist)) + geom_point()
  html_default <- render(p, target = "interactive")
  html_dark <- render(p + theme_dark(), target = "interactive")
  expect_false(grepl("#F4F4F6", html_dark, fixed = TRUE))
  expect_true(grepl("#F4F4F6", html_default, fixed = TRUE))
})
