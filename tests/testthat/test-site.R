# The logo and the documentation-site generator.

test_that("ggplot3_logo() draws a well-formed hex SVG", {
  skip_if_not_installed("xml2")
  svg <- ggplot3_logo()
  doc <- xml2::read_xml(svg)
  expect_equal(xml2::xml_name(doc), "svg")
  # A hexagon plus the border polygon, both with 6 vertices.
  polys <- xml2::xml_find_all(doc, ".//*[local-name() = 'polygon']")
  expect_gte(length(polys), 2)
  pts <- strsplit(xml2::xml_attr(polys[[1]], "points"), " ")[[1]]
  expect_length(pts, 6)
  expect_match(svg, ">ggplot3</text>", fixed = TRUE)
})

test_that("the logo has light and dark variants and writes to file", {
  light <- ggplot3_logo()
  dark <- ggplot3_logo(dark = TRUE)
  expect_false(identical(light, dark))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path))
  ggplot3_logo(file = path)
  expect_true(file.exists(path))
})

test_that("every gallery example renders", {
  # The gallery is the package's own regression net: if a geom breaks, the
  # site build must fail rather than quietly drop a figure.
  examples <- ggplot3:::gallery_examples()
  expect_gt(length(examples), 20)
  for (nm in names(examples)) {
    svg <- tryCatch(
      render(eval(examples[[nm]]$code), target = "static"),
      error = function(e) structure(conditionMessage(e), class = "failed")
    )
    expect_false(inherits(svg, "failed"),
                 label = paste0("gallery example '", nm, "' renders"))
    expect_match(svg, "^<svg ")
  }
})

test_that("build_site() writes a self-contained site", {
  dir <- tempfile("site")
  on.exit(unlink(dir, recursive = TRUE))
  build_site(dir, quiet = TRUE)

  for (page in c("index.html", "gallery.html", "reference.html",
                 "themes.html", "logo.svg")) {
    expect_true(file.exists(file.path(dir, page)), label = page)
  }
  # One figure per gallery example, plus one per theme.
  expect_gte(length(list.files(dir, pattern = "^fig-.*\\.svg$")), 20)
  expect_gte(length(list.files(dir, pattern = "^theme-.*\\.svg$")), 6)
})

test_that("site pages load no external assets", {
  # Hyperlinks to prior-art packages are expected on the credits page; what
  # must never appear is a *loaded* external asset (script, stylesheet,
  # font, or image), which would break the site offline.
  dir <- tempfile("site")
  on.exit(unlink(dir, recursive = TRUE))
  build_site(dir, quiet = TRUE)
  for (page in list.files(dir, pattern = "\\.html$", full.names = TRUE)) {
    html <- paste(readLines(page, warn = FALSE), collapse = "\n")
    expect_no_match(html, "<script[^>]+src=", label = basename(page))
    expect_no_match(html, "<link[^>]+href=", label = basename(page))
    expect_no_match(html, "<img[^>]+src=\"https?://", label = basename(page))
    expect_no_match(html, "@import", label = basename(page))
  }
})

test_that("the credits page names its prior art", {
  html <- ggplot3:::site_credits()
  for (pkg in c("ggplot2", "ggridges", "ggalluvial", "ggraph", "treemapify",
                "gganimate", "ggiraph", "survminer", "circlize")) {
    expect_match(html, pkg, fixed = TRUE)
  }
  expect_match(html, "Wilkinson", fixed = TRUE)
  expect_match(html, "Heckbert", fixed = TRUE)
})

test_that("the reference index covers every export", {
  html <- ggplot3:::site_reference(tempfile())
  for (fn in c("ggplot3", "geom_point", "geom_sankey", "geom_forest",
               "plot_data", "theme_modern", "facet_wrap", "coord_polar")) {
    expect_match(html, paste0("<code>", fn, "</code>"), fixed = TRUE)
  }
})

test_that("drawing a plot never disturbs the caller's random stream", {
  # CRAN policy, and plain good manners: rendering must not reset a user's
  # simulation. Every seeded layout goes through with_seed().
  set.seed(99)
  before <- .Random.seed

  invisible(ggplot3_logo())
  expect_identical(.Random.seed, before)

  d <- data.frame(f = c("A", "A", "B", "C"), t = c("B", "C", "C", "A"))
  invisible(render(ggplot3(d, aes(x = f, xend = t)) + geom_network()))
  expect_identical(.Random.seed, before)

  invisible(render(ggplot3(iris, aes(Species, Sepal.Length)) + geom_jitter()))
  expect_identical(.Random.seed, before)

  # And the seeded output is still reproducible run to run.
  expect_identical(ggplot3_logo(), ggplot3_logo())
})

test_that("with_seed() restores an absent .Random.seed", {
  if (exists(".Random.seed", globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }
  x <- ggplot3:::with_seed(1, stats::runif(3))
  expect_length(x, 3)
  expect_false(exists(".Random.seed", globalenv()))
})
