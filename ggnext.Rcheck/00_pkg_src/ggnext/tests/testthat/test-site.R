# The logo and the documentation-site generator.

test_that("ggnext_logo() draws a well-formed hex SVG", {
  skip_if_not_installed("xml2")
  svg <- ggnext_logo()
  doc <- xml2::read_xml(svg)
  expect_equal(xml2::xml_name(doc), "svg")
  # A hexagon plus the border polygon, both with 6 vertices.
  polys <- xml2::xml_find_all(doc, ".//*[local-name() = 'polygon']")
  expect_gte(length(polys), 2)
  pts <- strsplit(xml2::xml_attr(polys[[1]], "points"), " ")[[1]]
  expect_length(pts, 6)
  expect_match(svg, ">ggnext</text>", fixed = TRUE)
})

test_that("the logo has light and dark variants and writes to file", {
  light <- ggnext_logo()
  dark <- ggnext_logo(dark = TRUE)
  expect_false(identical(light, dark))
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path))
  ggnext_logo(file = path)
  expect_true(file.exists(path))
})

test_that("every gallery example renders", {
  # The gallery is the package's own regression net: if a geom breaks, the
  # site build must fail rather than quietly drop a figure.
  examples <- ggnext:::gallery_examples()
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
  build_site(dir, quiet = TRUE, cookbook = FALSE)

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
  build_site(dir, quiet = TRUE, cookbook = FALSE)
  for (page in list.files(dir, pattern = "\\.html$", full.names = TRUE)) {
    html <- paste(readLines(page, warn = FALSE), collapse = "\n")
    expect_no_match(html, "<script[^>]+src=", label = basename(page))
    expect_no_match(html, "<link[^>]+href=", label = basename(page))
    expect_no_match(html, "<img[^>]+src=\"https?://", label = basename(page))
    expect_no_match(html, "@import", label = basename(page))
  }
})

test_that("the credits page names its prior art", {
  html <- ggnext:::site_credits()
  for (pkg in c("ggplot2", "ggridges", "ggalluvial", "ggraph", "treemapify",
                "gganimate", "ggiraph", "survminer", "circlize")) {
    expect_match(html, pkg, fixed = TRUE)
  }
  expect_match(html, "Wilkinson", fixed = TRUE)
  expect_match(html, "Heckbert", fixed = TRUE)
})

test_that("the reference index covers every export", {
  html <- ggnext:::site_reference(tempfile())
  for (fn in c("ggnext", "geom_point", "geom_sankey", "geom_forest",
               "plot_data", "theme_modern", "facet_wrap", "coord_polar")) {
    expect_match(html, paste0("<code>", fn, "</code>"), fixed = TRUE)
  }
})

test_that("drawing a plot never disturbs the caller's random stream", {
  # CRAN policy, and plain good manners: rendering must not reset a user's
  # simulation. Every seeded layout goes through with_seed().
  set.seed(99)
  before <- .Random.seed

  invisible(ggnext_logo())
  expect_identical(.Random.seed, before)

  d <- data.frame(f = c("A", "A", "B", "C"), t = c("B", "C", "C", "A"))
  invisible(render(ggnext(d, aes(x = f, xend = t)) + geom_network()))
  expect_identical(.Random.seed, before)

  invisible(render(ggnext(iris, aes(Species, Sepal.Length)) + geom_jitter()))
  expect_identical(.Random.seed, before)

  # And the seeded output is still reproducible run to run.
  expect_identical(ggnext_logo(), ggnext_logo())
})

test_that("with_seed() restores an absent .Random.seed", {
  if (exists(".Random.seed", globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }
  x <- ggnext:::with_seed(1, stats::runif(3))
  expect_length(x, 3)
  expect_false(exists(".Random.seed", globalenv()))
})

test_that("the cookbook page knits the shipped reference document", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("markdown")

  html <- ggnext:::site_cookbook()
  expect_false(is.null(html))
  expect_match(html, "<h1>Cookbook</h1>", fixed = TRUE)

  # Every plot in the document rendered, and none is an empty panel.
  n_svg <- length(gregexpr("<svg ", html, fixed = TRUE)[[1]])
  expect_gt(n_svg, 80)
  expect_false(grepl("Error in", html, fixed = TRUE))

  # A contents list was built from the document's own headings.
  expect_match(html, "class=\"toc\"", fixed = TRUE)

  # The document's standalone page styling must not leak into the site,
  # where it would fight the site stylesheet.
  body <- sub(".*</head>", "", html)
  expect_false(grepl("<style", body, fixed = TRUE))
})

test_that("build_site(cookbook = FALSE) skips the slow page", {
  dir <- tempfile("site")
  on.exit(unlink(dir, recursive = TRUE))
  build_site(dir, quiet = TRUE, cookbook = FALSE)
  expect_false(file.exists(file.path(dir, "cookbook.html")))
  expect_true(file.exists(file.path(dir, "index.html")))
})

test_that("inline SVG is capped so wide plots cannot force page scroll", {
  # The cookbook embeds plots as raw <svg>, not <img>; without a cap a
  # 900px-wide plot makes the whole page scroll sideways.
  dir <- tempfile("site")
  on.exit(unlink(dir, recursive = TRUE))
  build_site(dir, quiet = TRUE, cookbook = FALSE)
  css <- paste(readLines(file.path(dir, "index.html"), warn = FALSE),
               collapse = "\n")
  expect_match(css, "main svg", fixed = TRUE)
  expect_match(css, "max-width: 100%", fixed = TRUE)
})
