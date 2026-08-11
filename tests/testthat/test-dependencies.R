# The zero-ggplot2-ecosystem guarantee, enforced as tests so a future
# dependency can never sneak in silently.

FORBIDDEN <- c(
  "ggplot2", "grid", "gtable", "gridSVG", "scales", "munsell",
  "gganimate", "ggiraph", "ggraph", "patchwork", "cowplot", "ggridges",
  "ggalluvial", "ggforce", "ggrepel", "plotly"
)

test_that("DESCRIPTION declares no ggplot2-ecosystem package anywhere", {
  desc_path <- system.file("DESCRIPTION", package = "ggplot3")
  fields <- read.dcf(desc_path,
    fields = c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")
  )
  declared <- unlist(strsplit(fields[!is.na(fields)], ","))
  # Strip version requirements and whitespace: "S7 (>= 0.1.0)" -> "S7".
  declared <- trimws(sub("\\(.*\\)", "", declared))
  expect_length(intersect(declared, FORBIDDEN), 0)
})

test_that("the transitive dependency graph is free of the ecosystem", {
  db <- installed.packages()
  skip_if_not(
    "ggplot3" %in% rownames(db),
    "ggplot3 not installed (running under load_all)"
  )
  # Walk Depends/Imports/LinkingTo recursively from ggplot3 itself.
  deps <- tools::package_dependencies(
    "ggplot3",
    db = db, recursive = TRUE,
    which = c("Depends", "Imports", "LinkingTo")
  )[["ggplot3"]]
  expect_length(intersect(deps, FORBIDDEN), 0)
  # And the hard dependency set is exactly what we designed: S7 only
  # (plus base/recommended packages R itself provides).
  base_pkgs <- rownames(db)[db[, "Priority"] %in% "base"]
  expect_setequal(setdiff(deps, base_pkgs), "S7")
})

test_that("loading ggplot3 does not load any ecosystem namespace", {
  # library(ggplot3) has already run via testthat.R by the time this test
  # executes, so loadedNamespaces() reflects everything the package pulled in.
  expect_length(intersect(loadedNamespaces(), FORBIDDEN), 0)
})

test_that("no package function calls into a forbidden namespace", {
  # Audit the deparsed body of every function in the loaded namespace: no
  # library()/require() of a forbidden package and no `pkg::` qualified
  # call into one. Deparsing the live namespace works identically for the
  # installed package (R CMD check) and a devtools::load_all() session.
  ns <- asNamespace("ggplot3")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns,
               ifnotfound = list(NULL), inherits = FALSE)
  fns <- Filter(is.function, objs)
  code <- paste(
    unlist(lapply(fns, function(f) deparse(f, width.cutoff = 500))),
    collapse = "\n"
  )
  for (pkg in FORBIDDEN) {
    expect_no_match(code, sprintf("(library|require|requireNamespace|loadNamespace)\\([\"']?%s[\"']?[),]", pkg))
    expect_no_match(code, sprintf("\\b%s::", pkg))
  }
})
