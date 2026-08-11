#' @keywords internal
"_PACKAGE"

## ggplot3 deliberately imports the whole of S7: the class system is used in
## every file and a curated importFrom() list would need constant maintenance.
## S7 has zero hard dependencies of its own, so this keeps the transitive
## dependency graph at exactly: ggplot3 -> S7 -> (nothing).
#' @import S7
#' @importFrom grDevices col2rgb
#' @importFrom utils browseURL
NULL

## The site generator and the roxygen examples build real plots, so they
## name dataset columns inside aes() — non-standard evaluation that R CMD
## check reads as undefined globals. Declaring them here keeps the check
## clean without importing anything at runtime; `datasets` is attached by
## default in every R session, so the example data is always available.
utils::globalVariables(c(
  # datasets::cars / datasets::iris and their columns
  "cars", "iris", "speed", "dist",
  "Sepal.Length", "Sepal.Width", "Petal.Length", "Species",
  # Column names of the small inline data frames the guide figures build
  "g", "v", "grp"
))

#' Evaluate an expression under a fixed seed, then restore the RNG
#'
#' Several layouts need reproducible randomness (jitter offsets, the
#' force-layout starting positions). Seeding the global generator directly
#' would silently reset the caller's random stream, so the previous
#' `.Random.seed` is saved and put back on exit — drawing a plot must never
#' change a user's simulation.
#'
#' @param seed Integer seed.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @noRd
with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", globalenv())
  old <- if (had_seed) get(".Random.seed", globalenv())
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old, globalenv())
    } else {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    }
  })
  set.seed(seed)
  expr
}

# --- knitr integration -------------------------------------------------------
#
# In an R Markdown or Quarto document, writing `p` in a chunk should show the
# plot. knitr dispatches on knit_print(), so registering methods here means a
# report author never has to hand-roll an `results='asis'` helper.
#
# SVG is emitted inline for HTML output. Other formats (PDF via LaTeX) cannot
# consume an inline SVG string, so those fall back to the console summary
# rather than dumping markup into the document.

knit_print_plot <- function(x, ...) {
  if (!isTRUE(knitr::is_html_output())) {
    return(knitr::normal_print(x))
  }
  knitr::asis_output(paste0("\n\n", as.character(render(x)), "\n\n"))
}

knit_print_render <- function(x, ...) {
  if (!identical(attr(x, "target"), "static") ||
      !isTRUE(knitr::is_html_output())) {
    return(knitr::normal_print(x))
  }
  knitr::asis_output(paste0("\n\n", as.character(x), "\n\n"))
}

.onLoad <- function(libname, pkgname) {
  ## Register all S7 methods (including the `+` Ops method that powers
  ## `plot + geom_point()`) with the S3/S4 dispatch tables.
  S7::methods_register()
  ## S7's registration of its own print methods (S4, on base::print) drops
  ## this package's NAMESPACE-directive S3 registrations for print, so
  ## plain-S3 print methods must be re-registered explicitly (reproduced
  ## with a minimal package; S7 0.2.2, R 4.5.2).
  registerS3method("print", "ggplot3_aes", print.ggplot3_aes,
                   envir = asNamespace(pkgname))
  registerS3method("print", "ggplot3_render", print.ggplot3_render,
                   envir = asNamespace(pkgname))

  ## Plots render inline in R Markdown / Quarto when knitr is present.
  ## Registered conditionally so knitr stays an optional dependency.
  if (requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "ggplot3::Ggplot3Plot", knit_print_plot,
                     envir = asNamespace("knitr"))
    registerS3method("knit_print", "ggplot3_render", knit_print_render,
                     envir = asNamespace("knitr"))
  }
}
