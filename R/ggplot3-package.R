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
}
