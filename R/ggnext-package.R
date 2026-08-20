#' @keywords internal
"_PACKAGE"

## ggnext deliberately imports the whole of S7: the class system is used in
## every file and a curated importFrom() list would need constant maintenance.
## S7 has zero hard dependencies of its own, so this keeps the transitive
## dependency graph at exactly: ggnext -> S7 -> (nothing).
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

#' A small self-contained, seedable random stream
#'
#' Several layouts need reproducible randomness (jitter offsets, the
#' force-layout starting positions, the gallery's illustrative data). Calling
#' `set.seed()` would reset R's global generator out from under the caller,
#' so this implements its own minimal-standard Lehmer generator (Park &
#' Miller 1988) instead — it never reads or writes `.Random.seed` and has no
#' effect outside the returned object.
#'
#' @param seed Integer seed.
#' @return A list of draw functions (`unif`, `norm`, `exp`, `bernoulli`,
#'   `choice`), each advancing the same private stream.
#' @noRd
local_rng <- function(seed) {
  state <- (abs(as.integer(seed)) %% 2147483646L) + 1L
  step <- function() {
    state <<- (state * 16807) %% 2147483647
    state / 2147483647
  }
  draws <- function(n) vapply(seq_len(n), function(i) step(), numeric(1))
  list(
    unif = function(n, min = 0, max = 1) min + draws(n) * (max - min),
    norm = function(n, mean = 0, sd = 1) {
      # Box-Muller: each pair of uniforms yields two normals (cos and sin
      # branches), so half as many pairs as n are needed, not n.
      half <- ceiling(n / 2)
      u <- draws(2L * half)
      r <- sqrt(-2 * log(pmax(u[seq_len(half)], .Machine$double.eps)))
      theta <- 2 * pi * u[half + seq_len(half)]
      z <- c(r * cos(theta), r * sin(theta))
      mean + sd * z[seq_len(n)]
    },
    exp = function(n, rate = 1) -log(pmax(draws(n), .Machine$double.eps)) / rate,
    bernoulli = function(n, prob) as.integer(draws(n) < prob),
    choice = function(values, n, prob) {
      cum <- cumsum(prob) / sum(prob)
      values[findInterval(draws(n), cum) + 1L]
    }
  )
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
  registerS3method("print", "ggnext_aes", print.ggnext_aes,
                   envir = asNamespace(pkgname))
  registerS3method("print", "ggnext_render", print.ggnext_render,
                   envir = asNamespace(pkgname))
  registerS3method("print", "ggnext_check", print.ggnext_check,
                   envir = asNamespace(pkgname))

  ## Plots render inline in R Markdown / Quarto when knitr is present.
  ## Registered conditionally so knitr stays an optional dependency.
  if (requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "ggnext::GgnextPlot", knit_print_plot,
                     envir = asNamespace("knitr"))
    registerS3method("knit_print", "ggnext_render", knit_print_render,
                     envir = asNamespace("knitr"))
  }
}
