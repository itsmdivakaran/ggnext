#' @include api.R theme.R labels.R facet.R scale-discrete.R geoms.R geoms-base.R geoms-clinical.R geoms-layout.R geoms-ml.R geoms-special.R
NULL

# Pipe sugar --------------------------------------------------------------
#
# Every grammar constructor (geom_*(), theme_*(), scale_*(), coord_*(),
# facet_*(), labs() and friends) already composes with `+`: `p + geom_point()`
# calls plot_add() (R/api.R) to fold the returned Layer/Theme/Scale/... into
# the plot. This file makes the same constructors usable as the first stage
# of a pipe too: `p |> geom_point()` desugars to `geom_point(p)`, so when the
# first argument turns out to be a GgnextPlot, plot_add() runs instead of the
# constructor treating the plot as its own first parameter.
#
# `+` stays the one true composition operator - reusable Theme/Scale bundles
# and Reduce(`+`, layers, p) keep working exactly as before. This is a
# call-site adapter, applied uniformly at load time, that never touches an
# existing constructor's body or signature.

#' Wrap a grammar constructor so a leading `GgnextPlot` argument routes
#' through `plot_add()` instead of being treated as the constructor's own
#' first parameter
#'
#' A constructor's own first argument may rely on non-standard evaluation
#' (`facet_wrap(cyl)` captures `cyl` unevaluated, to be resolved against the
#' plot's data later). So this only evaluates the *first* call argument, to
#' test whether it is a plot; if that probe errors (an unbound NSE symbol
#' like `cyl`) or returns anything other than a `GgnextPlot`, the original
#' call is re-run unevaluated and unchanged - which is what keeps
#' `facet_wrap(cyl)` working exactly as it always has.
#'
#' The body never binds any of its own formal arguments by name (it works
#' entirely off `sys.call()`), so the wrapper's formals are overwritten with
#' the constructor's own afterward - purely so `args()`, RStudio's
#' autocomplete, and the generated `\usage` entries still show the real
#' parameter list instead of a bare `...`. Because of that overwrite, the
#' constructor itself cannot be captured under a name that might collide
#' with one of its own parameters - `geom_function(fn, xlim, n, ...)` has a
#' parameter named `fn`, which would otherwise shadow a same-named capture
#' with the caller's argument the moment the formals were copied across.
#' `.ggnext_pipe_target` is deliberately not a plausible parameter name.
#'
#' @param .ggnext_pipe_target A constructor function to make pipe-aware.
#' @return A function with the same formals and calling convention as
#'   `.ggnext_pipe_target`, plus support for `plot |> fn(...)`.
#' @noRd
pipe_ready <- function(.ggnext_pipe_target) {
  force(.ggnext_pipe_target)
  wrapper <- function(...) {
    call_args <- as.list(sys.call())[-1]
    if (length(call_args) == 0) {
      return(.ggnext_pipe_target())
    }
    first_val <- tryCatch(
      eval(call_args[[1]], envir = parent.frame()),
      error = function(e) NULL
    )
    if (!is.null(first_val) && S7::S7_inherits(first_val, GgnextPlot)) {
      rest_call <- as.call(c(.ggnext_pipe_target, call_args[-1]))
      return(plot_add(first_val, eval(rest_call, envir = parent.frame())))
    }
    full_call <- as.call(c(.ggnext_pipe_target, call_args))
    eval(full_call, envir = parent.frame())
  }
  formals(wrapper) <- formals(.ggnext_pipe_target)
  wrapper
}

#' Make every grammar constructor defined so far in `env` pipe-aware
#'
#' Called once, as top-level code below, while this file is being sourced
#' during package build/load - i.e. after every `geom_*`/`theme_*`/
#' `scale_*`/`coord_*`/`facet_*` constructor has been defined (enforced by
#' this file's `@include` tag, which collates it last among them) but before
#' the namespace is sealed. Rebinding already-exported names from `.onLoad()`
#' does not work reliably: by then the namespace binding lock is already in
#' effect under some loading paths (observed with `pkgload::load_all()`),
#' so this must run at source-time instead, while the namespace is still an
#' ordinary, writable environment.
#'
#' A handful of internal helpers happen to match the naming pattern too
#' (`theme_to_list()`, `facet_split()`, ...); wrapping them is harmless; they
#' are never called with a plot as their first argument, so they always take
#' the pass-through path.
#'
#' A genuinely zero-argument constructor (`theme_minimal()`, `coord_flip()`,
#' ...) has no parameter to route a piped plot through in the first place:
#' `x |> f()` always desugars to `f(x)`, so `f` needs at least one formal,
#' real or `...`, to receive `x`. Giving one to a function documented and
#' implemented as taking none would only reintroduce the `\usage` mismatch
#' `formals(wrapper) <- formals(...)` exists to avoid, so these are left
#' unwrapped entirely and stay `+`-only, same as always.
#'
#' @param env The namespace environment under construction.
#' @return Invisibly, the names rebound.
#' @noRd
enable_pipe_sugar <- function(env) {
  pattern <- "^(geom_|theme_|scale_|coord_|facet_)|^(labs|ggtitle|xlab|ylab|xlim|ylim|lims|interact|animate|plot_size)$"
  nms <- ls(envir = env, pattern = pattern)
  nms <- nms[vapply(nms, function(nm) is.function(get(nm, envir = env)), logical(1))]
  nms <- nms[vapply(nms, function(nm) length(formals(get(nm, envir = env))) > 0, logical(1))]
  for (nm in nms) {
    assign(nm, pipe_ready(get(nm, envir = env)), envir = env)
  }
  invisible(nms)
}

enable_pipe_sugar(environment())
