# Aesthetic mappings ----------------------------------------------------------

# Aesthetics the grammar understands. `colour` is accepted as an alias for
# `color` at construction time so both spellings work. time/status feed
# stat_km(); truth/score feed stat_roc().
GGPLOT3_AESTHETICS <- c(
  "x", "y", "color", "size", "group", "label",
  "ymin", "ymax", "xend", "yend",
  "time", "status", "truth", "score"
)

#' Construct aesthetic mappings
#'
#' `aes()` captures unevaluated expressions that describe how columns of the
#' data map onto visual properties. The first two unnamed arguments are taken
#' as `x` and `y`. Expressions are evaluated later, against the layer data,
#' in the environment where `aes()` was called.
#'
#' @param ... Name-value pairs of aesthetics and expressions, e.g.
#'   `aes(displ, hwy, color = class)`. Supported aesthetics: `x`, `y`,
#'   `color` (alias `colour`), `size`.
#' @return An object of class `ggplot3_aes`.
#' @examples
#' aes(speed, dist)
#' aes(x = speed, y = dist, color = gear)
#' @export
aes <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  nms <- names(exprs)
  if (is.null(nms)) nms <- rep("", length(exprs))

  # Positional arguments fill x then y, matching ggplot muscle memory.
  unnamed <- which(nms == "")
  if (length(unnamed) > 2) {
    stop("aes() accepts at most two unnamed arguments (taken as x and y).")
  }
  nms[unnamed] <- c("x", "y")[seq_along(unnamed)]
  nms[nms == "colour"] <- "color"

  unknown <- setdiff(nms, GGPLOT3_AESTHETICS)
  if (length(unknown) > 0) {
    stop(
      "Unsupported aesthetic(s): ", paste(unknown, collapse = ", "),
      ". This spike supports: ", paste(GGPLOT3_AESTHETICS, collapse = ", "), "."
    )
  }
  if (anyDuplicated(nms)) {
    stop("Duplicated aesthetic(s) in aes().")
  }
  names(exprs) <- nms

  structure(
    list(exprs = exprs, env = parent.frame()),
    class = "ggplot3_aes"
  )
}

#' @export
print.ggplot3_aes <- function(x, ...) {
  cat("<ggplot3 aesthetic mapping>\n")
  for (nm in names(x$exprs)) {
    cat("  ", nm, " -> ", deparse(x$exprs[[nm]]), "\n", sep = "")
  }
  invisible(x)
}

# Evaluate every mapped expression against `data`, falling back to the
# environment captured at aes() time for symbols that are not columns.
# Returns a named list of equal-length vectors.
eval_aes <- function(mapping, data) {
  if (is.null(mapping)) {
    return(list())
  }
  values <- lapply(names(mapping$exprs), function(nm) {
    e <- mapping$exprs[[nm]]
    v <- eval(e, data, mapping$env)
    # A classic trap: aes(color = class) where `class` is not a column falls
    # back to the calling environment and finds base R's class() function.
    # Catch it here with an error that names the actual problem.
    if (is.function(v)) {
      stop(
        "Aesthetic `", nm, " = ", deparse(e), "` evaluated to a function, ",
        "not a data column. Is `", deparse(e), "` a column in your data? ",
        "(Columns present: ", paste(names(data), collapse = ", "), ")"
      )
    }
    v
  })
  names(values) <- names(mapping$exprs)
  n <- nrow(data)
  values <- lapply(values, function(v) if (length(v) == 1L) rep(v, n) else v)
  bad <- vapply(values, function(v) length(v) != n, logical(1))
  if (any(bad)) {
    stop(
      "Aesthetic(s) of wrong length: ",
      paste(names(values)[bad], collapse = ", ")
    )
  }
  values
}

# Merge plot-level and layer-level mappings; layer mappings win.
merge_aes <- function(plot_mapping, layer_mapping) {
  if (is.null(plot_mapping)) {
    return(layer_mapping)
  }
  if (is.null(layer_mapping)) {
    return(plot_mapping)
  }
  merged <- plot_mapping
  merged$exprs[names(layer_mapping$exprs)] <- layer_mapping$exprs
  # Expressions from the layer must evaluate in the layer's environment;
  # this simplification is acceptable because both environments are almost
  # always the same user frame in practice. Documented for the full build.
  merged
}
