# Faceting ---------------------------------------------------------------------
#
# A facet splits the plot data into subsets and draws one panel per subset.
# The buffer therefore carries a *list* of panels; a non-faceted plot is
# simply the one-panel case, so renderers have a single code path.

#' @include classes.R
NULL

#' Wrap panels into a grid
#'
#' Splits the data by one or more variables and lays the resulting panels
#' out in a rectangular grid, wrapping to a new row as needed.
#'
#' @param vars Faceting variables: unquoted names (`facet_wrap(cyl)`),
#'   a character vector, or several names.
#' @param ncol,nrow Force a panel-grid shape; `NULL` picks a near-square
#'   layout.
#' @param scales `"fixed"` (all panels share axes — best for comparison),
#'   `"free_x"`, `"free_y"`, or `"free"` (each panel scales to its own data).
#' @return A [Facet] object to add to a plot with `+`.
#' @examples
#' ggnext(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   facet_wrap(Species)
#' @export
facet_wrap <- function(vars, ncol = NULL, nrow = NULL, scales = "fixed") {
  Facet(
    vars = facet_var_names(substitute(vars), parent.frame()),
    ncol = ncol, nrow = nrow,
    scales = match.arg(scales, c("fixed", "free", "free_x", "free_y")),
    type = "wrap"
  )
}

#' Lay panels out in a rows-by-columns grid
#'
#' `facet_grid(rows, cols)` builds a two-way panel matrix: one row per level
#' of `rows`, one column per level of `cols`.
#'
#' @param rows Variable defining the panel rows (unquoted name or string).
#' @param cols Variable defining the panel columns; `NULL` for a single
#'   column.
#' @param scales As in [facet_wrap()].
#' @return A [Facet] object to add to a plot with `+`.
#' @examples
#' d <- transform(mtcars, cyl = factor(cyl), am = factor(am))
#' ggnext(d, aes(disp, mpg)) + geom_point() + facet_grid(am, cyl)
#' @export
facet_grid <- function(rows, cols = NULL, scales = "fixed") {
  row_var <- facet_var_names(substitute(rows), parent.frame())
  col_expr <- substitute(cols)
  col_var <- if (is.null(col_expr)) character() else {
    facet_var_names(col_expr, parent.frame())
  }
  Facet(
    vars = c(row_var, col_var),
    ncol = NULL, nrow = NULL,
    scales = match.arg(scales, c("fixed", "free", "free_x", "free_y")),
    type = "grid"
  )
}

# Resolve a faceting argument to plain column names. Accepts an unquoted
# symbol, a character vector, or c(a, b) / vars(a, b) style calls.
facet_var_names <- function(expr, env) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (is.character(expr)) {
    return(expr)
  }
  if (is.call(expr) && as.character(expr[[1]]) %in% c("c", "vars")) {
    return(unlist(lapply(as.list(expr)[-1], function(e) {
      if (is.symbol(e)) as.character(e) else as.character(eval(e, env))
    })))
  }
  val <- tryCatch(eval(expr, env), error = function(e) NULL)
  if (is.character(val)) {
    return(val)
  }
  stop("Faceting variables must be column names, e.g. facet_wrap(Species).")
}

# Split layer data into facet subsets, returning a list of
# list(key = <panel label>, rows = <integer row indices>) in panel order.
# Panel order is the order of the levels, columns varying fastest for grids.
facet_split <- function(facet, data) {
  missing_vars <- setdiff(facet@vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      "Faceting variable(s) not found in the data: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  cols <- lapply(facet@vars, function(v) {
    x <- data[[v]]
    if (is.factor(x)) x else factor(x, levels = sort(unique(as.character(x))))
  })
  names(cols) <- facet@vars
  keys <- do.call(paste, c(lapply(cols, as.character), sep = " | "))
  # interaction() orders with the first variable varying slowest, which is
  # what both wrap (reading order) and grid (rows then columns) want.
  combos <- unique(keys[order(do.call(order, rev(unname(cols))))])
  lapply(combos, function(k) list(key = k, rows = which(keys == k)))
}

# Choose a panel grid shape: honor ncol/nrow when given, else near-square.
facet_layout <- function(facet, n_panels, data) {
  if (facet@type == "grid") {
    if (length(facet@vars) >= 2) {
      n_col <- length(unique(as.character(data[[facet@vars[2]]])))
      return(list(ncol = n_col, nrow = ceiling(n_panels / n_col)))
    }
    return(list(ncol = 1, nrow = n_panels))
  }
  if (!is.null(facet@ncol)) {
    return(list(ncol = facet@ncol, nrow = ceiling(n_panels / facet@ncol)))
  }
  if (!is.null(facet@nrow)) {
    return(list(nrow = facet@nrow, ncol = ceiling(n_panels / facet@nrow)))
  }
  ncol <- ceiling(sqrt(n_panels))
  list(ncol = ncol, nrow = ceiling(n_panels / ncol))
}
