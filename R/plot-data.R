# Exact-data export ------------------------------------------------------------
#
# What a plot actually draws is often not what went in: stats aggregate,
# positions stack, facets split. plot_data() returns exactly the values each
# layer plots — no join columns, no intermediate scratch, no normalized
# coordinates — so the numbers behind a figure can be checked, tabled, or
# shipped alongside it.

#' @include build.R
NULL

# Columns that exist only to drive the internal pipeline and are never part
# of what a reader would call "the data behind this plot".
PLOT_DATA_INTERNAL <- c(
  "xwidth", "params", "xspan", "yspan", "xdomain", "ydomain",
  "theme", "marks_precomputed",
  # Reference-line positions: chrome the geom draws, not plotted data.
  "xref", "yref", "yzero", "ythresh", "xec50", "theta_order"
)

#' Extract the exact data a plot draws
#'
#' Returns the computed values behind each layer — post-stat, post-position,
#' post-facet — in data units (never normalized coordinates). For a
#' `geom_point()` layer that is the x/y you supplied; for `geom_histogram()`
#' it is the bin centers, counts, and bin edges actually drawn; for
#' `geom_boxplot()` the quartiles and whisker ends.
#'
#' Only columns the layer genuinely uses are returned. Constant grouping
#' columns are dropped, so a plot with no `group` aesthetic has no `group`
#' column.
#'
#' @param plot A [Ggplot3Plot].
#' @param layer Which layer to return: an integer index, or `NULL` (the
#'   default) for a list of all layers. With one layer, the data frame
#'   itself is returned rather than a one-element list.
#' @param panel Which facet panel to return: an integer index, `NULL` (the
#'   default) for all panels combined with a `panel` column, or `"list"` to
#'   get a list per panel.
#' @return A data frame, or a list of data frames.
#' @examples
#' p <- ggplot3(cars, aes(speed, dist)) + geom_point()
#' head(plot_data(p))
#'
#' # Stats: what geom_histogram() actually drew.
#' h <- ggplot3(cars, aes(speed)) + geom_histogram(bins = 5)
#' plot_data(h)
#'
#' # Facets keep a panel column.
#' f <- ggplot3(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() + facet_wrap(Species)
#' head(plot_data(f))
#' @export
plot_data <- function(plot, layer = NULL, panel = NULL) {
  if (!S7_inherits(plot, Ggplot3Plot)) {
    stop("`plot` must be a ggplot3 plot object.")
  }
  computed <- compute_layer_data(plot)

  # computed is [[panel]][[layer]]; pivot to layer-major output.
  n_layers <- length(plot@layers)
  per_layer <- lapply(seq_len(n_layers), function(li) {
    parts <- lapply(seq_along(computed), function(pi) {
      df <- computed[[pi]][[li]]
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      if (length(computed) > 1 && is.null(panel)) {
        df <- cbind(panel = names(computed)[pi], df, stringsAsFactors = FALSE)
      }
      df
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0) {
      return(data.frame())
    }
    if (identical(panel, "list")) {
      names(parts) <- names(computed)
      return(parts)
    }
    if (is.numeric(panel)) {
      if (panel > length(parts)) {
        stop("`panel` ", panel, " does not exist; the plot has ",
             length(parts), " panel(s).")
      }
      return(parts[[panel]])
    }
    do.call(rbind, parts)
  })

  if (!is.null(layer)) {
    if (!is.numeric(layer) || layer < 1 || layer > n_layers) {
      stop("`layer` must be an integer between 1 and ", n_layers, ".")
    }
    return(per_layer[[layer]])
  }
  if (n_layers == 1) per_layer[[1]] else per_layer
}

#' Write the exact plot data to CSV
#'
#' Convenience wrapper around [plot_data()] and [utils::write.csv()] for
#' publishing the numbers behind a figure next to the figure itself.
#'
#' @param plot A [Ggplot3Plot].
#' @param file Output path. With multiple layers and no `layer` given, the
#'   layer index is inserted before the extension
#'   (`fig.csv` -> `fig-1.csv`, `fig-2.csv`).
#' @param layer Optional layer index (see [plot_data()]).
#' @return The file path(s) written, invisibly.
#' @examples
#' p <- ggplot3(cars, aes(speed, dist)) + geom_point()
#' out <- file.path(tempdir(), "cars.csv")
#' write_plot_data(p, out)
#' @export
write_plot_data <- function(plot, file, layer = NULL) {
  dat <- plot_data(plot, layer = layer)
  if (is.data.frame(dat)) {
    utils::write.csv(dat, file, row.names = FALSE)
    return(invisible(file))
  }
  stem <- sub("\\.[^.]*$", "", file)
  ext <- regmatches(file, regexpr("\\.[^.]*$", file))
  if (length(ext) == 0) ext <- ".csv"
  paths <- vapply(seq_along(dat), function(i) {
    p <- paste0(stem, "-", i, ext)
    utils::write.csv(dat[[i]], p, row.names = FALSE)
    p
  }, character(1))
  invisible(paths)
}

# Run the grammar pipeline up to (but not through) scale mapping, and return
# the computed values as data frames: [[panel]][[layer]].
compute_layer_data <- function(plot) {
  if (length(plot@layers) == 0) {
    stop("The plot has no layers, so there is no plotted data.")
  }
  # Reuse build_geometry()'s own pipeline by asking it for the pre-scaling
  # values. Rather than duplicate the walk, run the shared helper below.
  staged <- stage_values(plot)
  lapply(staged, function(ps) {
    lapply(ps$layers, function(lv) values_to_df(lv))
  })
}

# One computed layer's values as a tidy data frame, with internal scratch
# columns and constant grouping columns removed.
values_to_df <- function(lv) {
  values <- lv$values
  values <- values[setdiff(names(values), PLOT_DATA_INTERNAL)]
  keep <- vapply(values, function(v) is.atomic(v) && !is.null(v), logical(1))
  values <- values[keep]
  if (length(values) == 0) {
    return(data.frame())
  }
  n <- max(vapply(values, length, integer(1)))
  values <- lapply(values, function(v) if (length(v) == n) v else rep_len(v, n))
  # A `group` column that never varies carries no information for a reader.
  if (!is.null(values$group) && length(unique(values$group)) == 1) {
    values$group <- NULL
  }
  as.data.frame(values, stringsAsFactors = FALSE)
}
