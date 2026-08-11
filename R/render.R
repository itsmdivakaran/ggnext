# Render dispatch: one plot object, multiple targets --------------------------
#
# render() is an S7 generic dispatching on the plot. Every target follows the
# same two-step contract:
#
#   buffer <- build_geometry(plot)   # shared, computed exactly once per call
#   output <- render_<target>(buffer)  # target-specific serialization only
#
# Renderers never receive the plot object, so they cannot reach around the
# buffer — that is what makes "no code duplication in the geometry step"
# structural rather than aspirational.

#' @include classes.R
NULL

#' Render a ggplot3 plot
#'
#' Computes the plot geometry once and serializes it to the chosen render
#' target: a standalone SVG document (`"static"`) or a self-contained
#' interactive HTML page with a `<canvas>` element, hover tooltips, and
#' scroll-to-zoom (`"interactive"`).
#'
#' @param plot A [Ggplot3Plot] object.
#' @param ... Method arguments. The [Ggplot3Plot] method takes `target`
#'   (`"static"` for SVG, `"interactive"` for HTML/canvas; defaults to
#'   `"static"` unless the plot carries an [interact()] spec) and `file`
#'   (optional path to write the output to — use `.svg` for the static
#'   target and `.html` for the interactive one).
#' @return The rendered document as a length-1 character vector of class
#'   `ggplot3_render` (printed as a short summary, not the full source),
#'   invisibly when `file` is given.
#' @examples
#' p <- ggplot3(cars, aes(speed, dist)) + geom_point()
#' svg <- render(p)                          # static SVG (the default)
#' html <- render(p + interact())            # interactive HTML/canvas
#' @export
render <- new_generic("render", dispatch_args = "plot")

method(render, Ggplot3Plot) <- function(plot, ..., target = NULL, file = NULL) {
  # Static-first: an un-adorned plot renders to SVG; `+ interact()` flips
  # the default to the interactive target. An explicit `target` wins.
  if (is.null(target)) {
    # Static-first: an un-adorned plot renders to SVG. Adding interact() or
    # animate() opts into the interactive target.
    target <- if (is.null(plot@interaction) && is.null(plot@animation)) {
      "static"
    } else {
      "interactive"
    }
  }
  target <- match.arg(target, c("static", "interactive"))
  buffer <- build_geometry(plot)
  output <- switch(target,
    static = render_svg(buffer),
    # Frames are only computed for the target that can play them; a static
    # render of an animated plot shows all the data at once.
    interactive = render_html(build_animation(plot, buffer))
  )
  output <- structure(output, class = c("ggplot3_render", "character"), target = target)
  if (!is.null(file)) {
    writeLines(output, file, useBytes = TRUE)
    return(invisible(output))
  }
  output
}

#' @export
print.ggplot3_render <- function(x, ...) {
  # The rendered document is a single long string; dumping it to the
  # console is never what an interactive user wants.
  cat(
    "<ggplot3 ", attr(x, "target"), " render: ",
    format(nchar(unclass(x)), big.mark = ","), " characters>\n",
    "Use render(p, file = ...) to save it, or cat(x) to see the source.\n",
    sep = ""
  )
  invisible(x)
}

#' Render a geometry buffer to a self-contained interactive HTML page
#'
#' Injects the JSON-serialized buffer into the vanilla-JS canvas template
#' shipped in `inst/templates/`. No CDN, no D3, no plotly.js, no ggiraph
#' assets — the page works offline from a `file://` URL.
#'
#' @param buffer Output of build_geometry().
#' @return Length-1 character vector: a complete HTML document.
#' @noRd
render_html <- function(buffer) {
  template_path <- system.file("templates", "canvas.html", package = "ggplot3")
  if (template_path == "") {
    stop("Interactive template not found; reinstall ggplot3.")
  }
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  spec_json <- to_json(buffer)
  title <- paste0(buffer$y$title, " vs ", buffer$x$title)
  out <- sub("__GGPLOT3_TITLE__", xml_escape(title), template, fixed = TRUE)
  # fixed = TRUE keeps JSON contents from being interpreted as replacement
  # backreferences.
  sub("__GGPLOT3_SPEC__", spec_json, out, fixed = TRUE)
}
