# Plot labels and title block --------------------------------------------------

#' @include classes.R
NULL

#' Labels: plot title block and axis/legend titles
#'
#' Created by [labs()], [ggtitle()], [xlab()], [ylab()]; added with `+`.
#' Any field left `NULL` falls back to the default (for axes, the deparsed
#' aesthetic mapping).
#'
#' @param title,subtitle,caption,tag Plot title block text.
#' @param x,y Axis titles.
#' @param color,size,fill Legend titles.
#' @return An S7 object of class `Labels`; usually built by [labs()].
#' @export
Labels <- new_class("Labels", properties = list(
  title = class_any,
  subtitle = class_any,
  caption = class_any,
  tag = class_any,
  x = class_any,
  y = class_any,
  color = class_any,
  size = class_any,
  fill = class_any
))

#' Set plot and axis labels
#'
#' Every argument is optional; only the ones you supply are changed, so
#' `labs()` can be added repeatedly to build up a title block.
#'
#' @param title Plot title, drawn above the panel.
#' @param subtitle Subtitle, drawn under the title in a lighter style.
#' @param caption Caption, drawn bottom-right (source notes, methods).
#' @param tag Panel tag (e.g. `"A"`), drawn top-left — for multi-panel
#'   figures in publications.
#' @param x,y Axis titles; override the deparsed `aes()` expression.
#' @param color,size,fill Legend titles for the matching aesthetic.
#' @return A [Labels] object to add to a plot with `+`.
#' @examples
#' ggplot3(cars, aes(speed, dist)) +
#'   geom_point() +
#'   labs(
#'     title = "Stopping distance rises with speed",
#'     subtitle = "1920s road tests, 50 observations",
#'     x = "Speed (mph)",
#'     y = "Stopping distance (ft)",
#'     caption = "Source: datasets::cars"
#'   )
#' @export
labs <- function(title = NULL, subtitle = NULL, caption = NULL, tag = NULL,
                 x = NULL, y = NULL, color = NULL, size = NULL, fill = NULL) {
  Labels(
    title = title, subtitle = subtitle, caption = caption, tag = tag,
    x = x, y = y, color = color, size = size, fill = fill
  )
}

#' Set the plot title (and optionally the subtitle)
#'
#' @param label Title text.
#' @param subtitle Optional subtitle text.
#' @return A [Labels] object to add to a plot with `+`.
#' @export
ggtitle <- function(label, subtitle = NULL) {
  Labels(title = label, subtitle = subtitle)
}

#' Set the x axis title
#'
#' @param label Axis title text.
#' @return A [Labels] object to add to a plot with `+`.
#' @export
xlab <- function(label) Labels(x = label)

#' Set the y axis title
#'
#' @param label Axis title text.
#' @return A [Labels] object to add to a plot with `+`.
#' @export
ylab <- function(label) Labels(y = label)

# Merge a new Labels object over the plot's existing one; non-NULL wins.
merge_labels <- function(old, new) {
  if (is.null(old)) {
    return(new)
  }
  for (nm in names(props(new))) {
    v <- prop(new, nm)
    if (!is.null(v)) prop(old, nm) <- v
  }
  old
}

#' Set continuous axis limits
#'
#' A shorthand for `scale_x_continuous(limits = )`. Data outside the limits
#' is still computed by stats but clipped to the panel when drawn.
#'
#' @param ... Two numbers giving the lower and upper limit, or one length-2
#'   numeric vector.
#' @return A scale object to add to a plot with `+`.
#' @examples
#' ggplot3(cars, aes(speed, dist)) + geom_point() + xlim(0, 30) + ylim(0, 150)
#' @export
xlim <- function(...) scale_x_continuous(limits = collect_limits(...))

#' @rdname xlim
#' @export
ylim <- function(...) scale_y_continuous(limits = collect_limits(...))

#' Set both axis limits at once
#'
#' @param x Length-2 numeric vector for the x axis, or `NULL`.
#' @param y Length-2 numeric vector for the y axis, or `NULL`.
#' @return A list of scales, which `+` adds one at a time.
#' @export
lims <- function(x = NULL, y = NULL) {
  out <- list()
  if (!is.null(x)) out <- c(out, list(scale_x_continuous(limits = x)))
  if (!is.null(y)) out <- c(out, list(scale_y_continuous(limits = y)))
  out
}

# Accept either xlim(0, 30) or xlim(c(0, 30)).
collect_limits <- function(...) {
  v <- unlist(list(...))
  if (length(v) != 2) {
    stop("Limits must be two numbers, e.g. xlim(0, 30) or xlim(c(0, 30)).")
  }
  as.numeric(v)
}
