# Animation: one geometry buffer per frame -------------------------------------
#
# Animation reuses the whole static pipeline: for each level of the
# transition variable, the plot is rebuilt with the data filtered to that
# level, and the resulting marks are collected as a frame. Scales are trained
# once on the *full* data so axes hold still while frames play — the thing
# that makes an animation readable.

#' @include build.R
NULL

# Attach frames to an already-built buffer. Returns the buffer unchanged when
# the plot has no animation spec.
build_animation <- function(plot, buffer) {
  anim <- plot@animation
  if (is.null(anim)) {
    return(buffer)
  }
  data <- plot@data
  if (is.null(data) || !anim@var %in% names(data)) {
    stop(
      "animate() variable `", anim@var, "` not found in the plot data.",
      call. = FALSE
    )
  }
  key <- data[[anim@var]]
  levels_ <- if (is.factor(key)) levels(key) else sort(unique(key))

  # Fix the axis limits to the full-data domains from the already-built
  # buffer, so every frame shares one coordinate system.
  fixed <- plot
  ref <- buffer$panels[[1]]
  if (is.null(fixed@scales$x)) {
    fixed@scales$x <- ScaleContinuous(aesthetic = "x")
  }
  if (is.null(fixed@scales$y)) {
    fixed@scales$y <- ScaleContinuous(aesthetic = "y")
  }
  if (S7_inherits(fixed@scales$x, ScaleContinuous)) {
    fixed@scales$x@limits <- trans_invert(fixed@scales$x@trans, ref$x$domain)
    fixed@scales$x@expand <- 0
  }
  if (S7_inherits(fixed@scales$y, ScaleContinuous)) {
    fixed@scales$y@limits <- trans_invert(fixed@scales$y@trans, ref$y$domain)
    fixed@scales$y@expand <- 0
  }
  fixed@animation <- NULL

  frames <- lapply(levels_, function(lv) {
    sub <- fixed
    keep <- which(as.character(key) == as.character(lv))
    sub@data <- data[keep, , drop = FALSE]
    # An empty frame would fail scale training; emit no marks instead.
    if (nrow(sub@data) == 0) {
      return(lapply(buffer$panels, function(p) list()))
    }
    fb <- tryCatch(build_geometry(sub), error = function(e) NULL)
    if (is.null(fb)) {
      return(lapply(buffer$panels, function(p) list()))
    }
    lapply(seq_along(buffer$panels), function(i) {
      if (i <= length(fb$panels)) fb$panels[[i]]$layers else list()
    })
  })

  buffer$animation <- list(
    var = anim@var,
    duration = anim@duration,
    easing = anim@easing,
    loop = anim@loop,
    labels = as.list(as.character(levels_)),
    frames = frames
  )
  buffer
}
