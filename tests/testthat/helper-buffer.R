# Test helpers for reaching into the computed-geometry buffer.
#
# The buffer holds a list of panels (a non-faceted plot is the one-panel
# case), so tests that only care about a single panel go through these
# helpers rather than repeating `$panels[[1]]` everywhere.

# The whole buffer for a plot.
buf <- function(plot) ggplot3:::build_geometry(plot)

# One panel of a plot's buffer (the only panel, by default).
panel_of <- function(plot, i = 1) buf(plot)$panels[[i]]

# One layer's marks from one panel.
marks_of <- function(plot, layer = 1, panel = 1) {
  panel_of(plot, panel)$layers[[layer]]$marks
}

# Every mark of a given type across a panel's layers.
marks_typed <- function(plot, type, panel = 1) {
  all <- unlist(
    lapply(panel_of(plot, panel)$layers, function(l) l$marks),
    recursive = FALSE
  )
  Filter(function(m) m$type == type, all)
}

# Pull one field out of a list of marks.
mark_field <- function(marks, field) {
  vapply(marks, function(m) m[[field]], FUN.VALUE = marks[[1]][[field]][1])
}
