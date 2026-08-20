# Machine-learning diagnostic geoms --------------------------------------------
#
# First-class geoms for the plots that normally require hand-wrangling a
# model object into a data frame first: SHAP beeswarms, partial dependence,
# confusion matrices, calibration curves, lift/gain, learning curves,
# residual diagnostics, silhouettes, and embeddings.
#
# Each takes a tidy data frame (never a fitted model object — that would
# pull in a modelling dependency), so any model that can produce these
# columns works.

#' @include classes.R marks.R geoms.R
NULL

# --- SHAP beeswarm -----------------------------------------------------------

#' GeomShap: SHAP beeswarm, one row per feature
#'
#' @noRd
GeomShap <- new_class("GeomShap", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "shap",
      default_params = list(alpha = 0.7, size = 2.5),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomShap) <- function(geom, scaled) {
  lapply(seq_along(scaled$x), function(i) {
    mk_circle(scaled$x[[i]], scaled$y[[i]], scaled$params$size,
              scaled$color[[i]], alpha = scaled$params$alpha)
  })
}

#' SHAP value beeswarm
#'
#' One horizontal swarm per feature showing every observation's SHAP value,
#' with points nudged vertically so overlapping values stay countable.
#' Color the points by the feature value to read the direction of effect.
#'
#' @param mapping,data Standard layer overrides. Map the SHAP value to `x`
#'   and the feature name to `y`; map `color` to the feature value.
#' @param size Point radius in px.
#' @param alpha Point opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   feature = rep(c("age", "income", "tenure"), each = 40),
#'   shap = c(rnorm(40, 0.3, 0.2), rnorm(40, -0.1, 0.3), rnorm(40, 0, 0.15)),
#'   value = runif(120)
#' )
#' ggnext(d, aes(shap, feature, color = value)) +
#'   geom_shap() +
#'   geom_vline(0, dash = "3,3") +
#'   labs(title = "SHAP value by feature", x = "SHAP value", y = NULL)
#' @export
geom_shap <- function(mapping = NULL, data = NULL, size = NULL, alpha = NULL) {
  layer_new(GeomShap(), StatBeeswarm(), mapping, data,
            list(size = size, alpha = alpha))
}

# --- Partial dependence / ICE ------------------------------------------------

#' Partial dependence and ICE curves
#'
#' A thick average partial-dependence line over thin per-observation ICE
#' curves. Map `group` to the observation id to draw the ICE spaghetti.
#'
#' @param mapping,data Standard layer overrides. Map the feature to `x` and
#'   the prediction to `y`; map `group` for ICE curves.
#' @param color Curve color.
#' @param ice Draw the individual ICE curves under the average.
#' @param ice_alpha Opacity of the ICE curves.
#' @param linewidth Width of the average line.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   x = rep(1:10, 5), id = rep(1:5, each = 10),
#'   pred = as.vector(sapply(1:5, function(i) (1:10) * 0.1 * i + rnorm(10, 0, .1)))
#' )
#' ggnext(d, aes(x, pred, group = id)) + geom_partial_dependence()
#' @export
geom_partial_dependence <- function(mapping = NULL, data = NULL, color = NULL,
                                    ice = TRUE, ice_alpha = 0.25,
                                    linewidth = NULL) {
  layer_new(GeomPDP(), StatPDP(), mapping, data,
            list(color = color, ice = ice, ice_alpha = ice_alpha,
                 linewidth = linewidth))
}

#' GeomPDP: average partial-dependence line over ICE curves
#' @noRd
GeomPDP <- new_class("GeomPDP", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "pdp",
      default_params = list(
        color = "#2B6BE0", linewidth = 3, ice = TRUE, ice_alpha = 0.25,
        alpha = 1
      )
    ))
  }
)

method(build_marks, GeomPDP) <- function(geom, scaled) {
  marks <- list()
  avg <- which(scaled$group == ".average")
  if (isTRUE(scaled$params$ice)) {
    for (g in setdiff(unique(scaled$group), ".average")) {
      idx <- which(scaled$group == g)
      ord <- idx[order(scaled$x[idx])]
      marks <- c(marks, list(mk_line(
        scaled$x[ord], scaled$y[ord], stroke = scaled$color[[ord[1]]],
        width = 1, alpha = scaled$params$ice_alpha
      )))
    }
  }
  if (length(avg) > 0) {
    ord <- avg[order(scaled$x[avg])]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = scaled$params$color,
      width = scaled$params$linewidth
    )))
  }
  marks
}

# --- Confusion matrix --------------------------------------------------------

#' GeomConfusionMatrix: annotated tile heatmap of a confusion matrix
#' @noRd
GeomConfusionMatrix <- new_class("GeomConfusionMatrix", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "confusion_matrix",
      default_params = list(alpha = 1, label_size = 13),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomConfusionMatrix) <- function(geom, scaled) {
  marks <- list()
  for (i in seq_along(scaled$x)) {
    marks <- c(marks, list(mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha,
      stroke = "#FFFFFF", stroke_width = 2
    )))
  }
  for (i in seq_along(scaled$x)) {
    marks <- c(marks, list(mk_text(
      scaled$x[[i]], scaled$y[[i]], scaled$label[[i]],
      size = scaled$params$label_size, color = contrast_text(scaled$color[[i]])
    )))
  }
  marks
}

#' Confusion matrix
#'
#' A tile heatmap of predicted vs actual classes, shaded by row-normalized
#' rate and annotated with counts, so class imbalance does not hide errors.
#'
#' @param mapping,data Standard layer overrides. Map the predicted class to
#'   `x`, the actual class to `y`. Provide counts via `size`, or pass raw
#'   per-observation rows and let the stat count them.
#' @param normalize `"row"` (default), `"col"`, `"all"`, or `"none"` —
#'   which total the shading is relative to.
#' @param label_size Annotation size in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   predicted = c("cat", "cat", "dog", "dog", "dog", "cat"),
#'   actual    = c("cat", "dog", "dog", "dog", "cat", "cat")
#' )
#' ggnext(d, aes(predicted, actual)) + geom_confusion_matrix()
#' @export
geom_confusion_matrix <- function(mapping = NULL, data = NULL,
                                  normalize = "row", label_size = NULL) {
  layer_new(GeomConfusionMatrix(), StatConfusion(normalize = normalize),
            mapping, data, list(label_size = label_size))
}

# --- Calibration -------------------------------------------------------------

#' Calibration curve
#'
#' Bins predicted probabilities and plots the observed event rate in each
#' bin against the mean prediction, with the diagonal marking perfect
#' calibration. Points off the diagonal show over- or under-confidence.
#'
#' @param mapping,data Standard layer overrides. Map the predicted
#'   probability to `x` and the binary outcome (0/1) to `y`.
#' @param bins Number of probability bins.
#' @param color Curve color.
#' @param diagonal Draw the perfect-calibration reference line.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' p <- runif(300)
#' d <- data.frame(pred = p, obs = rbinom(300, 1, p^1.3))
#' ggnext(d, aes(pred, obs)) + geom_calibration()
#' @export
geom_calibration <- function(mapping = NULL, data = NULL, bins = 10,
                             color = NULL, diagonal = TRUE) {
  layer_new(GeomCalibration(), StatCalibration(bins = bins), mapping, data,
            list(color = color, diagonal = diagonal))
}

#' GeomCalibration: binned reliability curve with a diagonal reference
#' @noRd
GeomCalibration <- new_class("GeomCalibration", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "calibration",
      default_params = list(
        color = "#2B6BE0", linewidth = 2, alpha = 1, diagonal = TRUE
      )
    ))
  }
)

method(build_marks, GeomCalibration) <- function(geom, scaled) {
  marks <- list()
  if (isTRUE(scaled$params$diagonal)) {
    # Perfect calibration: the y = x line in normalized panel space.
    marks <- c(marks, list(mk_line(
      c(0, 1), c(0, 1), stroke = "#9A9AA6", width = 1, dash = "4,3"
    )))
  }
  ord <- order(scaled$x)
  marks <- c(marks, list(mk_line(
    scaled$x[ord], scaled$y[ord], stroke = scaled$params$color,
    width = scaled$params$linewidth
  )))
  for (i in ord) {
    marks <- c(marks, list(mk_circle(
      scaled$x[[i]], scaled$y[[i]], 4, scaled$params$color
    )))
  }
  marks
}

# --- Lift / gain -------------------------------------------------------------

#' Lift and cumulative gain curves
#'
#' Sorts observations by predicted score and plots the cumulative share of
#' positives captured against the share of the population targeted — the
#' standard way to size a marketing or triage cutoff.
#'
#' @param mapping,data Standard layer overrides. Map the score to `score`
#'   and the binary outcome to `truth`.
#' @param type `"gain"` (cumulative captured) or `"lift"` (ratio to random).
#' @param color Curve color.
#' @param baseline Draw the random-targeting reference.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' s <- runif(200)
#' d <- data.frame(score = s, y = rbinom(200, 1, s))
#' ggnext(d, aes(score = score, truth = y)) + geom_lift_gain()
#' @export
geom_lift_gain <- function(mapping = NULL, data = NULL, type = "gain",
                           color = NULL, baseline = TRUE) {
  type <- match.arg(type, c("gain", "lift"))
  layer_new(GeomLiftGain(), StatLiftGain(type = type), mapping, data,
            list(color = color, baseline = baseline, curve_type = type))
}

#' GeomLiftGain: cumulative gain or lift curve
#' @noRd
GeomLiftGain <- new_class("GeomLiftGain", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "lift_gain",
      default_params = list(
        color = "#12A594", linewidth = 2, alpha = 1, baseline = TRUE,
        curve_type = "gain"
      ),
      required_aes = c("score", "truth")
    ))
  }
)

method(build_marks, GeomLiftGain) <- function(geom, scaled) {
  marks <- list()
  if (isTRUE(scaled$params$baseline)) {
    # Random targeting: the diagonal for gain, the flat 1 line for lift.
    ref <- if (scaled$params$curve_type == "gain") c(0, 1) else c(0.5, 0.5)
    marks <- c(marks, list(mk_line(
      c(0, 1), ref, stroke = "#9A9AA6", width = 1, dash = "4,3"
    )))
  }
  ord <- order(scaled$x)
  c(marks, list(mk_line(
    scaled$x[ord], scaled$y[ord], stroke = scaled$params$color,
    width = scaled$params$linewidth
  )))
}

# --- Precision-Recall ---------------------------------------------------------

#' Precision-recall curve
#'
#' Sweeps the classification threshold and plots recall (x) against
#' precision (y), with a horizontal baseline at the class prevalence
#' (instead of the diagonal [geom_roc()] uses) — the more informative
#' curve when positives are rare.
#'
#' @param mapping,data Standard layer overrides. Map `truth` (actual
#'   class; for factors the second level is the positive class) and
#'   `score` (predicted score); map `color` to compare models.
#' @param color,alpha As in [geom_point()].
#' @param linewidth Curve width.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' s <- runif(200)
#' d <- data.frame(truth = rbinom(200, 1, s * 0.3), score = s)
#' ggnext(d, aes(truth = truth, score = score)) + geom_pr()
#' @export
geom_pr <- function(mapping = NULL, data = NULL, color = NULL,
                    alpha = NULL, linewidth = NULL) {
  layer_new(GeomPR(), StatPR(), mapping, data,
            list(color = color, alpha = alpha, linewidth = linewidth))
}

#' GeomPR: precision-recall curve with a prevalence baseline
#' @noRd
GeomPR <- new_class("GeomPR", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "pr",
      default_params = list(color = "#4A6DB5", linewidth = 1.8, alpha = 1),
      required_aes = c("truth", "score")
    ))
  }
)

method(build_marks, GeomPR) <- function(geom, scaled) {
  p <- scaled$params
  groups <- group_rows(scaled)
  marks <- list()
  ref <- which(scaled$role == "ref")[1:2]
  marks[[1]] <- mk_line(
    scaled$x[ref], scaled$y[ref], stroke = "#8A8A94", width = 1, dash = "4,3"
  )
  for (idx in groups) {
    curve <- idx[scaled$role[idx] == "curve"]
    marks[[length(marks) + 1]] <- mk_line(
      scaled$x[curve], scaled$y[curve],
      stroke = scaled$color[idx][1], width = p$linewidth,
      alpha = scaled$alpha[idx][1]
    )
  }
  marks
}

# --- Feature importance ---------------------------------------------------

#' Feature importance plot
#'
#' A ranked point-and-whisker plot of feature importance, with optional
#' whiskers from `ymin`/`ymax` (e.g. permutation-importance standard
#' errors) and a reference line at zero importance. Built on the same
#' point+whisker rendering as [geom_forest()].
#'
#' A discrete axis otherwise sorts alphabetically; pass `y` as a factor
#' already ordered by importance (ascending, since the first level sits
#' at the panel's bottom) to get the classic "most important at the top"
#' layout — the same convention [geom_waterfall()] uses for category
#' order.
#'
#' @param mapping,data Standard layer overrides. Map the importance value
#'   to `x` and the feature name to `y`; map `ymin`/`ymax` for whiskers.
#' @param color Marker and whisker color.
#' @param linewidth Whisker width.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   feature = c("age", "income", "tenure", "region"),
#'   imp = c(0.42, 0.31, 0.18, 0.05),
#'   se = c(0.05, 0.04, 0.03, 0.02)
#' )
#' d$feature <- factor(d$feature, levels = d$feature[order(d$imp)])
#' ggnext(d, aes(imp, feature, ymin = imp - se, ymax = imp + se)) +
#'   geom_importance() +
#'   labs(title = "Permutation importance", x = "Importance", y = NULL)
#' @export
geom_importance <- function(mapping = NULL, data = NULL, color = NULL,
                            linewidth = NULL) {
  layer_new(GeomForest(), StatImportance(), mapping, data,
            list(color = color, linewidth = linewidth, ref = 0))
}

# --- Leverage / Cook's distance -----------------------------------------------

#' Leverage / Cook's distance plot
#'
#' Standardized residual against leverage (hat value), the companion to
#' [geom_residual()] for spotting influential observations. Map `size` to
#' Cook's distance for the classic three-way read (leverage, residual
#' size, influence) in one scatter.
#'
#' @param mapping,data Standard layer overrides. Map leverage to `x` and
#'   the standardized residual to `y`; map `size` to Cook's distance.
#' @param color Point color.
#' @param size Point radius (used when `size` is not mapped).
#' @param alpha Point opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' m <- lm(dist ~ speed, cars)
#' d <- data.frame(
#'   hat = hatvalues(m), rstd = rstandard(m), cooksd = cooks.distance(m)
#' )
#' ggnext(d, aes(hat, rstd, size = cooksd)) +
#'   geom_leverage() +
#'   labs(title = "Leverage vs standardized residual",
#'        x = "Leverage (hat value)", y = "Standardized residual")
#' @export
geom_leverage <- function(mapping = NULL, data = NULL, color = NULL,
                          size = NULL, alpha = NULL) {
  layer_new(GeomLeverage(), StatLeverage(), mapping, data,
            list(color = color, size = size, alpha = alpha))
}

#' GeomLeverage: leverage/residual scatter with a zero-residual line
#' @noRd
GeomLeverage <- new_class("GeomLeverage", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "leverage",
      default_params = list(color = "#4A5568", size = 3, alpha = 0.75)
    ))
  }
)

method(build_marks, GeomLeverage) <- function(geom, scaled) {
  marks <- list()
  if (!is.null(scaled$yzero)) {
    marks <- c(marks, list(mk_line(
      c(0, 1), rep(scaled$yzero[[1]], 2), stroke = "#9A9AA6", width = 1,
      dash = "4,3"
    )))
  }
  c(marks, lapply(seq_along(scaled$x), function(i) {
    mk_circle(scaled$x[[i]], scaled$y[[i]], scaled$size[[i]],
              scaled$color[[i]], alpha = scaled$alpha[[i]])
  }))
}

# --- Residual diagnostics ----------------------------------------------------

#' Residual diagnostic plot
#'
#' Residuals against fitted values with a zero reference line and a loess
#' trend, the first plot to look at when checking a linear model.
#'
#' @param mapping,data Standard layer overrides. Map fitted values to `x`
#'   and residuals to `y`.
#' @param color Point color.
#' @param smooth Overlay a loess trend through the residuals.
#' @param size Point radius.
#' @param alpha Point opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' m <- lm(dist ~ speed, cars)
#' d <- data.frame(fitted = fitted(m), resid = resid(m))
#' ggnext(d, aes(fitted, resid)) + geom_residual()
#' @export
geom_residual <- function(mapping = NULL, data = NULL, color = NULL,
                          smooth = TRUE, size = NULL, alpha = NULL) {
  layer_new(GeomResidual(), StatResidual(smooth = smooth), mapping, data,
            list(color = color, size = size, alpha = alpha, smooth = smooth))
}

#' GeomResidual: residual scatter with a zero line and trend
#' @noRd
GeomResidual <- new_class("GeomResidual", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "residual",
      default_params = list(
        color = "#4A5568", size = 3, alpha = 0.7, smooth = TRUE
      )
    ))
  }
)

method(build_marks, GeomResidual) <- function(geom, scaled) {
  pts <- which(scaled$group != ".trend")
  marks <- lapply(pts, function(i) {
    mk_circle(scaled$x[[i]], scaled$y[[i]], scaled$params$size,
              scaled$color[[i]], alpha = scaled$params$alpha)
  })
  # Zero residual line: y = 0 in data units, carried as `yzero`.
  if (!is.null(scaled$yzero)) {
    marks <- c(marks, list(mk_line(
      c(0, 1), rep(scaled$yzero[[1]], 2), stroke = "#9A9AA6",
      width = 1, dash = "4,3"
    )))
  }
  tr <- which(scaled$group == ".trend")
  if (length(tr) > 0) {
    ord <- tr[order(scaled$x[tr])]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = "#E05A2B", width = 2
    )))
  }
  marks
}

# --- Learning curve ----------------------------------------------------------

#' Learning curve
#'
#' Training and validation score against training-set size or epoch, the
#' standard read on whether a model is data-limited or over-fitting.
#'
#' @param mapping,data Standard layer overrides. Map size/epoch to `x`, the
#'   score to `y`, and the split ("train"/"validation") to `color`.
#' @param band Draw a ribbon between `ymin` and `ymax` when supplied.
#' @param linewidth Line width.
#' @param points Draw markers at each measured point.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   n = rep(c(50, 100, 200, 400), 2),
#'   score = c(.75, .82, .86, .88, .70, .78, .83, .86),
#'   split = rep(c("train", "validation"), each = 4)
#' )
#' ggnext(d, aes(n, score, color = split)) + geom_learning_curve()
#' @export
geom_learning_curve <- function(mapping = NULL, data = NULL, band = TRUE,
                                linewidth = NULL, points = TRUE) {
  layer_new(GeomLearningCurve(), StatIdentity(), mapping, data,
            list(band = band, linewidth = linewidth, points = points))
}

#' GeomLearningCurve: per-split score lines with optional bands
#' @noRd
GeomLearningCurve <- new_class("GeomLearningCurve", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "learning_curve",
      default_params = list(
        color = "#2B6BE0", linewidth = 2, alpha = 1, band = TRUE,
        points = TRUE
      )
    ))
  }
)

method(build_marks, GeomLearningCurve) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    ord <- idx[order(scaled$x[idx])]
    col <- scaled$color[[ord[1]]]
    if (isTRUE(scaled$params$band) && !is.null(scaled$ymin)) {
      marks <- c(marks, list(mk_polygon(
        c(scaled$x[ord], rev(scaled$x[ord])),
        c(scaled$ymax[ord], rev(scaled$ymin[ord])),
        fill = col, alpha = 0.18
      )))
    }
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = col,
      width = scaled$params$linewidth
    )))
    if (isTRUE(scaled$params$points)) {
      for (i in ord) {
        marks <- c(marks, list(mk_circle(scaled$x[[i]], scaled$y[[i]], 3.5, col)))
      }
    }
  }
  marks
}

# --- Silhouette --------------------------------------------------------------

#' Clustering silhouette plot
#'
#' Sorted silhouette widths grouped by cluster, with the mean marked — the
#' standard visual check on how well-separated a clustering is.
#'
#' @param mapping,data Standard layer overrides. Map the silhouette width
#'   to `x` and the cluster to `y` (or `color`).
#' @param alpha Bar opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   cluster = rep(c("1", "2", "3"), each = 20),
#'   width = c(runif(20, .3, .9), runif(20, .1, .7), runif(20, -.1, .6))
#' )
#' ggnext(d, aes(width, cluster, color = cluster)) + geom_silhouette()
#' @export
geom_silhouette <- function(mapping = NULL, data = NULL, alpha = NULL) {
  layer_new(GeomSilhouette(), StatSilhouette(), mapping, data,
            list(alpha = alpha))
}

#' GeomSilhouette: sorted per-observation silhouette bars
#' @noRd
GeomSilhouette <- new_class("GeomSilhouette", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "silhouette",
      default_params = list(alpha = 0.9)
    ))
  }
)

method(build_marks, GeomSilhouette) <- function(geom, scaled) {
  lapply(seq_along(scaled$x), function(i) {
    mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha
    )
  })
}

# --- Dendrogram ----------------------------------------------------------

#' Hierarchical clustering dendrogram
#'
#' Runs [stats::hclust()] (base R, not an added dependency) on the
#' Euclidean distances between rows of `data` and draws the merge tree as
#' the classic bracket shape: two vertical drops joined by one horizontal
#' bar at the merge height. Takes the raw wide data frame directly (like
#' [geom_cor()]), since the layout needs the whole numeric block at once,
#' not row-wise aesthetics.
#'
#' @param data A data frame with one row per observation to cluster.
#' @param vars Character vector of numeric columns to cluster on; default
#'   all numeric columns in `data`.
#' @param method Linkage method, passed to [stats::hclust()] (default
#'   `"complete"`).
#' @param color,linewidth Line appearance.
#' @return A [Layer] to add with `+`.
#' @examples
#' ggnext() +
#'   geom_dendrogram(mtcars[1:10, c("mpg", "hp", "wt", "qsec")]) +
#'   theme_void()
#' @export
geom_dendrogram <- function(data, vars = NULL, method = "complete",
                            color = NULL, linewidth = NULL) {
  if (!is.data.frame(data)) {
    stop("geom_dendrogram() needs a data.frame as `data`.", call. = FALSE)
  }
  nums <- vars %||% names(data)[vapply(data, is.numeric, logical(1))]
  if (length(nums) < 1) {
    stop("geom_dendrogram() needs at least one numeric column.", call. = FALSE)
  }
  if (nrow(data) < 2) {
    stop("geom_dendrogram() needs at least two observations to cluster.", call. = FALSE)
  }
  dm <- stats::dist(data[nums])
  hc <- stats::hclust(dm, method = method)
  n <- length(hc$order)
  leaf_rank <- integer(n)
  leaf_rank[hc$order] <- seq_len(n)
  node_x <- numeric(nrow(hc$merge))
  # hc$merge[k, ]: negative entries are leaf indices, positive entries are
  # earlier merge rows; each merge's own x sits midway between its two
  # children so the tree stays visually balanced.
  child_xy <- function(idx) {
    if (idx < 0) list(x = leaf_rank[-idx], y = 0) else list(x = node_x[idx], y = hc$height[idx])
  }
  x0 <- x1 <- y0 <- y1 <- numeric(0)
  for (k in seq_len(nrow(hc$merge))) {
    a <- child_xy(hc$merge[k, 1])
    b <- child_xy(hc$merge[k, 2])
    h <- hc$height[k]
    node_x[k] <- (a$x + b$x) / 2
    x0 <- c(x0, a$x, b$x, a$x)
    x1 <- c(x1, a$x, b$x, b$x)
    y0 <- c(y0, a$y, b$y, h)
    y1 <- c(y1, h, h, h)
  }
  segs <- data.frame(x0 = x0, x1 = x1, y0 = y0, y1 = y1)
  layer_new(
    GeomDendrogram(), stat_identity(),
    mapping = aes_internal(x = "x0", y = "y0", xend = "x1", yend = "y1"),
    data = segs, inherit = FALSE,
    params = list(color = color, linewidth = linewidth)
  )
}

#' GeomDendrogram: merge-tree brackets from precomputed segments
#' @noRd
GeomDendrogram <- new_class("GeomDendrogram", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "dendrogram",
      default_params = list(color = "#2A3F5F", linewidth = 1.3, alpha = 1),
      required_aes = c("x", "y", "xend", "yend")
    ))
  }
)

method(build_marks, GeomDendrogram) <- function(geom, scaled) {
  p <- scaled$params
  lapply(seq_along(scaled$x), function(i) {
    mk_line(
      c(scaled$x[[i]], scaled$xend[[i]]), c(scaled$y[[i]], scaled$yend[[i]]),
      stroke = scaled$color[[i]], width = p$linewidth, alpha = scaled$alpha[[i]]
    )
  })
}

# --- Embedding ---------------------------------------------------------------

#' Embedding scatter (t-SNE / UMAP / PCA)
#'
#' A two-dimensional embedding scatter with optional cluster hulls, so
#' cluster shape is visible rather than inferred from point color alone.
#'
#' @param mapping,data Standard layer overrides. Map the two embedding
#'   dimensions to `x` and `y`, and the cluster to `color`.
#' @param hull Draw a convex hull around each cluster.
#' @param size Point radius.
#' @param alpha Point opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   d1 = c(rnorm(30), rnorm(30, 4)), d2 = c(rnorm(30), rnorm(30, 3)),
#'   cl = rep(c("a", "b"), each = 30)
#' )
#' ggnext(d, aes(d1, d2, color = cl)) + geom_embedding()
#' @export
geom_embedding <- function(mapping = NULL, data = NULL, hull = TRUE,
                           size = NULL, alpha = NULL) {
  layer_new(GeomEmbedding(), StatIdentity(), mapping, data,
            list(hull = hull, size = size, alpha = alpha))
}

#' GeomEmbedding: cluster scatter with optional convex hulls
#' @noRd
GeomEmbedding <- new_class("GeomEmbedding", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "embedding",
      default_params = list(size = 3, alpha = 0.8, hull = TRUE)
    ))
  }
)

method(build_marks, GeomEmbedding) <- function(geom, scaled) {
  marks <- list()
  if (isTRUE(scaled$params$hull)) {
    for (g in unique(scaled$group)) {
      idx <- which(scaled$group == g)
      if (length(idx) < 3) next
      h <- convex_hull(scaled$x[idx], scaled$y[idx])
      marks <- c(marks, list(mk_polygon(
        h$x, h$y, fill = scaled$color[[idx[1]]], alpha = 0.12
      )))
    }
  }
  c(marks, lapply(seq_along(scaled$x), function(i) {
    mk_circle(scaled$x[[i]], scaled$y[[i]], scaled$params$size,
              scaled$color[[i]], alpha = scaled$params$alpha)
  }))
}

# Convex hull by gift wrapping (Jarvis march): start at the leftmost point
# and repeatedly pick the most counter-clockwise neighbour.
convex_hull <- function(x, y) {
  n <- length(x)
  if (n < 3) {
    return(list(x = x, y = y))
  }
  start <- which.min(x)
  hull <- integer(0)
  current <- start
  repeat {
    hull <- c(hull, current)
    nxt <- if (current == 1) 2 else 1
    for (i in seq_len(n)) {
      if (i == current) next
      # Cross product: positive means i is more counter-clockwise.
      cross <- (x[nxt] - x[current]) * (y[i] - y[current]) -
        (y[nxt] - y[current]) * (x[i] - x[current])
      if (cross < 0) nxt <- i
    }
    current <- nxt
    if (current == start || length(hull) > n) break
  }
  list(x = x[hull], y = y[hull])
}

# --- Decision boundary -------------------------------------------------------

#' Decision-boundary region plot
#'
#' Shades a grid of predictions to show a two-dimensional classifier's
#' decision regions; overlay `geom_point()` for the training data.
#'
#' @param mapping,data Standard layer overrides. Map the grid coordinates to
#'   `x`/`y` and the predicted class to `color`.
#' @param alpha Region opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' g <- expand.grid(x = seq(0, 1, 0.05), y = seq(0, 1, 0.05))
#' g$cls <- ifelse(g$x + g$y > 1, "a", "b")
#' ggnext(g, aes(x, y, color = cls)) + geom_decision_boundary()
#' @export
geom_decision_boundary <- function(mapping = NULL, data = NULL, alpha = NULL) {
  layer_new(GeomDecisionBoundary(), StatDecisionBoundary(), mapping, data,
            list(alpha = alpha))
}

#' GeomDecisionBoundary: shaded prediction grid
#' @noRd
GeomDecisionBoundary <- new_class("GeomDecisionBoundary", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "decision_boundary",
      default_params = list(alpha = 0.35)
    ))
  }
)

method(build_marks, GeomDecisionBoundary) <- function(geom, scaled) {
  lapply(seq_along(scaled$x), function(i) {
    mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha
    )
  })
}

# --- Forecast band -----------------------------------------------------------

#' Time-series forecast with confidence bands
#'
#' Historical actuals as a solid line, the forecast dashed, and one or two
#' nested confidence ribbons.
#'
#' @param mapping,data Standard layer overrides. Map time to `x`, the value
#'   to `y`, the interval to `ymin`/`ymax`, and the
#'   actual-vs-forecast split to `group`.
#' @param color Line color.
#' @param linewidth Line width.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   t = 1:10, v = c(1:6, 7, 8, 9, 10),
#'   lo = c(rep(NA, 6), 6, 6.5, 7, 7.5),
#'   hi = c(rep(NA, 6), 8, 9.5, 11, 12.5),
#'   part = rep(c("actual", "forecast"), c(6, 4))
#' )
#' ggnext(d, aes(t, v, ymin = lo, ymax = hi, group = part)) +
#'   geom_forecast_band()
#' @export
geom_forecast_band <- function(mapping = NULL, data = NULL, color = NULL,
                               linewidth = NULL) {
  layer_new(GeomForecastBand(), StatIdentity(), mapping, data,
            list(color = color, linewidth = linewidth))
}

#' GeomForecastBand: actual line, dashed forecast, confidence ribbon
#' @noRd
GeomForecastBand <- new_class("GeomForecastBand", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "forecast_band",
      default_params = list(color = "#2B6BE0", linewidth = 2, alpha = 1)
    ))
  }
)

method(build_marks, GeomForecastBand) <- function(geom, scaled) {
  marks <- list()
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    ord <- idx[order(scaled$x[idx])]
    col <- scaled$color[[ord[1]]]
    # A ribbon only where the interval is present (forecast rows).
    band <- ord[!is.na(scaled$ymin[ord]) & !is.na(scaled$ymax[ord])]
    if (length(band) > 1) {
      marks <- c(marks, list(mk_polygon(
        c(scaled$x[band], rev(scaled$x[band])),
        c(scaled$ymax[band], rev(scaled$ymin[band])),
        fill = col, alpha = 0.2
      )))
    }
    # Forecast segments are dashed; history is solid.
    dash <- if (length(band) > 1) "6,4" else ""
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = col,
      width = scaled$params$linewidth, dash = dash
    )))
  }
  marks
}
