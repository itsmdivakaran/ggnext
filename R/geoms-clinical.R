# Biostatistics and clinical-trial geoms ---------------------------------------
#
# Native geoms for the figures that clinical reporting needs constantly and
# that otherwise take dozens of lines of hand-built layering: forest plots,
# swimmer plots, spider (response-over-time) plots, RECIST waterfalls,
# Bland-Altman agreement, cumulative incidence, spaghetti trajectories,
# adverse-event heatmaps, dose-response curves, shift tables, and CONSORT
# flow diagrams.

#' @include classes.R marks.R geoms.R
NULL

# --- Forest plot -------------------------------------------------------------

#' GeomForest: point estimates with confidence intervals down a category axis
#'
#' @noRd
GeomForest <- new_class("GeomForest", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "forest",
      default_params = list(
        color = "#2A3F5F", size = 4, alpha = 1, linewidth = 1.5,
        ref = 1, box_by_weight = TRUE
      ),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomForest) <- function(geom, scaled) {
  marks <- list()
  # Reference line (no effect): 1 for ratios, 0 for differences.
  if (!is.null(scaled$xref)) {
    marks <- c(marks, list(mk_line(
      rep(scaled$xref[[1]], 2), c(0, 1),
      stroke = "#9A9AA6", width = 1, dash = "4,3"
    )))
  }
  for (i in seq_along(scaled$x)) {
    col <- scaled$color[[i]]
    # Whisker with end caps.
    marks <- c(marks, list(mk_line(
      c(scaled$xmin[[i]], scaled$xmax[[i]]), rep(scaled$y[[i]], 2),
      stroke = col, width = scaled$params$linewidth
    )))
    cap <- 0.012
    for (xx in c(scaled$xmin[[i]], scaled$xmax[[i]])) {
      marks <- c(marks, list(mk_line(
        rep(xx, 2), c(scaled$y[[i]] - cap, scaled$y[[i]] + cap),
        stroke = col, width = scaled$params$linewidth
      )))
    }
    # Point estimate: a square whose area encodes study weight, the
    # convention that keeps meta-analysis forests readable.
    half <- scaled$size[[i]] / 500
    marks <- c(marks, list(mk_rect(
      scaled$x[[i]] - half, scaled$x[[i]] + half,
      scaled$y[[i]] - half * 2, scaled$y[[i]] + half * 2,
      fill = col, alpha = scaled$params$alpha
    )))
  }
  marks
}

#' Forest plot
#'
#' Point estimates with confidence intervals down a category axis, with a
#' dashed no-effect reference line — the standard display for hazard/odds
#' ratios, subgroup analyses, and meta-analyses. Marker area encodes study
#' weight when `size` is mapped.
#'
#' @param mapping,data Standard layer overrides. Map the estimate to `x`,
#'   the study/subgroup to `y`, and the interval to `xmin`/`xmax`
#'   (via `ymin`/`ymax`, which are swapped for you). Map `size` to weight.
#' @param ref Reference line position: `1` for ratios (default), `0` for
#'   mean differences, `NA` to omit.
#' @param color Marker and whisker color.
#' @param linewidth Whisker width.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   study = c("Trial A", "Trial B", "Trial C", "Pooled"),
#'   hr = c(0.82, 0.71, 0.95, 0.83),
#'   lo = c(0.65, 0.52, 0.78, 0.74),
#'   hi = c(1.03, 0.97, 1.16, 0.93),
#'   weight = c(30, 22, 28, 100)
#' )
#' ggplot3(d, aes(hr, study, ymin = lo, ymax = hi, size = weight)) +
#'   geom_forest() +
#'   labs(title = "Hazard ratio by trial", x = "Hazard ratio (95% CI)", y = NULL)
#' @export
geom_forest <- function(mapping = NULL, data = NULL, ref = 1, color = NULL,
                        linewidth = NULL) {
  layer_new(GeomForest(), StatForest(ref = ref), mapping, data,
            list(color = color, linewidth = linewidth, ref = ref))
}

# --- Swimmer plot ------------------------------------------------------------

#' GeomSwimmer: per-subject event timeline bars
#'
#' @noRd
GeomSwimmer <- new_class("GeomSwimmer", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "swimmer",
      default_params = list(alpha = 1, bar_height = 0.6, arrow = TRUE),
      required_aes = c("x", "y")
    ))
  }
)

method(build_marks, GeomSwimmer) <- function(geom, scaled) {
  marks <- list()
  h <- scaled$params$bar_height / 2 / max(1, scaled$yspan)
  for (i in seq_along(scaled$x)) {
    marks <- c(marks, list(mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]],
      scaled$y[[i]] - h, scaled$y[[i]] + h,
      fill = scaled$color[[i]], alpha = scaled$params$alpha
    )))
    # An arrowhead marks subjects still on treatment at data cutoff.
    if (isTRUE(scaled$params$arrow) && !is.null(scaled$ongoing) &&
        isTRUE(as.logical(scaled$ongoing[[i]]))) {
      tip <- scaled$xmax[[i]] + 0.02
      marks <- c(marks, list(mk_polygon(
        c(scaled$xmax[[i]], tip, scaled$xmax[[i]]),
        c(scaled$y[[i]] - h, scaled$y[[i]], scaled$y[[i]] + h),
        fill = scaled$color[[i]], alpha = scaled$params$alpha
      )))
    }
  }
  marks
}

#' Swimmer plot
#'
#' One horizontal bar per subject showing time on treatment, with optional
#' arrowheads for subjects still ongoing at the data cutoff — the standard
#' patient-level timeline in oncology reporting.
#'
#' @param mapping,data Standard layer overrides. Map duration to `x`, the
#'   subject to `y`, and (optionally) response category to `color`. Map
#'   `label` to a logical "still ongoing" flag to draw arrowheads.
#' @param bar_height Bar thickness in category-slot units.
#' @param arrow Draw arrowheads for ongoing subjects.
#' @param alpha Bar opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   subject = paste0("S", 1:6),
#'   months = c(4, 9, 14, 6, 20, 11),
#'   response = c("PR", "CR", "CR", "SD", "PR", "SD"),
#'   ongoing = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' )
#' ggplot3(d, aes(months, subject, color = response, label = ongoing)) +
#'   geom_swimmer() +
#'   labs(title = "Time on treatment", x = "Months", y = NULL)
#' @export
geom_swimmer <- function(mapping = NULL, data = NULL, bar_height = 0.6,
                         arrow = TRUE, alpha = NULL) {
  layer_new(GeomSwimmer(), StatSwimmer(), mapping, data,
            list(bar_height = bar_height, arrow = arrow, alpha = alpha))
}

# --- Spider (response over time) ---------------------------------------------

#' Oncology spider plot
#'
#' Percent change from baseline over time, one trajectory per subject, with
#' the +20% (progression) and -30% (response) RECIST thresholds marked.
#' Distinct from [geom_radar()], which is a multivariate star chart.
#'
#' @param mapping,data Standard layer overrides. Map time to `x`, percent
#'   change to `y`, and the subject to `group` (or `color`).
#' @param thresholds Draw the RECIST +20% / -30% reference lines.
#' @param linewidth Line width.
#' @param points Draw a marker at each visit.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   month = rep(c(0, 2, 4, 6), 3),
#'   pct = c(0, -20, -35, -40, 0, 10, 25, 40, 0, -5, -10, -8),
#'   subject = rep(c("S1", "S2", "S3"), each = 4)
#' )
#' ggplot3(d, aes(month, pct, color = subject)) + geom_spider_response()
#' @export
geom_spider_response <- function(mapping = NULL, data = NULL,
                                 thresholds = TRUE, linewidth = NULL,
                                 points = TRUE) {
  # StatIdentity never produced `ythresh`, so the documented threshold
  # lines could never draw; StatSpiderResponse supplies them.
  layer_new(GeomSpiderResponse(), StatSpiderResponse(), mapping, data,
            list(thresholds = thresholds, linewidth = linewidth,
                 points = points))
}

#' GeomSpiderResponse: per-subject percent-change trajectories
#' @noRd
GeomSpiderResponse <- new_class("GeomSpiderResponse", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "spider_response",
      default_params = list(
        linewidth = 1.8, alpha = 1, thresholds = TRUE, points = TRUE,
        color = "#2B6BE0"
      )
    ))
  }
)

method(build_marks, GeomSpiderResponse) <- function(geom, scaled) {
  marks <- list()
  if (isTRUE(scaled$params$thresholds) && !is.null(scaled$ythresh)) {
    for (t in scaled$ythresh) {
      marks <- c(marks, list(mk_line(
        c(0, 1), rep(t, 2), stroke = "#9A9AA6", width = 1, dash = "4,3"
      )))
    }
  }
  for (g in unique(scaled$group)) {
    idx <- which(scaled$group == g)
    ord <- idx[order(scaled$x[idx])]
    col <- scaled$color[[ord[1]]]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = col,
      width = scaled$params$linewidth
    )))
    if (isTRUE(scaled$params$points)) {
      for (i in ord) {
        marks <- c(marks, list(mk_circle(scaled$x[[i]], scaled$y[[i]], 3, col)))
      }
    }
  }
  marks
}

# --- RECIST waterfall --------------------------------------------------------

#' RECIST best-response waterfall
#'
#' Subjects ordered by best percent change from baseline, one bar each,
#' with the +20% / -30% RECIST thresholds marked. Distinct from
#' [geom_waterfall()], which shows a running total.
#'
#' @param mapping,data Standard layer overrides. Map the subject to `x`
#'   (or omit and let ordering supply it) and percent change to `y`; map
#'   `color` to response category.
#' @param thresholds Draw the RECIST reference lines.
#' @param alpha Bar opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   subject = paste0("S", 1:20),
#'   pct = sort(runif(20, -75, 45), decreasing = TRUE)
#' )
#' ggplot3(d, aes(subject, pct)) +
#'   geom_waterfall_response() +
#'   labs(title = "Best response", y = "% change from baseline", x = NULL)
#' @export
geom_waterfall_response <- function(mapping = NULL, data = NULL,
                                    thresholds = TRUE, alpha = NULL) {
  layer_new(GeomWaterfallResponse(), StatWaterfallResponse(), mapping, data,
            list(thresholds = thresholds, alpha = alpha))
}

#' GeomWaterfallResponse: ordered percent-change bars
#' @noRd
GeomWaterfallResponse <- new_class("GeomWaterfallResponse", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "waterfall_response",
      default_params = list(alpha = 1, thresholds = TRUE)
    ))
  }
)

method(build_marks, GeomWaterfallResponse) <- function(geom, scaled) {
  marks <- list()
  if (isTRUE(scaled$params$thresholds) && !is.null(scaled$ythresh)) {
    for (t in scaled$ythresh) {
      marks <- c(marks, list(mk_line(
        c(0, 1), rep(t, 2), stroke = "#9A9AA6", width = 1, dash = "4,3"
      )))
    }
  }
  for (i in seq_along(scaled$x)) {
    marks <- c(marks, list(mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha
    )))
  }
  marks
}

# --- Bland-Altman ------------------------------------------------------------

#' Bland-Altman agreement plot
#'
#' Difference against mean for two measurement methods, with the mean bias
#' and 95% limits of agreement drawn as reference lines — the standard
#' method-comparison plot.
#'
#' @param mapping,data Standard layer overrides. Map the two methods'
#'   measurements to `x` and `y`; the stat computes mean and difference.
#' @param color Point color.
#' @param size Point radius.
#' @param alpha Point opacity.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' a <- rnorm(60, 100, 12)
#' d <- data.frame(method_a = a, method_b = a + rnorm(60, 2, 5))
#' ggplot3(d, aes(method_a, method_b)) + geom_bland_altman()
#' @export
geom_bland_altman <- function(mapping = NULL, data = NULL, color = NULL,
                              size = NULL, alpha = NULL) {
  layer_new(GeomBlandAltman(), StatBlandAltman(), mapping, data,
            list(color = color, size = size, alpha = alpha))
}

#' GeomBlandAltman: difference-vs-mean scatter with bias and LoA lines
#' @noRd
GeomBlandAltman <- new_class("GeomBlandAltman", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "bland_altman",
      default_params = list(color = "#4A5568", size = 3, alpha = 0.75)
    ))
  }
)

method(build_marks, GeomBlandAltman) <- function(geom, scaled) {
  marks <- list()
  if (!is.null(scaled$yref)) {
    styles <- list(
      list(w = 2, d = ""),      # mean bias
      list(w = 1, d = "5,4"),   # upper limit of agreement
      list(w = 1, d = "5,4")    # lower limit of agreement
    )
    for (k in seq_along(scaled$yref)) {
      marks <- c(marks, list(mk_line(
        c(0, 1), rep(scaled$yref[[k]], 2), stroke = "#C1462F",
        width = styles[[k]]$w, dash = styles[[k]]$d
      )))
    }
  }
  c(marks, lapply(seq_along(scaled$x), function(i) {
    mk_circle(scaled$x[[i]], scaled$y[[i]], scaled$params$size,
              scaled$color[[i]], alpha = scaled$params$alpha)
  }))
}

# --- Cumulative incidence ----------------------------------------------------

#' Cumulative incidence (competing risks)
#'
#' Aalen-Johansen cumulative incidence curves by event type, the correct
#' estimator when competing events make 1 - Kaplan-Meier biased upward.
#'
#' @param mapping,data Standard layer overrides. Map follow-up time to
#'   `time`, the event code to `status` (0 = censored, 1, 2, ... = event
#'   types), and optionally a treatment arm to `color`.
#' @param linewidth Line width.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   t = rexp(120, 0.1),
#'   ev = sample(0:2, 120, replace = TRUE, prob = c(.4, .35, .25))
#' )
#' ggplot3(d, aes(time = t, status = ev)) + geom_cuminc()
#' @export
geom_cuminc <- function(mapping = NULL, data = NULL, linewidth = NULL) {
  layer_new(GeomCuminc(), StatCuminc(), mapping, data,
            list(linewidth = linewidth))
}

#' GeomCuminc: step curves of cumulative incidence per event type
#' @noRd
GeomCuminc <- new_class("GeomCuminc", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "cuminc",
      default_params = list(linewidth = 2, alpha = 1),
      required_aes = c("time", "status")
    ))
  }
)

method(build_marks, GeomCuminc) <- function(geom, scaled) {
  unname(lapply(group_rows(scaled), function(idx) {
    ord <- idx[order(scaled$x[idx])]
    st <- step_path(scaled$x[ord], scaled$y[ord])
    mk_line(st$x, st$y, stroke = scaled$color[[ord[1]]],
            width = scaled$params$linewidth)
  }))
}

# --- Spaghetti ---------------------------------------------------------------

#' Individual longitudinal trajectories (spaghetti plot)
#'
#' One thin line per subject with an optional bold group mean overlaid —
#' the standard way to show within-subject change without hiding spread.
#'
#' @param mapping,data Standard layer overrides. Map time to `x`, the
#'   measurement to `y`, and the subject to `group`.
#' @param mean_line Overlay the group mean trajectory.
#' @param alpha Opacity of the individual lines.
#' @param linewidth Width of the individual lines.
#' @return A [Layer] to add with `+`.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   week = rep(0:4, 8), id = rep(1:8, each = 5),
#'   score = as.vector(sapply(1:8, function(i) 50 + i + (0:4) * 2 + rnorm(5, 0, 3)))
#' )
#' ggplot3(d, aes(week, score, group = id)) + geom_spaghetti()
#' @export
geom_spaghetti <- function(mapping = NULL, data = NULL, mean_line = TRUE,
                           alpha = NULL, linewidth = NULL) {
  layer_new(GeomSpaghetti(), StatSpaghetti(mean_line = mean_line),
            mapping, data,
            list(alpha = alpha, linewidth = linewidth, mean_line = mean_line))
}

#' GeomSpaghetti: per-subject trajectories plus a mean line
#' @noRd
GeomSpaghetti <- new_class("GeomSpaghetti", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "spaghetti",
      default_params = list(
        color = "#7A8598", linewidth = 1, alpha = 0.5, mean_line = TRUE
      )
    ))
  }
)

method(build_marks, GeomSpaghetti) <- function(geom, scaled) {
  marks <- list()
  for (g in setdiff(unique(scaled$group), ".mean")) {
    idx <- which(scaled$group == g)
    ord <- idx[order(scaled$x[idx])]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = scaled$color[[ord[1]]],
      width = scaled$params$linewidth, alpha = scaled$params$alpha
    )))
  }
  mn <- which(scaled$group == ".mean")
  if (length(mn) > 0) {
    ord <- mn[order(scaled$x[mn])]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = "#16324F", width = 3
    )))
  }
  marks
}

# --- Adverse-event heatmap ---------------------------------------------------

#' Adverse-event incidence heatmap
#'
#' Incidence by preferred term and treatment arm (or severity grade),
#' shaded by rate and annotated with counts.
#'
#' @param mapping,data Standard layer overrides. Map the arm/grade to `x`,
#'   the AE term to `y`, and the incidence to `size`.
#' @param label Annotate each cell with its value.
#' @param label_size Annotation size in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- expand.grid(arm = c("Placebo", "Low", "High"),
#'                  ae = c("Nausea", "Fatigue", "Headache"))
#' d$pct <- c(5, 12, 22, 8, 15, 26, 3, 6, 11)
#' ggplot3(d, aes(arm, ae, size = pct)) + geom_ae_heatmap()
#' @export
geom_ae_heatmap <- function(mapping = NULL, data = NULL, label = TRUE,
                            label_size = NULL) {
  layer_new(GeomAEHeatmap(), StatAEHeatmap(), mapping, data,
            list(label = label, label_size = label_size))
}

#' GeomAEHeatmap: shaded incidence tiles with counts
#' @noRd
GeomAEHeatmap <- new_class("GeomAEHeatmap", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "ae_heatmap",
      default_params = list(alpha = 1, label = TRUE, label_size = 12)
    ))
  }
)

method(build_marks, GeomAEHeatmap) <- function(geom, scaled) {
  marks <- lapply(seq_along(scaled$x), function(i) {
    mk_rect(
      scaled$xmin[[i]], scaled$xmax[[i]], scaled$ymin[[i]], scaled$ymax[[i]],
      fill = scaled$color[[i]], alpha = scaled$params$alpha,
      stroke = "#FFFFFF", stroke_width = 2
    )
  })
  if (isTRUE(scaled$params$label) && !is.null(scaled$label)) {
    marks <- c(marks, lapply(seq_along(scaled$x), function(i) {
      mk_text(scaled$x[[i]], scaled$y[[i]], scaled$label[[i]],
              size = scaled$params$label_size,
              color = contrast_text(scaled$color[[i]]))
    }))
  }
  marks
}

# --- Dose-response -----------------------------------------------------------

#' Dose-response curve
#'
#' A four-parameter log-logistic curve fitted to dose-response data, with a
#' confidence band and the fitted EC50 marked.
#'
#' @param mapping,data Standard layer overrides. Map dose to `x` and
#'   response to `y`.
#' @param color Curve color.
#' @param points Draw the observed points.
#' @param ec50 Mark the fitted EC50 with a vertical line.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   dose = rep(c(0.1, 1, 10, 100, 1000), each = 3),
#'   resp = c(5, 7, 6, 18, 22, 20, 52, 48, 55, 82, 79, 85, 95, 97, 93)
#' )
#' ggplot3(d, aes(dose, resp)) + geom_dose_response() + scale_x_log10()
#' @export
geom_dose_response <- function(mapping = NULL, data = NULL, color = NULL,
                               points = TRUE, ec50 = TRUE) {
  layer_new(GeomDoseResponse(), StatDoseResponse(), mapping, data,
            list(color = color, points = points, ec50 = ec50))
}

#' GeomDoseResponse: fitted sigmoid with observed points
#' @noRd
GeomDoseResponse <- new_class("GeomDoseResponse", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "dose_response",
      default_params = list(
        color = "#12A594", linewidth = 2, alpha = 1, points = TRUE,
        ec50 = TRUE
      )
    ))
  }
)

method(build_marks, GeomDoseResponse) <- function(geom, scaled) {
  marks <- list()
  fit <- which(scaled$group == ".fit")
  obs <- which(scaled$group == ".obs")
  if (length(fit) > 0) {
    ord <- fit[order(scaled$x[fit])]
    marks <- c(marks, list(mk_line(
      scaled$x[ord], scaled$y[ord], stroke = scaled$params$color,
      width = scaled$params$linewidth
    )))
  }
  if (isTRUE(scaled$params$ec50) && !is.null(scaled$xec50)) {
    marks <- c(marks, list(mk_line(
      rep(scaled$xec50[[1]], 2), c(0, 1), stroke = "#9A9AA6",
      width = 1, dash = "4,3"
    )))
  }
  if (isTRUE(scaled$params$points)) {
    marks <- c(marks, lapply(obs, function(i) {
      mk_circle(scaled$x[[i]], scaled$y[[i]], 3.5, scaled$params$color,
                alpha = 0.8)
    }))
  }
  marks
}

# --- Shift plot --------------------------------------------------------------

#' Categorical shift plot
#'
#' How subjects move between categories from baseline to follow-up (lab
#' grade shifts, response categories), drawn as a counted tile grid.
#'
#' @param mapping,data Standard layer overrides. Map the baseline category
#'   to `x` and the follow-up category to `y`.
#' @param label Annotate cells with counts.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   baseline = c("G0", "G0", "G1", "G1", "G2", "G0"),
#'   followup = c("G0", "G1", "G1", "G2", "G2", "G0")
#' )
#' ggplot3(d, aes(baseline, followup)) + geom_shift()
#' @export
geom_shift <- function(mapping = NULL, data = NULL, label = TRUE) {
  layer_new(GeomShift(), StatConfusion(normalize = "all"), mapping, data,
            list(label = label))
}

#' GeomShift: baseline-to-followup category grid
#' @noRd
GeomShift <- new_class("GeomShift", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "shift",
      default_params = list(alpha = 1, label = TRUE, label_size = 12)
    ))
  }
)

method(build_marks, GeomShift) <- function(geom, scaled) {
  build_marks(GeomAEHeatmap(), scaled)
}

# --- CONSORT flow diagram ----------------------------------------------------

#' CONSORT participant flow diagram
#'
#' Boxes and arrows tracing participants from screening through analysis,
#' laid out automatically from a stage/count table.
#'
#' @param mapping,data Standard layer overrides. Map the stage label to
#'   `label` and the participant count to `size`; map `x` to a stage index
#'   to control ordering.
#' @param box_fill Box fill color.
#' @param label_size Text size in px.
#' @return A [Layer] to add with `+`.
#' @examples
#' d <- data.frame(
#'   stage = c("Assessed for eligibility", "Randomised",
#'             "Received allocation", "Analysed"),
#'   n = c(420, 300, 291, 285)
#' )
#' ggplot3(d, aes(label = stage, size = n)) + geom_consort()
#' @export
geom_consort <- function(mapping = NULL, data = NULL, box_fill = NULL,
                         label_size = NULL) {
  layer_new(GeomConsort(), StatConsort(), mapping, data,
            list(box_fill = box_fill, label_size = label_size))
}

#' GeomConsort: stacked flow boxes joined by arrows
#' @noRd
GeomConsort <- new_class("GeomConsort", parent = Geom,
  constructor = function() {
    new_object(Geom(
      name = "consort",
      default_params = list(
        alpha = 1, box_fill = "#EDF1F7", label_size = 12
      ),
      required_aes = "label"
    ))
  }
)

method(build_marks, GeomConsort) <- function(geom, scaled) {
  scaled$marks_precomputed %||% list()
}
