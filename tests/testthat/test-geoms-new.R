# New geom/stat pairs: KM enhancements, clinical, ML, and EDA additions.
#
# marks_of() / panel_of() come from helper-buffer.R.

mtype <- function(marks) vapply(marks, function(m) m$type, character(1))

# --- Kaplan-Meier: Greenwood confidence band ----------------------------------

test_that("stat_km(conf_int = TRUE) reproduces Greenwood's CI by hand", {
  # Same 4-subject example as the base stat_km() test: events at t=1,2,4,
  # censored at t=3. S(2) = 0.5, Greenwood sum term at t=2 is
  # 1/(4*3) + 1/(3*2) = 0.25, so Var(S(2)) = 0.5^2 * 0.25 = 0.0625.
  values <- list(
    time = c(1, 2, 3, 4), status = c(1, 1, 0, 1), group = rep("all", 4)
  )
  out <- compute_stat(stat_km(conf_int = TRUE), values)
  curve <- which(out$role == "curve")
  at_s_half <- curve[out$x[curve] == 2 & out$y[curve] == 0.5]
  expect_length(at_s_half, 1)
  expect_equal(out$ymin[at_s_half], 0.05784708, tolerance = 1e-6)
  expect_equal(out$ymax[at_s_half], 0.84486125, tolerance = 1e-6)
  # Bounds always bracket the estimate and stay inside [0, 1].
  expect_true(out$ymin[at_s_half] < out$y[at_s_half])
  expect_true(out$y[at_s_half] < out$ymax[at_s_half])
  # Censor rows carry no band.
  expect_true(all(is.na(out$ymin[out$role == "censor"])))
})

test_that("geom_km(conf_int = TRUE) draws a ribbon under the curve", {
  d <- data.frame(t = c(1, 2, 3, 4), ev = c(1, 1, 0, 1))
  m <- marks_of(ggnext(d, aes(time = t, status = ev)) + geom_km(conf_int = TRUE))
  expect_equal(mtype(m)[1], "polygon") # ribbon drawn before the line
  expect_true("line" %in% mtype(m))
  # Without conf_int, no ribbon at all.
  m2 <- marks_of(ggnext(d, aes(time = t, status = ev)) + geom_km())
  expect_false("polygon" %in% mtype(m2))
})

# --- Nelson-Aalen cumulative hazard --------------------------------------------

test_that("stat_nelson_aalen accumulates d_i / n_i additively", {
  values <- list(
    time = c(1, 2, 3, 4), status = c(1, 1, 0, 1), group = rep("all", 4)
  )
  out <- compute_stat(stat_nelson_aalen(), values)
  # New-level points after each event, in x order: H(1)=1/4, H(2)=1/4+1/3,
  # H(4)=that+1/1.
  post <- out$y[c(3, 5, 7)]
  expect_equal(post, c(0.25, 0.25 + 1 / 3, 0.25 + 1 / 3 + 1), tolerance = 1e-9)
})

test_that("geom_nelson_aalen draws one step curve per group", {
  d <- data.frame(
    t = c(1, 2, 3, 4, 2, 5, 6, 3), ev = c(1, 1, 0, 1, 1, 1, 0, 1),
    g = rep(c("a", "b"), each = 4)
  )
  m <- marks_of(ggnext(d, aes(time = t, status = ev, color = g)) + geom_nelson_aalen())
  expect_equal(mtype(m), c("line", "line"))
})

# --- KM number-at-risk table ---------------------------------------------------

test_that("stat_km_risktable counts subjects still at risk at each tick", {
  values <- list(
    time = c(1, 2, 3, 4, 5, 6), status = c(1, 1, 0, 1, 0, 1),
    group = rep("all", 6)
  )
  out <- compute_stat(stat_km_risktable(breaks = c(0, 2, 4)), values)
  expect_equal(out$table$breaks, c(0, 2, 4))
  expect_equal(out$table$counts[[1]], c(6, 5, 3))
})

test_that("geom_km_risktable draws a text row per group plus a header", {
  d <- data.frame(
    t = c(1, 2, 3, 4, 5, 6, 2, 3), ev = c(1, 1, 0, 1, 0, 1, 1, 1),
    g = rep(c("a", "b"), c(6, 2))
  )
  p <- ggnext(d, aes(time = t, status = ev, color = g)) +
    geom_km() + geom_km_risktable(breaks = c(0, 3, 6))
  m <- marks_of(p, layer = 2)
  expect_true(all(mtype(m) == "text"))
  # Header + 2 group rows, each with a label plus 3 tick counts = 1 + 2*4.
  expect_length(m, 9)
})

# --- Cox proportional hazards / geom_hr ----------------------------------------

test_that("stat_coxph matches a hand-checked Newton-Raphson fit", {
  # Two groups, no ties: beta and its SE were cross-checked once against
  # survival::coxph() (not a package dependency, just how the constants
  # below were derived) and are pinned here as the from-scratch estimator's
  # contract.
  set.seed(42)
  time <- c(stats::rexp(50, 0.08), stats::rexp(50, 0.05))
  status <- stats::rbinom(100, 1, 0.85)
  arm <- rep(c("placebo", "treatment"), each = 50)
  out <- compute_stat(
    ggnext:::StatCoxph(ref_level = "placebo"),
    list(time = time, status = status, group = arm)
  )
  expect_equal(out$y, "treatment")
  expect_equal(log(out$x), -0.341172, tolerance = 1e-5)
  expect_equal(out$xref, 1)
  # CI brackets the point estimate.
  expect_true(out$xmin < out$x && out$x < out$xmax)
})

test_that("stat_coxph errors clearly with only one group", {
  values <- list(time = 1:4, status = c(1, 0, 1, 1), group = rep("a", 4))
  expect_error(compute_stat(ggnext:::StatCoxph(), values), "at least two groups")
})

test_that("geom_hr renders a forest-style layer with a HR = 1 reference line", {
  set.seed(1)
  d <- data.frame(
    time = c(rexp(30, 0.08), rexp(30, 0.05)),
    status = rbinom(60, 1, 0.8),
    arm = rep(c("placebo", "treatment"), each = 30)
  )
  m <- marks_of(ggnext(d, aes(time = time, status = status, group = arm)) +
                  geom_hr(ref_level = "placebo"))
  expect_true("rect" %in% mtype(m)) # the point estimate square
  # A dashed no-effect reference line at HR = 1.
  ref <- Filter(function(x) x$type == "line" && x$dash != "", m)
  expect_length(ref, 1)
})

# --- Correlation matrix heatmap -------------------------------------------

test_that("geom_cor computes the exact Pearson correlation per cell", {
  p <- ggnext() + geom_cor(mtcars, vars = c("mpg", "cyl", "wt"))
  m <- marks_of(p)
  expect_equal(sum(mtype(m) == "rect"), 9) # 3x3 grid
  expect_equal(sum(mtype(m) == "text"), 9)
  labels <- vapply(Filter(function(x) x$type == "text", m), function(x) x$text, character(1))
  expect_true(sprintf("%.2f", cor(mtcars$mpg, mtcars$cyl)) %in% labels)
  expect_true("1.00" %in% labels) # the diagonal
})

test_that("geom_cor validates its inputs", {
  expect_error(geom_cor(mtcars, vars = "mpg"), "at least two")
  expect_error(geom_cor(list(a = 1)), "data.frame")
})

# --- Precision-Recall curve -----------------------------------------------

test_that("stat_pr starts at (0, 1) and ends inside the panel", {
  values <- list(truth = c(1, 1, 0, 0), score = c(0.9, 0.8, 0.2, 0.1),
                 group = rep("all", 4))
  out <- compute_stat(stat_pr(), values)
  curve <- which(out$role == "curve")
  expect_equal(out$x[curve][1], 0)
  expect_equal(out$y[curve][1], 1)
  # Perfect ranking: precision stays 1 until recall reaches 1.
  expect_equal(out$y[curve], c(1, 1, 1, 2 / 3, 0.5))
})

test_that("geom_pr draws a prevalence baseline plus one curve per group", {
  m <- marks_of(ggnext(
    data.frame(truth = c(1, 1, 0, 0), score = c(0.9, 0.8, 0.2, 0.1)),
    aes(truth = truth, score = score)
  ) + geom_pr())
  expect_equal(mtype(m), c("line", "line"))
})

# --- Feature importance ---------------------------------------------------

test_that("geom_importance draws a whisker plus point per feature", {
  d <- data.frame(
    feature = c("age", "income", "tenure"), imp = c(0.4, 0.3, 0.1),
    se = c(0.05, 0.04, 0.02)
  )
  d$feature <- factor(d$feature, levels = d$feature[order(d$imp)])
  m <- marks_of(ggnext(d, aes(imp, feature, ymin = imp - se, ymax = imp + se)) +
                  geom_importance())
  expect_equal(sum(mtype(m) == "rect"), 3) # point squares
  # Whisker + 2 caps per row, plus the zero reference line.
  expect_equal(sum(mtype(m) == "line"), 3 * 3 + 1)
})

# --- Leverage / Cook's distance --------------------------------------------

test_that("geom_leverage draws a zero-residual line and a point per row", {
  m_lm <- lm(dist ~ speed, cars)
  d <- data.frame(hat = hatvalues(m_lm), rstd = rstandard(m_lm),
                  cooksd = cooks.distance(m_lm))
  m <- marks_of(ggnext(d, aes(hat, rstd, size = cooksd)) + geom_leverage())
  expect_equal(sum(mtype(m) == "circle"), nrow(cars))
  expect_equal(sum(mtype(m) == "line"), 1)
  # Larger Cook's distance draws a visibly larger point.
  radii <- vapply(Filter(function(x) x$type == "circle", m), function(x) x$r, numeric(1))
  expect_gt(max(radii), min(radii))
})

# --- Concordance (Lin's CCC) ------------------------------------------------

test_that("geom_concordance annotates the exact CCC", {
  set.seed(1)
  a <- rnorm(40, 10, 2)
  d <- data.frame(m1 = a, m2 = a * 0.9 + rnorm(40, 0, 0.4))
  ccc <- with(d, 2 * cov(m1, m2) / (var(m1) + var(m2) + (mean(m1) - mean(m2))^2))
  m <- marks_of(ggnext(d, aes(m1, m2)) + geom_concordance())
  label <- Filter(function(x) x$type == "text", m)[[1]]$text
  expect_equal(label, sprintf("CCC = %.3f", ccc))
  expect_equal(sum(mtype(m) == "circle"), 40)
})

# --- Baseline covariate balance (SMD) --------------------------------------

test_that("geom_smd computes Cohen's d per covariate", {
  set.seed(9)
  d <- data.frame(
    arm = rep(c("t", "c"), each = 30), x1 = c(rnorm(30, 5, 1), rnorm(30, 5.5, 1))
  )
  # geom_smd() takes the two levels in sorted order ("c" before "t").
  x1 <- d$x1[d$arm == "c"]
  x2 <- d$x1[d$arm == "t"]
  expected <- (mean(x1) - mean(x2)) / sqrt((var(x1) + var(x2)) / 2)
  p <- ggnext() + geom_smd(d, group = "arm")
  buffer <- ggnext:::build_geometry(p)
  # The point square's x-center, denormalized back to data units.
  xdom <- buffer$panels[[1]]$x$domain
  rects <- Filter(function(m) m$type == "rect", buffer$panels[[1]]$layers[[1]]$marks)
  cx <- (rects[[1]]$x0 + rects[[1]]$x1) / 2
  smd_est <- xdom[1] + cx * diff(xdom)
  expect_equal(smd_est, expected, tolerance = 1e-6)
})

test_that("geom_smd requires exactly two groups", {
  d <- data.frame(arm = c("a", "b", "c"), x = 1:3)
  expect_error(geom_smd(d, group = "arm"), "exactly two groups")
})

# --- Alluvial diagram -------------------------------------------------------

test_that("geom_alluvial tallies consecutive-stage transitions into ribbons", {
  d <- data.frame(
    subject = rep(1:4, each = 2),
    visit = rep(c("V1", "V2"), 4),
    resp = c("A", "A", "A", "B", "B", "B", "B", "A")
  )
  d$visit <- factor(d$visit, levels = c("V1", "V2"))
  m <- marks_of(ggnext(d, aes(x = visit, y = resp, group = subject)) + geom_alluvial())
  # A Sankey-style layout: node bars (rects) plus flow ribbons (polygons).
  expect_true("rect" %in% mtype(m))
  expect_true("polygon" %in% mtype(m))
})

test_that("geom_alluvial requires a shared subject across at least two stages", {
  values <- list(x = c("V1", "V2"), y = c("A", "B"), group = c("s1", "s2"))
  expect_error(
    compute_stat(ggnext:::StatAlluvial(), values),
    "no subject with data at two consecutive"
  )
})

# --- Dendrogram --------------------------------------------------------------

test_that("geom_dendrogram draws 3 segments per merge (the bracket shape)", {
  d <- mtcars[1:6, c("mpg", "hp", "wt")]
  m <- marks_of(ggnext() + geom_dendrogram(d))
  expect_true(all(mtype(m) == "line"))
  expect_length(m, 3 * (nrow(d) - 1)) # 2 verticals + 1 horizontal per merge
})

# --- Missing-data pattern ----------------------------------------------------

test_that("geom_missing_pattern groups rows into distinct patterns", {
  d <- data.frame(
    a = c(1, NA, 1, NA), b = c(1, 1, NA, NA), c = c(1, 1, 1, 1)
  )
  # 4 distinct 3-variable missingness patterns among 4 rows.
  p <- ggnext() + geom_missing_pattern(d)
  m <- marks_of(p)
  expect_equal(sum(mtype(m) == "rect"), 4 * 3) # 4 patterns x 3 variables
  labels <- vapply(Filter(function(x) x$type == "text", m), function(x) x$text, character(1))
  expect_equal(sum(labels == "NA"), 4) # one NA cell per pattern here
})
