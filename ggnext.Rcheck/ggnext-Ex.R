pkgname <- "ggnext"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ggnext-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ggnext')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aes")
### * aes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aes
### Title: Construct aesthetic mappings
### Aliases: aes

### ** Examples

aes(speed, dist)
aes(x = speed, y = dist, color = gear)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("animate")
### * animate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: animate
### Title: Animate a plot over a transition variable
### Aliases: animate

### ** Examples

d <- data.frame(
  x = rep(1:5, 3), y = c(1:5, (1:5)^1.5, (1:5)^2),
  step = rep(c(1, 2, 3), each = 5)
)
p <- ggnext(d, aes(x, y)) + geom_point(size = 5) + animate(step)
html <- render(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("animate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("build_site")
### * build_site

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build_site
### Title: Build the ggnext documentation site
### Aliases: build_site

### ** Examples

# cookbook = FALSE keeps this quick; the cookbook page knits ~100 plots.
out <- file.path(tempdir(), "ggnext-site")
build_site(out, quiet = TRUE, cookbook = FALSE)
list.files(out)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build_site", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coord_flip")
### * coord_flip

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coord_flip
### Title: Flipped Cartesian coordinates
### Aliases: coord_flip

### ** Examples

d <- data.frame(g = c("alpha", "beta", "gamma"), v = c(3, 7, 5))
ggnext(d, aes(g, v)) + geom_col() + coord_flip()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coord_flip", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coord_polar")
### * coord_polar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coord_polar
### Title: Polar coordinates
### Aliases: coord_polar

### ** Examples

d <- data.frame(g = c("A", "B", "C", "D"), v = c(4, 7, 3, 6))
ggnext(d, aes(g, v)) + geom_col() + coord_polar()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coord_polar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("facet_grid")
### * facet_grid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: facet_grid
### Title: Lay panels out in a rows-by-columns grid
### Aliases: facet_grid

### ** Examples

d <- transform(mtcars, cyl = factor(cyl), am = factor(am))
ggnext(d, aes(disp, mpg)) + geom_point() + facet_grid(am, cyl)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("facet_grid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("facet_wrap")
### * facet_wrap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: facet_wrap
### Title: Wrap panels into a grid
### Aliases: facet_wrap

### ** Examples

ggnext(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  facet_wrap(Species)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("facet_wrap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_abline")
### * geom_abline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_abline
### Title: Sloped reference line
### Aliases: geom_abline

### ** Examples

ggnext(cars, aes(speed, dist)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 3, dash = "5,4")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_abline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_ae_heatmap")
### * geom_ae_heatmap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_ae_heatmap
### Title: Adverse-event incidence heatmap
### Aliases: geom_ae_heatmap

### ** Examples

d <- expand.grid(arm = c("Placebo", "Low", "High"),
                 ae = c("Nausea", "Fatigue", "Headache"))
d$pct <- c(5, 12, 22, 8, 15, 26, 3, 6, 11)
ggnext(d, aes(arm, ae, size = pct)) + geom_ae_heatmap()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_ae_heatmap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_bland_altman")
### * geom_bland_altman

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_bland_altman
### Title: Bland-Altman agreement plot
### Aliases: geom_bland_altman

### ** Examples

set.seed(1)
a <- rnorm(60, 100, 12)
d <- data.frame(method_a = a, method_b = a + rnorm(60, 2, 5))
ggnext(d, aes(method_a, method_b)) + geom_bland_altman()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_bland_altman", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_blank")
### * geom_blank

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_blank
### Title: Blank layer
### Aliases: geom_blank

### ** Examples

ggnext(cars, aes(speed, dist)) + geom_blank()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_blank", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_bump")
### * geom_bump

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_bump
### Title: Bump chart (rank over time)
### Aliases: geom_bump

### ** Examples

d <- data.frame(
  year = rep(2021:2023, 3),
  rank = c(1, 2, 3, 2, 1, 1, 3, 3, 2),
  team = rep(c("A", "B", "C"), each = 3)
)
ggnext(d, aes(year, rank, color = team)) +
  geom_bump() + scale_y_reverse()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_bump", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_calibration")
### * geom_calibration

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_calibration
### Title: Calibration curve
### Aliases: geom_calibration

### ** Examples

set.seed(1)
p <- runif(300)
d <- data.frame(pred = p, obs = rbinom(300, 1, p^1.3))
ggnext(d, aes(pred, obs)) + geom_calibration()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_calibration", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_chord")
### * geom_chord

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_chord
### Title: Chord diagram
### Aliases: geom_chord

### ** Examples

d <- data.frame(
  from = c("A", "A", "B", "C"), to = c("B", "C", "C", "A"),
  n = c(5, 3, 7, 2)
)
ggnext(d, aes(x = from, xend = to, y = n)) + geom_chord() + theme_void()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_chord", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_confusion_matrix")
### * geom_confusion_matrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_confusion_matrix
### Title: Confusion matrix
### Aliases: geom_confusion_matrix

### ** Examples

d <- data.frame(
  predicted = c("cat", "cat", "dog", "dog", "dog", "cat"),
  actual    = c("cat", "dog", "dog", "dog", "cat", "cat")
)
ggnext(d, aes(predicted, actual)) + geom_confusion_matrix()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_confusion_matrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_consort")
### * geom_consort

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_consort
### Title: CONSORT participant flow diagram
### Aliases: geom_consort

### ** Examples

d <- data.frame(
  stage = c("Assessed for eligibility", "Randomised",
            "Received allocation", "Analysed"),
  n = c(420, 300, 291, 285)
)
ggnext(d, aes(label = stage, size = n)) + geom_consort()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_consort", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_count")
### * geom_count

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_count
### Title: Counted scatter
### Aliases: geom_count

### ** Examples

d <- data.frame(x = c(1, 1, 1, 2, 2, 3), y = c(1, 1, 2, 2, 2, 3))
ggnext(d, aes(x, y)) + geom_count()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_count", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_crossbar")
### * geom_crossbar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_crossbar
### Title: Crossbar: an interval box with the estimate marked
### Aliases: geom_crossbar

### ** Examples

d <- data.frame(g = c("a", "b"), m = c(2, 3), lo = c(1, 2), hi = c(4, 5))
ggnext(d, aes(g, m, ymin = lo, ymax = hi)) + geom_crossbar()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_crossbar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_cuminc")
### * geom_cuminc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_cuminc
### Title: Cumulative incidence (competing risks)
### Aliases: geom_cuminc

### ** Examples

set.seed(1)
d <- data.frame(
  t = rexp(120, 0.1),
  ev = sample(0:2, 120, replace = TRUE, prob = c(.4, .35, .25))
)
ggnext(d, aes(time = t, status = ev)) + geom_cuminc()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_cuminc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_curve")
### * geom_curve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_curve
### Title: Curved connector
### Aliases: geom_curve

### ** Examples

d <- data.frame(x = 1, y = 1, xe = 3, ye = 3)
ggnext(d, aes(x, y, xend = xe, yend = ye)) + geom_curve()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_curve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_decision_boundary")
### * geom_decision_boundary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_decision_boundary
### Title: Decision-boundary region plot
### Aliases: geom_decision_boundary

### ** Examples

g <- expand.grid(x = seq(0, 1, 0.05), y = seq(0, 1, 0.05))
g$cls <- ifelse(g$x + g$y > 1, "a", "b")
ggnext(g, aes(x, y, color = cls)) + geom_decision_boundary()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_decision_boundary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_dose_response")
### * geom_dose_response

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_dose_response
### Title: Dose-response curve
### Aliases: geom_dose_response

### ** Examples

d <- data.frame(
  dose = rep(c(0.1, 1, 10, 100, 1000), each = 3),
  resp = c(5, 7, 6, 18, 22, 20, 52, 48, 55, 82, 79, 85, 95, 97, 93)
)
ggnext(d, aes(dose, resp)) + geom_dose_response() + scale_x_log10()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_dose_response", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_dotplot")
### * geom_dotplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_dotplot
### Title: Dot plot
### Aliases: geom_dotplot

### ** Examples

ggnext(cars, aes(speed)) + geom_dotplot(binwidth = 2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_dotplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_embedding")
### * geom_embedding

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_embedding
### Title: Embedding scatter (t-SNE / UMAP / PCA)
### Aliases: geom_embedding

### ** Examples

set.seed(1)
d <- data.frame(
  d1 = c(rnorm(30), rnorm(30, 4)), d2 = c(rnorm(30), rnorm(30, 3)),
  cl = rep(c("a", "b"), each = 30)
)
ggnext(d, aes(d1, d2, color = cl)) + geom_embedding()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_embedding", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_errorbarh")
### * geom_errorbarh

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_errorbarh
### Title: Horizontal error bars
### Aliases: geom_errorbarh

### ** Examples

d <- data.frame(g = c("a", "b"), lo = c(1, 2), hi = c(4, 5))
ggnext(d, aes(y = g, xmin = lo, xmax = hi)) + geom_errorbarh()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_errorbarh", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_forecast_band")
### * geom_forecast_band

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_forecast_band
### Title: Time-series forecast with confidence bands
### Aliases: geom_forecast_band

### ** Examples

d <- data.frame(
  t = 1:10, v = c(1:6, 7, 8, 9, 10),
  lo = c(rep(NA, 6), 6, 6.5, 7, 7.5),
  hi = c(rep(NA, 6), 8, 9.5, 11, 12.5),
  part = rep(c("actual", "forecast"), c(6, 4))
)
ggnext(d, aes(t, v, ymin = lo, ymax = hi, group = part)) +
  geom_forecast_band()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_forecast_band", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_forest")
### * geom_forest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_forest
### Title: Forest plot
### Aliases: geom_forest

### ** Examples

d <- data.frame(
  study = c("Trial A", "Trial B", "Trial C", "Pooled"),
  hr = c(0.82, 0.71, 0.95, 0.83),
  lo = c(0.65, 0.52, 0.78, 0.74),
  hi = c(1.03, 0.97, 1.16, 0.93),
  weight = c(30, 22, 28, 100)
)
ggnext(d, aes(hr, study, ymin = lo, ymax = hi, size = weight)) +
  geom_forest() +
  labs(title = "Hazard ratio by trial", x = "Hazard ratio (95% CI)", y = NULL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_forest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_freqpoly")
### * geom_freqpoly

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_freqpoly
### Title: Frequency polygon
### Aliases: geom_freqpoly

### ** Examples

ggnext(cars, aes(speed)) + geom_freqpoly(bins = 8)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_freqpoly", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_function")
### * geom_function

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_function
### Title: Curve of a function
### Aliases: geom_function

### ** Examples

ggnext(data.frame(x = c(-3, 3)), aes(x)) +
  geom_function(dnorm, xlim = c(-3, 3))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_function", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_funnel")
### * geom_funnel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_funnel
### Title: Conversion funnel
### Aliases: geom_funnel

### ** Examples

d <- data.frame(
  stage = factor(c("Visits", "Signups", "Trials", "Paid"),
                 levels = c("Visits", "Signups", "Trials", "Paid")),
  n = c(10000, 3200, 1100, 420)
)
ggnext(d, aes(stage, n, color = stage)) + geom_funnel() + theme_void()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_funnel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_label")
### * geom_label

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_label
### Title: Text on a background plate
### Aliases: geom_label

### ** Examples

d <- data.frame(x = c(1, 2), y = c(2, 1), l = c("alpha", "beta"))
ggnext(d, aes(x, y, label = l)) + geom_label()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_label", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_learning_curve")
### * geom_learning_curve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_learning_curve
### Title: Learning curve
### Aliases: geom_learning_curve

### ** Examples

d <- data.frame(
  n = rep(c(50, 100, 200, 400), 2),
  score = c(.75, .82, .86, .88, .70, .78, .83, .86),
  split = rep(c("train", "validation"), each = 4)
)
ggnext(d, aes(n, score, color = split)) + geom_learning_curve()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_learning_curve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_lift_gain")
### * geom_lift_gain

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_lift_gain
### Title: Lift and cumulative gain curves
### Aliases: geom_lift_gain

### ** Examples

set.seed(1)
s <- runif(200)
d <- data.frame(score = s, y = rbinom(200, 1, s))
ggnext(d, aes(score = score, truth = y)) + geom_lift_gain()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_lift_gain", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_linerange")
### * geom_linerange

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_linerange
### Title: Vertical interval without a marker
### Aliases: geom_linerange

### ** Examples

d <- data.frame(g = c("a", "b"), lo = c(1, 2), hi = c(4, 5))
ggnext(d, aes(g, ymin = lo, ymax = hi)) + geom_linerange()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_linerange", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_network")
### * geom_network

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_network
### Title: Network (node-link) diagram
### Aliases: geom_network

### ** Examples

d <- data.frame(
  from = c("A", "A", "B", "C", "D", "E"),
  to = c("B", "C", "C", "D", "E", "A")
)
ggnext(d, aes(x = from, xend = to)) + geom_network() + theme_void()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_network", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_parallel")
### * geom_parallel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_parallel
### Title: Parallel coordinates plot
### Aliases: geom_parallel

### ** Examples

d <- data.frame(
  id = rep(1:3, each = 3),
  var = rep(c("a", "b", "c"), 3),
  val = c(1, 9, 4, 3, 5, 8, 7, 2, 6)
)
ggnext(d, aes(var, val, group = id, color = factor(id))) + geom_parallel()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_parallel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_partial_dependence")
### * geom_partial_dependence

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_partial_dependence
### Title: Partial dependence and ICE curves
### Aliases: geom_partial_dependence

### ** Examples

d <- data.frame(
  x = rep(1:10, 5), id = rep(1:5, each = 10),
  pred = as.vector(sapply(1:5, function(i) (1:10) * 0.1 * i + rnorm(10, 0, .1)))
)
ggnext(d, aes(x, pred, group = id)) + geom_partial_dependence()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_partial_dependence", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_point")
### * geom_point

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_point
### Title: Scatter-plot layer: one point per observation
### Aliases: geom_point

### ** Examples

ggnext(cars, aes(speed, dist)) + geom_point(color = "steelblue", size = 4)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_point", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_polygon")
### * geom_polygon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_polygon
### Title: Polygons
### Aliases: geom_polygon

### ** Examples

d <- data.frame(x = c(1, 3, 2), y = c(1, 1, 3))
ggnext(d, aes(x, y)) + geom_polygon()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_polygon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_qq")
### * geom_qq

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_qq
### Title: Quantile-quantile plot
### Aliases: geom_qq geom_qq_line

### ** Examples

set.seed(1)
d <- data.frame(v = rnorm(100))
ggnext(d, aes(v)) + geom_qq() + geom_qq_line()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_qq", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_quantile")
### * geom_quantile

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_quantile
### Title: Quantile regression lines
### Aliases: geom_quantile

### ** Examples

ggnext(cars, aes(speed, dist)) +
  geom_point(alpha = 0.5) +
  geom_quantile()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_quantile", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_radar")
### * geom_radar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_radar
### Title: Radar (spider / star) chart
### Aliases: geom_radar

### ** Examples

d <- data.frame(
  axis = rep(c("Speed", "Power", "Range", "Cost", "Safety"), 2),
  value = c(8, 6, 7, 4, 9, 5, 9, 4, 8, 6),
  model = rep(c("A", "B"), each = 5)
)
ggnext(d, aes(axis, value, color = model)) +
  geom_radar() +
  coord_polar() +
  theme_minimal()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_radar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_raster")
### * geom_raster

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_raster
### Title: Raster
### Aliases: geom_raster

### ** Examples

g <- expand.grid(x = 1:20, y = 1:20)
g$z <- as.vector(outer(1:20, 1:20, function(a, b) sin(a / 3) * cos(b / 3)))
ggnext(g, aes(x, y, color = z)) + geom_raster()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_raster", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_rect")
### * geom_rect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_rect
### Title: Rectangles
### Aliases: geom_rect

### ** Examples

d <- data.frame(x1 = c(1, 3), x2 = c(2, 5), y1 = c(1, 2), y2 = c(4, 3))
ggnext(d, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2)) + geom_rect()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_rect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_residual")
### * geom_residual

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_residual
### Title: Residual diagnostic plot
### Aliases: geom_residual

### ** Examples

m <- lm(dist ~ speed, cars)
d <- data.frame(fitted = fitted(m), resid = resid(m))
ggnext(d, aes(fitted, resid)) + geom_residual()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_residual", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_ridgeline")
### * geom_ridgeline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_ridgeline
### Title: Ridgeline plot (joyplot)
### Aliases: geom_ridgeline

### ** Examples

ggnext(iris, aes(Sepal.Length, Species)) + geom_ridgeline()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_ridgeline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_rug")
### * geom_rug

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_rug
### Title: Marginal rug
### Aliases: geom_rug

### ** Examples

ggnext(cars, aes(speed, dist)) + geom_point() + geom_rug()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_rug", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_sankey")
### * geom_sankey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_sankey
### Title: Sankey / alluvial flow diagram
### Aliases: geom_sankey geom_alluvial

### ** Examples

d <- data.frame(
  from = c("Visited", "Visited", "Signed up", "Signed up"),
  to = c("Signed up", "Left", "Purchased", "Churned"),
  n = c(400, 600, 150, 250)
)
ggnext(d, aes(x = from, xend = to, y = n)) +
  geom_sankey() +
  theme_void()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_sankey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_shap")
### * geom_shap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_shap
### Title: SHAP value beeswarm
### Aliases: geom_shap

### ** Examples

set.seed(1)
d <- data.frame(
  feature = rep(c("age", "income", "tenure"), each = 40),
  shap = c(rnorm(40, 0.3, 0.2), rnorm(40, -0.1, 0.3), rnorm(40, 0, 0.15)),
  value = runif(120)
)
ggnext(d, aes(shap, feature, color = value)) +
  geom_shap() +
  geom_vline(0, dash = "3,3") +
  labs(title = "SHAP value by feature", x = "SHAP value", y = NULL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_shap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_shift")
### * geom_shift

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_shift
### Title: Categorical shift plot
### Aliases: geom_shift

### ** Examples

d <- data.frame(
  baseline = c("G0", "G0", "G1", "G1", "G2", "G0"),
  followup = c("G0", "G1", "G1", "G2", "G2", "G0")
)
ggnext(d, aes(baseline, followup)) + geom_shift()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_shift", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_silhouette")
### * geom_silhouette

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_silhouette
### Title: Clustering silhouette plot
### Aliases: geom_silhouette

### ** Examples

set.seed(1)
d <- data.frame(
  cluster = rep(c("1", "2", "3"), each = 20),
  width = c(runif(20, .3, .9), runif(20, .1, .7), runif(20, -.1, .6))
)
ggnext(d, aes(width, cluster, color = cluster)) + geom_silhouette()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_silhouette", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_spaghetti")
### * geom_spaghetti

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_spaghetti
### Title: Individual longitudinal trajectories (spaghetti plot)
### Aliases: geom_spaghetti

### ** Examples

set.seed(1)
d <- data.frame(
  week = rep(0:4, 8), id = rep(1:8, each = 5),
  score = as.vector(sapply(1:8, function(i) 50 + i + (0:4) * 2 + rnorm(5, 0, 3)))
)
ggnext(d, aes(week, score, group = id)) + geom_spaghetti()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_spaghetti", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_spider_response")
### * geom_spider_response

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_spider_response
### Title: Oncology spider plot
### Aliases: geom_spider_response

### ** Examples

d <- data.frame(
  month = rep(c(0, 2, 4, 6), 3),
  pct = c(0, -20, -35, -40, 0, 10, 25, 40, 0, -5, -10, -8),
  subject = rep(c("S1", "S2", "S3"), each = 4)
)
ggnext(d, aes(month, pct, color = subject)) + geom_spider_response()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_spider_response", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_spoke")
### * geom_spoke

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_spoke
### Title: Spokes (vector field)
### Aliases: geom_spoke

### ** Examples

g <- expand.grid(x = 1:5, y = 1:5)
g$angle <- atan2(g$y - 3, g$x - 3)
g$radius <- 0.4
ggnext(g, aes(x, y, xend = angle, yend = radius)) + geom_spoke()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_spoke", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_stream")
### * geom_stream

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_stream
### Title: Streamgraph
### Aliases: geom_stream

### ** Examples

d <- data.frame(
  t = rep(1:6, 3),
  v = c(2, 4, 6, 5, 3, 2, 1, 3, 5, 8, 6, 4, 5, 4, 3, 4, 6, 7),
  grp = rep(c("a", "b", "c"), each = 6)
)
ggnext(d, aes(t, v, color = grp)) + geom_stream() + theme_minimal()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_stream", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_swimmer")
### * geom_swimmer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_swimmer
### Title: Swimmer plot
### Aliases: geom_swimmer

### ** Examples

d <- data.frame(
  subject = paste0("S", 1:6),
  months = c(4, 9, 14, 6, 20, 11),
  response = c("PR", "CR", "CR", "SD", "PR", "SD"),
  ongoing = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
)
ggnext(d, aes(months, subject, color = response, label = ongoing)) +
  geom_swimmer() +
  labs(title = "Time on treatment", x = "Months", y = NULL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_swimmer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_treemap")
### * geom_treemap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_treemap
### Title: Treemap
### Aliases: geom_treemap

### ** Examples

d <- data.frame(
  region = c("North", "South", "East", "West", "Central"),
  revenue = c(52, 38, 27, 19, 11)
)
ggnext(d, aes(size = revenue, label = region, color = region)) +
  geom_treemap() +
  theme_void()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_treemap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_upset")
### * geom_upset

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_upset
### Title: UpSet plot (set intersections)
### Aliases: geom_upset

### ** Examples

d <- data.frame(sets = c("A", "A&B", "B", "A&B&C", "C", "A&B", "A"))
ggnext(d, aes(label = sets)) + geom_upset() + theme_void()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_upset", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_waterfall_response")
### * geom_waterfall_response

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_waterfall_response
### Title: RECIST best-response waterfall
### Aliases: geom_waterfall_response

### ** Examples

set.seed(1)
d <- data.frame(
  subject = paste0("S", 1:20),
  pct = sort(runif(20, -75, 45), decreasing = TRUE)
)
ggnext(d, aes(subject, pct)) +
  geom_waterfall_response() +
  labs(title = "Best response", y = "% change from baseline", x = NULL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_waterfall_response", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ggnext")
### * ggnext

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ggnext
### Title: Create a new ggnext plot
### Aliases: ggnext

### ** Examples

p <- ggnext(cars, aes(speed, dist)) + geom_point()
svg <- render(p)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ggnext", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ggnext_logo")
### * ggnext_logo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ggnext_logo
### Title: Draw the ggnext hex sticker
### Aliases: ggnext_logo

### ** Examples

svg <- ggnext_logo()
substr(svg, 1, 30)

# App-icon variant.
icon <- ggnext_logo(style = "monogram", width = 256)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ggnext_logo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("interact")
### * interact

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Interact
### Title: Add interactivity to a plot
### Aliases: Interact interact

### ** Examples

p <- ggnext(cars, aes(speed, dist)) + geom_point()
render(p)                 # static SVG
pi <- p + interact()      # same plot, interactive by default
html <- render(pi)        # HTML/canvas with tooltip + zoom + brush



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("interact", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("labs")
### * labs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: labs
### Title: Set plot and axis labels
### Aliases: labs

### ** Examples

ggnext(cars, aes(speed, dist)) +
  geom_point() +
  labs(
    title = "Stopping distance rises with speed",
    subtitle = "1920s road tests, 50 observations",
    x = "Speed (mph)",
    y = "Stopping distance (ft)",
    caption = "Source: datasets::cars"
  )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("labs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_data")
### * plot_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_data
### Title: Extract the exact data a plot draws
### Aliases: plot_data

### ** Examples

p <- ggnext(cars, aes(speed, dist)) + geom_point()
head(plot_data(p))

# Stats: what geom_histogram() actually drew.
h <- ggnext(cars, aes(speed)) + geom_histogram(bins = 5)
plot_data(h)

# Facets keep a panel column.
f <- ggnext(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() + facet_wrap(Species)
head(plot_data(f))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_size")
### * plot_size

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_size
### Title: Set the output size
### Aliases: plot_size

### ** Examples

ggnext(cars, aes(speed, dist)) + geom_point() + plot_size(900, 600)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_size", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("render")
### * render

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: render
### Title: Render a ggnext plot
### Aliases: render

### ** Examples

p <- ggnext(cars, aes(speed, dist)) + geom_point()
svg <- render(p)                          # static SVG (the default)
html <- render(p + interact())            # interactive HTML/canvas



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("render", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scale_color_gradient")
### * scale_color_gradient

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scale_color_gradient
### Title: Set the continuous color gradient
### Aliases: scale_color_gradient

### ** Examples

ggnext(iris, aes(Sepal.Length, Sepal.Width, color = Petal.Length)) +
  geom_point() +
  scale_color_gradient(low = "#FFF3B0", high = "#9E2A2B")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scale_color_gradient", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scale_color_manual")
### * scale_color_manual

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scale_color_manual
### Title: Set the discrete color palette
### Aliases: scale_color_manual

### ** Examples

ggnext(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(c("#D55E00", "#0072B2", "#009E73"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scale_color_manual", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scale_x_continuous")
### * scale_x_continuous

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scale_x_continuous
### Title: Continuous scale for the x axis
### Aliases: scale_x_continuous scale_y_continuous

### ** Examples

ggnext(cars, aes(speed, dist)) +
  geom_point() +
  scale_x_continuous(
    name = "Speed (mph)",
    breaks = c(5, 10, 15, 20, 25),
    expand = 0
  ) +
  scale_y_continuous(labels = function(v) paste0(v, " ft"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scale_x_continuous", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scale_x_log10")
### * scale_x_log10

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scale_x_log10
### Title: Log10 and square-root scales
### Aliases: scale_x_log10 scale_y_log10 scale_x_sqrt scale_y_sqrt
###   scale_x_reverse scale_y_reverse

### ** Examples

d <- data.frame(x = 10^(1:5), y = 1:5)
ggnext(d, aes(x, y)) + geom_point() + scale_x_log10()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scale_x_log10", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("theme")
### * theme

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Theme
### Title: Customize theme settings
### Aliases: Theme theme

### ** Examples

ggnext(cars, aes(speed, dist)) + geom_point() +
  theme(panel_fill = "#FFF8F0", grid_major_x = FALSE)

# Restyle a preset rather than the default.
ggnext(cars, aes(speed, dist)) + geom_point() +
  theme(base = theme_dark(), plot_title_size = 22)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("theme", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("theme_ggnext")
### * theme_ggnext

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: theme_ggnext
### Title: The default ggnext theme
### Aliases: theme_ggnext

### ** Examples

p <- ggnext(cars, aes(speed, dist)) + geom_point() + theme_dark()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("theme_ggnext", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_plot_data")
### * write_plot_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_plot_data
### Title: Write the exact plot data to CSV
### Aliases: write_plot_data

### ** Examples

p <- ggnext(cars, aes(speed, dist)) + geom_point()
out <- file.path(tempdir(), "cars.csv")
write_plot_data(p, out)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_plot_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xlim")
### * xlim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xlim
### Title: Set continuous axis limits
### Aliases: xlim ylim

### ** Examples

ggnext(cars, aes(speed, dist)) + geom_point() + xlim(0, 30) + ylim(0, 150)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xlim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
