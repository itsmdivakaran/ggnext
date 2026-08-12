# Domain -> range mapping math and tick placement, tested directly against
# hand-computed values: there are no ggplot2 internals to cross-check with.

test_that("normalize() maps a domain linearly onto [0, 1]", {
  expect_equal(
    ggnext:::normalize(c(0, 5, 10), c(0, 10)),
    c(0, 0.5, 1)
  )
  expect_equal(
    ggnext:::normalize(c(-2, 0, 2), c(-2, 2)),
    c(0, 0.5, 1)
  )
  # Values outside the domain map outside [0, 1] (clipping is the
  # renderer's job, not the scale's).
  expect_equal(ggnext:::normalize(15, c(0, 10)), 1.5)
  expect_equal(ggnext:::normalize(-5, c(0, 10)), -0.5)
})

test_that("normalize() refuses a degenerate domain", {
  expect_error(ggnext:::normalize(1, c(3, 3)), "non-degenerate")
})

test_that("expand_domain() pads by 5% of the span on each side", {
  expect_equal(ggnext:::expand_domain(c(0, 10)), c(-0.5, 10.5))
  expect_equal(ggnext:::expand_domain(c(-10, 10)), c(-11, 11))
})

test_that("expand_domain() handles zero-span domains", {
  # A single value expands to +/- 1 (or 5% of magnitude if larger) so no
  # downstream division by zero can occur.
  expect_equal(ggnext:::expand_domain(c(5, 5)), c(4, 6))
  expect_equal(ggnext:::expand_domain(c(0, 0)), c(-1, 1))
  expect_equal(ggnext:::expand_domain(c(1000, 1000)), c(950, 1050))
})

test_that("nice_ticks() produces uniform, step-aligned, nice-valued ticks", {
  ticks <- ggnext:::nice_ticks(4, 25, n = 5)
  expect_equal(ticks, seq(0, 25, by = 5))

  ticks <- ggnext:::nice_ticks(0, 1, n = 5)
  expect_equal(ticks, seq(0, 1, by = 0.2))

  # Steps are always 1, 2, or 5 times a power of ten.
  for (rng in list(c(0.001, 0.017), c(-3, 141), c(2.2, 2.9), c(-1e6, 1e6))) {
    ticks <- ggnext:::nice_ticks(rng[1], rng[2], n = 5)
    steps <- diff(ticks)
    expect_true(all(abs(steps - steps[1]) < 1e-9 * abs(steps[1])))
    mantissa <- steps[1] / 10^floor(log10(steps[1]))
    expect_true(any(abs(mantissa - c(1, 2, 5, 10)) < 1e-9))
    # Ticks cover the requested range.
    expect_lte(ticks[1], rng[1])
    expect_gte(ticks[length(ticks)], rng[2])
  }
})

test_that("nice_ticks() snaps away floating point noise", {
  ticks <- ggnext:::nice_ticks(0, 0.9, n = 5)
  # Without snapping, seq() would produce 0.30000000000000004-style values.
  expect_identical(ticks, c(0, 0.2, 0.4, 0.6, 0.8, 1))
})

test_that("nice_ticks() handles a degenerate range", {
  expect_equal(ggnext:::nice_ticks(7, 7), 7)
})

test_that("ScaleContinuous trains its domain from data", {
  s <- scale_x_continuous()
  s <- scale_train(s, c(3, 9, 4, 7))
  expect_equal(s@domain, c(3, 9))
})

test_that("user limits override the trained domain", {
  s <- scale_x_continuous(limits = c(0, 100))
  s <- scale_train(s, c(3, 9))
  expect_equal(s@domain, c(0, 100))
})

test_that("scale_map() normalizes through the trained domain", {
  s <- scale_train(scale_y_continuous(), c(0, 50))
  expect_equal(scale_map(s, c(0, 25, 50)), c(0, 0.5, 1))
  # An explicit expanded domain overrides the trained one.
  expect_equal(scale_map(s, 0, expand = c(-50, 50)), 0.5)
})

test_that("scale_breaks() returns ticks over the trained domain", {
  s <- scale_train(scale_x_continuous(), c(4, 25))
  expect_equal(scale_breaks(s), seq(0, 25, by = 5))
})

test_that("continuous scales reject non-numeric data and bad limits", {
  expect_error(
    scale_train(scale_x_continuous(), letters),
    "must be numeric"
  )
  expect_error(scale_x_continuous(limits = c(5, 1)), "increasing")
  expect_error(scale_x_continuous(limits = 1:3), "length 2")
  expect_error(scale_map(scale_x_continuous(), 1), "not been trained")
})
