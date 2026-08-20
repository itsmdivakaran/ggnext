# Pipe (`|>`) sugar alongside `+` composition.

test_that("piping a plot into a geom is identical to adding it", {
  plus <- ggnext(cars, aes(speed, dist)) + geom_point(color = "steelblue")
  piped <- ggnext(cars, aes(speed, dist)) |> geom_point(color = "steelblue")
  expect_equal(plus, piped)
})

test_that("a full pipe chain matches the equivalent `+` chain", {
  plus <- ggnext(cars, aes(speed, dist)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name = "Speed") +
    coord_flip() +
    labs(title = "T") +
    theme_minimal()
  # coord_flip()/theme_minimal() take no arguments at all - see the scope
  # note below - so they join the tail of the chain with `+`, same as ever.
  piped <- (ggnext(cars, aes(speed, dist)) |>
    geom_point() |>
    geom_smooth(method = "lm") |>
    scale_x_continuous(name = "Speed") |>
    labs(title = "T")) +
    coord_flip() +
    theme_minimal()
  expect_equal(plus, piped)
})

test_that("called without a plot, a constructor still returns its usual spec object", {
  expect_true(S7::S7_inherits(geom_point(color = "red"), Layer))
  expect_true(S7::S7_inherits(theme_minimal(), Theme))
  expect_true(S7::S7_inherits(coord_flip(), Coord))
})

test_that("a genuinely zero-argument constructor has no slot to pipe into", {
  # theme_minimal(), coord_flip(), and friends take no arguments at all, so
  # there is nowhere for a piped plot to bind; `x |> f()` always desugars to
  # `f(x)`, which a zero-arg `f` rejects regardless of pipe sugar. These stay
  # `+`-only, exactly as before pipe support was added.
  expect_error(ggnext(cars, aes(speed, dist)) |> theme_minimal(), "unused argument")
  expect_error(ggnext(cars, aes(speed, dist)) |> coord_flip(), "unused argument")
})

test_that("facet_wrap()'s non-standard evaluation still works, piped or not", {
  plus <- ggnext(mtcars, aes(mpg, wt)) + geom_point() + facet_wrap(cyl)
  piped <- ggnext(mtcars, aes(mpg, wt)) |> geom_point() |> facet_wrap(cyl)
  expect_equal(plus, piped)
  expect_equal(plus@facet@vars, "cyl")
})

test_that("a required, non-default first argument still binds correctly when piped", {
  plus <- ggnext(cars, aes(speed, dist)) + geom_point() + geom_hline(yintercept = 50)
  piped <- ggnext(cars, aes(speed, dist)) |> geom_point() |> geom_hline(50)
  expect_equal(plus, piped)
})

test_that("piping still errors the same way a `+` chain would", {
  expect_error(
    ggnext(cars, aes(speed, dist)) |> geom_point() |> render(target = "webgl"),
    "should be one of"
  )
})
