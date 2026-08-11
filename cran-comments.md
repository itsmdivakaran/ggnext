# cran-comments

## Submission

This is a new submission: ggplot3 0.1.0.

## Test environments

* local macOS (darwin), R 4.5.2 — `R CMD check --as-cran`
* (add win-builder devel/release and R-hub results before submitting)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

**On the package name.** `ggplot3` is not affiliated with, endorsed by, or
derived from `ggplot2`; it is an independent implementation of the Grammar
of Graphics built on S7. The name signals the shared grammar lineage, and
the DESCRIPTION, README and package documentation all state the
relationship explicitly and credit `ggplot2` and the wider extension
ecosystem as design references. If the CRAN team considers the name
misleading, I am happy to rename the package before publication.

**Dependencies.** The only hard dependency is `S7`. `grDevices` and `utils`
are base packages. `testthat` and `xml2` are used only in tests.

**Writing to the file system.** No function writes anywhere by default.
`render()`, `write_plot_data()`, `ggplot3_logo()` and `build_site()` write
only to a path the caller supplies, and every example writes to
`tempdir()`.

**Random number generation.** Two layouts use seeded randomness (point
jitter and the force-directed graph layout). Both save and restore
`.Random.seed`, so calling them never perturbs the user's random stream;
this is covered by a regression test.

**Interactive use.** Printing a plot opens a viewer only when
`interactive()` is `TRUE`.

## Downstream dependencies

None — this is a new package.
