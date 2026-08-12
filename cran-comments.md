# cran-comments

## Submission

This is a new submission: ggnext 0.1.0.

## Test environments

* local macOS (darwin), R 4.5.2 — `R CMD check --as-cran`
* win-builder, R-devel (2026-08-10 r90389 ucrt), Windows Server 2022 x64

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard "New submission", together with the possibly
misspelled words addressed under Spelling below.

An earlier win-builder run also flagged the `build_site()` example at 23s.
That was a genuine inefficiency rather than a slow example: the helper
building the reference page reparsed the whole Rd database once per
function. It is now parsed once and cached, which took the full site build
from 3.6s to 0.9s locally, and the example itself runs in well under a
second.

## Notes for the reviewer

**On the package name.** The `gg` prefix marks the shared Grammar of
Graphics lineage. `ggnext` is an independent implementation built on S7,
not a `ggplot2` extension and not affiliated with or endorsed by the
`ggplot2` authors; it does not compose with `ggplot2` objects. The README
and package documentation state this in their opening paragraphs and
credit `ggplot2` and the wider extension ecosystem as design references.

**Dependencies.** The only hard dependency is `S7`. `grDevices` and `utils`
are base packages. `testthat` and `xml2` are used only in tests.

**Writing to the file system.** No function writes anywhere by default.
`render()`, `write_plot_data()`, `ggnext_logo()` and `build_site()` write
only to a path the caller supplies, and every example writes to
`tempdir()`.

**Random number generation.** Two layouts use seeded randomness (point
jitter and the force-directed graph layout). Both save and restore
`.Random.seed`, so calling them never perturbs the user's random stream;
this is covered by a regression test.

**Interactive use.** Printing a plot opens a viewer only when
`interactive()` is `TRUE`.

**Spelling.** The words flagged by the incoming check are surnames from the
cited algorithm references (Bruls, Huizing, van Wijk, Fruchterman, Reingold,
Kaplan) and established technical terms (Sankey, SHAP, treemap, treemaps,
squarified). All are spelled correctly.

## Downstream dependencies

None — this is a new package.
