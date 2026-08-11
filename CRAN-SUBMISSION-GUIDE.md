# Submitting ggplot3 to CRAN

A step-by-step checklist. Steps 1-3 need network access, so run them
yourself. Everything in step 0 is already done.

---

## Step 0 — What is already in place

- `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes (locally)
- 565 tests passing; full suite runs in ~6 seconds
- Every exported object documents its `\value`
- `DESCRIPTION`: title case, quoted software names, DOI-cited references,
  `URL` and `BugReports` set
- `cran-comments.md` drafted (you fill in test environments — step 2)
- `inst/CITATION` present
- No function writes outside a path the caller supplies
- Seeded layouts restore `.Random.seed`, so plotting never disturbs a
  user's random stream

---

## Step 1 — Decide on the package name

**Read this before anything else.** `ggplot3` reads as an official
successor to `ggplot2`. CRAN policy says a package name must not mislead
about origin or affiliation, and reviewers do question names implying a
relationship that does not exist — even when the code is independent.

Three possible outcomes: accepted as-is; accepted after the explanation in
`cran-comments.md`; or asked to rename and resubmit.

If you would rather not risk a rejection round-trip, rename now — it is far
cheaper before submission than after. Candidates that keep the lineage
legible: `gramgraph`, `ggnext`, `vizgrammar`, `grammargraphics`.

To rename, change: `DESCRIPTION` (Package, Title, URL, BugReports), the
package-doc alias, `inst/CITATION`, `library()` calls in examples and
README, the `ggplot3()` entry-point function name, and the logo wordmark.

Also check the name is free:

```r
available::available("ggplot3")
```

---

## Step 2 — Check on other platforms

CRAN builds on Windows and Linux; your local macOS check is not enough.

```r
# Windows, R-devel and R-release (results arrive by email in ~30 min)
devtools::check_win_devel()
devtools::check_win_release()

# Linux + more, via R-hub v2 (needs a GitHub repo; one-time setup)
rhub::rhub_setup()
rhub::rhub_check()
```

Then paste the actual results into the "Test environments" section of
`cran-comments.md`, replacing the placeholder line. CRAN reviewers do read
this, and an unfilled template looks careless.

---

## Step 3 — Final local verification

```r
devtools::document()
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)
```

`remote = TRUE` validates the URLs in DESCRIPTION and README.
`manual = TRUE` builds the PDF manual, which needs LaTeX
(`tinytex::install_tinytex()` if you do not have it).

Also worth running:

```r
urlchecker::url_check()      # dead links in Rd and README
spelling::spell_check_package()
```

Target: **0 errors, 0 warnings, 0 notes**. A first submission with notes
usually bounces.

---

## Step 4 — Build the tarball

```r
devtools::build()
```

This writes `../ggplot3_0.1.0.tar.gz`. Confirm it is what you expect:

```bash
tar -tzf ../ggplot3_0.1.0.tar.gz | head -30
```

The `docs/` site, `.preview/`, `pkgdown/` and `cran-comments.md` are
build-ignored and should not appear.

---

## Step 5 — Submit

Either use the helper, which walks you through the web form:

```r
devtools::release()
```

It re-runs checks, asks a series of confirmation questions, and submits.
Answer honestly — the questions are the point of it.

Or submit manually at <https://cran.r-project.org/submit.html>:

1. Name: **Mahesh Divakaran**
2. Email: the address in `DESCRIPTION` — **it must match exactly**
3. Upload `ggplot3_0.1.0.tar.gz`
4. Paste the contents of `cran-comments.md` into the comments box

---

## Step 6 — Confirm the email

CRAN sends a confirmation link to the maintainer address within minutes.
**The submission does not enter the queue until you click it.** This is the
most common reason a submission silently disappears.

---

## Step 7 — Respond to the review

An automated check result arrives within a few hours. Human review takes
days to a couple of weeks.

If changes are requested:

1. Fix the issues
2. Bump the version (`0.1.0` → `0.1.1`)
3. Add a "Resubmission" section at the top of `cran-comments.md` saying
   what you changed
4. Resubmit

Reply on the same email thread. Do not argue with reviewers — if you
disagree, explain the reasoning briefly and let them decide.

---

## After acceptance

- The package appears on CRAN within a day
- Tag the release in git: `git tag v0.1.0 && git push --tags`
- Publish the docs: commit `docs/` and enable GitHub Pages on that folder
- Keep `NEWS.md` current — CRAN shows it, and users read it

---

## Common first-submission rejections

| Reason | Status here |
|---|---|
| Missing `\value` in exported Rd | Fixed |
| Title not in title case / too long | Fixed (37 chars) |
| `DESCRIPTION` starts with "This package…" | Not applicable |
| Software names not quoted | Fixed |
| No `URL` / `BugReports` | Fixed |
| Examples writing to the user's filespace | None — all use `tempdir()` |
| Examples taking > 5s | Slowest is ~1.2s |
| Modifying global state (`.Random.seed`, options, par) | Fixed |
| Unconditional `print()`/`cat()` in functions | Only in `print()` methods |
| `\dontrun{}` around runnable examples | None used |
| Misleading package name | **Open — see step 1** |
