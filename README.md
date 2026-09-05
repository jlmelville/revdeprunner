# revdeprunner

<!-- badges: start -->
[![R-CMD-check](https://github.com/jlmelville/revdeprunner/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/jlmelville/revdeprunner/actions/workflows/R-CMD-check.yaml)
[![Last commit](https://img.shields.io/github/last-commit/jlmelville/revdeprunner)](https://github.com/jlmelville/revdeprunner)
<!-- badges: end -->

Reverse-dependency checks for R packages, with reusable binaries and resumable runs.

`revdeprunner` prepares reusable package binaries before comparing reverse
dependencies against a released package and your development checkout. It
reports dependency failures before checks begin, resumes preparation after
fixes, and runs the comparison through stock
[`revdepcheck`](https://github.com/r-lib/revdepcheck).

This is an experimental, Linux-only tool, distributed through GitHub rather than
CRAN. Its three-step workflow has completed
a direct reverse-dependency acceptance run for RcppHNSW, including binary reuse
and checkpoint reuse. It is not yet validated for general use across machines.

## Installation

Install from GitHub with the comparison tools. If needed, first
install `pak` with `install.packages("pak")`:

```r
pak::pak("jlmelville/revdeprunner", dependencies = TRUE)
```

The machine also needs the compilers and system libraries required by the
reverse dependencies being checked. The runner reports declared system
requirements and preparation failures, but it does not install operating-system
packages.
The adapter relies on the `revdepcheck` and `crancache` revisions pinned in
`DESCRIPTION`; the installation command includes those dependencies.

## Quick start

Start by inspecting the work required for a package checkout:

```r
library(revdeprunner)

package <- "/path/to/development/package"
plan <- revdep_plan(package)

plan
plan$targets[plan$targets$selected, ]
plan$requirements
plan$unavailable
```

`revdep_plan()` queries the configured CRAN and Bioconductor repositories and
selects direct CRAN reverse dependencies by default. It inventories the
required packages and compatible cached binaries without downloading, building,
or checking packages. The printed plan summarizes how much preparation is
needed; `plan$requirements` identifies reusable binaries, source builds, native
compilation, and declared system requirements package by package.
`plan$targets` also includes unselected recursive candidates; filter on
`selected` to see the packages that will be checked.

Prepare the required packages:

```r
prepared <- revdep_prepare(plan)

prepared
prepared$problems
```

Preparation obtains the released version to use as the baseline, reuses
compatible binaries, and builds the remaining requirements in isolated run
state. It stops before the old/new comparison.

When preparation succeeds, run the checks:

```r
if (nrow(prepared$problems) == 0L) {
  result <- revdep_check(prepared)

  result
  result$results
  result$diagnostics
}
```

If the plan does not need inspection, `revdep_prepare(package)` combines the
planning and preparation steps.

Inspect `result$results` and `result$diagnostics` before treating a comparison as
complete. Results distinguish `unchanged`, `changed`, `incomplete`, and
`not_checked`; a returned object alone does not mean every check succeeded.
Repeat `revdep_check(prepared)` with the same candidate checkout to reuse its
saved comparison. See `?revdep_plan`, `?revdep_prepare`, and `?revdep_check` for
argument and return-value details.

## Expanding reverse-dependency coverage

Direct reverse dependencies are always selected. Include recursive strong
reverse dependencies with `recursive = TRUE`. A reproducible sample can bound
the additional targets without dropping direct targets:

```r
plan <- revdep_plan(
  package,
  recursive = TRUE,
  max_recursive = 20
)
```

By default, reverse targets come from CRAN while their dependency closure can
use the standard Bioconductor repositories. Alternative repositories may be
supplied with the `repos` argument.

## Recovering from preparation failures

`revdep_prepare()` returns normally when a package cannot be prepared.
`prepared$problems` identifies the package and stage and includes a diagnostic
excerpt and raw log paths. After fixing an external prerequisite, repeat the
same preparation call. The frozen plan and completed work are reused. To
refresh repository metadata and package versions, create a new plan with
`revdep_plan(package)` and pass it to `revdep_prepare()`.

Packages named only in `Suggests` but absent from the frozen repositories remain
visible in `plan$unavailable` and do not block preparation. Unavailable
`Depends`, `Imports`, and `LinkingTo` packages are blocking problems.

Preparation continues with independent packages after a failure or timeout and
marks dependent packages as blocked.

## Data and cache locations

Runner-owned data lives outside the Git checkout:

| Purpose | Default | Override |
|---|---|---|
| Runner binary and source caches, plus preparation and comparison checkpoints | `tools::R_user_dir("revdeprunner", "data")` | `REVDEP_RUNNER_DATA` |
| Disposable checkouts, caches, worker state, and logs | `tools::R_user_dir("revdeprunner", "cache")` | `REVDEP_RUNNER_RUNS` |

With `cache = NULL`, planning and preparation inspect the ordinary `crancache`
directory and the current runner-managed binary cache. An explicit `cache`
replaces default discovery, so include every cache you want inspected.
Preparation requires external cache directories to be separate from its data,
run-state, and package checkout trees; the runner's current managed source and
binary caches are exceptions. Planning alone does not check these boundaries.

Git does not back up these external directories. Back them up separately if you
need the cached packages and run evidence after losing the machine. Checkpoints
and binaries are not guaranteed to be portable to a different machine or R
installation.

### Reusing caches from before the storage redesign

Preparations from before the storage redesign used per-run repositories that
are no longer discovered by default. Their package archives can still be
reused by passing the preserved `src/contrib` directories explicitly. If those
repositories are beneath the old runner data directory, select a new,
separate data directory before planning and preparation:

```r
Sys.setenv(REVDEP_RUNNER_DATA = "/path/to/new/runner-data")
plan <- revdep_plan(
  package,
  cache = c("/path/to/crancache", "/path/to/old/repository/src/contrib")
)
```

Check the plan's reuse and build counts before preparing. Recreating an
incompatible preparation checkpoint does not make old repository locations
discoverable. Keep exploratory `crancache` calls away from preserved caches,
because even update-disabled operation can refresh their metadata.

## Reproducibility and run state

Each preparation freezes its repository snapshot, targets, package baseline,
and artifact identities. Builds use an isolated preparation library; checks use
disposable copies and separate stock old/new libraries. Saved checkpoints retain
the statuses, logs, hashes, and candidate identity needed to diagnose or resume
the run.

## Current limitations

- Planning and preparation can spend minutes validating metadata without
  printing progress. In the RcppHNSW acceptance run, repeated preparation took
  about 22 minutes despite reusing all completed work. Avoiding compilation
  does not yet make preparation fast.
- The runner does not make stock reverse-dependency checks intrinsically
  faster. Completed comparisons can return from their checkpoints, but new
  comparisons still require the actual checks.
- Libraries and worker state are isolated for reproducibility, not security.
  Preparation and checking execute package code; use trusted sources.

## Development checks

From the repository root, with `devtools`, `testthat`, `lintr`, and `pkgload`
installed, run the package checks and the same lint command used by CI:

```r
testthat::test_local()
devtools::check(document = FALSE, args = "--no-manual", error_on = "note")
pkgload::load_all(quiet = TRUE)
lints <- lintr::lint_package()
print(lints)
stopifnot(length(lints) == 0L)
```

Run `air format . --check` with Air 0.4.1, matching the CI pin. These checks
exercise package behavior and fixtures; they do not replace an operational run
against real reverse dependencies.

## License

Copyright (c) 2026 James Melville. Licensed under the
[GNU General Public License, version 3](LICENSE.md) or any later version.
