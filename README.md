# revdep-runner

Reverse-dependency checks can spend hours downloading and compiling packages,
only to bury a missing system library in pages of installation output and
repeat completed work once that prerequisite is fixed.

`revdeprunner` separates that unavoidable preparation from the old/new
comparison. It freezes the packages needed for a comparison, prepares reusable
binary artifacts, reports actionable dependency failures before checks begin,
resumes after fixes, and then runs the comparison through stock `revdepcheck`.

## Installation

Install from a local checkout with the optional comparison tools:

```r
pak::local_install("/path/to/revdep-runner", dependencies = TRUE)
```

`revdeprunner` currently runs on Linux.

The machine also needs the compilers and system libraries required by the
reverse dependencies being checked. The runner reports declared system
requirements and preparation failures, but it does not install operating-system
packages.

## Quick start

Start by inspecting the work required for a package checkout:

```r
library(revdeprunner)

package <- "/path/to/development/package"
plan <- revdep_plan(package)

plan
plan$targets
plan$requirements
plan$unavailable
```

`revdep_plan()` queries the configured CRAN and Bioconductor repositories and
selects direct CRAN reverse dependencies by default. It inventories the
required packages and compatible cached binaries without downloading, building,
or checking packages. The printed plan summarizes how much preparation is
needed; `plan$requirements` identifies reusable binaries, source builds, native
compilation, and declared system requirements package by package.

Prepare the selected dependency universe:

```r
prepared <- revdep_prepare(plan)

prepared
prepared$problems
```

Preparation acquires the matching repository baseline, reuses compatible
binaries, and builds the remaining requirements in isolated run state. It stops
before the old/new comparison.

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
same preparation call. The frozen plan and completed work are reused.

Packages named only in `Suggests` but absent from the frozen repositories remain
visible in `plan$unavailable` and do not block preparation. Unavailable
`Depends`, `Imports`, and `LinkingTo` packages are blocking problems.

Preparation continues with independent packages after a failure or timeout and
marks dependent packages as blocked. Comparisons retain stock old/new statuses
and typed `unchanged`, `changed`, `incomplete`, and `not_checked` results.

## Data and cache locations

Runner-owned data lives outside the Git checkout:

| Purpose | Default | Override |
|---|---|---|
| Runner binary and source caches, plus preparation checkpoints | `tools::R_user_dir("revdeprunner", "data")` | `REVDEP_RUNNER_DATA` |
| Disposable checkouts, caches, worker state, and logs | `tools::R_user_dir("revdeprunner", "cache")` | `REVDEP_RUNNER_RUNS` |

With `cache = NULL`, planning and preparation reuse compatible packages from
the ordinary `crancache` directory and earlier runner preparations. Set `cache`
to one or more directories when a run should inspect only those package caches.

Only package/version pairs needed by the current preparation are inspected.
Before invoking stock tooling, the runner copies the required archives into
disposable run state.
Keep exploratory `crancache` calls away from preserved caches, because even
update-disabled operation can refresh `_meta/`.

## Reproducibility and run state

Each preparation freezes its repository snapshot, targets, package baseline,
and artifact identities. Builds use an isolated preparation library; checks use
disposable copies and separate stock old/new libraries. Saved checkpoints retain
the statuses, logs, hashes, and candidate identity needed to diagnose or resume
the run.
