# Prepare reverse dependencies

Freeze and prepare the packages needed for a reverse-dependency
comparison. Preparation stops before any old/new checks begin. If a
package cannot be prepared, the call returns normally with actionable
entries in `problems`. Repeat the same call after resolving external
prerequisites to resume the frozen preparation.

## Usage

``` r
revdep_prepare(
  package,
  recursive = FALSE,
  max_recursive = NULL,
  sample_seed = NULL,
  cache = NULL,
  repos = NULL,
  timeout_seconds = 1800L,
  verbose = FALSE
)
```

## Arguments

- package:

  A development package checkout or an existing
  [`revdep_plan()`](https://jlmelville.github.io/revdeprunner/reference/revdep_plan.md)
  object.

- recursive:

  Include recursive strong reverse dependencies as candidates.

- max_recursive:

  Optional maximum number of recursive-only targets to select. All
  direct targets remain selected.

- sample_seed:

  Optional non-negative integer used to choose a different reproducible
  recursive sample. It can be supplied only with `max_recursive`.

- cache:

  Cache directories to inspect for compatible binaries. `NULL` uses the
  ordinary `crancache` directory and the current runner-managed binary
  cache when they exist;
  [`character()`](https://rdrr.io/r/base/character.html) disables cache
  inspection. An explicit value replaces default discovery.

- repos:

  Named source repository base URLs. `NULL` combines the configured
  repositories with the standard Bioconductor repositories. An explicit
  value is used exactly. When a named `CRAN` repository is present,
  reverse targets come from CRAN while dependencies can come from every
  configured repository.

- timeout_seconds:

  Positive whole seconds allowed for each preparation subprocess
  (building, installing, or checking loadability). Increasing this
  budget retains compatible completed preparation. Defaults to 1800.

- verbose:

  Show phase, package, and completion progress. Defaults to `FALSE`.

## Value

A `revdep_prepared` object with `summary`, `problems`, `plan`,
`evidence`, and `timings`. Raw preparation log paths are retained in
`problems` and the complete private preparation report is available as
advanced evidence. `evidence$report` is `NULL` if setup failed before a
package report was saved. `timings` has one row per preparation
requirement, with `package`, `role` (`target` or `dependency`),
`build_attempts`, `build_seconds`, `install_seconds`, `load_seconds`,
and `latest_successful_build_seconds`. Durations sum unique recorded
subprocess attempts, including failures, cumulatively across resumes.
Stages without recorded attempts contribute zero; the latest successful
build is `NA` if none was recorded, including binary reuse without
source building. The table is empty before a report exists. These
measurements exclude downloads, validation, and released-subject setup;
they are neither current-call wall time nor a comparison ETA. Printing
the object or using `verbose = TRUE` shows a compact timing summary.

## Details

The durable data directory defaults to
`tools::R_user_dir("revdeprunner", "data")`, and disposable run state
defaults to `tools::R_user_dir("revdeprunner", "cache")`. Set
`REVDEP_RUNNER_DATA` or `REVDEP_RUNNER_RUNS` to override them. Supplying
a plan freezes its selected targets; planning arguments cannot also be
supplied. A checkout call reuses its first matching frozen plan. Create
and pass a new
[`revdep_plan()`](https://jlmelville.github.io/revdeprunner/reference/revdep_plan.md)
when a refreshed repository snapshot is wanted. Repository-unavailable
`Suggests` remain visible in the plan but do not block preparation,
matching stock checks with forced Suggests disabled. Available
suggestions are prepared; failures remain checking prerequisites without
blocking installation of their suggestors. Candidate hard dependency
declarations and constraints must still match the plan.

Successful binaries and package checkpoints survive removal of working
libraries and historical logs. Readiness includes a fresh-process
namespace load check in the current environment; this does not exercise
every compiled function or detect every system change.

This workflow supports Linux and macOS. Missing dependencies are built
from source, so compilers and package-specific system libraries must be
available. On macOS, use Apple's Command Line Tools and a Fortran
compiler compatible with your R installation when required. Both
source-type R (such as Homebrew) and native macOS binary repository
layouts are supported. It installs trusted package code in isolated
libraries but is not an operating-system security sandbox.

## Examples

``` r
if (FALSE) { # \dontrun{
prepared <- revdep_prepare("/path/to/package", verbose = TRUE)
prepared$problems

plan <- revdep_plan(
  "/path/to/package",
  recursive = TRUE,
  max_recursive = 20
)
prepared <- revdep_prepare(plan)
} # }
```
