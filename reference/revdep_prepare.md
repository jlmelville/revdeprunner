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
  repos = NULL
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

## Value

A `revdep_prepared` object with `summary`, `problems`, `plan`, and
`evidence`. Raw preparation log paths are retained in `problems` and the
complete private preparation report is available as advanced evidence.

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
matching stock checks with forced Suggests disabled.

This workflow currently supports Linux. It installs trusted package code
in isolated libraries but is not an operating-system security sandbox.

## Examples

``` r
if (FALSE) { # \dontrun{
prepared <- revdep_prepare("/path/to/package")
prepared$problems

plan <- revdep_plan(
  "/path/to/package",
  recursive = TRUE,
  max_recursive = 20
)
prepared <- revdep_prepare(plan)
} # }
```
