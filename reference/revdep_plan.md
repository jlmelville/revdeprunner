# Plan a reverse-dependency check

Discover reverse dependencies and estimate the preparation work without
downloading, building, or checking packages. Direct reverse dependencies
are always selected. Recursive-strong-only targets are opt-in and can be
limited with a reproducible sample.

## Usage

``` r
revdep_plan(
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

  Path to the development package checkout.

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

A `revdep_plan` list with `summary`, `targets`, `requirements`,
`unavailable`, `repository_alternates`, and `workload` tables.
Per-target requirement counts overlap; use `summary` for totals over
unique selected targets and their installation dependencies in
`requirements`. Declared system requirements are metadata clues, not a
platform-readiness check. Repository-unavailable `Suggests` remain in
`unavailable`, but do not enter preparation requirements.

## Details

`workload` explains the selected targets and their direct dependency
roots. Its columns are `target`, `relationship`, `root`, `requirements`,
`native_requirements`, and `source_builds`. Rows with
`relationship = "target"` count the target itself and its installation
dependencies. Other rows count a direct dependency root and its
reachable installation dependencies, using the same frozen dependency
policy. Base packages, the supplied subject, and unavailable packages do
not count as preparation requirements. Counts overlap across roots and
targets; they are not estimates of packages saved by dropping a
dependency. Native counts mean packages declaring
`NeedsCompilation: yes`; source-build counts reflect cache availability
at planning time.

Preparation creates reusable dependency binaries. Comparison still runs
source checks of each selected target against both released and
development versions of the subject, including target compilation when
needed.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- revdep_plan("/path/to/package")
plan$summary

sampled <- revdep_plan(
  "/path/to/package",
  recursive = TRUE,
  max_recursive = 20
)
sampled$targets
} # }
```
