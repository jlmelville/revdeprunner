# Configuration and troubleshooting

For installation and the plan–prepare–check workflow, see the [quick
start](https://jlmelville.github.io/revdeprunner/index.html#quick-start).
The examples below use the `package`, `plan`, and `prepared` objects
from that workflow.

## Selecting packages and repositories

To list the packages selected by a plan:

``` r

plan$targets[plan$targets$selected, ]
```

`plan$targets` also includes unselected recursive candidates. Use
`recursive = TRUE` to include recursive strong reverse dependencies, and
`max_recursive` to limit the additional targets. Sampling is
reproducible; set `sample_seed` alongside `max_recursive` to choose a
different sample. All direct targets remain selected.

By default, reverse dependencies come from CRAN, and their requirements
can also come from the standard Bioconductor repositories. To use
alternative repositories, pass a named vector of source repository URLs
as `repos` to
[`revdep_plan()`](https://jlmelville.github.io/revdeprunner/reference/revdep_plan.md).
An explicit `repos` replaces the defaults. When it contains a repository
named `CRAN`, reverse dependencies come from that repository.

## Unavailable dependencies and preparation failures

Inspect `plan$unavailable` for packages missing from the repositories.
Missing packages required by `Depends`, `Imports`, or `LinkingTo` block
preparation; packages needed only by `Suggests` do not.

Preparation continues with independent packages after a failure or
timeout and marks dependent packages as blocked. Check
`prepared$problems` for diagnostics and log paths, fix the prerequisite,
then repeat the same preparation call.

`revdep_prepare(package)` combines planning and preparation and reuses
its first matching saved plan on subsequent calls. To refresh repository
metadata and versions explicitly:

``` r

plan <- revdep_plan(package)
prepared <- revdep_prepare(plan)
```

## Storage and caches

Data is stored outside your package checkout:

| Purpose | Default | Override |
|----|----|----|
| Package caches and saved preparation/comparison checkpoints | `tools::R_user_dir("revdeprunner", "data")` | `REVDEP_RUNNER_DATA` |
| Disposable checkouts, worker state, and logs | `tools::R_user_dir("revdeprunner", "cache")` | `REVDEP_RUNNER_RUNS` |

Set the environment variables before planning or preparing to use
different locations:

``` r

Sys.setenv(
  REVDEP_RUNNER_DATA = "/path/to/runner-data",
  REVDEP_RUNNER_RUNS = "/path/to/runner-runs"
)
```

By default, planning and preparation look for binaries in the ordinary
`crancache` directory and the runner’s binary cache. An explicit `cache`
replaces that discovery, so include every cache you want inspected:

``` r

plan <- revdep_plan(package, cache = c("/path/to/cache-a", "/path/to/cache-b"))
```

External cache directories must be separate from the runner’s data and
run directories and your package checkout. The runner’s own current
source and binary caches are exceptions. Use `cache = character()` to
disable cache inspection.

Back up these directories separately if you need to preserve cached
packages and run evidence. Binaries and checkpoints are not guaranteed
to work on a different machine or R installation.
