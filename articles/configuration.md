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
different sample. All direct targets remain selected. Preparation totals
count each selected target or required dependency once, even when a
target is also another target’s dependency.

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
preparation; packages needed only by `Suggests` do not. Available
suggested packages are prepared for checking. A failure to prepare one
remains a problem that must be resolved before comparison, but does not
block installing the package that suggests it.

Preparation continues with independent packages after a failure or
timeout and marks dependent packages as blocked. Check
`prepared$problems` for diagnostics and log paths, fix the prerequisite,
then repeat the same preparation call.

Download failures and failures to install the released subject also
appear in `prepared$problems`. They stop the current preparation attempt
while retaining its frozen plan and recorded successes. Retry the same
call after fixing the reported cause. Invalid metadata or inconsistent
artifacts still raise errors.

`revdep_prepare(package)` combines planning and preparation and reuses
its first matching saved plan on subsequent calls. To refresh repository
metadata and versions explicitly:

``` r

plan <- revdep_plan(package)
prepared <- revdep_prepare(plan)
```

The plan includes hard dependencies from both the released subject and
the candidate checkout. If you edit candidate `Depends`, `Imports`, or
`LinkingTo`, including version constraints, create a new plan and
prepare it again. Unsatisfied constraints produce an error identifying
the required and selected versions. Ordinary source edits can reuse
preparation.

## Resuming checks and adjusting timeouts

After interruption or timeout, repeat `revdep_check(prepared)` to resume
unfinished targets. Completed baseline/candidate pairs are retained,
including changed results; an interrupted pair may run both sides again.
Keep the run directory while resuming an unfinished comparison.

If a target needs more time, increase the worker budget. The overall
process budget includes subject installation and all target checks, and
defaults to two hours:

``` r

result <- revdep_check(
  prepared,
  worker_timeout_seconds = 1800,
  process_timeout_seconds = 14400,
  verbose = TRUE
)
```

Changing these budgets retains completed work. The automatic worker
budget is at least ten minutes and grows with recorded target build
times. Preparation has its own `timeout_seconds` argument, defaulting to
1800 seconds for each build, install, or loadability subprocess.

Use `result$changes` to read added and removed errors, warnings, and
notes. These details are saved with the result and remain inspectable
after comparison logs are removed. An `unchanged` outcome means stock
found no new problems; it can still have removed problems in `changes`.

After repairing an external library or changing the environment in a way
that may affect checks, run
`revdep_check(prepared, repeat_checks = TRUE)`. This repeats both sides
for every selected target and retains eligible dependency binaries.

Preparation and check admission test each prepared package’s namespace
in a fresh R process. This catches load failures even when a namespace
is already loaded in your interactive session. It does not exercise
every compiled function or detect every system change. Binary reuse
requires the same R major/minor version, platform, architecture, and OS
tag; completed comparison reuse also requires the same full R version. A
patch upgrade can therefore reuse binaries while starting new checks.

## Storage and caches

Data is stored outside your package checkout:

| Purpose | Default | Override |
|----|----|----|
| Package caches and saved preparation/comparison checkpoints | `tools::R_user_dir("revdeprunner", "data")` | `REVDEP_RUNNER_DATA` |
| Working libraries, comparison databases, checkouts, and logs | `tools::R_user_dir("revdeprunner", "cache")` | `REVDEP_RUNNER_RUNS` |

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

Successful preparation is checkpointed package by package, with binaries
retained under immutable content-addressed paths in the data directory.
Removing working libraries or historical preparation logs does not
discard those binaries; the next preparation call reconstructs what it
needs.

Back up both directories if you need to preserve an unfinished
comparison and its evidence. Completed comparison results can be reused
from their checkpoint without the old comparison workspace. Binaries and
checkpoints are not guaranteed to work on a different machine or R
installation.
