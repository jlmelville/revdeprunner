# revdep-runner

`revdep-runner` is Linux infrastructure for making repeated R package
reverse-dependency checks faster and easier to resume around expensive binary
package preparation.

The project starts as a guarded wrapper around stock `revdepcheck` and
`crancache`. It does not start as a fork. The existing comparison behavior is
preserved while this project adds dependency plans, reusable artifacts,
separate run-local state, and retained evidence for comparison failures.

## Repository and data boundaries

This Git repository contains only code, tests, and configuration. Large or
mutable runtime data must live outside the checkout:

```text
$REVDEP_RUNNER_DATA/
├── warehouse/       # durable validated binary artifacts
├── manifests/       # exact dependency and artifact inventories
└── repositories/    # generated repository projections

$REVDEP_RUNNER_RUNS/ # disposable checkouts, caches, worker state, and logs
```

By default, durable data uses
`tools::R_user_dir("revdeprunner", "data")` and disposable run state uses
`tools::R_user_dir("revdeprunner", "cache")`. Set `REVDEP_RUNNER_DATA` or
`REVDEP_RUNNER_RUNS` to override them. Existing caches are inventoried as
inputs; stock tooling receives disposable copies rather than the preserved
cache or warehouse.

## Current status

The internal pipeline through the guarded stock adapter is complete. The
package can inventory existing caches, select and promote compatible binaries
without modifying their source, and derive exact source-acquisition plans. It
can acquire or reuse the planned sources needed for binary misses, traverse the
frozen dependency universe in a stable dependency order, reuse binary hits, and
build and verify both compiled and pure-R misses in disposable run-local state.
Exact binary hits and successful source builds populate one isolated run-local
preparation library, so later builds see the prepared dependency versions
without falling back to ambient user or site libraries.
Before a reverse-dependency target is prepared, the exact frozen baseline of
the package under test is installed into that isolated library as a runner-
supplied input. The same input is available during repository verification but
remains outside ordinary preparation results, artifacts, and projected
packages; stock `revdepcheck` still installs its separate old and development
copies for the actual comparison.
Independent work continues after typed failures or timeouts, downstream
packages are marked as blocked, and the result is one complete preparation
report with hashed raw logs. Re-running from an exact prior result reuses
successful work while retrying eligible failures. From a completely prepared
universe, it can copy the exact validated binaries into a staged
`src/contrib` view, generate stock `PACKAGES` metadata, and atomically publish
or reuse that view. It then installs every projected package through the view
in dependency order and loads each eligible namespace in its own vanilla R
process, retaining typed outcomes and hashed raw logs in an updated preparation
report.

The internal Linux stock adapter now turns that ready state plus a baseline
source archive matching its frozen checksum into a resumable pre-worker
checkpoint. It copies the candidate checkout and validated source and binary
artifacts into disposable state, initializes stock `revdepcheck` from the
frozen cohort, and verifies its todo rows and dependency requests before
workers start. A resumed serial run retains stock old/new statuses, typed
unchanged/changed/incomplete results, private-library versions, complete log
hashes, compiler-invocation evidence, and the candidate identity. Explicitly
excluded targets remain `not_checked`.
When no per-worker timeout is supplied, the adapter reports and uses at least
ten minutes, increasing that budget to twice the longest successful requested-
target preparation build and rounding up to five minutes. An explicit timeout
is reported and left unchanged, including when the retained timing suggests a
larger value. A timed-out target can be run again in a fresh disposable stock
workspace by excluding the other cohort targets and reusing the same prepared
repository; the adapter does not retry checks automatically.
The selected worker R must expose the pinned development revisions of
`revdepcheck` and `crancache`; the adapter does not query live metadata or
expose the preserved warehouse to stock tooling.

The completed Linux mize pilot exercised this path from one frozen CRAN
snapshot. Its direct universe prepared 232 package/version requirements; the
recursive-strong universe added `CoTiMA` and 36 requirements while reusing all
232 direct artifacts. `CMTFtoolbox` and `CoTiMA` were unchanged. `ctsem` first
timed out symmetrically while compiling its own source, so that comparison was
retained as incomplete rather than reported as a mize regression. A fresh
ctsem-only run reused the same prepared repository and completed both checks as
`OK`, classifying `ctsem` as unchanged. No comparison compiled a dependency
package, and the final targeted run added about 2.6 seconds of runner overhead
around a 2,205-second stock command. The public preparation and comparison
functions now compose these accepted internal boundaries without exposing
their contract and checkpoint choreography.

## Prepare and check

Pass the development package checkout. Direct reverse dependencies are the
default:

```r
library(revdeprunner)

prepared <- revdep_prepare("/path/to/development/package")
prepared$summary
prepared$problems
```

Preparation acquires the matching repository baseline, prepares dependencies,
and stops before old/new checks. If `problems` is non-empty, its diagnostic and
raw log paths help identify missing system libraries or package failures. Fix
external prerequisites and repeat the same call; the frozen plan and completed
work are reused.

Once preparation is ready, run the comparisons:

```r
result <- revdep_check(prepared)
result$results
result$diagnostics
```

If repository verification cannot make the selected targets ready, the call
returns a `repository-incomplete` result before stock initialization or checks.
Its targets are `not_checked`, and `diagnostics` contains the failing package,
stage, diagnostic excerpt, and raw log paths. Fix the external prerequisite,
repeat `revdep_prepare()`, and then call `revdep_check()` again.

Substituting mize, RcppHNSW, uwot, or rnndescent changes only the checkout path.
Recursive strong coverage is explicit and can be prepared directly with
`recursive = TRUE`, or inspected and bounded with `revdep_plan()` first.

The read-only preflight queries an unfiltered
repository snapshot, selects direct reverse dependencies by default, and
reports the expected requirement, compilation, system-library, unavailable-
package, and compatible-cache scope without downloading, building, or checking
packages:

```r
library(revdeprunner)

plan <- revdep_plan("/path/to/development/package")
plan$summary
plan$targets
```

Recursive strong coverage is explicit and can retain every direct target while
sampling a reproducible number of recursive-only targets:

```r
plan <- revdep_plan(
  "/path/to/development/package",
  recursive = TRUE,
  max_recursive = 20
)
```

The plan accounts for the complete candidate set and constructs its preparation
forecast from only the selected targets. It does not predict elapsed time or
download size, install operating-system libraries, or start a reverse-
dependency check.

Like ordinary R tooling, the runner trusts package code from the repositories
selected by its user. It separates runner-owned state and validates artifacts;
it is not an operating-system security sandbox.

Do not point exploratory `crancache` calls at a preserved cache: even
update-disabled operation can refresh `_meta/`.

## Development checks

From the repository root:

```sh
Rscript --vanilla -e 'devtools::document()'
air format . --check
Rscript --vanilla -e 'lints <- lintr::lint_package(); print(lints); quit(status = if (length(lints) > 0L) 1L else 0L)'
Rscript --vanilla -e 'testthat::test_local()'
Rscript --vanilla -e 'devtools::check(document = FALSE, args = "--no-manual", error_on = "note")'
```

Run `air format .` to apply formatting. A clean validation run has no errors,
warnings, notes, failing tests, lints, or Air formatting failures. A skipped,
unavailable, or interrupted check leaves validation incomplete. CI repeats
formatting, linting, tests, and package checks on supported R versions and
operating systems; the executable repository-projection path is currently
Linux-only.

No Git remote or publishing workflow is configured yet.
