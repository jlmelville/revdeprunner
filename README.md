# revdep-runner

`revdep-runner` is internal Linux infrastructure for making repeated R package
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

Runtime defaults have deliberately not been fixed. Compatibility lanes and
artifact identities are explicit. Existing caches are inventoried as inputs;
stock tooling receives disposable copies rather than the preserved cache or
warehouse.

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
The selected worker R must expose the pinned development revisions of
`revdepcheck` and `crancache`; the adapter does not query live metadata or
expose the preserved warehouse to stock tooling. These helpers remain internal,
the mize end-to-end pilot is underway, and operational commands do not exist
yet.

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
Rscript --vanilla -e 'devtools::check(document = FALSE, error_on = "note")'
```

Run `air format .` to apply formatting. A clean validation run has no errors,
warnings, notes, failing tests, lints, or Air formatting failures. A skipped,
unavailable, or interrupted check leaves validation incomplete. CI repeats
formatting, linting, tests, and package checks on supported R versions and
operating systems; the executable repository-projection path is currently
Linux-only.

No Git remote or publishing workflow is configured yet.
