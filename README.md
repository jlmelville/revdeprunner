# revdep-runner

`revdep-runner` is internal infrastructure for making repeated R package
reverse-dependency checks fast, reproducible, and safe around expensive binary
package caches.

The project starts as a guarded wrapper around stock `revdepcheck` and
`crancache`. It does not start as a fork. The existing comparison behavior is
preserved while this project adds exact manifests, immutable artifact reuse,
isolated writable state, and explicit verification that a run did not modify a
preserved cache.

## Repository and data boundaries

This Git repository contains only code, tests, configuration, and plans. Large
or mutable runtime data must live outside the checkout:

```text
$REVDEP_RUNNER_DATA/
├── warehouse/       # durable validated binary artifacts
├── manifests/       # exact dependency and artifact inventories
└── repositories/    # generated repository projections

$REVDEP_RUNNER_RUNS/ # disposable per-run work, writable overlays, and logs
```

Runtime defaults have deliberately not been fixed. Compatibility lanes and
artifact identities are being defined as versioned, fail-closed contracts in
the execution plan. Existing caches remain immutable inputs.

## Current status

The read-only inventory work package is complete. It observes package artifacts,
writes immutable per-cache inventories, and reports cross-cache duplicates,
collisions, metadata issues, and likely compatibility conflicts. These helpers
remain internal while the next work package defines versioned identities,
manifests, preparation evidence, and command contracts. Operational commands do
not exist yet.

Do not point exploratory `crancache` calls at a preserved cache: even
update-disabled operation can refresh `_meta/`.

Start with [plans/revdep-runner-execplan.md](plans/revdep-runner-execplan.md).

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
operating systems.

No Git remote or publishing workflow is configured yet.
