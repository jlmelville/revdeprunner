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

The defaults and compatibility-lane schema have deliberately not been fixed in
the scaffold. They are decisions in the execution plan. Existing caches must
remain untouched until they have been inventoried and fingerprinted.

## Current status

The package skeleton and execution plan exist; operational commands do not.
The next work is a read-only inventory of existing package artifacts. Do not
point exploratory `crancache` calls at a preserved cache: even update-disabled
operation can refresh `_meta/`.

Start with [plans/revdep-runner-execplan.md](plans/revdep-runner-execplan.md).

## Development checks

From the repository root:

```sh
R CMD build .
R CMD check --no-manual ../revdeprunner_0.0.0.9000.tar.gz
```

No Git remote or publishing workflow is configured yet.

