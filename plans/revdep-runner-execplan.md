# Build reusable reverse-dependency check infrastructure

Type: ExecPlan

Status: Active; scaffold complete, operational implementation not started

Owner: James Melville

Last updated: 2026-08-28

Next action: Execute Work Package 1 as a read-only inventory; do not expose any
preserved cache to `crancache` while doing so.

## Scope decision

Owner objective: Make repeated R package reverse-dependency comparisons reuse
already-built dependencies instead of recompiling large dependency closures,
without damaging the package caches that make the fast path possible.

Already-working capabilities: Stock `revdepcheck` can compare old and new
package versions, `revdep_add_new()` can add only newly discovered or changed
targets, and a guarded Linux run has already completed in about four minutes
without compilation while preserving its cache byte-for-byte.

Explicit non-goals: This plan does not delete, merge in place, or mutate any
existing cache; replace `revdepcheck` before its behavior is measured; publish
a GitHub repository; provide cross-platform isolation in the first milestone;
or make shared installed libraries writable by comparison workers.

Existing proven path: Perform discovery with `CRANCACHE_DISABLE=yes` and
isolated temporary/XDG state, verify the exact todo table and database stage,
then run workers with the binary cache mounted read-only and a writable copy of
only `_meta/` overlaid at the cache's `_meta/` path. Keep the candidate checkout,
run-specific home directories, temporary directories, and logs writable.

Cheapest capacity or scope adjustment preserving that path: Automate the
proven stock-runner path and feed it a generated repository view over a durable
binary warehouse. This removes compilation before attempting to remove the
smaller cost of repeatedly extracting binaries into target-private libraries.

Why the proven path is insufficient: It is currently a manual, package-specific
procedure. Artifact identities, cache compatibility, discovery isolation,
mount construction, logging, and before/after verification are not encoded in
one tested tool, so a fresh run can silently choose a different cache or write
metadata into a preserved one.

Replacement mechanism: None in the initial implementation. A custom comparison
runner or fork is an owner-reviewed later decision, gated by measurements from
the guarded stock adapter.

Initial reviewable scope: eight work packages and 367 plan lines. Reopen scope
with the owner if the accepted plan grows by more than 50 percent and by at
least three work packages or 50 lines.

## Purpose / Big Picture

After this plan is complete, a maintainer can ask for a reverse-dependency check
of an R package and get a deterministic description of the target cohort,
dependency universe, artifact sources, and comparison results. Exact compatible
binaries are reused from a protected warehouse. Missing artifacts are built
once in staging, validated, and promoted. A warm run invokes no compiler and
cannot modify preserved warehouse artifacts.

The durable reusable object is a binary artifact warehouse with compatibility
lanes, not one forever-mutable installed R library. A later optimization may
materialize a read-only installed library for one exact manifest, but it must
not blur R-version, platform, architecture, or toolchain boundaries.

## Current State

- The repository is an intentionally minimal R package named `revdeprunner`.
- `README.md` defines the repository/data boundary.
- No operational cache, discovery, installation, or comparison code exists.
- No existing cache has been copied, linked, merged, or modified by this repo.
- No remote is configured.
- The known Linux reference run used stock development versions of
  `revdepcheck` and `crancache`, kept one completed target, added one new target,
  compiled nothing, preserved identical before/after cache manifests, and
  produced old/new OK results for both targets.

## Progress

- [x] (2026-08-28) Create the standalone package and tracked ExecPlan.
- [ ] Work Package 1: Inventory existing artifacts without invoking `crancache`.
- [ ] Work Package 2: Define manifests, compatibility lanes, and command contracts.
- [ ] Work Package 3: Implement staged validation and immutable promotion.
- [ ] Work Package 4: Generate exact repository projections and metadata overlays.
- [ ] Work Package 5: Implement the guarded stock-`revdepcheck` adapter.
- [ ] Work Package 6: Reproduce the small-package reference run.
- [ ] Work Package 7: Evaluate a shared installed-library optimization.
- [ ] Work Package 8: Pilot a larger cohort and make the fork/no-fork decision.

## Surprises & Discoveries

- `CRANCACHE_DISABLE_UPDATES=yes` prevents adding package builds but does not
  make a cache read-only; stock `crancache` can still refresh `_meta/`.
- Discovery needs repository metadata, not cached binaries. It should run with
  `CRANCACHE_DISABLE=yes`, live or deliberately frozen repository metadata, and
  isolated `TMPDIR`, home, and XDG directories.
- Stock `revdepcheck` installs each target's dependencies into a private library
  shared by that target's old and new checks, then deletes that library after
  completion. Binary tarballs can persist in `crancache`, but installed files do
  not persist across targets or runs.
- In the development `crancache` version used by the reference run,
  `available_packages()` worked on its stock no-filter path; supplying `filters`
  triggered an unrelated malformed helper expression.
- With `bubblewrap`, `/dev` must be writable enough for R to use `/dev/null`.
  Durable logging should live outside the mount namespace; `script(1)` inside a
  read-only namespace may fail to allocate a pseudo-terminal.

## Decision Log

- Decision: Create a standalone repository rather than putting infrastructure
  in an individual package repository.
  Rationale: The warehouse, manifests, isolation policy, and runner apply to
  multiple packages and must not enter their source tarballs.
  Date/Author: 2026-08-28 / Codex

- Decision: Preserve stock `revdepcheck` as the first comparison engine.
  Rationale: Its old/new semantics and result database already work; the
  immediate waste is dependency preparation and unsafe cache handling.
  Date/Author: 2026-08-28 / Codex

- Decision: Treat all discovered caches as immutable sources until inventory
  and fingerprinting are complete.
  Rationale: Blind merging can overwrite same-name artifacts built for
  incompatible R, operating-system, architecture, or toolchain lanes.
  Date/Author: 2026-08-28 / Codex

- Decision: Keep warehouse data and run state outside Git.
  Rationale: These artifacts are large, machine-specific, and partly disposable;
  the repository should contain only reproducible logic and small fixtures.
  Date/Author: 2026-08-28 / Codex

## Context and Orientation

The first inventory candidates are currently at these generalized locations:

- `$HOME/.cache/R-crancache`
- `$HOME/dev/uwot-revdep-work/xdg-cache/R-crancache`
- `$HOME/dev/mize-revdep-20260828/cache-run/R-crancache`

Do not assume these are mutually compatible. The broad cache from the larger
run has excellent coverage for the small current cohort, while the smaller
cache contains a few exact artifacts missing from the broad one. The immediate
goal is to describe those facts, not consolidate them.

The system has three separate state classes:

1. Source inputs: preserved existing caches and downloaded source tarballs.
2. Durable outputs: validated binary artifacts, exact manifests, checksums, and
   generated repository projections.
3. Disposable run state: writable homes, XDG directories, temporary files,
   target libraries, metadata overlays, candidate checkouts, and verbose logs.

The package checkout is never a member of any of these classes. Runtime paths
must resolve outside the repository before a mutating command starts.

## Interfaces and Dependencies

The first public surface should be a small command-line interface backed by R
functions that are independently testable. Work Package 2 must freeze command
names and exit behavior before implementation. At minimum it needs operations
for inventory, preparation, guarded comparison, and verification.

Inputs must include an explicit package checkout, data root, run root,
repository snapshot or repository set, R executable, and compatibility lane.
Commands that can mutate data must support a dry-run or plan output and must
print their resolved paths before acting.

Prefer base R and narrow dependencies. Linux mount isolation may use
`bubblewrap` through a small shell launcher; apply shell-specific validation to
that launcher when it is introduced. Do not hide mount policy inside a long
quoted `system()` expression.

Machine-readable outputs should be stable TSV or JSON with an explicit schema
version. Human reports are derived views, not the only durable evidence.

## Plan of Work

### Work Package 1: Read-only artifact inventory

Walk each known cache using filesystem and archive readers only. Do not call
`crancache`. For every binary or source artifact, record its cache root,
relative path, filename, package, version, size, modification time, SHA-256,
archive type, `Built` metadata, platform, and whether compilation is required
when that fact is available. Inventory `_meta/` separately, including each
`PACKAGES` file's path, size, modification time, and SHA-256.

Emit one immutable inventory per source root into a new staging area. Report
duplicate hashes, same package/version with different hashes, unreadable
archives, incomplete metadata, and likely compatibility conflicts. Do not
deduplicate or choose winners yet.

### Work Package 2: Contracts and compatibility lanes

Define the artifact identity, lane schema, manifest schemas, resolved-path
policy, command contracts, and typed exit states. At minimum, a binary lane must
distinguish R major/minor version, R platform/architecture, operating-system ABI,
and an explicit toolchain tag when the available metadata cannot prove
compatibility without it.

Specify how repository metadata snapshots are identified and how direct
targets, hard dependency closures, runner-supplied packages, unavailable
packages, and source checksums are represented. Add small synthetic fixtures
covering duplicates, collisions, pure-R packages, compiled packages, and corrupt
archives.

### Work Package 3: Staging and immutable promotion

Create a new warehouse root without changing source caches. Select compatible
artifacts from inventories, copy or hard-link them into staging only after the
link/copy policy is explicitly selected, validate identity and hashes, then
promote atomically. Never overwrite a different hash at an existing identity.

Build missing artifacts into a separate writable build cache. Validate each
result before promotion. A failed or interrupted build must leave the durable
warehouse unchanged and be resumable from already validated artifacts.

### Work Package 4: Repository projections

Generate an exact, read-only repository view for one manifest and compatibility
lane. It must contain no ambiguous package/version selection. Generate and
validate `PACKAGES` metadata in staging, then publish the complete projection.

Keep mutable `_meta/` state outside the warehouse and projection. Prove that a
projection can satisfy installation in a clean disposable library without
compiler invocation for every artifact expected to be binary-backed.

### Work Package 5: Guarded stock adapter

Automate two deliberately separate phases:

1. Discovery: hide the preserved warehouse, set `CRANCACHE_DISABLE=yes`, use
   isolated writable directories, and resolve repository metadata. Immediately
   verify the exact todo table and expected database stage.
2. Comparison: record the warehouse baseline before any operation that could
   invoke `crancache`; mount the warehouse read-only; overlay only a writable
   copy of `_meta/`; bind the candidate and run roots writable; provide usable
   `/dev`; and keep durable logs outside the namespace.

Keep `CRANCACHE_DIR` pointed at the mounted preserved path. Compare before/after
file count plus `PACKAGES` sizes, modification times, and hashes. Treat any
warehouse change, unexpected target, unexpected stage, compiler invocation, or
missing typed result as infrastructure failure.

### Work Package 6: Small-package reference pilot

Use the previously successful two-target package as the first end-to-end pilot.
Start from its preserved completed target and add only the new target. Acceptance
requires the exact intended todo set, zero compilation on the warm run,
byte-identical warehouse manifests, completion in the same order of magnitude
as the roughly four-minute reference, and valid old/new OK outcomes for both
the retained and newly run targets.

Also exercise restart after an intentional pre-worker stop. The restart must
not discard the completed target or repeat discovery mutations.

### Work Package 7: Shared installed-library evaluation

Measure time spent extracting binaries after compilation has been eliminated.
Only if extraction is material, create one installed dependency library for an
exact frozen manifest. Validate all installed versions and direct-target
namespace loads, then expose that library read-only.

Old and new package-under-test installs and all homes, XDG caches, temporary
directories, and check roots remain side-specific and writable. Exercise a
fixture in which both sides write the same default cache key and require the
writes to remain isolated. Compare results with the stock adapter before
accepting this optimization.

### Work Package 8: Larger pilot and fork decision

Run a small representative cohort from the larger package before attempting its
full reverse-dependency universe. Require one typed result per requested target
and reject missing, duplicate, or unattributed shared failures.

Decide among three outcomes using measured evidence: keep the stock adapter;
add a separate custom fast comparator; or propose a narrowly scoped
`revdepcheck` fork. A fork is justified only if necessary hooks for dependency
libraries, cleanup, or result reporting cannot be supplied safely from the
wrapper and upstream will not accept them.

## Concrete Steps

A fresh agent begins with:

```sh
cd "$HOME/dev/revdep-runner"
git status --short --branch --untracked-files=all
sed -n '1,260p' plans/revdep-runner-execplan.md
R CMD build .
R CMD check --no-manual ../revdeprunner_0.0.0.9000.tar.gz
```

Then execute only Work Package 1. Before writing inventory code, capture the R
version/platform and list the three source roots with ordinary read-only
filesystem commands. Update this plan's Current State, Progress, discoveries,
and validation evidence before pausing or moving to Work Package 2.

Do not run an unattended build or comparison loop during Work Package 1. Later
long-running commands must define progress, success, regression, failure, and a
human-judgment stop boundary before launch.

## Validation and Acceptance

Scaffold acceptance:

- `R CMD build .` succeeds.
- `R CMD check --no-manual` reports zero errors, warnings, and notes attributable
  to the package.
- Git starts clean on `main` after the initial commit and has no remote.
- No runtime data exists under the repository root.

Inventory acceptance:

- Every source-root artifact is represented exactly once or has an explicit
  unreadable/error record.
- Re-running inventory changes no source-root file metadata or content and
  produces identical normalized inventory content.
- Collision and compatibility reports are deterministic and fixture-tested.

End-to-end acceptance:

- Discovery cannot read or write the preserved warehouse.
- The worker can read compatible artifacts but cannot mutate the warehouse.
- A warm small-package pilot launches no compiler, preserves identical cache
  manifests, and yields the expected old/new result pairs.
- A small larger-package cohort produces complete typed results with parity to
  the stock runner for the same manifest.
- Failures in preparation or infrastructure are never reported as unchanged
  reverse dependencies.

## Idempotence and Recovery

All source caches are read-only inputs. Never repair them in place. Inventories
are content-addressed or written to new versioned paths. Warehouse promotion is
staged and atomic, and an identity collision fails closed without overwrite.

Per-run directories receive unique IDs and may be resumed only when their
manifest, package commits, repositories, R executable, and lane match. Cleanup
must accept one fully resolved run directory, refuse broad paths, and default to
preserving logs and typed results.

If mount isolation fails, stop before workers. If a worker changes a preserved
cache despite the boundary, stop, retain before/after manifests and logs, and
do not launch another target. If discovery produces an unexpected todo set or
stage, stop for human review rather than repairing the database automatically.

## Artifacts and Notes

Tracked artifacts belong in `R/`, `tests/`, small fixtures, documentation, and
this plan. Large inventories, tarballs, repository projections, installed
libraries, check trees, and raw logs remain outside Git under explicitly
resolved data or run roots.

Keep sanitized command summaries and bounded failure excerpts in this plan;
do not paste complete compiler logs. Record exact generated artifact paths and
hashes in machine-readable run manifests.

## Outcomes & Retrospective

The repository boundary and implementation sequence are established. The
important early outcome is deliberately modest: turn the successful guarded
manual procedure into a repeatable wrapper before designing a replacement
runner. Update this section after each pilot with timings, compilation counts,
cache-manifest equality, result parity, and the fork decision.
