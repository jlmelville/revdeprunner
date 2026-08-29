# Build reusable reverse-dependency check infrastructure

Type: ExecPlan

Status: Active; WP1-B implemented and validated; independent review pending

Owner: James Melville

Last updated: 2026-08-29

Next action: Freeze the exact WP1-B target and send one self-contained,
read-only review packet to the independent reviewer.

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
- Air and lintr configuration plus hardened package-check, formatting, and lint
  workflows establish the development baseline. No remote has exercised those
  workflows yet.
- `R/inventory-observe.R` now contains an internal read-only observation and
  content-addressed serialization layer. Its opaque RDS payload is not a public
  API or manifest schema; those contracts remain Work Package 2 decisions.
- No existing cache has been copied, linked, merged, or modified by this repo.
- No remote is configured.
- The accumulated plan exceeded its recorded 367-line baseline by more than 50
  percent without adding work packages. The owner confirmed the expanded review
  and reverse-dependency cohort contract on 2026-08-29 before WP1-B began.
- The Work Package 1 preflight observed R 4.5.2 on
  `x86_64-pc-linux-gnu`. All three known cache roots exist. At the time of the
  read-only listing, they contained 4,269, 2,724, and 385 regular files in the
  order listed under Context and Orientation.
- The initial scaffold commit is `9645667073c415becc8653964f193a35ed95a64f`.
- On R 4.5.2 for `x86_64-pc-linux-gnu`, the scaffold built successfully and
  `R CMD check --no-manual` completed with `Status: OK`. The restricted check
  environment could not reach the configured CRAN index, but all declared
  dependencies were available locally and dependency checking completed.
- The known Linux reference run used stock development versions of
  `revdepcheck` and `crancache`, kept one completed target, added one new target,
  compiled nothing, preserved identical before/after cache manifests, and
  produced old/new OK results for both direct reverse dependencies.
- An unfiltered CRAN inventory queried on 2026-08-29 identifies
  `CMTFtoolbox` and `ctsem` as direct reverse dependencies of `mize`, with
  `CoTiMA` added only by the recursive-strong query. The two-target reference
  run is therefore a valid direct-cohort checkpoint, not a completed
  recursive-strong reverse-dependency check.

## Progress

- [x] (2026-08-28) Create, validate, and locally commit the standalone package
  and tracked ExecPlan.
- [x] (2026-08-28) Add local formatting and linting configuration, hardened CI
  scaffolding, and repeatable development checks.
- [x] (2026-08-29) Record the owner-required chunk and bounded independent
  review protocol and complete the read-only Work Package 1 preflight.
- [x] (2026-08-29) Replace the initial three-turn review allowance with one
  correction and one re-review, and add owner-approved direct versus
  recursive-strong cohort discovery.
- [ ] Work Package 1: Inventory existing artifacts without invoking `crancache`.
  - [x] (2026-08-29) WP1-A: Implement read-only artifact and
    repository-metadata observation. Reviewer turn 1 requested safer archive
    reads, fail-closed traversal, and real ZIP/link fixtures. Reviewer turn 2
    accepted the corrected implementation.
  - [ ] WP1-B: Emit deterministic immutable per-root inventories and prove
    source-root invariance. Implementation, focused tests, a real-cache smoke,
    and the complete development gate pass; independent review is pending.
  - [ ] WP1-C: Report duplicates, collisions, incomplete metadata, unreadable
    archives, and likely compatibility conflicts across roots.
- [ ] Work Package 2: Define manifests, compatibility lanes, and command contracts.
- [ ] Work Package 3: Implement staged validation and immutable promotion.
- [ ] Work Package 4: Generate exact repository projections and metadata overlays.
- [ ] Work Package 5: Implement the guarded stock-`revdepcheck` adapter.
- [ ] Work Package 6: Reproduce the small-package reference run.
- [ ] Work Package 7: Evaluate a shared installed-library optimization.
- [ ] Work Package 8: Pilot a larger cohort and make the fork/no-fork decision.

## Active Chunk: WP1-B

Scope: Add deterministic, immutable per-root inventory serialization for the
existing WP1-A observation records, and prove that creating the serialized
inventory does not alter any source-root content or metadata.

Non-goals: Do not add cross-root collision or compatibility reports (WP1-C),
freeze public manifest or CLI contracts (WP2), call `crancache`, mutate or
consolidate a cache, or begin reverse-dependency discovery and execution.

Exit criteria: Repeated serialization of the same normalized observation
produces byte-identical output; outputs are written only beneath a validated
staging destination outside every source root and the package checkout; source
roots have identical before/after invariance observations; failures are
explicit and leave no accepted partial inventory.

Validation: Add focused deterministic-serialization, destination-boundary,
failure, and source-invariance tests; run the complete repository development
gate; perform a bounded read-only review against a frozen target using the
protocol below.

Current state: The internal writer serializes the normalized WP1-A observation,
publishes it under cache-root and content SHA-256 identities without
overwriting, rejects overlapping or linked output paths, and verifies a
complete source-file observation before publication. Focused tests, the
smallest-cache smoke, and the complete development gate pass.

Next action: Commit this plan, source, and test state as the immutable review
target, then obtain the bounded read-only verdict. Do not begin WP1-C.

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
- The three known caches share the same broad directory shape: source
  repositories under `cran`, `bioc`, and `other`; binary repositories under
  corresponding `-bin` directories; and repository snapshots under `_meta`.
  This supports one filesystem scanner but does not establish artifact
  compatibility between roots.
- The initial extraction-based reader took about 85 seconds on the smallest
  preserved cache. After reviewer-requested in-memory archive reads, the same
  observation took about 26 seconds. A full-cache command will still need
  bounded progress reporting before launch.
- Of 343 package artifacts in the smallest cache, 89 have a `Built` field whose
  platform component is empty. Their platform can be observed from the binary
  filename, but the incomplete `Built` metadata remains explicit rather than
  being silently treated as complete.
- CRAN package pages expose direct reverse relationships but do not identify
  transitive consumers. With an unfiltered CRAN package database on 2026-08-29,
  `tools::package_dependencies("mize", which = "most", recursive = FALSE,
  reverse = TRUE)` returned `CMTFtoolbox` and `ctsem`, while
  `recursive = "strong"` also returned `CoTiMA` through `ctsem`.

## Chunk and Review Protocol

Work on one coherent, substantive chunk at a time. Before source editing, record
the chunk's scope, non-goals, exit criteria, validation, current state, and next
action in this plan. Complete and validate only that chunk; do not combine
unrelated work because context remains.

After each substantive source-changing chunk:

1. Update this plan and freeze the exact review target. Prefer a commit or tree;
   otherwise preserve a frozen patch or packet and its digest.
2. Give exactly one separate reviewer a self-contained, read-only packet with
   the owner objective, chunk scope and non-goals, exit criteria, target
   identity, relevant evidence, and validation results.
3. Require the reviewer to echo the target identity and return `PASS`,
   `NEEDS_CHANGES`, or `SCOPE_REOPEN`. The reviewer must separate blocking
   correctness, safety, regression, or contract findings from optional
   suggestions and must not edit the repository, plan, or external learning
   state.
4. Verify every finding against the current source and accepted contract. Apply
   only confirmed, in-scope blocking fixes; review feedback does not expand
   goals, cost, semantics, or authority.
5. Allow one correction pass, rerun validation, freeze the corrected target,
   and obtain one re-review. Stop on `PASS`.

Stop and ask the owner for direction if the re-review does not pass, a concern
is repeated or disputed, the proposed review work becomes comparable in size
to the original chunk, the target changes during review, or the reviewer
returns `SCOPE_REOPEN`.

If compaction occurs during implementation, re-read the repository
instructions, the planning workflow, this plan, and the latest handoff;
reconcile them with the worktree, reach the next safe coherent boundary, update
the plan and handoff, and stop for owner confirmation. If compaction occurs
after review starts but before `PASS`, end the live review immediately and do
not infer acceptance. After owner confirmation, resume from durable artifacts,
refresh the target identity, and rerun the outstanding review.

The coordinating agent owns applicable papercut capture and one consolidated
skill-retrospective evaluation after validation and review complete. Reviewers
may report possible workflow lessons to the coordinator but must not create or
change papercut, retrospective, or verification records.

Unless the owner explicitly authorizes more, complete one chunk and its bounded
review loop, then stop with a handoff.

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

- Decision: Keep WP1-A internal and use `digest` for portable SHA-256 hashing.
  Rationale: Work Package 2 still owns public command and schema names, while
  `digest` supplies the required hash on the release and old-release R versions
  exercised by CI.
  Date/Author: 2026-08-29 / Codex

- Decision: Read archive DESCRIPTION payloads directly and traverse cache
  directories fail-closed without following directory links.
  Rationale: Reviewer turn 1 showed that extraction can materialize tar links
  and that recursive `list.files()` can silently omit unreadable subtrees. An
  in-memory tar/ZIP reader removes scratch writes, while an explicit directory
  walk turns incomplete traversal into an error.
  Date/Author: 2026-08-29 / Codex

- Decision: Discover and freeze both direct and recursive-strong reverse-
  dependency cohorts from one unfiltered repository snapshot, and exercise the
  full recursive-strong cohort in the mize pilot.
  Rationale: CRAN package pages show direct relationships only, while CRAN's
  submission policy recommends checking recursive strong dependencies when
  possible. Recording both sets prevents a direct-only run from being mistaken
  for complete recursive-strong coverage without forcing one cohort policy on
  every future run.
  Date/Author: 2026-08-29 / Codex

- Decision: Keep WP1-B serialization internal, content-addressed, and
  append-only, using the normalized WP1-A observation as an opaque RDS payload.
  Rationale: Byte-identical serialization and no-overwrite publication satisfy
  this chunk's durable inventory requirement without pre-empting Work Package
  2's public manifest and compatibility contracts.
  Date/Author: 2026-08-29 / Codex

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
repository snapshot or repository set, reverse-dependency cohort policy, R
executable, and compatibility lane. Commands that can mutate data must support
a dry-run or plan output and must print their resolved paths before acting.

Cohort discovery must use the same frozen repository metadata as comparison,
without implicit local/platform filtering. Preserve the direct query and the
`which = "most", recursive = "strong", reverse = TRUE` query separately, then
record each selected target as direct or recursive-strong-only. A live CRAN
query may establish or refresh the snapshot, but it is not a reproducible run
identity by itself.

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

Execute this work package in three chunks. WP1-A implements only the internal
read-only observation layer: discover package archives and `_meta/PACKAGES*`,
record filesystem facts and SHA-256, read `DESCRIPTION` metadata without
writing to the source root, and return explicit error records for unreadable or
incomplete archives. WP1-B owns deterministic per-root serialization and
before/after source-root invariance. WP1-C owns cross-root reports and the final
Work Package 1 acceptance check. These are execution boundaries, not new public
command or schema commitments; Work Package 2 still freezes those contracts.

### Work Package 2: Contracts and compatibility lanes

Define the artifact identity, lane schema, manifest schemas, resolved-path
policy, command contracts, and typed exit states. At minimum, a binary lane must
distinguish R major/minor version, R platform/architecture, operating-system ABI,
and an explicit toolchain tag when the available metadata cannot prove
compatibility without it.

Specify how repository metadata snapshots are identified and how direct
targets, recursive-strong-only targets, dependency paths, hard dependency
closures, runner-supplied packages, unavailable packages, and source checksums
are represented. Freeze the exact dependency-query arguments and require an
unfiltered inventory equivalent to `available.packages(filters = list())`.
Add small synthetic fixtures covering duplicates, collisions, pure-R packages,
compiled packages, corrupt archives, and a transitive reverse dependency.

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
   isolated writable directories, resolve or load frozen repository metadata,
   and compute both direct and recursive-strong target sets. Immediately verify
   the selected cohort, exact todo table, and expected database stage.
2. Comparison: record the warehouse baseline before any operation that could
   invoke `crancache`; mount the warehouse read-only; overlay only a writable
   copy of `_meta/`; bind the candidate and run roots writable; provide usable
   `/dev`; and keep durable logs outside the namespace.

Keep `CRANCACHE_DIR` pointed at the mounted preserved path. Compare before/after
file count plus `PACKAGES` sizes, modification times, and hashes. Treat any
warehouse change, unexpected target, unexpected stage, compiler invocation, or
missing typed result as infrastructure failure.

### Work Package 6: Small-package reference pilot

Use the package from the previously successful two-target run as the first
end-to-end pilot. First reproduce its direct-cohort checkpoint from the
preserved completed target, then extend the same frozen repository snapshot to
the recursive-strong cohort. For the 2026-08-29 CRAN inventory this means direct
targets `CMTFtoolbox` and `ctsem`, plus recursive-strong-only target `CoTiMA`.

Acceptance requires the exact frozen direct and recursive-strong target sets,
an explicit direct or recursive-strong-only classification for every target,
zero compilation on the warm run, byte-identical warehouse manifests,
completion in a measured and explained time relative to the roughly four-minute
direct reference, and valid typed old/new outcomes for every selected target.

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

Freeze and report both direct and recursive-strong cohorts for the larger
package. Run a small representative subset before attempting the selected full
cohort, and keep cohort policy explicit rather than deriving it from the package
web page. Require one typed result per requested target and reject missing,
duplicate, or unattributed shared failures.

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
Rscript --vanilla -e 'devtools::document()'
air format . --check
Rscript --vanilla -e 'lints <- lintr::lint_package(); print(lints); quit(status = if (length(lints) > 0L) 1L else 0L)'
Rscript --vanilla -e 'testthat::test_local()'
Rscript --vanilla -e 'devtools::check(document = FALSE, error_on = "note")'
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

- `devtools::document()` succeeds and its generated files are current.
- `air format . --check` succeeds and `lintr::lint_package()` reports no lints.
- `testthat::test_local()` passes.
- `devtools::check(document = FALSE, error_on = "note")` reports zero errors,
  warnings, and notes.
- A skipped, unavailable, or interrupted gate command leaves the work chunk
  unfinished.
- Git starts clean on `main` after the initial commit and has no remote.
- No runtime data exists under the repository root.

Inventory acceptance:

- Every source-root artifact is represented exactly once or has an explicit
  unreadable/error record.
- Re-running inventory changes no source-root file metadata or content and
  produces identical normalized inventory content.
- Collision and compatibility reports are deterministic and fixture-tested.

WP1-A validation evidence:

- After reviewer turn 1, `testthat::test_local(filter = "inventory-observe")`
  passes 38 assertions covering source, Linux binary, and ZIP archives; corrupt
  and incomplete archives; repository metadata; deterministic repeats;
  archive-member and tar-link safety; fail-closed unreadable and linked
  directories; and source-tree invariance.
- A read-only smoke test against the smallest known preserved cache observed
  343 package artifacts and 12 `_meta/PACKAGES*` files. All archives were
  readable; 254 artifact rows were `ok` and 89 explicitly reported incomplete
  `Built` platform metadata, with the platform retained from the filename. A
  complete in-memory before/after file manifest, including SHA-256, was
  identical. No `crancache` call was made.
- Reviewer turn 1 requested changes. After the correction, the complete gate
  passes: documentation is current, Air and lintr are clean, all 39 tests pass,
  and `devtools::check()` reports zero errors, warnings, or notes. Reviewer turn
  2 accepted corrected staged target
  `52140be0582cdd756235aa3e6cbb0339c6e2ec97eab23af1be0e8a3a6ccb5529`
  against base `30cbf4d11c60c98d368ead43a2a28c3acae7d3c8`, with no remaining
  material findings.

WP1-B pre-review validation evidence:

- `testthat::test_local(filter = "inventory-observe")` passes 65 assertions,
  including deterministic reuse, append-only content addresses, disjoint path
  validation, linked-directory and linked-file refusal, source mutation
  detection before publication, and collision failure without overwrite.
- A bounded smoke against the smallest preserved cache wrote a 186,701-byte
  inventory with SHA-256
  `a941210089e364e29e0af76b101c7426dba79291b4074a31b348a481aed0b0f7`.
  A second complete observation reused the same path and hash; both returned
  source snapshot SHA-256
  `60e9112af6e2ea42d8bd2b4add09820e8ddb69610f74e8ea0a2141a15c9b4047`.
  The temporary staging tree was removed after the bounded run.
- The complete gate passes after correcting a test fixture that assumed source-
  checkout paths under installed-package checks: documentation is current, Air
  and lintr are clean, all 66 tests pass, and `devtools::check()` reports zero
  errors, warnings, or notes.

End-to-end acceptance:

- Discovery cannot read or write the preserved warehouse.
- The worker can read compatible artifacts but cannot mutate the warehouse.
- A warm small-package pilot launches no compiler, preserves identical cache
  manifests, covers the exact frozen direct and recursive-strong cohorts, and
  yields one classified old/new result pair per selected target.
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
