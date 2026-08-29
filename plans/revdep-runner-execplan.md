# Build reusable reverse-dependency check infrastructure

Type: ExecPlan

Status: Active; WP1 complete; WP2 complete through WP2-F; paused before next chunk

Owner: James Melville

Last updated: 2026-08-29

Next action: Stop and await owner direction before defining the next bounded
Work Package 2 contract chunk.

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
- `R/inventory-report.R` now reads those immutable payloads and produces
  deterministic internal cross-root evidence without selecting artifacts or
  defining public compatibility lanes.
- `R/contracts-artifact.R` now implements the frozen internal version-1
  artifact-identity and binary compatibility-lane records. Their constructors
  and validators perform no filesystem or external-package operation beyond
  SHA-256 calculation over canonical in-memory record keys.
- Work Package 1's observation, immutable per-root inventory, and cross-root
  reporting layers have passed their focused tests, real-cache smokes, complete
  development gates, and bounded independent reviews.
- No existing cache has been copied, linked, merged, or modified by this repo.
- No remote is configured.
- The accumulated plan exceeded its recorded 367-line baseline by more than 50
  percent without adding work packages. The owner confirmed the expanded review
  and reverse-dependency cohort contract on 2026-08-29 before WP1-B began, and
  confirmed the dependency-installability gate before WP2.
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
- The owner has confirmed that dependency preparation must be a semi-automated
  gate before comparison. It must preserve structured per-package outcomes and
  complete raw diagnostics so a human or agent can identify missing operating-
  system libraries without searching undifferentiated `R CMD` output.

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
- [x] (2026-08-29) Add the owner-approved dependency-installability preflight
  and structured preparation-evidence contract to Work Packages 2 through 5.
- [x] (2026-08-29) Work Package 1: Inventory existing artifacts without invoking
  `crancache`.
  - [x] (2026-08-29) WP1-A: Implement read-only artifact and
    repository-metadata observation. Reviewer turn 1 requested safer archive
    reads, fail-closed traversal, and real ZIP/link fixtures. Reviewer turn 2
    accepted the corrected implementation.
  - [x] (2026-08-29) WP1-B: Emit deterministic immutable per-root inventories
    and prove source-root invariance. Implementation, focused tests, a
    real-cache smoke, the complete development gate, and independent review
    pass.
  - [x] (2026-08-29) WP1-C: Report duplicates, collisions, incomplete metadata,
    unreadable archives, and likely compatibility conflicts across roots.
    Implementation, focused tests, the three-root smoke, the complete
    development gate, and independent review pass.
- [ ] Work Package 2: Define manifests, compatibility lanes, and command contracts.
  - [x] (2026-08-29) WP2-A: Freeze and implement artifact-identity and binary
    compatibility-lane records. The initial review found one generic-placeholder
    validation gap; the one correction pass, complete gate, and re-review pass.
  - [x] (2026-08-29) WP2-B: Freeze repository-metadata snapshot identity and
    direct versus recursive-strong reverse-dependency cohort records. The
    initial review found locale-sensitive normalization; the one radix-order
    correction, complete gate, and re-review pass.
  - [x] (2026-08-29) WP2-C: Freeze the stock-runner dependency-universe record,
    including selected cohort policy, root-qualified dependency edges, and
    explicit install or exclusion dispositions. The bounded review found
    permissive dependency-constraint parsing and then a remaining strict-
    whitespace mismatch. After the owner authorized one narrow additional
    correction, the exact gate and fresh read-only review pass.
  - [x] (2026-08-29) WP2-D: Freeze the preparation-result and raw-process-
    evidence record that gates later comparison work. The initial review found
    permissive clock-component validation; the one correction, complete gate,
    and resumed read-only re-review pass.
  - [x] (2026-08-29) WP2-E: Freeze the runtime-root safety record, including
    physically resolved anchor paths, fixed durable descendants, an exact per-
    run root, immutable source-cache roots, access/lifecycle labels, and cleanup
    eligibility. Implementation, complete gate, and read-only review pass.
  - [x] (2026-08-29) WP2-F: Freeze the command-plan and typed-exit contracts
    that later inventory, preparation, comparison, and verification entry
    points must consume. Implementation, complete gate, and read-only review
    pass.
- [ ] Work Package 3: Implement preparation, staged validation, and immutable
  promotion.
- [ ] Work Package 4: Generate exact repository projections and metadata overlays.
- [ ] Work Package 5: Implement the guarded stock-`revdepcheck` adapter.
- [ ] Work Package 6: Reproduce the small-package reference run.
- [ ] Work Package 7: Evaluate a shared installed-library optimization.
- [ ] Work Package 8: Pilot a larger cohort and make the fork/no-fork decision.

## Completed Chunk: WP2-F

Scope decision: Preserve the accepted stock-runner path and every WP1 and WP2
contract already in source. The missing capability is a machine contract that
names the four planned operations, binds each operation to its required frozen
inputs and runtime roots, makes dry-run intent explicit, and gives future CLI
code one unambiguous typed exit vocabulary. No working mechanism is replaced;
this chunk supplies only the contract that later entry points must obey.

Scope: Freeze and implement one internal version-1 command-plan contract and
one internal version-1 command-exit catalog. The exact planned command names
are `revdep-runner inventory`, `revdep-runner prepare`, `revdep-runner compare`,
and `revdep-runner verify`. Every command plan binds a currently valid runtime-
root plan, one physically resolved existing R-executable file locator, an
explicit dry-run flag, the command's fixed write scope, and the frozen exit-
catalog identity. `inventory` has no manifest identity inputs. `prepare` binds
one mutually consistent repository snapshot, reverse-dependency cohort,
dependency universe, and compatibility lane. `compare` and `verify` bind those
same inputs plus one mutually consistent preparation report. Record the
selected cohort policy explicitly for every command that consumes a dependency
universe. Freeze typed states and numeric codes for success, a successful dry-
run plan, invalid invocation, failed preconditions, incomplete inventory,
incomplete preparation, detected comparison changes, incomplete comparison,
failed verification, internal error, and interruption, together with their
operation applicability and failure class.

Non-goals: Do not expose an R API or implement CLI parsing, help text, shell
launchers, subprocess execution, filesystem creation or mutation, cleanup,
mounts, environment variables, inner run-directory layouts, progress output,
logging, result serialization, or signal handling. Do not run the selected R
executable, attest its version or compatibility lane, hash the executable,
infer a default checkout, root, repository, cohort policy, lane, or dry-run
setting, or change accepted path, snapshot, cohort, dependency, preparation,
or artifact contracts. Do not define append-only diagnostic annotations,
system-package hints, comparison-result schemas, or user-authorized exclusion
semantics. Later launch preflight must still revalidate paths, probe the R
executable and lane, and enforce the recorded command contract immediately
before acting.

Exit criteria: Both records have exact versioned fields and deterministic,
locale-independent content identities. Command plans accept only the four
frozen operations and derive, rather than accept, command name and write scope.
All plans contain a resolved absolute forward-slash R-executable file path, one
validated runtime-root identity, normalized `true` or `false` dry-run intent,
and the exact exit-catalog identity. Optional manifest identifiers are `NA`
only where the operation forbids them; every required object validates against
the accepted contract and its related objects. The selected cohort policy
matches the bound dependency universe. The exit catalog fixes `success` and
`plan-ready` at code 0, `invalid-invocation` at 2, `precondition-failed` at 3,
operation findings at codes 20 through 24, `internal-error` at 70, and
`interrupted` at 130, with exact applicability and classification. Constructors
and validators fail closed on missing or extra fields, unsupported operations,
implicit or surplus bindings, missing or changed executable paths, inconsistent
object relationships, denormalized flags or paths, catalog mutation, semantic
mutation, or identity mismatch. Construction and validation perform no
filesystem mutation or subprocess execution.

Validation: Add focused internal tests for the exact command names, write
scopes, required and forbidden bindings for all four operations, direct and
recursive-strong policy retention, physical R-executable alias resolution,
missing or directory executable paths, source-tree invariance, deterministic
repeats, cross-locale identity, the complete typed exit table and code classes,
and structural, semantic, relationship, normalization, current-filesystem, and
identity mutation. Run `testthat::test_local(filter = "contracts-command")`,
the accepted path and preparation contract suites, then the exact repository
completion gate. Freeze a commit and tree containing source, tests, and this
plan, then complete the bounded single-reviewer protocol.

Current state: The clean starting point is accepted WP2-E handoff commit
`3df8e42bf1d09bedf5e91208cb3792ba153f1c85`, tree
`a36ead039f70ee4f2bd3c866d0b5280ddecd387d`, and parent
`ffa54983dce526e5bfdf13885b541b759ef19f0c`; no remote, upstream, or stash is
configured. Accepted internal contracts already provide strict identities and
relationship validators for runtime roots, snapshots, cohorts, dependency
universes, compatibility lanes, and preparation reports. No command or typed
exit contract existed at the start of this chunk.

`R/contracts-command.R` now implements the internal version-1 command-exit
catalog and command-plan records. The catalog fixes eleven typed states, their
codes, classifications, and operation applicability. Command plans derive the
exact name and write scope for each operation, normalize dry-run intent and a
physical existing R-executable file path, bind the current runtime-root plan,
and require exactly the accepted manifest objects assigned to that operation.
Validation repeats all current filesystem and cross-contract checks and binds
the exact exit-catalog identity. No command is parsed or executed and no file
is changed by either constructor.

`tests/testthat/test-contracts-command.R` adds seven named tests with 64
structured expectations. They cover every operation and binding combination,
dry-run and cohort-policy identity, forbidden and missing inputs, physical
executable aliases and current-filesystem revalidation, the complete typed-exit
table, structural and semantic mutation, source-tree invariance, deterministic
repeats, and cross-locale identity. The focused suite passes all 64
expectations. The combined command, path, and preparation suites pass all 219
expectations without failures, warnings, or skips. Air and lintr are clean.
The first complete-gate attempt reached package checking and reported one NOTE
because the new regular-file probe called `file_test()` without its `utils::`
namespace. The source now uses `utils::file_test()` explicitly; the focused
64-expectation suite, Air, lintr, and whitespace checks pass after that narrow
correction. The interrupted check attempt is not completion evidence.

The complete gate rerun passes: documentation generation makes no tracked
generated changes, Air and lintr are clean, all 506 tests pass without warnings
or skips, and package check reports zero errors, warnings, or notes. The
generated-file, build-artifact, whitespace, and diff audits are clean. The
repeated exact gate after this evidence update reports the same 506 passing
tests and zero errors, warnings, or notes.

The frozen implementation target is commit
`86e7fc1b6d12831ab9b839001f7a1f6956f5bf67`, tree
`3dbcb79970a8bf176e49997645882c799ddce2b7`, and parent
`3df8e42bf1d09bedf5e91208cb3792ba153f1c85`. Its artifact set is
`R/contracts-command.R`, `tests/testthat/test-contracts-command.R`, and this
plan. The sole read-only reviewer echoed that exact identity and an unchanged
clean worktree, independently passed all seven tests and 64 focused
expectations, confirmed the parent diff contains exactly the stated artifact
set, and returned `PASS` with no blocking findings or optional suggestions. No
correction pass was needed.

Next action: Run the exact repository completion gate after this acceptance
update, commit the plan handoff, and stop for owner direction.

## Completed Chunk: WP2-E

Scope: Freeze and implement one internal version-1 runtime-root safety record.
Accept an existing R package checkout, existing durable data and runs anchors,
one portable run identifier, and one or more existing immutable source-cache
roots. Physically resolve every existing anchor, normalize source-cache order,
and derive an exact run root plus fixed `warehouse`, `manifests`, and
`repositories` descendants beneath the data root. Publish a deterministic path
table that labels the package checkout as operator-managed external input,
source caches as read-only inputs, the three data descendants as managed
durable outputs, and the exact run root as writable disposable state and the
only cleanup-eligible path. Reject anchor overlap, nested source caches, unsafe
run identifiers, linked or non-directory derived paths, derived paths that
escape their anchors, and any package root without a `DESCRIPTION` file.
Revalidation must repeat the filesystem boundary checks rather than trusting a
previously constructed record.

Non-goals: Do not create, remove, clean, copy, link, mount, or write any file or
directory; define inner preparation or baseline/candidate `HOME`, XDG, temp,
package-cache, download-cache, log, metadata-overlay, or runner-work layouts;
define command names, arguments, dry-run behavior, typed process or command
exit states, annotations, system-package hints, environment variables, or a
public R/CLI API; authorize mutation merely because a path appears in the
record; or change accepted artifact, cohort, dependency, and preparation
contracts. The path record is a precondition and identity, not a substitute
for immediate operation-specific revalidation or filesystem isolation.

Exit criteria: The record has exact versioned fields and a deterministic
content identity. Existing anchors are represented by absolute forward-slash
physical paths; source-cache roots are unique, radix-sorted, and pairwise
non-overlapping. The package, data, runs, and source-cache trees are mutually
disjoint. The package root identifies an R checkout. The run root is exactly
one child named by the validated run identifier beneath the runs anchor, and
the durable output roots are exactly the three fixed children beneath the data
anchor. The normalized path table contains every operational root exactly once
with its frozen access, lifecycle, and cleanup policy; only the exact run root
is cleanup-eligible. Existing derived paths and every existing component on a
derived route are real directories, are not symbolic links, and remain inside
their anchor. Constructors and validators fail closed on missing or extra
fields, malformed anchors or run identifiers, missing paths, file collisions,
overlap or escape, symlink substitution, denormalization, semantic mutation,
or identity mismatch. Construction performs no filesystem mutation.

Validation: Add focused internal temporary-filesystem tests for deterministic
construction and repeated validation; exact schema, roots, path-table roles,
access, lifecycle, and cleanup policy; source-cache input-order and locale
invariance; path expansion and physical symlink resolution at anchors; missing
or non-directory anchors; a non-package checkout; invalid run identifiers;
every anchor-overlap class and nested or duplicate source caches; existing
derived directories; linked derived paths; file collisions; source-tree
invariance; and post-construction structural, semantic, normalization,
filesystem, and identity mutation. Run
`testthat::test_local(filter = "contracts-path")`, the accepted artifact and
preparation contract suites, then the exact repository completion gate. Freeze
a commit and tree containing source, tests, and this plan, then complete the
bounded single-reviewer protocol.

Current state: The clean starting point is accepted WP2-D handoff commit
`5e22ecf4bdb71e4e4827e0e08df4c4523a17535e` and tree
`ed4c349b666f94f5fdad9a79bfb6dd15f0ffcf41`; no remote, upstream, or stash is
configured. `R/contracts-path.R` now implements the internal version-1 runtime-
root plan. It physically resolves existing anchors, requires an R package
checkout, normalizes source-cache roots with radix ordering, rejects every
anchor overlap, derives only the three fixed durable roots and one exact run
root, and publishes access, lifecycle, and cleanup policy in a content-
addressed path table. Construction is read-only; validation rechecks anchors,
derived paths, collisions, and symlink substitution against current filesystem
state. The package checkout remains operator-managed rather than being forced
read-only, preserving the accepted stock-runner mount option, while preserved
source caches remain explicitly read-only inputs.

`tests/testthat/test-contracts-path.R` adds ten named tests with 58 structured
expectations. They cover exact schema and policy fields, source-tree
invariance, deterministic repeats, input-order and cross-locale identity,
physical anchor alias resolution, malformed anchors and run IDs, every pairwise
anchor boundary, nested and duplicate caches, existing safe descendants, file
and symlink collisions, filesystem change detection, structural and semantic
mutation, and identity separation. The focused suite passes all 58
expectations; the anchored path, artifact, and preparation suites pass all 200
expectations with no failures, warnings, or skips. Air and lintr are clean.

The final repeated exact completion gate passes. Documentation generation makes
no tracked generated changes, Air and lintr are clean, all 442 tests pass
without warnings or skips, and package check reports zero errors, warnings, or
notes. Generated-file, build-artifact, whitespace, and diff audits are clean.

The accepted implementation target is commit
`ffa54983dce526e5bfdf13885b541b759ef19f0c`, tree
`9622864d4b0fb8d52b892b91b81650a58208d4da`, and parent
`5e22ecf4bdb71e4e4827e0e08df4c4523a17535e`. Its artifact set is
`R/contracts-path.R`, `tests/testthat/test-contracts-path.R`, and this plan. The
sole read-only reviewer echoed that identity and unchanged clean worktree,
independently passed all 58 focused expectations, confirmed physical alias
normalization for every anchor class and fail-closed dangling derived links,
and returned `PASS` with no blocking findings or optional suggestions.

Next action: Stop and await owner direction before defining the next bounded
Work Package 2 contract chunk.

## Completed Chunk: WP2-D

Scope: Freeze and implement one internal version-1 preparation-report machine
contract. Bind it to one accepted repository snapshot, reverse-dependency
cohort, dependency universe, and compatibility lane. Derive its target and
closure requirements from that universe rather than accepting a second
caller-authored dependency set. Record normalized source identities and source
URLs, validated source and prepared artifact identities, complete content-
addressed process-attempt evidence, and exactly one current result for every
unique package that preparation must resolve. The process evidence records the
stage, exact display command, UTC start time, elapsed milliseconds, exit
status, separate relative stdout/stderr artifact paths and SHA-256 values, and
a bounded diagnostic excerpt. Result states are `prepared`, `ready`,
`unavailable`, `missing-system-requirements`, `compilation-failure`,
`installation-failure`, `namespace-load-failure`, `timeout`, `blocked`, and
`not_checked`. The complete record and every process attempt are independently
content-addressed version-1 contracts.

Non-goals: Do not access the filesystem or network; resolve, download, build,
install, load, retry, stage, promote, or execute a command; inspect or infer
system packages; generate apt, Homebrew, or other remediation commands; define
the append-only human/agent annotation schema; define general data-root,
run-root, or command-line path policy; expose a public R API or CLI; or change
the accepted cohort and dependency-universe contracts. A relative raw-log
artifact locator is evidence metadata only and does not select or authorize a
runtime root. Do not materialize an exponential dependency-path table: the
report binds the accepted root-qualified dependency graph and derives compact
target/package requirement rows from it.

Exit criteria: The report has exact versioned fields and a deterministic
locale-independent identity. Its requirements contain every selected target
and every `install` or `unavailable` closure package, preserve target/closure
roles and roots, and exclude base and runner-supplied packages. Results contain
each unique required package exactly once, with a single consistent selected
version or explicit unavailable version. Every available result has one source
record whose source artifact identity matches its package, version, and
checksum. Every artifact record validates against the frozen artifact contract;
binary artifacts belong to the report lane. Every attempted process has two
complete raw-log references and hashes, a valid stage/outcome/exit-status
combination, and at most 4,096 UTF-8 bytes of diagnostic excerpt. Failure and
readiness results reference semantically compatible process evidence;
`prepared` and `ready` identify an artifact, `blocked` names another required
package with a non-success result, and `unavailable`, `blocked`, and
`not_checked` cannot masquerade as attempted failures. Constructors and
validators fail closed on missing or extra fields, malformed locators, hashes,
timestamps, durations, statuses or outcomes, inconsistent cross-record
references, denormalization, semantic mutation, or identity mismatch. Empty
selected cohorts produce a valid empty report.

Validation: Add focused internal synthetic tests for pure-R and compiled source
records; repository and archive URLs; reused and newly prepared artifacts;
successful namespace-load readiness; all required typed failure, timeout,
blocked, and owner-exclusion states; separate complete stdout/stderr evidence;
bounded multiline diagnostic text; multi-target shared closures; unavailable
dependencies; an empty cohort; input-order and locale invariance; and
post-construction structural, normalization, relationship, semantic, and
identity mutation. Run
`testthat::test_local(filter = "contracts-preparation")`, the accepted
dependency, cohort, and artifact contract suites, then the exact repository
completion gate. Freeze a commit and tree containing source, tests, and this
plan, then complete the bounded single-reviewer protocol.

Current state: The clean starting point is accepted WP2-C handoff commit
`5478feccf7a7076bdb15efc3d7d2e2d5d657e5c4` and tree
`5abacd43a10444efba5edb1ae1c278199800a978`. No remote, upstream, or stash is
configured. The accepted contracts already provide strict content identities
for artifacts, compatibility lanes, repository snapshots, cohorts, and the
stock-runner dependency universe. `R/contracts-preparation.R` now implements
the version-1 preparation-attempt and preparation-report contracts. Attempts
bind process stage, display command, UTC timing, normalized exit status,
separate relative stdout/stderr artifact locators and hashes, and a bounded
multiline diagnostic excerpt to an independent SHA-256 identity. Reports bind
the accepted snapshot, cohort, universe, and lane; derive compact target and
closure requirements from the accepted graph; validate source and binary
artifact identities; retain repository or archive source provenance,
compilation requirements, and declared system requirements; and enforce one
typed result per unique required package. Blocked results must cite a failing
required package reachable within one root-qualified dependency graph, and
blocking chains cannot cycle. All constructors remain internal and perform no
filesystem, network, process, package-installation, annotation, or command-line
operation.

`tests/testthat/test-contracts-preparation.R` adds nine named tests with 92
structured expectations. They cover independent attempt identities and raw-log
references; malformed attempt evidence; multi-target shared requirements;
repository and archive sources; pure-R and compiled packages; multiline system
requirements and diagnostic excerpts; input-order and cross-locale stability;
all ten result outcomes; reused and newly prepared binary evidence; unavailable
and transitively blocked packages; invalid cross-record references and blocker
ancestry; structural, semantic, normalization, and identity mutation; and a
valid empty cohort. The exact focused suite passes all 92 expectations, and the
anchored preparation, dependency, cohort, and artifact suite passes all 284
expectations, with no failures, warnings, skips, or errors. Air and lintr are
clean. The first complete gate before the final test additions passed all 375
then-current tests and package check reported zero errors, warnings, or notes.
The exact gate against the final 379-test implementation also passes:
documentation generation makes no tracked generated changes, Air and lintr are
clean, all tests pass without warnings or skips, and package check reports zero
errors, warnings, or notes. Generated-file, build-artifact, whitespace, and
diff audits are clean. The exact gate will be repeated once after this evidence
update so the frozen target itself has complete validation.

Initial review action: Repeat the exact completion gate, freeze the
implementation commit and tree, and send the self-contained packet to the sole
read-only reviewer.

The initial frozen target is commit
`7241f26363669c0c89d245308f75bf8574ba1099`, tree
`d9f2fdf2dcf25097d0022a0d29c9d16dd8348ed7`, and parent
`5478feccf7a7076bdb15efc3d7d2e2d5d657e5c4`. The sole reviewer echoed that
identity and the unchanged clean worktree, independently passed all 92 focused
expectations, and returned `NEEDS_CHANGES` with no optional suggestions. Its
one blocking finding reproduced that the timestamp shape check plus
`as.POSIXct()` accepts the impossible seconds component in
`2026-08-29T12:00:99Z`, violating the fail-closed malformed-timestamp exit
criterion. The coordinator reproduced the same constructor and validator
acceptance against the frozen target. The one allowed correction will constrain
hour, minute, and second components to portable clock ranges before parsing and
add boundary regressions including seconds `99`; no other contract behavior is
reopened.

Next action: Apply the one timestamp correction, rerun the focused and exact
completion gates, freeze the corrected target, and return it to the same sole
reviewer for the one allowed re-review.

The one correction constrains hours to `00` through `23` and minutes and seconds
to `00` through `59` before the existing calendar-date parse. Focused coverage
now rejects hour `24`, minute `60`, leap-second-like `60`, and the reviewer's
seconds `99` input while preserving a valid nine-digit fractional timestamp.
The focused suite passes all 97 expectations, and the corrected complete suite
passes all 384 tests without warnings or skips. The first corrected exact gate
passes: documentation generation makes no tracked changes, Air and lintr are
clean, and package check reports zero errors, warnings, or notes. The exact gate
will be repeated after this evidence update before the corrected target is
frozen.

The corrected implementation target is frozen at commit
`616d4008fa6cab86a930adedde9c84dcb2a18007`, tree
`19e9810e0f34f6618d4d5a70d087dc4d099ce8fe`, and parent
`7241f26363669c0c89d245308f75bf8574ba1099`. The exact completion gate against
that target passes: documentation generation makes no tracked changes, Air and
lintr are clean, all 384 tests pass without warnings or skips, and package
check reports zero errors, warnings, or notes. Generated-file, build-artifact,
whitespace, and diff audits are clean.

A context compaction occurred after the initial review began and before a
reviewer returned `PASS`, after the correction was frozen but before the
re-review was requested. Per the owner-approved review protocol, the live
review cycle ended immediately and no acceptance is inferred. The outstanding
re-review has not been sent.

Compaction handoff action: Stop for the owner's go-ahead. After approval,
re-read the repository instructions and planning workflow, reconcile this plan
with the worktree, refresh the exact target identity, and send the corrected
packet to the same sole reviewer for the one allowed read-only re-review.

On the owner's go-ahead, the coordinator reconciled the durable handoff with a
clean worktree and refreshed the corrected review target to commit
`a3b07e060b9ab4f0b6a5a11462a4c11a542edfd7`, tree
`705aa92867840fd5ef74925347a9e2f84bcdaec0`, and parent
`616d4008fa6cab86a930adedde9c84dcb2a18007`. Relative to the accepted WP2-C
base, its artifact set is `R/contracts-preparation.R`,
`tests/testthat/test-contracts-preparation.R`, and this plan. The focused suite
again passed all 97 expectations, and the original seconds-`99` example failed
closed in a direct probe.

The same sole read-only reviewer echoed the refreshed identity and artifact
set, confirmed the worktree remained clean, independently passed all 97
focused expectations, and returned `PASS` with no blocking findings or optional
suggestions. Its boundary probes rejected hour `24`, minute `60`, seconds `60`
and `99`, and an invalid calendar date while accepting and independently
validating a nine-digit fractional timestamp.

Next action: Stop and await owner direction before defining the next bounded
Work Package 2 contract chunk.

## Completed Chunk: WP2-C

Scope: Freeze and implement one internal version-1 stock-runner dependency-
universe machine contract. It binds a validated repository snapshot and
reverse-dependency cohort to an explicit `direct` or `recursive-strong` target
policy, the exact first-level `Depends`, `Imports`, `LinkingTo`, and `Suggests`
fields, the recursive `Depends`, `Imports`, and `LinkingTo` fields, the package
under test as runner-supplied, and an explicit normalized base-package set.
For every selected target, retain the complete root-qualified reachable
dependency edge graph with relationship labels; this compact graph preserves
multiple-parent routes and can reconstruct dependency paths without storing an
exponential list of path strings. Record every reached dependency once per
target with its selected snapshot version when installed from a repository and
one disposition: `install`, `unavailable`, `runner-supplied`, `base`, or
`target-supplied`. Keep the constructor and validator internal, but treat the
record fields, disposition vocabulary, and content identity as a frozen
version-1 machine contract.

Non-goals: Do not resolve source URLs or checksums; define artifact-selection,
preparation-result, raw-log, annotation, system-requirement interpretation,
resolved-path, command, or exit-state schemas; expose a public R API or CLI; or
perform filesystem, network, repository, `crancache`, `revdepcheck`, download,
install, build, namespace-load, staging, or promotion operations. Do not choose
one cohort policy as the release default. Do not enumerate materialized path
strings when the root-qualified edge graph preserves the same dependency
relationships without combinatorial growth.

Exit criteria: `direct` selects only direct cohort targets and `recursive-
strong` selects both direct and recursive-strong-only targets. For each target,
the first expansion uses all four stock-runner fields and every later expansion
uses only the three hard fields. The normalized edge graph retains every
reachable field-labelled relationship, including shared dependencies reached
through multiple parents, while terminating safely on cycles. Repository
priority selects the first snapshot row for cross-repository duplicates.
Per-target dependency rows explicitly identify packages the stock runner would
install, silently drop as unavailable, obtain from the package-under-test
checkout, obtain from R's base installation, or supply separately as that
target. Reached non-base names expand only when the snapshot contains selected
metadata; absent names remain explicit unavailable leaves, while a runner-
supplied package can contribute hard dependencies when its snapshot row exists.
The explicit base-package set, cohort policy, selected targets, query fields,
edges, dispositions, selected versions, snapshot identity, and cohort identity
are all content-bound with locale-independent normalization. Empty cohorts and
empty dependency sets are valid. Constructors and validators fail closed on
unsupported policy, malformed base-package input or dependency syntax,
incompatible snapshot/cohort pairs, missing or extra record fields, semantic
mutation, denormalization, or identity mismatch.

Validation: Add focused internal synthetic tests for both cohort policies,
first-level Suggests versus recursive hard-only expansion, shared and cyclic
paths, base/R and target exclusions, runner-supplied expansion, unavailable
direct and recursive dependencies, repository-priority version selection,
empty results, deterministic locale-independent identities, and post-
construction structural, semantic, normalization, and identity mutation. Use
an independent fixture calculation to compare the `install` disposition with
the locally observed stock `revdepcheck` 1.0.0.9002 closure operations after
the frozen snapshot has selected one row per package. Run
`testthat::test_local(filter = "contracts-dependency")`, the accepted cohort
and artifact contract suites, then the exact repository completion gate. Freeze
a commit and tree containing source, tests, and this plan, and complete the
bounded single-reviewer protocol.

Current state: The clean starting point was accepted WP2-B handoff commit
`f95f2a003c8f5ea2f3a535cf09fcc017080d8acc` and tree
`09c70b37f1a948bc46f9190d05b2a4a1af9fcadd`; no remote, upstream, or stash is
configured. `R/contracts-dependency.R` now implements the frozen internal
dependency-universe constructor and validator. It binds the snapshot, cohort,
selected policy, exact field vectors, explicit sorted base-package set, package
under test, selected targets, per-target dependency dispositions, and complete
root-qualified edges to one SHA-256 identity. Traversal expands one selected
snapshot row per package, retains base and unavailable leaves as evidence,
expands snapshot-present runner-supplied packages, and terminates cycles through
an explicit visited set. Every identity-affecting character ordering uses radix
ordering. No public API, external I/O, source resolution, preparation result,
path, command, or execution behavior was added.

The focused suite passes 69 assertions covering both cohort policies, direct
Suggests and recursive hard-only behavior, shared routes, cycles, all five
dispositions, runner-supplied expansion, direct and recursive unavailable
leaves, repository-priority metadata and version selection, empty cohorts,
strict input and mutation rejection, normalized identity inputs, cross-locale
identity and validation, and install-set parity with an independent translation
of the observed stock closure operations. The accepted cohort and artifact
suites still pass 70 and 45 assertions. The first complete gate passes with all
279 tests, no warnings or skips, and zero errors, warnings, or notes from
`devtools::check(document = FALSE, error_on = "note")`; documentation generation
made no tracked changes. The exact gate will be repeated after this plan update
before the target is frozen.

The sole reviewer echoed initial commit
`c4b2ed0cda7ef09fa5e5e35ea31f5ddcc8d11d71`, tree
`5521cd026a5f7f6cbfe06dafd90ae5a21afbc2dd`, and parent
`f95f2a003c8f5ea2f3a535cf09fcc017080d8acc`; confirmed a clean worktree; and
returned `NEEDS_CHANGES`. Its one blocking finding reproduced that stripping
parenthesized text before grammar validation accepted an invalid constraint
body, repeated constraints, and trailing text, including silently turning the
latter into a different unavailable package name. This violates the frozen
fail-closed malformed-syntax criterion. It made no optional suggestions.

The one allowed correction validates each complete comma-separated entry as a
package name or `R`, optionally followed by exactly one parenthesized recognized
R dependency operator and valid package version, before extracting the package
name. It also accepts R's `rNNN` revision form only for `R` and adds valid-
operator coverage plus constructor regressions for an invalid operator/version,
repeated constraints, and trailing text. The corrected focused suite passes 75
assertions, and direct probes reject all three reviewer examples. The first
corrected complete gate passes with all 285 tests, no warnings or skips, and zero
errors, warnings, or notes from package check. It will be repeated after this
evidence is recorded so the corrected frozen target itself has complete
validation.

The corrected target is commit
`ce9013b9f738eddb8f6600c541b66ea555067abc`, tree
`ccdb659285137d0d1a01c53cd3a4b8be67a62e04`, and parent
`c4b2ed0cda7ef09fa5e5e35ea31f5ddcc8d11d71`. The same reviewer confirmed that
identity and a clean worktree, confirmed the three original examples now fail,
and returned `NEEDS_CHANGES` because the grammar still accepts whitespace
immediately after `(` and before `)`, as explicitly exercised by
`OpLE ( <= 2.0 )`. A read-only probe of R 4.5.2's strict
`tools:::.check_package_description()` accepted `OpLE (<= 2.0)` and rejected the
inner-whitespace form as a bad dependency entry, confirming the finding against
the frozen fail-closed criterion. The reviewer made no optional suggestions.

On 2026-08-29, the owner explicitly authorized one additional narrow
correction and fresh review. A read-only API audit found that
`usethis::use_package()` manages dependencies in this package's own
`DESCRIPTION` through `desc`, while the installed `desc` dependency parser
accepts the malformed constraint body, repeated constraint, trailing text, and
inner-whitespace examples that this frozen repository-metadata contract must
reject. Adopting either package would therefore not satisfy the exit criterion
and would expand the approved correction into a dependency/API change.

The owner-authorized correction removes only the two inner-whitespace
allowances, changes the valid fixture to `OpLE (<= 2.0)`, and adds separate
rejection coverage for whitespace immediately after `(` and before `)`. The
focused dependency-contract suite passes 77 assertions with no failures,
warnings, or skips. The first exact completion gate after this correction also
passes: documentation generation made no tracked changes, Air and lintr are
clean, all 287 tests pass with no warnings or skips, and package check reports
zero errors, warnings, or notes. Generated-file, build-artifact, whitespace,
and diff audits are clean.

The accepted target is commit
`51d6943eea244731c43c5f2e2315e9fc3c4eeba2`, tree
`6d9646379dc62287c8bca13a821314f37e572379`, and parent
`222de4a86871b70595509dde442624f4d9bb8a8b`. The sole read-only reviewer echoed
that identity and the unchanged clean worktree, independently passed all 77
focused dependency assertions plus the 70 cohort and 45 artifact assertions,
and returned `PASS` with no blocking findings or optional suggestions.

Next action: Stop and await owner direction before defining or starting the
next Work Package 2 contract chunk.

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
- In locally installed `revdepcheck` 1.0.0.9002, `cran_deps()` first includes a
  selected target's `Depends`, `Imports`, `LinkingTo`, and `Suggests`, then
  recursively expands only `Depends`, `Imports`, and `LinkingTo`. Its install
  path intersects that result with the available-package inventory, silently
  dropping unavailable names. The runner contract must reproduce the intended
  dependency universe while reporting every unavailable package explicitly.
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
- A complete three-root WP1-C smoke produced 1,145 duplicate-hash member rows,
  2,246 package/version collision member rows, 1,874 unreadable or incomplete
  member rows, and 622 likely compatibility-conflict member rows. Reversing the
  three inventory arguments produced an identical report, and before/after
  hashes of all temporary immutable inventory inputs were identical.
- CRAN package pages expose direct reverse relationships but do not identify
  transitive consumers. With an unfiltered CRAN package database on 2026-08-29,
  `tools::package_dependencies("mize", which = "most", recursive = FALSE,
  reverse = TRUE)` returned `CMTFtoolbox` and `ctsem`, while
  `recursive = "strong"` also returned `CoTiMA` through `ctsem`.
- `available.packages(filters = list())` retains cross-repository duplicate
  package rows. `tools::package_dependencies()` keeps the first row for a
  package, so repository priority must determine normalized row order before a
  snapshot can be both presentation-invariant and runner-equivalent.
- Stock `revdepcheck` 1.0.0.9002 asks `crancache::available_packages()` for its
  live forward-dependency database with the default filters, which currently
  include R-version, OS, subarchitecture, and highest-version duplicate
  removal. WP2-C instead applies the same closure field rules after the frozen
  unfiltered snapshot has selected one row per package by repository priority.
  The later stock adapter must therefore expose and verify an unambiguous
  one-row-per-package projection rather than assume a live filtered query is
  identical to the frozen snapshot.

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

Every compaction handoff that interrupts a live review must also summarize the
review verdict and each finding, state whether the coordinator confirmed or
disputed it against the frozen contract, and identify the exact remediation in
progress plus the validation or re-review still outstanding. This summary is
diagnostic context for the owner and never implies acceptance.

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

- Decision: Keep WP1-C reports internal and row-level, and classify a likely
  binary compatibility conflict only when observed R major/minor or platform
  values disagree between roots for the same package/version.
  Rationale: Returning every qualifying member preserves evidence for WP2's
  eventual schema and winner policy, while treating missing metadata or
  source-versus-binary differences as conflicts would claim compatibility facts
  the current inventory cannot establish.
  Date/Author: 2026-08-29 / Codex

- Decision: Require a resumable dependency-installability preflight and durable
  preparation report before comparison workers may start.
  Rationale: Missing operating-system libraries and other common dependency
  failures are external preparation failures, not evidence about the package
  under test. Structured outcomes, bounded excerpts, and complete raw logs make
  the failures practical for a human or agent to interpret without authorizing
  the tool to change the host system.
  Date/Author: 2026-08-29 / Codex

- Decision: Make version-1 artifact and binary-lane identities strict internal
  records before exposing any R or command-line API.
  Rationale: Later manifests need deterministic join keys now, but no direct
  user task yet justifies supporting constructors as public R API. Explicit
  compatibility tags fail closed when archive metadata cannot establish ABI or
  toolchain equivalence.
  Date/Author: 2026-08-29 / Codex

- Decision: Retain cross-repository duplicate package rows in configured
  repository priority order, while rejecting duplicates within one repository.
  Rationale: An unfiltered multi-repository package database can legitimately
  contain the same package more than once, and the exact dependency query
  chooses the first row. Preserving that order records the real selection rule
  without making incidental input row order part of the snapshot identity.
  Date/Author: 2026-08-29 / Codex

- Decision: Keep WP2-C's complete-entry dependency grammar validation local
  instead of adopting `usethis` or `desc` for the owner-authorized correction.
  Rationale: `usethis::use_package()` edits the current project's
  `DESCRIPTION`, while `desc` parses but does not strictly validate the
  repository dependency entries that WP2-C must reject. Neither helper owns
  this input boundary, and adopting one would not remove the local validation.
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

Dependency preparation must derive the exact package universe that the chosen
runner path will request. For the initial stock adapter, record each selected
target's direct `Depends`, `Imports`, `LinkingTo`, and `Suggests`, followed by
the recursive `Depends`, `Imports`, and `LinkingTo` closure, with every
dependency path retained. Never silently remove unavailable packages.

The preparation output must combine stable machine-readable records with
complete per-package process logs. At minimum, record package and version,
target or closure role, dependency paths, source URL and checksum, artifact
identity, compilation requirement, declared `SystemRequirements`, command and
timing, exit status, typed outcome, blocking dependency, a bounded verbatim
diagnostic excerpt, and the path and checksum of the complete captured output.
Suggested package-manager commands are advisory derived views with provenance,
not executable instructions or proof that a diagnosis is correct. Store agent
or human interpretations as append-only annotations keyed to the immutable raw-
log checksum rather than changing the captured evidence.

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
Define a versioned preparation-result schema with typed states that distinguish
ready packages, unavailable packages, missing system requirements, compilation
failures, installation failures, namespace-load failures, timeouts, and
packages blocked by an earlier dependency failure. Preserve raw stdout and
stderr even when a normalized classification or suggested apt, Homebrew, or
equivalent command is available. Add small synthetic fixtures covering
duplicates, collisions, pure-R packages, compiled packages, corrupt archives,
a transitive reverse dependency, a missing system library, and a dependent
blocked by that failure.

### Work Package 3: Preparation, staging, and immutable promotion

Create a new warehouse root without changing source caches. Select compatible
artifacts from inventories, copy or hard-link them into staging only after the
link/copy policy is explicitly selected, validate identity and hashes, then
promote atomically. Never overwrite a different hash at an existing identity.

Build missing artifacts into a separate writable build cache. Validate each
result before promotion. A failed or interrupted build must leave the durable
warehouse unchanged and be resumable from already validated artifacts.

Prepare the frozen dependency universe in dependency order with bounded per-
package processes. Reuse compatible validated artifacts, but attempt every
unresolved build separately and capture its complete output. When a package
fails, classify packages that depend on it as blocked rather than repeating a
cascade of misleading install errors. Inventory declared system requirements
and derive best-effort hints from them and from captured diagnostics, but do
not install operating-system packages or execute suggested commands.

Publish the versioned preparation report at the human-judgment boundary. Any
unavailable, failed, timed-out, or blocked package stops promotion of an
apparently complete manifest and downstream comparison work unless the owner
explicitly records a bounded exclusion. After external remediation, resume from
the exact failed and blocked nodes without rebuilding or redownloading artifacts
already validated for the same snapshot and lane.

### Work Package 4: Repository projections

Generate an exact, read-only repository view for one manifest and compatibility
lane. It must contain no ambiguous package/version selection. Generate and
validate `PACKAGES` metadata in staging, then publish the complete projection.

Keep mutable `_meta/` state outside the warehouse and projection. Prove that a
projection can satisfy installation in a clean disposable library without
compiler invocation for every artifact expected to be binary-backed.

Materialize every package in the frozen preparation universe into an isolated
library through the exact repository projection, record an install result for
each package, verify its selected version, and load every non-base namespace in
a separate process. Merge these results into the preparation report, retaining
complete logs and distinguishing a root failure from packages blocked by it.

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

Before stock-runner initialization or worker launch, require the exact cohort
and preparation-manifest identities and a ready result for every requested
package. Probe the stock runner's real private-library path and prove that it
consumes the validated projection rather than silently redownloading or
dropping dependencies. An owner-approved exclusion remains a typed
`not_checked` preparation outcome and can never become an old/new comparison
result.

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

WP1-B validation and review evidence:

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
- The sole read-only reviewer echoed commit
  `8ba89ee7e30db9e64bd13b475cffeda3c156712b` and tree
  `e117576c69257a823796d1931d14b5faab3f00e8`, independently reran the focused
  suite with 65 of 65 assertions passing, and returned `PASS` with no blocking
  findings or optional suggestions. A non-target parent-hash typo in the packet
  was corrected to `3555350ccd4197e03b245edb15b4315d4f4d67c4`; the frozen
  commit and tree never changed.

WP1-C validation evidence:

- `testthat::test_local(filter = "inventory-report")` passes 29 assertions
  covering complete row membership, input-order invariance, source and
  inventory invariance, duplicate hashes, package/version hash collisions,
  unreadable and incomplete artifacts, R-major/minor and platform conflicts,
  missing compatibility dimensions, content-address failures, incompatible
  structures, duplicate cache roots, concurrent input mutation, linked inputs,
  and empty inventories.
- A bounded smoke generated temporary immutable inventories for all three known
  preserved roots. Their inventory/source SHA-256 pairs were
  `949df23c21cae0b0828b750e995d9500a4607425317276e69280fcc634474b5b` /
  `46a193f53cacb49097b4e94b66e185098397325b676482c41caa53c6db1d3303`,
  `7fe13f2b403b5f7d01bb84347b54206c7b8a10f3cb76119ce43412eed6ea1752` /
  `0f794074b6d16e5aa196eff86a36f06b58ea64c65ade16831e5f65606b95c219`,
  and `a941210089e364e29e0af76b101c7426dba79291b4074a31b348a481aed0b0f7` /
  `60e9112af6e2ea42d8bd2b4add09820e8ddb69610f74e8ea0a2141a15c9b4047`.
  The report was identical with reversed inputs, every input inventory hash was
  unchanged, no `crancache` call occurred, and temporary staging was removed.
- The complete gate passes: documentation is current, Air and lintr are clean,
  all 95 tests pass, and `devtools::check(document = FALSE, error_on = "note")`
  reports zero errors, warnings, and notes.
- The sole read-only reviewer echoed commit
  `2e943e0b6391a5518bfb92f5a134895c4a2ed787`, tree
  `a82eba65664825f7af92d6b3b6c7cb2939955108`, and parent
  `f120c61d1f9da526c19a0e8cfb9157d7a3ae161b`; independently reran the focused
  suite with 29 of 29 assertions passing; and returned `PASS` with no blocking
  findings or optional suggestions. The worktree remained clean and the review
  target did not change.

WP2-A review and correction evidence:

- Initial frozen commit `4d4d2ac76c1ca22f4532de0caa974bb4f5e1ab7f`
  and tree `bd4fcfbde0725a8a66523f71cce39fd6ffedb9f1` passed the complete
  gate. The sole reviewer echoed that identity and returned `NEEDS_CHANGES`
  because `r_platform` and `architecture` accepted generic compatibility
  placeholders even though the frozen chunk prohibited fallback values.
- The one correction pass applies the specific-compatibility-tag validator to
  both fields and exercises `unknown`, `unspecified`, and `default` for each.
- `testthat::test_local(filter = "contracts-artifact")` now passes 45 assertions
  covering exact versioned field sets, source and binary lane rules, every lane
  dimension, identity separation, strict scalar and token validation, canonical
  key stability, and structural or identity mutation detection.
- `testthat::test_local(filter = "inventory-report")` still passes all 29
  assertions after renaming the private package/version grouping helper.
- The corrected complete gate passes: documentation is current, Air and lintr
  are clean, all 140 tests pass with no warnings or skips, and
  `devtools::check(document = FALSE, error_on = "note")` reports zero errors,
  warnings, and notes.
- The same sole reviewer echoed corrected commit
  `965e0d3227ac79cc31dde1f70cd1cbe51ad459b9`, tree
  `06908c70c5c5276c5b7a95c03e054951b3319993`, and parent
  `4d4d2ac76c1ca22f4532de0caa974bb4f5e1ab7f`; independently reran both
  focused suites, verified constructor and independently rehashed validator
  rejection for generic platform and architecture placeholders, and returned
  `PASS` with no blocking findings or optional suggestions.

WP2-B implementation validation evidence:

- `testthat::test_local(filter = "contracts-cohort")` passes 54 assertions
  covering normalized and content-addressed repository snapshots, explicit
  `filters = list()` policy, repository priority, cross-repository duplicate
  selection, malformed or ambiguous inputs, exact direct and recursive-strong
  `tools::package_dependencies()` results, transitive-only classification,
  empty cohorts, and structural, semantic, or identity mutation detection.
- `testthat::test_local(filter = "contracts-artifact")` still passes all 45
  assertions after the new contracts reused WP2-A's canonical identity and
  validation primitives.
- The first complete gate passes: documentation is current, Air and lintr are
  clean, all 194 tests pass with no warnings or skips, and
  `devtools::check(document = FALSE, error_on = "note")` reports zero errors,
  warnings, and notes. The gate will be repeated after this evidence is recorded
  so the frozen review target itself has complete validation.
- The sole reviewer echoed initial commit
  `13736dbcf393f633370e6462de88a298f7118c4c`, tree
  `9672332170a0f514de5333684025761dc0a35f00`, and parent
  `d77e87885bb73d2ce858e86ac54317bac6b80e58`, confirmed a clean
  worktree, and returned `NEEDS_CHANGES`. Its one blocking finding reproduced
  different snapshot identities under `C` and `C.UTF-8` because four
  normalization orderings were locale-sensitive; it made no optional
  suggestions.
- The one correction pass gives package rows, metadata columns, final targets,
  and normalized query results explicit radix ordering. A mixed-case fixture
  constructs byte-identical snapshots in both available collations and
  validates the first snapshot after every locale switch. The corrected focused
  suite passes 70 assertions, the complete suite passes all 210 assertions with
  no warnings or skips, and the first corrected complete gate again reports no
  formatting failures, lints, errors, warnings, or notes. The gate will be
  repeated after this evidence is recorded before the corrected target is
  frozen.
- The same sole reviewer echoed corrected commit
  `86e66921e2dc030bd143ca41e65de7497a52b53b`, tree
  `a1f23d6cfd8b1213f1dfa5406482e7aec758ab08`, and parent
  `13736dbcf393f633370e6462de88a298f7118c4c`; confirmed the
  worktree stayed clean; independently reproduced stable mixed-case snapshots,
  cohorts, and cross-locale validation; and returned `PASS` with no blocking
  findings. Its single optional portability suggestion did not change the
  accepted target.

WP2-C implementation validation evidence:

- `testthat::test_local(filter = "contracts-dependency")` passes 69 assertions
  covering both target policies, exact expansion fields, complete root-qualified
  edges, multiple-parent routes, cycle termination, every disposition, explicit
  unavailable leaves, runner-supplied expansion, repository-priority selection,
  strict validation, empty cohorts, and deterministic cross-locale identity.
- For every selected direct fixture target, the packages marked `install` equal
  an independent translation of `revdepcheck` 1.0.0.9002's `cran_deps()` and
  `deps_opts()` closure operations after resolving one snapshot row per package.
  Suggested packages receive hard recursive expansion, while recursive Suggests
  are absent as required.
- The accepted cohort and artifact contract suites still pass 70 and 45
  assertions. The first complete gate passes: documentation is current, Air and
  lintr are clean, all 279 tests pass with no warnings or skips, and
  `devtools::check(document = FALSE, error_on = "note")` reports zero errors,
  warnings, and notes. The gate will be repeated after this evidence is recorded
  so the frozen review target itself has complete validation.
- The sole reviewer echoed initial commit
  `c4b2ed0cda7ef09fa5e5e35ea31f5ddcc8d11d71`, tree
  `5521cd026a5f7f6cbfe06dafd90ae5a21afbc2dd`, and parent
  `f95f2a003c8f5ea2f3a535cf09fcc017080d8acc`; confirmed the
  worktree stayed clean; and returned `NEEDS_CHANGES` because dependency text
  was stripped before its complete constraint grammar was validated. It
  independently reproduced acceptance of an invalid version body, repeated
  constraints, and trailing text that became a different unavailable name.
  There were no optional suggestions.
- The one correction pass validates the complete entry before name extraction,
  with the operators accepted by R's dependency-field checker, valid package
  versions, and R's `rNNN` revision form only for `R`. The focused suite now
  passes 75 assertions and direct probes reject all three reviewer examples.
  The first corrected complete gate passes: documentation is current, Air and
  lintr are clean, all 285 tests pass with no warnings or skips, and package
  check reports zero errors, warnings, and notes. The gate will be repeated
  after this evidence is recorded; corrected re-review remains.
- The corrected frozen target is commit
  `ce9013b9f738eddb8f6600c541b66ea555067abc`, tree
  `ccdb659285137d0d1a01c53cd3a4b8be67a62e04`, and parent
  `c4b2ed0cda7ef09fa5e5e35ea31f5ddcc8d11d71`. Its final complete gate passes
  all 285 tests with no warnings or skips and zero package-check diagnostics.
  The same reviewer confirmed the identity and original fix but returned
  `NEEDS_CHANGES`: inner-parenthesis whitespace remains accepted by the new
  parser even though R 4.5.2's strict DESCRIPTION checker rejects it. An
  independent read-only strict-checker probe confirmed that exact mismatch. The
  bounded loop is exhausted, so no further correction is authorized without
  owner direction.
- The owner authorized one additional narrow correction. The accepted target is
  commit `51d6943eea244731c43c5f2e2315e9fc3c4eeba2`, tree
  `6d9646379dc62287c8bca13a821314f37e572379`, and parent
  `222de4a86871b70595509dde442624f4d9bb8a8b`. Its focused suite passes 77
  assertions; its final exact gate passes all 287 tests without warnings or
  skips and reports no formatting failures, lints, errors, warnings, or notes.
  The sole fresh read-only reviewer echoed that identity, independently
  reproduced the focused correction and accepted adjacent contract suites, and
  returned `PASS` with no blocking findings or optional suggestions.

WP2-D implementation validation evidence:

- `testthat::test_local(filter = "contracts-preparation")` passes 97
  expectations covering preparation requirements, sources and artifacts, all
  ten result outcomes, complete process evidence, shared closures, unavailable
  and blocked packages, empty cohorts, locale and input-order invariance, and
  structural, semantic, normalization, relationship, and identity mutation.
- The corrected exact completion gate passes: documentation generation makes
  no tracked changes, Air and lintr are clean, all 384 tests pass without
  warnings or skips, and
  `devtools::check(document = FALSE, error_on = "note")` reports zero errors,
  warnings, or notes. Generated-file, build-artifact, whitespace, and diff
  audits are clean.
- The sole reviewer returned `NEEDS_CHANGES` on the initial target after
  reproducing acceptance of an impossible seconds component. The one allowed
  correction constrains clock components before calendar parsing and adds
  boundary regressions. After the compaction-required pause and owner go-ahead,
  the same reviewer echoed refreshed commit
  `a3b07e060b9ab4f0b6a5a11462a4c11a542edfd7`, tree
  `705aa92867840fd5ef74925347a9e2f84bcdaec0`, and parent
  `616d4008fa6cab86a930adedde9c84dcb2a18007`; independently passed all 97
  focused expectations and the timestamp boundary probes; and returned `PASS`
  with no blocking findings or optional suggestions.

WP2-E implementation validation evidence:

- `testthat::test_local(filter = "contracts-path")` passes 58 expectations
  covering exact path roles and policies, read-only construction, physical
  anchor normalization, disjoint roots, portable run IDs, derived-path safety,
  current-filesystem revalidation, deterministic identity, and mutation.
- The exact completion gate passes all 442 tests without warnings or skips;
  documentation and generated files are current, Air and lintr are clean, and
  package check reports zero errors, warnings, and notes.
- The sole reviewer echoed commit
  `ffa54983dce526e5bfdf13885b541b759ef19f0c`, tree
  `9622864d4b0fb8d52b892b91b81650a58208d4da`, and parent
  `5e22ecf4bdb71e4e4827e0e08df4c4523a17535e`; confirmed the
  exact artifact set and clean worktree; independently passed all 58 focused
  expectations plus alias and dangling-link probes; and returned `PASS` with no
  blocking findings or optional suggestions.

End-to-end acceptance:

- Discovery cannot read or write the preserved warehouse.
- The worker can read compatible artifacts but cannot mutate the warehouse.
- The frozen runner-equivalent dependency universe has exactly one structured
  preparation result per package, complete captured output per attempted
  process, no silently dropped unavailable package, verified installed
  versions, and successful isolated namespace loads before comparison begins.
- Preparation failures identify their root package and bounded external
  attribution; dependent cascades are reported as blocked. Retrying after
  external system-library remediation reuses every already validated artifact.
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

Preparation resumes only when the repository snapshot, dependency-universe
identity, source checksums, R executable, and compatibility lane still match.
Successful nodes are immutable inputs to the retry; failed and transitively
blocked nodes are the only eligible work unless the manifest identity changes.

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

Complete per-package preparation stdout and stderr, suggested system-package
commands, and agent or human annotations are run artifacts outside Git. The
versioned structured result records their paths and hashes so interpretation
can be repeated without making raw logs part of the repository.

## Outcomes & Retrospective

The repository boundary and implementation sequence are established. Work
Package 1 supplies accepted read-only observations, immutable per-root
inventories, and deterministic cross-root evidence without mutating or choosing
among source-cache artifacts. WP2-A adds accepted deterministic version-1
artifact and binary-lane identities. WP2-B adds accepted deterministic
repository-snapshot and direct/recursive-strong cohort identities, including
repository-priority selection and an explicit transitive-only classification,
and WP2-C adds the accepted deterministic stock-runner dependency-universe
identity with complete root-qualified edges and explicit install/exclusion
dispositions. Every constructor remains internal and all filesystem actions
remain in later chunks. WP2-D adds the accepted preparation-result and raw-
process-evidence identities, including complete log references, typed package
outcomes, dependency blocking, and strict timestamp semantics. WP2-E adds the
accepted runtime-root safety identity, including physically resolved disjoint
anchors, fixed durable descendants, immutable source inputs, and one exact
cleanup-eligible run root. WP2-F adds the accepted command-plan and typed-exit
catalog identities, including exact operation bindings, explicit dry-run
intent, current physical executable locators, and stable exit classifications.
The next step remains modest: freeze the append-only annotation contract before
turning the successful guarded manual procedure into a repeatable wrapper or
designing a replacement runner.
Update this section after each pilot with timings, compilation counts, cache-
manifest equality, result parity, and the fork decision.
