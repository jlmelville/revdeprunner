# Architecture

`revdeprunner` freezes a dependency plan, prepares reusable binaries,
and compares reverse dependencies against released and development
versions of a package. The public operations are
[`revdep_plan()`](https://jlmelville.github.io/revdeprunner/reference/revdep_plan.md),
[`revdep_prepare()`](https://jlmelville.github.io/revdeprunner/reference/revdep_prepare.md),
and
[`revdep_check()`](https://jlmelville.github.io/revdeprunner/reference/revdep_check.md).

This guide locates changes and explains the boundaries they must
preserve. R files share one namespace: the divisions below express
responsibility, not enforced module isolation.

## Follow a public operation

| Operation | Coordinator | Route |
|----|----|----|
| Plan | [revdep-plan.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdep-plan.R) | Read package and repository metadata; freeze versions; discover targets and dependencies; estimate cache reuse. |
| Prepare | [revdep-prepare.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdep-prepare.R) | Admit a supplied plan or create one; read or create preparation state; acquire the baseline; prepare dependencies; check namespace loadability; save progress. |
| Check | [revdep-check.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdep-check.R) | Read saved preparation; admit its report; restore libraries and check loadability; identify candidate contents; reuse or resume a comparison through the stock adapter. |

Preparation’s **context** holds the frozen snapshot, cohort, dependency
universe, source plan, cache selection, compatibility lane, paths, and R
executable. Its **gate** holds accumulated acquisitions, builds,
attempts, and package outcomes. The context describes the work; the gate
records progress.

## Where changes belong

| Responsibility | Files | Reason for the division |
|----|----|----|
| Checkpoint identity, I/O, and saved-state admission | [revdep-checkpoints.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdep-checkpoints.R) | Owns checkpoint formats and the checks needed when reading them. Public coordinators choose when to save. |
| Current execution environment | [revdep-runtime.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdep-runtime.R) | Resolves storage roots, R compatibility, platform support, and public time budgets. |
| Public preparation and comparison presentation | [revdep-results.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdep-results.R) | Converts private evidence into summaries and problem tables; owns their print methods. |
| Candidate inputs | [candidate-dependencies.R](https://github.com/jlmelville/revdeprunner/blob/main/R/candidate-dependencies.R), [checkout-snapshot.R](https://github.com/jlmelville/revdeprunner/blob/main/R/checkout-snapshot.R) | Dependency declarations govern preparation; content fingerprints govern comparison reuse. Git metadata does not define candidate identity. |
| Frozen metadata and dependency rules | [contracts-cohort.R](https://github.com/jlmelville/revdeprunner/blob/main/R/contracts-cohort.R), [contracts-dependency.R](https://github.com/jlmelville/revdeprunner/blob/main/R/contracts-dependency.R) | Repository snapshots and target cohorts precede dependency closure. These files also own their identities and validators. |
| Artifact and path contracts | [contracts-artifact.R](https://github.com/jlmelville/revdeprunner/blob/main/R/contracts-artifact.R), [contracts-path.R](https://github.com/jlmelville/revdeprunner/blob/main/R/contracts-path.R), [artifact-files.R](https://github.com/jlmelville/revdeprunner/blob/main/R/artifact-files.R) | Record identity, filesystem ownership, and actual archive inspection are distinct checks. |
| Cache observation, selection, and reuse | [cache-observe.R](https://github.com/jlmelville/revdeprunner/blob/main/R/cache-observe.R), [cache-select.R](https://github.com/jlmelville/revdeprunner/blob/main/R/cache-select.R), [cache-reuse.R](https://github.com/jlmelville/revdeprunner/blob/main/R/cache-reuse.R) | Observation gathers facts without selecting a winner; selection applies compatibility rules; reuse binds the selected artifacts to a run. |
| Durable cache publication | [binary-cache.R](https://github.com/jlmelville/revdeprunner/blob/main/R/binary-cache.R), [source-cache.R](https://github.com/jlmelville/revdeprunner/blob/main/R/source-cache.R) | Publishes retained artifacts and repository indexes independently of working libraries. |
| Source inputs | [source-acquisition-plan.R](https://github.com/jlmelville/revdeprunner/blob/main/R/source-acquisition-plan.R), [source-acquire.R](https://github.com/jlmelville/revdeprunner/blob/main/R/source-acquire.R), [source-baseline.R](https://github.com/jlmelville/revdeprunner/blob/main/R/source-baseline.R) | Separates required source identity from obtaining archives. The subject’s released baseline is shared by preparation and comparison. |
| Package preparation | [preparation-gate.R](https://github.com/jlmelville/revdeprunner/blob/main/R/preparation-gate.R), [source-prepare.R](https://github.com/jlmelville/revdeprunner/blob/main/R/source-prepare.R) | The gate owns dependency order, blockers, and progress; source preparation owns an individual build/install attempt and its subprocess. |
| Preparation reports and readiness | [contracts-preparation.R](https://github.com/jlmelville/revdeprunner/blob/main/R/contracts-preparation.R), [preparation-loadability.R](https://github.com/jlmelville/revdeprunner/blob/main/R/preparation-loadability.R), [preparation-failures.R](https://github.com/jlmelville/revdeprunner/blob/main/R/preparation-failures.R) | Reports describe accumulated evidence; fresh-process loads establish current readiness; failure conversion produces actionable problems. |
| Stock adapter coordination | [stock-revdepcheck.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-revdepcheck.R) | Binds prepared inputs into an initialization, then combines execution and observations into a comparison result. |
| Stock workspace | [stock-workspace.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-workspace.R) | Owns copied checkouts, private libraries, paths, and cleanup of an unfinished initialization. |
| Stock repositories | [stock-repositories.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-repositories.R) | Projects frozen inputs into the binary/source repositories expected by stock tooling and verifies their contents. |
| Stock runtime | [stock-runtime.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-runtime.R) | Guards upstream tool provenance and private entry points, constructs the child environment, and launches comparison processes. |
| Stock database | [stock-database.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-database.R) | Observes stock progress and resets unfinished work for retry while retaining completed target pairs. |
| Stock outcomes | [stock-results.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-results.R) | Interprets old/new results, normalizes upstream metadata, and validates comparison evidence, including saved results. |
| Stock diagnostics | [stock-diagnostics.R](https://github.com/jlmelville/revdeprunner/blob/main/R/stock-diagnostics.R) | Extracts actionable failure details and log paths without changing outcome classification. |
| Progress and package metadata | [progress.R](https://github.com/jlmelville/revdeprunner/blob/main/R/progress.R), [revdeprunner-package.R](https://github.com/jlmelville/revdeprunner/blob/main/R/revdeprunner-package.R) | Progress messages and package documentation have no ownership of execution state. |

## Durable state and recovery

The data root retains checkpoints and source/binary artifacts. The runs
root contains working libraries, stock workspaces, and logs. Removing
working state must allow reconstruction from durable artifacts; it must
not force completed comparisons to run again. A saved successful load
does not prove that a binary still loads in the current environment.

``` mermaid
flowchart TD
    A[Admit saved preparation and current inputs] --> B[Restore libraries and verify loadability]
    B --> C{Matching comparison checkpoint?}
    C -->|Completed| D[Return saved comparison]
    C -->|Absent| E[Initialize stock workspace]
    C -->|Unfinished target checks| F[Resume unfinished pairs]
    C -->|Interrupted subject installation| G[Abandon old workspace and initialize afresh]
    E --> H[Run comparison and save evidence]
    F --> H
    G --> H
```

`repeat_checks = TRUE` explicitly starts a fresh comparison. Candidate
content, preparation artifacts, or comparison environment changes select
a different comparison identity. An interrupted subject installation can
be restarted in a fresh workspace only after checking that no completed
pair would be discarded. Checkpoint publication uses a temporary file
and rename.

These contracts are exercised by [public workflow
tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-revdep-run.R),
[comparison
recovery](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-comparison-recovery.R),
[installation
recovery](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-installation-recovery.R),
and [loadability
tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-binary-loadability.R).

## Validation ownership

Validation belongs where ownership or trust changes. This includes
public API input, saved state, external metadata, mutable files, and
subprocess output. Once immutable context has been admitted, ordinary
internal transformations should use it without repeating its full
validation.

| Boundary | Admission and evidence |
|----|----|
| Public arguments and supplied plans | Public coordinators validate controls. `validate_public_revdep_plan()` admits a caller-supplied plan and its frozen metadata before request creation. [Corruption tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-revdep-run.R) protect this route. |
| Saved preparation | `validate_revdep_prepare_state()` checks checkpoint structure and bindings. It is not a full context validator. Preparation then admits context and prior gate evidence through `prepare_dependency_universe()`; checking explicitly calls `validate_preparation_report()` before restoration or loadability. [Comparison admission tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-comparison-admission.R) protect the latter ordering. |
| Saved comparison | `validate_revdep_check_state()` checks identity and result evidence. Completed-result reuse validates durable evidence without requiring historical workspaces or logs to exist. Resuming a worker additionally validates its live initialization. |
| Archives and filesystem effects | Archive readers check actual payload and checksums. Path checks precede publication, installation, and owned-workspace cleanup. Rechecking here can be necessary because external files can change after earlier admission. See [archive tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-cache-observe.R) and [path tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-contracts-path.R). |
| Child processes and upstream tooling | Process status, database observations, library isolation, runtime provenance, and returned results are checked at the adapter boundary. See [stock adapter tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-stock-revdepcheck.R) and [metadata normalization tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-comparison-metadata.R). |
| Newly accumulated progress | Report construction validates changing rows, artifact references, and identities. It relies on already admitted immutable context. [Report tests](https://github.com/jlmelville/revdeprunner/blob/main/tests/testthat/test-contracts-preparation.R) protect consistency. |

Validation is not yet perfectly factored. `validate_preparation_gate()`
combines context and gate validation; both descend into frozen metadata
validation. Some composite constructors, such as
`new_dependency_universe()`, also validate their newly constructed
records. These are candidates for further boundary consolidation, not
independent trust boundaries. Before removing a call, trace all
saved-state readers and test corruption rejection through each supported
route. A constructor must not accidentally become the only validator
protecting a restored object.

## Why retain a helper?

A single caller is a review prompt. Keep a helper when it owns a
coherent operation, contains a substantial branch, protects a protocol,
or scopes cleanup. For example:

- `recover_stock_initialization_workspace()` owns path validation and
  destructive cleanup.
- `stock_adapter_with_options()` owns restoration on normal return and
  error; inlining it would change the lifetime of temporary options
  unless the caller were carefully restructured.
- `read_tar_description()` has separate member parsing, checksum, and
  byte-reading helpers so archive validation remains understandable.
- The repository-query and download wrappers provide controlled external
  I/O seams for fixture tests.

Inline argument-forwarding wrappers and single-use literal providers
when the caller remains clear. Allowed stages and outcomes now appear
beside their validators, and comparison fingerprinting calls
`checkout_identity()` directly. Private tests should protect the
underlying behavior rather than require a redundant wrapper to survive.

## Keep this guide useful

Update the ownership row and relevant route or boundary paragraph in the
same change that moves a responsibility.
`Rscript scripts/check-architecture.R`, also run by the lint script,
checks that every R source file is listed and local links resolve. It
catches file-map drift; it cannot establish that the responsibility
descriptions are still true.

Use these questions when reviewing a change:

1.  Can its owner be named in one row, and can the public route still be
    followed without guessing?
2.  Does a new helper remove a meaningful chunk of reasoning, or merely
    add another name to follow?
3.  What changed trust at each validation call? Could admitted immutable
    data reach this point directly?
4.  Which state must survive an interruption, and which component alone
    decides its recovery?
5.  Does the change require the same policy to be edited in several
    owners? If so, is ownership wrong?

The broad preparation context and the large preparation/report contract
files remain design pressure points. So do shared contract utilities in
`contracts-cohort.R` and subprocess machinery currently owned by
`source-prepare.R` but used by the stock runtime. If a change repeatedly
crosses these responsibilities, reconsider that ownership. Splitting
files makes this coupling visible; it does not remove it. File count,
call count, and validator count are navigation aids, not quality scores.
