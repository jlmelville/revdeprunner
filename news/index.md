# Changelog

## revdeprunner 0.0.0.9000

- Cached preparation skips unchanged intermediate checkpoint writes.
  Record identities encode repeated metadata more efficiently while
  retaining their existing format. Verbose progress shows validation
  timings, processed cache entries, and the active comparison side,
  check stage, elapsed time, and log path.

- Incomplete comparisons retain a bounded runner error excerpt when
  per-side check details are missing. Integration test failures include
  this evidence before cleanup.

- Locally built binaries retain their known platform when R omits it
  from `Built`, including packages declaring `NeedsCompilation: yes`
  without installed native code. Archive validation failures now include
  the underlying metadata diagnostic.

- Preparation and comparison now support macOS alongside Linux,
  including native `.tgz` binaries, Homebrew and CRAN-style cache
  layouts, and resumable checks. Locally built pure-R binaries retain
  explicit platform identity; unlabeled external binaries are rebuilt
  rather than assigned to the host. Failed subject installation on Apple
  Silicon stops comparison and remains retryable.

- Preparation avoids revalidating unchanged repository metadata at every
  package checkpoint. Plan admission also avoids duplicate validation,
  while saved plans and reports remain fully validated.

- Comparisons restart interrupted subject installations in a fresh
  workspace, preserving preparation and leaving abandoned installation
  locks behind. Missing R-version display metadata no longer hides check
  problems in R-devel comparisons.

- Preparation isolates each installation attempt so abandoned R
  installation locks cannot block retry. Completed installations remain
  available to subsequent packages.

- Saved preparation uses adopted binary artifacts even after their
  original external cache directories are removed. Verbose preparation
  shows active log paths, budgets, and elapsed time.

- Preparation now saves each completed package and retains immutable
  binaries across removal of working libraries and historical logs.
  Readiness includes a fresh-process namespace load check. Download and
  released-subject installation failures return actionable preparation
  problems.

- Interrupted and timed-out comparisons resume unfinished targets while
  retaining complete pairs, including changed results.
  `repeat_checks = TRUE` repeats both sides after environmental repairs.

- Preparation and comparison timeouts are configurable without
  discarding compatible completed work. `verbose = TRUE` shows progress,
  and `result$changes` exposes added and removed check problems.

- Planning includes candidate hard dependencies and version constraints.
  Preparation counts include unique selected targets, and installation
  ordering uses only hard dependency edges.

- Saved state uses updated dependency and comparison identities. After
  updating, create a new plan and call
  [`revdep_prepare()`](https://jlmelville.github.io/revdeprunner/reference/revdep_prepare.md)
  again; compatible cached binaries remain eligible for reuse.
