# revdeprunner 0.0.0.9000

* Preparation now saves each completed package and retains immutable binaries across removal of
  working libraries and historical logs. Readiness includes a fresh-process namespace load check.
  Download and released-subject installation failures return actionable preparation problems.
* Interrupted and timed-out comparisons resume unfinished targets while retaining complete pairs,
  including changed results. `repeat_checks = TRUE` repeats both sides after environmental repairs.
* Preparation and comparison timeouts are configurable without discarding compatible completed work.
  `verbose = TRUE` shows progress, and `result$changes` exposes added and removed check problems.
* Planning includes candidate hard dependencies and version constraints. Preparation counts include
  unique selected targets, and installation ordering uses only hard dependency edges.
* Saved state uses updated dependency and comparison identities. After updating, create a new plan
  and call `revdep_prepare()` again; compatible cached binaries remain eligible for reuse.
