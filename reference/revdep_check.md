# Run reverse-dependency comparisons

Verify a completed preparation and compare the frozen CRAN baseline with
the current development checkout using the package's guarded stock
`revdepcheck` adapter.

## Usage

``` r
revdep_check(
  prepared,
  repeat_checks = FALSE,
  worker_timeout_seconds = NULL,
  process_timeout_seconds = 7200L,
  verbose = FALSE,
  retry_targets = NULL,
  prior_timings = NULL
)
```

## Arguments

- prepared:

  A ready object returned by
  [`revdep_prepare()`](https://jlmelville.github.io/revdeprunner/reference/revdep_prepare.md).

- repeat_checks:

  Repeat both baseline and candidate checks, including completed
  comparisons. Use after repairing external libraries. Compatible
  prepared binaries are retained. Defaults to `FALSE`, which resumes
  unfinished targets and reuses completed changed and unchanged
  comparisons.

- worker_timeout_seconds:

  Positive whole seconds allowed per stock check worker, or `NULL` to
  derive a budget from preparation evidence. The automatic budget is at
  least 600 seconds, or 1800 seconds if a requested target declares
  compilation and lacks a successful preparation build time.

- process_timeout_seconds:

  Positive whole seconds allowed for the entire stock comparison
  process, including subject installation. Defaults to 7200. Also bounds
  each library restoration or loadability subprocess during admission.
  Increasing either budget allows unfinished targets another attempt
  without invalidating completed comparisons.

- verbose:

  Show phase, package, and completion progress. Defaults to `FALSE`.

- retry_targets:

  Optional exact names of selected targets. Runs both sides in a
  separate workspace and checkpoint for this set of names. Other targets
  appear as `not_checked`. Repeat the same call to resume.

- prior_timings:

  Optional data frame of observed check durations with `package`,
  `version`, `side` (old or new), and numeric `seconds`, such as a
  previous result's `timings`. By default retries use available timings
  from the same comparison. With one worker, the runner warns when
  observed durations for pending sides already exhaust the overall
  process budget. Use `verbose = TRUE` to see matching observations
  below that threshold.

## Value

A `revdep_result` object with `summary`, `results`, `changes`,
`diagnostics`, `test_failures`, `timings`, the frozen `plan`, and
advanced `evidence`. When both sides have an ERROR and recognizable
testthat failures, `test_failures` lists added or removed locations and
both failure logs. Stock determines the comparison outcome. `timings`
records observed seconds and available stock library inventory paths per
target and side. Older checkpoints may lack this evidence.
`summary$elapsed_seconds` measures the stock comparison adapter,
excluding stock initialization. `changes` is a data frame with
`package`, `severity` (`error`, `warning`, `note`), `change` (`added`,
`removed`), and `message`, using stock's normalized comparison. These
details persist in the saved result. Stock calls a pair `unchanged` when
it has no new problems, so it may still have removed problems.

## Details

Ordinary retries retain complete pairs and retry unfinished pairs. Keep
the run directory to resume an unfinished comparison. Completed results
can be reused without the old comparison workspace or logs. Binary
admission checks the current R major/minor version, platform,
architecture, and OS tag; comparison identity also includes the full R
version and candidate source. After other environmental repairs, use
`repeat_checks = TRUE` to run both sides again, or pass `retry_targets`
for a separate paired comparison of specific packages. A forced repeat
saves the previous result checkpoint under
`evidence$previous_checkpoint`. The latest invocation records its
explicit `R_MAKEVARS_USER` override in `evidence$makevars`. Changing
candidate hard dependencies or constraints requires a new plan and
preparation; ordinary source edits can reuse preparation.

## Examples

``` r
if (FALSE) { # \dontrun{
prepared <- revdep_prepare("/path/to/package")
if (nrow(prepared$problems) == 0L) {
  result <- revdep_check(prepared, verbose = TRUE)
  result$results
  result$changes
}
} # }
```
