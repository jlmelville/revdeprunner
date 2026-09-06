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
  verbose = FALSE
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
  derive a budget from preparation evidence.

- process_timeout_seconds:

  Positive whole seconds allowed for the entire stock comparison
  process, including subject installation. Defaults to 7200. Also bounds
  each library restoration or loadability subprocess during admission.
  Increasing either budget allows unfinished targets another attempt
  without invalidating completed comparisons.

- verbose:

  Show phase, package, and completion progress. Defaults to `FALSE`.

## Value

A `revdep_result` object with `summary`, `results`, `changes`,
`diagnostics`, the frozen `plan`, and advanced `evidence`.
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
`repeat_checks = TRUE` to run both sides again. Changing candidate hard
dependencies or constraints requires a new plan and preparation;
ordinary source edits can reuse preparation.

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
