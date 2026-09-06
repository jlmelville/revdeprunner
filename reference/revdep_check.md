# Run reverse-dependency comparisons

Verify a completed preparation and compare the frozen CRAN baseline with
the current development checkout using the package's guarded stock
`revdepcheck` adapter.

## Usage

``` r
revdep_check(prepared)
```

## Arguments

- prepared:

  A ready object returned by
  [`revdep_prepare()`](https://jlmelville.github.io/revdeprunner/reference/revdep_prepare.md).

## Value

A `revdep_result` object with `summary`, `results`, `diagnostics`, the
frozen `plan`, and advanced `evidence`. `summary$elapsed_seconds`
measures the stock comparison adapter, excluding stock initialization.

## Examples

``` r
if (FALSE) { # \dontrun{
prepared <- revdep_prepare("/path/to/package")
if (nrow(prepared$problems) == 0L) {
  result <- revdep_check(prepared)
  result$results
}
} # }
```
