# Only absent optional packages justify skipping stock integration tests.
# With dependencies installed, an incompatible toolchain must fail visibly.
skip_if_stock_tools_unavailable <- function() {
  for (package in c("revdepcheck", "crancache", "cranlike")) {
    testthat::skip_if_not_installed(package)
  }
  revdeprunner:::require_stock_adapter_tools()
  invisible(NULL)
}

# Capture evidence in the expectation before on.exit() removes fixture logs.
stock_test_failure_info <- function(result) {
  evidence <- c(
    paste("Comparison state:", result$summary$state),
    capture.output(print(result$results, row.names = FALSE)),
    capture.output(print(result$diagnostics, row.names = FALSE))
  )
  paste(evidence, collapse = "\n")
}

expect_stock_comparison_success <- function(result) {
  testthat::expect_true(
    identical(result$summary$state, "success"),
    info = stock_test_failure_info(result)
  )
}
